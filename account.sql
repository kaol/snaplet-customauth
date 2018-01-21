create extension pgcrypto;

SET ROLE piperka;

create type integerpair as ("1" integer, "2" integer);

DROP FUNCTION auth_login(text,text);
DROP FUNCTION do_login(integer);
DROP FUNCTION recover_session(uuid);

CREATE OR REPLACE FUNCTION auth_create_password(password text, uid int) RETURNS int AS $$
  INSERT INTO login_method_passwd (uid, hash)
    VALUES (auth_create_password.uid, crypt(password, gen_salt('bf', 13)))
    ON CONFLICT (uid) DO UPDATE SET hash=EXCLUDED.hash, stamp=now()
    RETURNING login_method_passwd.lmid;
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION auth_create(name text, email text, lmid int, OUT uid integer, OUT p_session uuid, OUT csrf_ham uuid) AS $$
BEGIN
  INSERT INTO users (name, email, initial_lmid) VALUES (auth_create.name, auth_create.email, auth_create.lmid)
    RETURNING users.uid INTO uid;
  SELECT do_login.p_session, do_login.csrf_ham FROM do_login(uid) INTO p_session, csrf_ham;
END;
$$ LANGUAGE plpgsql;

-- Log the user in using name and cleartext password
CREATE OR REPLACE FUNCTION auth_login(name text, password text) RETURNS TABLE (uid integer, p_session uuid, csrf_ham uuid) AS $$
DECLARE
  pw_matches boolean;
  pw_convert boolean;
  old_hash character(32);
BEGIN
  SELECT users.uid, crypt(auth_login.password, hash)=hash, passwd
    INTO auth_login.uid, pw_matches, old_hash
    FROM users LEFT JOIN login_method_passwd USING (uid)
    WHERE LOWER(auth_login.name)=LOWER(users.name);
  IF pw_matches IS NULL AND old_hash IS NOT NULL THEN
    -- Check for old md5 hash
    BEGIN
      SELECT md5(convert_to(auth_login.password, 'latin1'))=old_hash INTO pw_matches;
    EXCEPTION WHEN untranslatable_character THEN
      SELECT md5(auth_login.password)=old_hash INTO pw_matches;
    END;
    -- Convert to new hash
    pw_convert := TRUE;
  END IF;
  IF pw_matches THEN
    IF pw_convert THEN
      PERFORM auth_create_password(auth_login.password, auth_login.uid);
      UPDATE users set passwd=null WHERE users.uid=auth_login.uid;
    END IF;
    RETURN QUERY SELECT auth_login.uid, do_login.p_session, do_login.csrf_ham FROM do_login(auth_login.uid);
  END IF;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION recover_session(session uuid)
RETURNS TABLE (uid integer, csrf_ham uuid) AS $$
BEGIN
  SELECT users.uid FROM p_session JOIN users USING (uid) INTO recover_session.uid WHERE ses = session AND token_for IS NULL;
  IF uid IS NOT NULL THEN
    UPDATE p_session SET last_active = NOW() WHERE ses=session;
    UPDATE users SET last_active = NOW() WHERE users.uid=recover_session.uid;
    SELECT ses FROM p_session WHERE token_for=session INTO csrf_ham;
    RETURN QUERY SELECT uid, csrf_ham;
  END IF;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION auth_oauth2(opid integer, token text) RETURNS TABLE (uid integer, p_session uuid, csrf_ham uuid) AS $$
BEGIN
  SELECT users.uid INTO auth_oauth2.uid
    FROM users JOIN login_method_oauth2 USING (uid)
    WHERE login_method_oauth2.identification = auth_oauth2.token
    AND login_method_oauth2.opid = auth_oauth2.opid;
  IF auth_oauth2.uid IS NOT NULL THEN
    RETURN QUERY SELECT auth_oauth2.uid, do_login.p_session, do_login.csrf_ham FROM do_login(auth_oauth2.uid);
  END IF;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION do_login(uid integer, OUT p_session uuid, OUT csrf_ham uuid) AS $$
BEGIN
  UPDATE users SET last_login = NOW() WHERE users.uid=do_login.uid;
  SELECT token INTO p_session FROM generate_session(uid, null);
  SELECT token INTO csrf_ham FROM generate_session(uid, p_session);
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION get_and_update_stats(uid integer, is_login boolean, OUT new_comics integer, OUT total_new integer, OUT new_in integer) AS $$
BEGIN
  IF is_login THEN
    UPDATE users SET seen_comics_before = COALESCE(last_active, NOW()) WHERE users.uid=get_and_update_stats.uid;
  ELSE
    IF (select last_active < NOW() - time '0:10' FROM users WHERE users.uid=get_and_update_stats.uid) THEN
      UPDATE users SET seen_comics_before = COALESCE(last_active,NOW()) WHERE o_uid=uid;
    END IF;
  END IF;
  SELECT * FROM user_unread_stats(uid) INTO total_new, new_in;
  SELECT COUNT(*) INTO new_comics FROM comics, users WHERE users.uid=get_and_update_stats.uid AND comics.added_on > users.seen_comics_before;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION user_unread_stats(uid integer, OUT total_new integer, OUT new_in integer) AS $$
BEGIN
  SELECT user_stats_cache.total_new, user_stats_cache.new_in FROM user_stats_cache WHERE user_stats_cache.uid=user_unread_stats.uid INTO total_new, new_in;
  IF total_new IS NULL THEN
    SELECT COALESCE(SUM(num), 0), COUNT(*) from comic_remain_frag(uid) JOIN comics USING (cid) WHERE num > 0 INTO total_new, new_in;
    INSERT INTO user_stats_cache (uid, total_new, new_in) VALUES (uid, total_new, new_in) ON CONFLICT ON CONSTRAINT user_stats_cache_pkey DO UPDATE SET total_new=EXCLUDED.total_new, new_in=EXCLUDED.new_in;
  END IF;
END;
$$ LANGUAGE plpgsql;

CREATE TABLE user_stats_cache (
  uid int NOT NULL PRIMARY KEY REFERENCES users (uid) ON DELETE CASCADE,
  total_new int NOT NULL,
  new_in int NOT NULL
);

CREATE OR REPLACE FUNCTION invalidate_user_stats_uid() RETURNS trigger AS $$
BEGIN
  IF TG_OP IN ('INSERT', 'UPDATE') THEN
    DELETE FROM user_stats_cache WHERE NEW.uid=uid;
  ELSIF TG_OP IN ('DELETE', 'TRUNCATE') THEN
    DELETE FROM user_stats_cache WHERE OLD.uid=uid;
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER invalidate_user_stats_subscriptions AFTER DELETE OR INSERT OR UPDATE ON subscriptions FOR EACH ROW EXECUTE PROCEDURE invalidate_user_stats_uid();

CREATE OR REPLACE FUNCTION invalidate_user_stats_cid() RETURNS trigger AS $$
BEGIN
  IF TG_OP IN ('INSERT', 'UPDATE') THEN
    DELETE FROM user_stats_cache WHERE uid IN (SELECT uid FROM subscriptions WHERE cid=NEW.cid);
  ELSIF TG_OP IN ('DELETE', 'TRUNCATE') THEN
    DELETE FROM user_stats_cache WHERE uid IN (SELECT uid FROM subscriptions WHERE cid=OLD.cid);
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER invalidate_user_stats_updates AFTER DELETE OR INSERT ON updates FOR EACH ROW EXECUTE PROCEDURE invalidate_user_stats_cid();
CREATE TRIGGER invalidate_user_stats_updates AFTER DELETE ON comics FOR EACH ROW EXECUTE PROCEDURE invalidate_user_stats_cid();

CREATE OR REPLACE FUNCTION must_have_login2() RETURNS trigger AS $$
DECLARE
  n text;
BEGIN
  IF OLD.uid IN (SELECT uid FROM users) AND NOT EXISTS (SELECT 1 FROM login_method AS lm WHERE OLD.uid = lm.uid AND OLD.lmid <> lm.lmid) THEN
    n := (SELECT name FROM users WHERE uid=OLD.uid);
    RAISE EXCEPTION 'Account % with no login credentials %', n, old.uid USING ERRCODE = 'PI001';
    RETURN NULL;
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER must_have_login2 AFTER DELETE ON login_method_passwd FOR EACH ROW EXECUTE PROCEDURE must_have_login2();
CREATE TRIGGER must_have_login2 AFTER DELETE ON login_method_oauth2 FOR EACH ROW EXECUTE PROCEDURE must_have_login2();

CREATE OR REPLACE FUNCTION prune_null_oauth2_login() RETURNS trigger AS $$
BEGIN
  DELETE FROM login_method_oauth2 WHERE uid IS NULL AND opid = NEW.opid AND identification = NEW.identification;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER prune_null_oauth2_login BEFORE INSERT ON login_method_oauth2 FOR EACH ROW EXECUTE PROCEDURE prune_null_oauth2_login();

CREATE SEQUENCE lmid_seq;

CREATE TABLE login_method (
  lmid int NOT NULL PRIMARY KEY DEFAULT nextval('lmid_seq'),
  uid int,
  stamp timestamp DEFAULT now()
);

CREATE TABLE login_method_passwd (
  hash char(60) NOT NULL,
  UNIQUE (uid)
) INHERITS (login_method);

ALTER TABLE pwdgen_hash ADD COLUMN stamp timestamp DEFAULT now();

CREATE TABLE login_method_oauth2 (
  opid int NOT NULL,
  identification text NOT NULL,
  UNIQUE (uid, opid),
  UNIQUE (opid, identification)
) INHERITS (login_method);

CREATE OR REPLACE FUNCTION create_recovery_key(name text, email text, hash character(32)) RETURNS boolean AS $$
DECLARE
  _uid integer;
BEGIN
  _uid := (SELECT users.uid FROM users WHERE LOWER($1)=LOWER(users.name) AND users.email=$2);
  IF _uid IS NULL THEN
    RETURN FALSE;
  END IF;
  DELETE FROM pwdgen_hash WHERE pwdgen_hash.uid=_uid;
  INSERT INTO pwdgen_hash (uid, hash) VALUES (_uid, hash);
  RETURN TRUE;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION reset_user_password(name text, hash character(32), password text) RETURNS boolean AS $$
<<func>>
DECLARE
  uid integer;
BEGIN
  uid := (SELECT users.uid FROM users JOIN pwdgen_hash USING (uid)
       WHERE lower(users.name)=lower($1) AND pwdgen_hash.hash = $2
       AND pwdgen_hash.stamp > now() - ('1 day'::interval));
  IF uid IS NULL THEN
    RETURN FALSE;
  END IF;
  DELETE FROM login_method_passwd WHERE login_method_passwd.uid=func.uid;
  PERFORM auth_create_password($3, uid);
  DELETE FROM pwdgen_hash WHERE pwdgen_hash.uid=func.uid;
  RETURN TRUE;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION initial_user_login() RETURNS trigger AS $$
BEGIN
  IF NEW.initial_lmid IS NULL THEN
    RAISE 'initial_lmid undefined for new user';
  END IF;
  UPDATE login_method SET uid=NEW.uid WHERE lmid=NEW.initial_lmid;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER initial_user_login AFTER INSERT ON users FOR EACH ROW EXECUTE PROCEDURE initial_user_login();

DROP VIEW alphabet_index;

CREATE MATERIALIZED VIEW alphabet_index AS
 SELECT alphabets.letter,
    count(*) AS ord
   FROM (alphabets
     JOIN ( SELECT ordering_form(comics.title) AS canon
           FROM comics) s ON ((lower((alphabets.letter)::text) > s.canon)))
  GROUP BY alphabets.letter
  ORDER BY alphabets.letter;
