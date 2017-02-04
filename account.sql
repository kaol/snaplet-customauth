create extension pgcrypto;

DROP FUNCTION auth_login(text,text);
DROP FUNCTION do_login(integer);
DROP FUNCTION recover_session(uuid);

CREATE OR REPLACE FUNCTION auth_create_password(password text, OUT lmid int) AS $$
BEGIN
INSERT INTO login_method_passwd (hash) VALUES (crypt(password, gen_salt('bf', 13))) RETURNING login_method_passwd.lmid INTO lmid;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION auth_create(name text, email text, lmid int, OUT uid integer, OUT p_session uuid, OUT csrf_ham uuid) AS $$
BEGIN
  IF NOT EXISTS(SELECT 1 FROM users WHERE lower(users.name)=lower(auth_create.name)) THEN
    INSERT INTO users (name, email) VALUES (auth_create.name, auth_create.email)
      RETURNING users.uid INTO uid;
    INSERT INTO user_login (uid, lmid) VALUES (auth_create.uid, auth_create.lmid);
    SELECT do_login.p_session, do_login.csrf_ham FROM do_login(uid) INTO p_session, csrf_ham;
  ELSE
    RAISE 'Duplicate user name' USING ERRCODE = 'unique_violation';
  END IF;
END;
$$ LANGUAGE plpgsql;

-- Log the user in using name and cleartext password
CREATE OR REPLACE FUNCTION auth_login(name text, password text) RETURNS TABLE (uid integer, p_session uuid, csrf_ham uuid) AS $$
DECLARE
  pw_matches boolean;
  pw_convert boolean;
  old_hash character(32);
  lmid int;
BEGIN
  SELECT users.uid, crypt(auth_login.password, hash)=hash, passwd
    INTO auth_login.uid, pw_matches, old_hash
    FROM users LEFT JOIN (user_login JOIN login_method_passwd USING (lmid)) USING (uid)
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
      SELECT auth_create_password.lmid INTO lmid FROM auth_create_password(auth_login.password);
      INSERT INTO user_login (uid, lmid) VALUES (auth_login.uid, lmid);
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
-- TODO: precalculate, this adds 30ms (on my dev box) to every page access
  SELECT COALESCE(SUM(num), 0), COUNT(*) from comic_remain_frag(uid) JOIN comics USING (cid) WHERE num > 0 INTO total_new, new_in;
  SELECT COUNT(*) INTO new_comics FROM comics, users WHERE users.uid=get_and_update_stats.uid AND comics.added_on > users.seen_comics_before;
END;
$$ LANGUAGE plpgsql;

CREATE TABLE user_login (
  uid int NOT NULL REFERENCES users (uid) ON DELETE CASCADE,
  lmid int NOT NULL,
  UNIQUE (uid, lmid)
);

CREATE OR REPLACE FUNCTION must_have_login2() RETURNS trigger AS $$
BEGIN
  IF NOT OLD.uid IN (SELECT uid FROM user_login JOIN login_method ON (lmid NOT IN (SELECT lmid FROM login_method_temp_hash))) THEN
    RAISE EXCEPTION 'Account % with no login credentials', OLD.name USING ERRCODE = 'PI001';
    RETURN NULL;
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER must_have_login2 AFTER UPDATE OR DELETE ON user_login FOR EACH ROW EXECUTE PROCEDURE must_have_login2();

CREATE SEQUENCE lmid_seq;

CREATE TABLE login_method (
  lmid int NOT NULL PRIMARY KEY DEFAULT nextval('lmid_seq'),
  stamp timestamp DEFAULT now()
);

CREATE TABLE login_method_passwd (
  hash char(60) NOT NULL,
) INHERITS login;

CREATE TABLE login_method_temp_hash (
  temp_hash uuid NOT NULL
);
