create extension pgcrypto;

create type integerpair as ("1" integer, "2" integer);

DROP FUNCTION auth_login(text,text);
DROP FUNCTION do_login(integer);
DROP FUNCTION recover_session(uuid);

CREATE OR REPLACE FUNCTION auth_create(in_name text, in_email text, in_password text, OUT o_uid integer, OUT p_session uuid, OUT csrf_ham uuid) AS $$
BEGIN
  IF NOT EXISTS(SELECT 1 FROM users WHERE lower(name)=lower(in_name)) THEN
    INSERT INTO users (name, email, hash) VALUES (in_name, in_email, crypt(in_password, gen_salt('bf', 13))) RETURNING uid INTO o_uid;
    SELECT o_uid, p_session, csrf_ham FROM do_login(o_uid) INTO o_uid, p_session, csrf_ham;
  END IF;
END;
$$ LANGUAGE plpgsql;

-- Log the user in using name and cleartext password
CREATE OR REPLACE FUNCTION auth_login(in_name text, in_password text, OUT o_uid integer, OUT p_session uuid, OUT csrf_ham uuid) AS $$
DECLARE
  tmp_uid integer;
  pw_matches boolean;
  pw_convert boolean;
  old_hash character(32);
BEGIN
  SELECT uid, crypt(in_password, hash)=hash, passwd FROM users WHERE LOWER(in_name)=LOWER(name) INTO tmp_uid, pw_matches, old_hash;
  IF pw_matches IS NULL AND old_hash IS NOT NULL THEN
    -- Check for old md5 hash
    BEGIN
      SELECT md5(convert_to(in_password, 'latin1'))=old_hash INTO pw_matches;
    EXCEPTION WHEN untranslatable_character THEN
      SELECT md5(in_password)=old_hash INTO pw_matches;
    END;
    -- Convert to new hash
    pw_convert := TRUE;
  END IF;
  IF pw_matches THEN
    IF pw_convert THEN
      UPDATE users SET passwd=null, hash=crypt(in_password, gen_salt('bf', 13)) where uid=tmp_uid;
    END IF;
    SELECT * FROM do_login(tmp_uid) INTO o_uid, p_session, csrf_ham;
  END IF;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION recover_session(session uuid, OUT o_uid integer, OUT o_name text, OUT new_comics integer, OUT total_new integer, OUT new_in integer, OUT csrf_ham uuid) AS $$
BEGIN
  SELECT uid, name FROM p_session JOIN users USING (uid) INTO o_uid, o_name WHERE ses = session AND token_for IS NULL;
  IF o_uid IS NOT NULL THEN
    UPDATE p_session SET last_active = NOW() WHERE ses=session;
    IF (select last_active < NOW() - time '0:10' FROM users WHERE uid=o_uid) THEN
      UPDATE users SET seen_comics_before = COALESCE(last_active,NOW()), last_active = NOW() WHERE o_uid=uid;
    ELSE
      UPDATE users SET last_active = NOW() WHERE o_uid=uid;
    END IF;
    SELECT ses FROM p_session WHERE token_for=session INTO csrf_ham;
  END IF;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION do_login(in_uid integer, OUT o_uid integer, OUT new_comics integer, OUT total_new integer, OUT new_in integer, OUT p_session uuid, OUT csrf_ham uuid) AS $$
BEGIN
  UPDATE users SET last_login = NOW() WHERE uid=in_uid;
  SELECT token INTO p_session FROM generate_session(in_uid, null);
  SELECT token INTO csrf_ham FROM generate_session(in_uid, p_session);
-- TODO: precalculate, this adds 30ms (on my dev box) to every page access
  o_uid := in_uid;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION get_and_update_stats(in_uid integer, is_login boolean, OUT new_comics integer, OUT total_new integer, OUT new_in integer) AS $$
BEGIN
  IF is_login THEN
    UPDATE users SET seen_comics_before = COALESCE(last_active, NOW()) WHERE in_uid=uid;
-- TODO: precalculate, this adds 30ms (on my dev box) to every page access
  END IF;
  SELECT SUM(num), COUNT(*) from comic_remain_frag(in_uid) JOIN comics USING (cid) WHERE num > 0 INTO total_new, new_in;
  SELECT COUNT(*) INTO new_comics FROM comics, users WHERE users.uid=in_uid AND comics.added_on > users.seen_comics_before;
END;
$$ LANGUAGE plpgsql;
