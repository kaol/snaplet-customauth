create extension pgcrypto;

-- Log the user in using name and cleartext password
CREATE OR REPLACE FUNCTION auth_login(in_name text, in_password text, OUT o_uid integer, OUT new_comics integer, OUT p_session uuid, OUT csrf_ham uuid) AS $$
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
    SELECT * FROM do_login(tmp_uid) INTO o_uid, new_comics, p_session, csrf_ham;
  END IF;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION auth_create(in_name text, in_email text, in_password text, OUT o_uid integer, OUT new_comics integer, OUT p_session uuid, OUT csrf_ham uuid) AS $$
BEGIN
  IF NOT EXISTS(SELECT 1 FROM users WHERE lower(name)=lower(in_name)) THEN
    INSERT INTO users (name, email, hash) VALUES (in_name, in_email, crypt(in_password, gen_salt('bf', 13))) RETURNING uid INTO o_uid;
    SELECT * FROM do_login(o_uid) INTO o_uid, new_comics, p_session, csrf_ham;
  END IF;
END;
$$ LANGUAGE plpgsql;
