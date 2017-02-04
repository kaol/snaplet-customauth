CREATE OR REPLACE FUNCTION set_bookmark(in_uid integer, in_cid integer, in_ord integer, in_subord integer, OUT total_new integer, OUT new_in integer) AS $$
BEGIN
  IF (SELECT (in_uid, in_cid) IN (SELECT uid, cid FROM subscriptions)) THEN
    UPDATE subscriptions SET ord=in_ord, subord=in_subord WHERE uid=in_uid AND cid=in_cid;
  ELSE
    INSERT INTO subscriptions (uid, cid, ord, subord) values (in_uid, in_cid, in_ord, in_subord);
  END IF;
  SELECT COALESCE(SUM(num), 0), COUNT(*) from comic_remain_frag(in_uid) JOIN comics USING (cid) WHERE num > 0 INTO total_new, new_in;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION set_bookmark(in_uid integer, in_cid integer, start_at_first boolean, OUT total_new integer, OUT new_in integer) AS $$
DECLARE
  was_subscribed boolean;
  new_ord integer;
  new_subord integer;
BEGIN
  SELECT (in_uid, in_cid) IN (SELECT uid, cid FROM subscriptions) INTO was_subscribed;
  IF start_at_first THEN
    new_ord := 0;
    new_subord := 0;
  ELSE
    SELECT ord, COALESCE((SELECT MAX(subord)+1 FROM page_fragments
	WHERE page_fragments.cid=in_cid AND ord=updates.ord), 1)
      FROM updates WHERE cid=in_cid ORDER BY ord DESC LIMIT 1 INTO new_ord, new_subord;
  END IF;
  IF was_subscribed THEN
    UPDATE subscriptions set ord=new_ord, subord=new_subord WHERE cid=in_cid AND uid=in_uid;
  ELSE
    INSERT INTO subscriptions (uid, cid, ord, subord) VALUES (in_uid, in_cid, new_ord, new_subord);
  END IF;
  SELECT COALESCE(SUM(num), 0), COUNT(*) FROM comic_remain_frag(in_uid) JOIN comics USING (cid) WHERE num > 0 INTO total_new, new_in;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION unset_bookmark(in_uid integer, in_cid integer, OUT total_new integer, OUT new_in integer) AS $$
BEGIN
  DELETE FROM subscriptions WHERE cid=in_cid AND uid=in_uid;
  SELECT COALESCE(SUM(num), 0), COUNT(*) FROM comic_remain_frag(in_uid) JOIN comics USING (cid) WHERE num > 0 INTO total_new, new_in;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION bookmark_and_log(text, boolean, inet, integer) RETURNS TABLE(v_cid integer, v_ord integer, v_subord integer, v_at_max boolean) AS $$
BEGIN
CREATE TEMPORARY table _results (cid int, ord int, subord int, at_max bool);
INSERT INTO _results SELECT * FROM bookmark($1, $2);
IF (SELECT COUNT(*) FROM _results) = 1 THEN
  INSERT INTO bookmarking_log (at_date, url, want_here, host, uid, cid, ord, subord, is_at_max) SELECT NOW(), $1, $2, $3::inet, $4, * FROM _results;
ELSE
  INSERT INTO bookmarking_log (at_date, url, want_here, host, uid) VALUES (NOW(), $1, $2, $3, $4);
END IF;
RETURN QUERY SELECT * FROM _results;
DROP TABLE _results;
END;
$$ LANGUAGE plpgsql;
