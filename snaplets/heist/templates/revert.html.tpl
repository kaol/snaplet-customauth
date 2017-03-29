<h:piperka>
  <h2>Revert updates</h2>
  <h:ifLoggedIn>
    Piperka can't keep track of whether our redirects were successful.
    We keep records of the redirects done in the last 24 hours to
    facilitate reverting the bookmarks.
    <h:csrfForm method="post" action="updates.html">
      <ul style="recent">
	<h:recent>
	  <li>
	    <input type="checkbox" name="revert" h:id="id"/>
	    <label h:id="for"><h:name/></label>
	  </li>
	</h:recent>
      </ul>
      <input type="submit" name="do" value="Revert"/>
    </h:csrfForm>
  </h:ifLoggedIn>
  <h:ifLoggedOut>
    This page would list your recently used redirects and allow you to
    revert them if you were logged on.
  </h:ifLoggedOut>
</h:piperka>
