<h:piperka>
  <h:ifLoggedOut>
    <h2>Why am I seeing this page?</h2>
    <p>
      You are most likely a webcomic author following site statistics.
      This page has bookmark links for logged in users.

      Check the <a href="help.html">help</a> or

      <a href="example.html">example</a> page to see what Piperka is
      about.
    </p>
  </h:ifLoggedOut>
  <h:ifLoggedIn>
    <h2>Set bookmark</h2>
    <form method="post" action="updates.html">
      <input type="hidden" name="csrf_ham" value="${h:csrf}"/>
      <p>URL: <input type="text" name="bookmark"/>
	<button name="action" value="set_bookmark">Set</button>
	<label for="wantbookmarkhere">Set bookmark here</label>
	<input type="checkbox" id="wantbookmarkhere" name="wantbookmarkhere"/>
	<a href="help.html#bookmarkhere" rel="help">?</a>
      </p>
    </form>
    <p><a href="revert.html">Revert</a> updates.</p>
    <h:listing mode="Update"/>
  </h:ifLoggedIn>
</h:piperka>
