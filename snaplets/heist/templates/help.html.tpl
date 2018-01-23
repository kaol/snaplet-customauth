<h:piperka>
  <h2>How it works</h2>
  <p>
    This is the side panel you will see when you are logged on.  Most
    of the links are self-explanatory, but there are a few points to
    make.
  </p>
  <p>
    The (<i>num</i> new) after <i>Browse comic list</i> are visible
    when new comics have been added to the database since your last
    visit.
  </p>
  <p>
    The (<i>num</i> new in <i>anothernum</i>) is a quick summary of
    how many comics there remain to be read in your subscriptions in
    total and in how many comics.  This number will change when comics
    get new pages and when you read them.
  </p>
  <p>
    "Your account" has options controlling the layout of the site and
    account details like your email address and password.
  </p>
  <div class="example">
    <ul class="control">
      <li><a href="top.html">Most popular</a></li>
      <li><a href="submit.html">Submit a comic</a></li>
      <li><a href="browse.html">Browse comics</a>
      (<a href="browse.html?sort=new">2 new</a>)</li>
      <p>You are logged in as <i>user</i>.</p>
      <li><a href="updates.html">Check updates</a> (1028 new in 3)</li>
      <li><a href="account.html">Edit your information</a></li>
      <hr/>
      <li><a href="index.html?action=logout">Logout</a></li>
    </ul>
  </div>
  <h3>Comic listings</h3>
  <div class="example">
    <p>
      <label for="atfirst">Bookmark the first comic</label>
      <input id="atfirst" type="checkbox" name="start_at_first" checked="1"/>
    </p>
    <div class="list1" name="single">
      <ul class="list1">
	<li>
	  <button class="minus" name="unsubscribe" value="363" type="button"> - </button>
	  <a href="info.html?cid=363">xkcd</a>
	</li>
	<li>
	  <button class="plus" name="subscribe" value="368" type="button"> + </button>
	  <a href="info.html?cid=368">Gunnerkrigg Court</a>
	</li>
	<li>
	  <button class="plus" name="subscribe" value="3015" type="button"> + </button>(new)
	  <a href="info.html?cid=3015">Oglaf</a>
	</li>
      </ul>
    </div>
  </div>
  <p>
    This is what comic listings generally look like.  The comics you
    have subscribed to have been marked with <button class="minus"
    type="button"> - </button> and the unsubscribed ones have <button
    class="plus" type="button"> + </button> next to them.  New
    additions have <i>(new)</i> in front of them.  The names
    themselves are links to the comics' info pages.
  </p>

  <p id="bookmarkfirst">
    The "Bookmark the first comic" checkbox controls whether your
    bookmark is set to the first or the last comic page when you
    subscribe to comics.  Bookmarking the first page is what you
    usually want to do if you haven't read a comic before and want to
    start from the beginning.
  </p>
  <h3>Checking for updates</h3>
  <div class="example">
    <h2>Set bookmark</h2>
    <p>
      URL: <input type="text" name="bookmark" readonly="1"/>
      <button name="action" value="set_bookmark">Set</button>
      <label for="wantbookmarkhere">Set bookmark here</label>
      <input type="checkbox" id="wantbookmarkhere" name="wantbookmarkhere"/>
    </p>
    <p>
      <a href="revert.html">Revert</a> updates.
    </p>
    <h2>Updated comics</h2>
    <div class="list1" name="single">
      <ul>
	<li><a href="updates.html?action=redir&amp;cid=89">Abstract Gender</a> (42 new)</li>
	<li><a href="updates.html?action=redir&amp;cid=23">Diesel Sweeties</a> (599 new)</li>
	<li><a href="updates.html?action=redir&amp;cid=41">fallen</a> (202 new)</li>
	<li><a href="updates.html?action=redir&amp;cid=49">Ghastly Comic</a> (227 new)</li>
      </ul>
    </div>
  </div>
  <p>
    This page is where the meat of Piperka's functionality lies.  In
    fact, http://piperka.net/updates.html may be the page you want to
    set your bookmark to from your browser.
  </p>
  <p>
    "Set bookmark" takes an URL from a comic's archive pages, finds
    the corresponding comic and the page the URL refers to and sets
    your bookmark to the next comic from there.  If you were not
    previously subscribed to the comic, it also adds it to your
    subscriptions.
  </p>
  <p>
    You can use this bookmark when on a comic's archive page to
    directly set the bookmark on Piperka and skip copying and pasting the
    URL on the updates.html page.
  </p>
  <p id="bookmarkhere">
    If the "Set bookmark here" checkbox is checked, the bookmark is
    set at the exact URL you have given.  Otherwise it will set the
    bookmark to the comic page following the URL you have given, i.e.
    you won't see the page you have already seen twice.
  </p>
  <p>
    "Revert updates" is there to undo the changes Piperka does to your
    bookmarks on redirects.  You'll find a list of recent redirects
    you have used, following the links there will set your bookmark to
    where it was before the redirect.  The net is an inherently
    unreliable medium and Piperka has no way of knowing whether you
    actually reach the comics.
  </p>
  <p id="redirectmode">
    Finally, there's the list of updates.  By default, the links here
    serve two purposes.  Firstly, it will redirect your browser to the
    page where you left off in the comic's archive.  If it says there,
    e.g., <i>(3 new)</i> it will redirect you to the third latest
    comic page.  And secondly, it will update your bookmark for that
    comic to point at the latest comic page.  You may want to return
    here to use "Set bookmark" to set the bookmark to some other
    position if you didn't read the archives that far yet.
  </p>
  <p>
    You can turn that second part of Piperka's functionality off on
    your account settings page.  That mode also adds buttons to move
    your bookmark to the newest page without redirecting you from the
    updates page.
  </p>
</h:piperka>
