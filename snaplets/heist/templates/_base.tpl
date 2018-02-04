<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8"/>
    <title>Piperka <h:unreadStats/></title>
    <h:stylesheet rel="stylesheet" type="text/css" href="/piperka.css"/>
    <h:stylesheet rel="stylesheet" type="text/css" href="/qsearch.css"/>

    <META HTTP-EQUIV="CACHE-CONTROL" CONTENT="NO-CACHE"/>
    <script src="https://code.jquery.com/jquery-3.3.1.min.js"></script>
    <script src="https://code.jquery.com/ui/1.12.1/jquery-ui.min.js"></script>
    <link rel="stylesheet" type="text/css" href="https://cdnjs.cloudflare.com/ajax/libs/jqPlot/1.0.9/jquery.jqplot.min.css"/>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/jqPlot/1.0.9/jquery.jqplot.min.js"></script>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/jqPlot/1.0.9/plugins/jqplot.dateAxisRenderer.min.js"></script>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/jqPlot/1.0.9/plugins/jqplot.canvasTextRenderer.min.js"></script>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/jqPlot/1.0.9/plugins/jqplot.canvasAxisTickRenderer.min.js"></script>
    <h:ifMod>
      <h:script src="/moderate.js"></h:script>
    </h:ifMod>
    <h:script src="/qsearch.js"></h:script>
    <h:script src="/piperka.js"></h:script>
  </head>

  <body>
    <h:ad>
      <h:adInit/>
    </h:ad>
    <div id="header" class="hiliteBG">
      <h:ad>
	<!-- Project Wonderful Ad Box Code -->
	<div id="pw_adbox_738_1_0"></div>
	<!-- End Project Wonderful Ad Box Code -->
      </h:ad>
      <a href="/">
	<img id="paprikat" src="/images/paprika.png" alt="Piperka">
      </a>
      <h1 style="margin-left:10%">Piperka</h1>
    </div>

    <div class="container">
      <div class="sidebar hiliteBG">
	<div class="control">
	  <p>
	    <a href="/about.html">About this site</a>
	    <br/><a href="/blog/">Blog</a>
	    <br/><a href="/top.html">Most popular</a>
	    <br/><a href="/submit.html">Submit a comic</a>
	    <br/><a href="/browse.html">Browse comics</a><h:newLink> (<a h:href=""><h:new/> new</a>)</h:newLink>
	    <h:ifLoggedIn>
	      <p>You are logged in as <a href="${h:profileLink}"><h:loggedInUser/></a></p>
	      <a href="/updates.html">Check updates</a> <span id="newin"><h:unreadStats/></span>
	      <br/><a href="/account.html">Your account</a>
	      <br/><a href="/?action=logout&csrf_ham=${h:csrf}">Logout</a>
	      <h:ifMod>
		<hr/>
		<a href="/moderate.html">Moderate</a>
		<h:modStats>
		  <h:haveModCount>(<h:modCount/>)</h:haveModCount>
		</h:modStats>
	      </h:ifMod>
	    </h:ifLoggedIn>
	    <h:ifLoggedOut>
	      <form method="post" action="/updates.html" style="padding-bottom:10px">
		<p>
		  User:
		  <a id="loginwith" href="loginwith.html">Login with...</a>
		  <br/><input class="login" type="text" name="_login" maxlength="40" style="width:140px"/>
		  <br/>Password:<br/>
		  <input class="login" type="password" name="_password" maxlength="40" style="width:140px"/>
		  <input id="loginsubmit" type="submit" name="action" value="Login"/>
		</p>
	      </form>
	      <br/><a href="/newuser.html">Create account</a>
	      <br/><a href="/lost.html">Lost password?</a>
	    </h:ifLoggedOut>
	    <h:ad>
	      <!-- Project Wonderful Ad Box Code -->
	      <div id="pw_adbox_599_3_0"></div>
	      <!-- End Project Wonderful Ad Box Code -->
	    </h:ad>
	  </p>
	</div>
      </div>
      <div id="maincornerback"></div>
      <div class="main">
	<div id="smalladbg" style="float:right; width:351px; height: 53px">
	  <h:ad>
	    <!-- Project Wonderful Ad Box Code -->
	    <div id="pw_adbox_602_2_0"></div>
	    <!-- End Project Wonderful Ad Box Code -->
	  </h:ad>
	</div>
	<div id="notmuch" style="width:1px; height:1px;"></div>
	<h:ifHasMessage>
	  <div class="error" id="message">
	    <h:message/>
	  </div>
	</h:ifHasMessage>
	<h:action>
	  <h:logout>
	    You have successfully logged out.
	  </h:logout>
	  <h:bookmark>
	    <h:success>
	      Bookmark
	      for <a href="/info.html?cid=${h:cid}"><h:title/></a> set
	      on <h:newest>newest page</h:newest><h:notNewest>page
	      <h:ord/></h:notNewest>.
	    </h:success>
	    <h:multiple>
	      <p>Bookmark matches multiple comics.</p>
	      <p>
		<ul>
		  <h:item>
		    <li><a href="/info.html?cid=${h:cid}"><h:title/></a></li>
		  </h:item>
		</ul>
	    </h:multiple>
	    <h:recognized>
	      <h3>Failed to set bookmark</h3>
	      <p>
		Looks like the URL you submitted might be for
		<a href="/info.html?cid=${h:cid}"><h:title/></a> but it
		failed to match with any page.  The error has been
		logged.
	      </p>
	      <h:ifLoggedIn>
		<p>
		  If you wish, you can set the bookmark for this
		  comic: <h:subscribeForm/>
		</p>
	      </h:ifLoggedIn>
	    </h:recognized>
	    <h:failed>
	      <h3>Failed to set bookmark</h3>
	      <p>
		For some reason, the URL failed to match with a comic
		in the database.  The error has been logged.
	      </p>
	      <p>
		The bookmarking algorithm is unfortunately, by
		necessity, quite ad hoc.  It tries its best effort
		with pages from comics' archives, but it may fail if
		you try to give the URL of the comic's front page.
		You might have better luck with choosing the comic
		directly from the list, especially since comics may be
		hosted on several sites and Piperka's keeping track of
		only one.
	      </p>
	    </h:failed>
	  </h:bookmark>
	  <h:ifLoggedIn>
	    <h:csrfFail>
	      You tried to
	      <h:describe>
		<logout>log out</logout>
		<bookmark>
		  set bookmark
		  for <a href="/info.html?cid=${h:cid}"><h:title/></a>
		  on <h:newest>newest page</h:newest>
		  <h:notNewest>page<h:ord/></h:notNewest>.
		</bookmark>
		<subscribe>
		  subscribe
		  to <a href="/info.html?cid=${h:cid}"><h:title/></a>
		</subscribe>
		<unsubscribe>
		  unsubscribe
		  from <a href="/info.html?cid=${h:cid}"><h:title/></a>
		</unsubscribe>
		<revert>
		  revert updates
		</revert>
	      </h:describe>
	      but this action failed the check to see that this request
	      originated from Piperka.  Please verify that you meant to
	      do this.
	      <h:csrfForm>
		<h:actionInputs>
		  <input type="hidden" name="${h:name}" value="${h:value}"/>
		</h:actionInputs>
		<input type="submit" value="Submit"/>
	      </h:csrfForm>
            </h:csrfFail>
	  </h:ifLoggedIn>
	  <h:unknownAction>
	    You requested an action from Piperka that failed a
	    precondition, like trying to subscribe to a comic not
	    listed on Piperka.  You shouldn't be seeing this error
	    unless you did something special like edited the post data
	    yourself or had the extraordinary luck of doing something
	    concurrently with some maintenance work.  Feel free to
	    contact the admin if this persists.
	  </h:unknownAction>
	  <h:sqlErr/>
	</h:action>
      </div>
    </div>
    <div class="legalese">
      Piperka.net copyright Kari Pahula &lt;<a
      href="kaol@piperka.net">kaol@piperka.net</a>&gt;
      2005-2018. Descriptions are user submitted and Piperka claims no
      copyright over them.  Banners copyright their respective
      authors.
    </div>
  </body>
</html>
