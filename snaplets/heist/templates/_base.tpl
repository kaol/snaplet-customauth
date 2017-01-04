<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/html4/strict.dtd">
<html lang="en">
  <head>
    <title>Piperka <h:unreadStats/></title>
    <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
    <link rel="stylesheet" type="text/css" href="/piperka.css">
    <link rel="stylesheet" type="text/css" href="/qsearch.css">
    <META HTTP-EQUIV="CACHE-CONTROL" CONTENT="NO-CACHE">
    <script src="https://ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js"></script>
    <script src="https://ajax.googleapis.com/ajax/libs/jqueryui/1.10.1/jquery-ui.min.js"></script>
    <link rel="stylesheet" type="text/css" href="/jqplot/jquery.jqplot.css">
    <script src="/jqplot/jquery.jqplot.min.js"></script>
    <script src="/jqplot/plugins/jqplot.dateAxisRenderer.min.js"></script>
    <script src="/jqplot/plugins/jqplot.canvasTextRenderer.min.js"></script>
    <script src="/jqplot/plugins/jqplot.canvasAxisTickRenderer.min.js"></script>
    <!--<h:moderateJs/>-->
    <script src="/qsearch.js"></script>
    <script src="/piperka.js"></script>
  </head>

  <body>
    <div id="header" class="hiliteBG">
      <a href="./">
	<img id="paprikat" src="/images/paprika.png" alt="Piperka">
      </a>
      <h1 style="margin-left:10%">Piperka</h1>
    </div>

    <div class="container">
      <div class="sidebar hiliteBG">
	<div class="control">
	  <p>
	    <a href="about.html">About this site</a>
	    <br/><a href="/blog/">Blog</a>
	    <br/><a href="top.html">Most popular</a>
	    <br/><a href="submit.html">Submit a comic</a>
	    <br/><a href="browse.html">Browse comics</a><h:newLink> (<a h:href=""><h:new/> new</a>)</h:newLink>
	    <h:ifLoggedIn>
	      <p>You are logged in as <a href="${h:profileLink}"><h:loggedInUser/></a></p>
	      <a href="updates.html">Check updates</a> <h:unreadStats/>
	      <br/><a href="account.html">Your account</a>
	      <br/><a href="/?action=logout&csrf_ham=${h:csrf}">Logout</a>
	    </h:ifLoggedIn>
	    <h:ifLoggedOut>
	      <form method="post" action="updates.html" style="padding-bottom:10px">
		<p>User:
		  <br/><input class="login" type="text" name="_login" maxlength="40" style="width:140px"/>
		  <br/>Password:<br/>
		  <input class="login" type="password" name="_password" maxlength="40" style="width:140px"/>
		  <input id="loginsubmit" type="submit" name="action" value="Login"/>
		</p>
	      </form>
	      <br/><a href="newuser.html">Create account</a>
	      <br/><a href="lost.html">Lost password?</a>
	    </h:ifLoggedOut>
	  </p>
	</div>
      </div>
      <div id="maincornerback"></div>
      <div class="main">
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
	      for <a href="info.html?cid=${h:cid}"><h:title/></a> set
	      on <h:newest>newest page</h:newest><h:notNewest>page
	      <h:ord/></h:notNewest>.
	    </h:success>
	    <h:multiple>
	      <p>Bookmark matches multiple comics.</p>
	      <p>
		<ul>
		  <h:item>
		    <li><a href="info.html?cid=${h:cid}"><h:title/></a></li>
		  </h:item>
		</ul>
	    </h:multiple>
	    <h:recognized>
	      <h3>Failed to set bookmark</h3>
	      <p>
		Looks like the URL you submitted might be for
		<a href="info.html?cid=${h:cid}"><h:title/></a> but it
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
	      <h:logout>log out</h:logout>
	      <h:bookmark>
		set bookmark
		for <a href="info.html?cid=${h:cid}"><h:title/></a>
		on <h:newest>newest page</h:newest><h:notNewest>page
		<h:ord/></h:notNewest>.
	      </h:bookmark>
	      <h:subscribe>
		subscribe
		to <a href="info.html?cid=${h:cid}"><h:title/></a>
	      </h:subscribe>
	      <h:unsubscribe>
		unsubscribe
		from <a href="info.html?cid=${h:cid}"><h:title/></a>
              </h:unsubscribe>
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
  </body>
</html>
