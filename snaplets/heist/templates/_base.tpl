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
	<h:apply-content/>
      </div>
    </div>
  </body>
</html>
