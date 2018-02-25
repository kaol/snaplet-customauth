<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8"/>
    <title>Piperka Reader (beta)</title>
    <link rel="stylesheet" type="text/css" href="https://${h:hostname}/reader.css">
    <META HTTP-EQUIV="CACHE-CONTROL" CONTENT="NO-CACHE">
    <script src="https://code.jquery.com/jquery-3.3.1.min.js"></script>
    <script src="https://code.jquery.com/ui/1.12.1/jquery-ui.min.js"></script>
    <script src="https://${h:hostname}/viewarchive.js"></script>
    <script src="https://${h:hostname}/reader.js"></script>
  </head>

  <body>
    <span id="currentpagemarker" class="marker">Viewing</span>
    <span id="bookmarkmarker" class="marker">Bookmark</span>
    <span id="readingmarker" class="marker">Now reading</span>
    <div id="moreoptions" class="script">
      <br/>
      <span title="Height fixing enables more options for how to display the embedded page.">
	<label for="fixiframe">Fix iframe height</label> <input type="checkbox" id="fixiframe">
      </span>
      <span id="withfix">
	<br/>
	<span id="fixheight">
	  Fix height at <input type="text"><button>Set</button> (Now: <span>2000</span>)
	</span>
	<br/>
	<label for="lockselect">Lock page position </label> <input type="checkbox" id="lockselect" checked="1">
	<input type="text" id="lockheight" value="0" readonly="1" title="Uncheck lock and scroll the embedded page to change the value.">
	<br/>
	<label for="arrownavigate">Bind left/right arrow</label><input type="checkbox" id="arrownavigate">
      </span>
    </div>
    <div id="navigation">
      <span id="logo">Piperka Reader <span class="beta">beta</span></span>
      | <button id="first" disabled="1">First</button>
      <button id="prev" disabled="1">Prev</button>
      <button id="next" disabled="1">Next</button>
      <button id="current" disabled="1">Current</button>
      | <button id="archive" disabled="1">Archive</button>
      <span class="user">| <button id="nextcomic">Next Comic</button> <button id="mycomics">My Comics</button>
	<label for="autoupdate">autoupdate</label><input type="checkbox" id="autoupdate" checked="1"/></span>
      | <span id="title"></span>
      | <div id="pagecount">Page <span id="pagenum"></span> / <span id="pagetotal"></span></div>
      <a href="https://${h:hostname}/" title="Return to Piperka">P</a> <a id="tocomic" title="Exit to comic">&darr;</a>
      <span id="preurl">&nbsp;</span><input type="text" id="url" readonly="1"/>
    </div>
    <div id="reader" tabindex="1000">
      <div id="welcome" title="About Piperka Reader">
	<h2>Piperka Reader</h2>
	<p>
	  Piperka Reader uses Piperka's web comic archive to display
	  web comic archives in an embedded form.  It offers a unified
	  navigation interface, preloads subsequent pages when reading
	  them in order and allows updating bookmarks on Piperka
	  directly.
	</p>
	<p>
	  This feature is still under development.  Hopefully the
	  buttons on the Reader bar are self-explanatory enough.  The
	  Reader won't know it if you navigate away within the
	  embedded comic.  More options for controlling how the
	  content is displayed are available from a menu available in
	  the top left corner.
	</p>
	<p id="nocomicselected">
	  To try it out, you'll need to select a comic.  Go back to
	  the <a href="/browse.html">main site</a> and select Reader
	  from a comic's info page.
	</p>
	<p>
	  Click "Archive" to see a list of comic's pages.  Clicking a
	  row there takes you that page.
	</p>
	<h3>Actions as logged in user</h3>
	<p class="user">Hello, <span id="namehere"/>.</p>
	<p>
	  Select a comic by clicking "My Comics" button.  You can move
	  your bookmark in the archive dialog by clicking an already
	  selected row.  The "Next" button automatically updates the
	  bookmark if the "autoupdate" option is selected.
	</p>
      </div>
    </div>
  </body>
</html>
