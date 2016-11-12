<h:piperka>
  <h:withCid>
    <h:exists>
      <h:related/>
      <div class="script chart" id="readerchart"></div>
      <h:comicInfo/>
      <h3>Actions</h3>
      <ul>
	<li><a href="edit_info.html?cid=${h:cid}">Edit information</a></li>
	<h:ifLoggedIn>
	  <li>
	    <h:ifSubscribed>
	      <true>
		<h:csrfForm method="post" action="${h:thisPage}">
		  <input type="hidden" name="subscribe" value="${h:cid}"/>
		  <input type="checkbox" id="start_at_first" name="start_at_first" value="1"/>
		  <label for="start_at_first">Set bookmark at the first page</label>
		  <input type="submit" name="action" value="Subscribe"/>
		</h:csrfForm>
	      </true>
	      <false>
		<h:csrfForm method="post" action="${h:thisPage}">
		  <input type="hidden" name="unsubscribe" value="${h:cid}"/>
		  <input type="submit" name="action" value="Unsubscribe"/>
		</h:csrfForm>
	      </false>
	    </h:ifSubscribed>
	  </li>
	</h:ifLoggedIn>
	<li><a href="/reader/?cid=${h:cid}">View in Piperka Reader</a>
	<h:ifMapped>
	  <li><a href="/map/?cid=${h:cid}">View on Piperka Map</a></li>
	</h:ifMapped>
      </ul>
      <h:crawlErrors>
	<h3>Crawl errors</h3>
	<p>
	  The last 5 crawl errors during the last 30 days.  Having
	  this empty doesn't necessarily imply that there isn't
	  something wrong with the crawler.  I'll go through these
	  eventually but I don't mind if you ask me to check whether
	  the crawler's doing the right thing.
	</p>
	<table id="crawlerr">
	  <tr>
	    <th>Page order</th>
	    <th>Time</th>
	    <th>URL</th>
	    <th colspan="2">HTTP status</th>
	  </tr>
	  <h:rows>
	    <tr>
	      <td><h:ord/></td>
	      <td><h:time/></td>
	      <td><h:url/></td>
	      <td><h:code/></td>
	      <td><h:msg/></td>
	    </tr>
	  </h:rows>
	</table>
      </h:crawlErrors>
    </h:exists>
    <h:dead>
      <h2>Comic removed</h2>
      <p>
	That comic has been marked as <a href="deadinfo.html?cid=${h:cid}">removed</a>.
      </p>
    </h:dead>
    <h:missing>
      <p>
	I'm the message that tells you about this page being the page
	that shows some details about a comic listed on Piperka.  But
	either you gave no <i>cid</i> or it is not used and you'll see
	no comic, only me.
      </p>
      <p>
	Click on the side bar to browse the comic list.
      </p>
    </h:missing>
  </h:withCid>
</h:piperka>
