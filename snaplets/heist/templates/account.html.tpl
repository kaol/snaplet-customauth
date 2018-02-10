<h:piperka>
  <h:ifLoggedIn>
    <h:accountForm>
      <h:hasError/>
      <h2>User options</h2>
      <h:csrfForm method="post" action="account.html">
	<input type="hidden" name="action" value="settings_common"/>
	<p>
	  <label for="new_windows">Open external links in new tabs</label>
	  <input type="checkbox" id="new_windows" name="new_windows" h:value="new_windows" value="1"/>
	</p>
	<p>
	  <label for="rows">Rows:</label>
	  <input type="text" name="set_rows" id="rows" h:value="rows" inputmode="numeric"/>
	</p>
	<p>
	  Columns:
	  <input type="radio" h:columns="1" name="set_columns" id="one" value="1"/><label for="one">1</label>
	  <input type="radio" h:columns="2" name="set_columns" id="two" value="2"/><label for="two">2</label>
	  <input type="radio" h:columns="3" name="set_columns" id="three" value="3"/><label for="three">3</label>
	</p>
	<h3>Update options</h3>
	<p>
	  These used to be embedded on the updates page itself.
	</p>
	<p>
	  Redirect type:
	  <input type="radio" h:holdBookmark="False" name="hold_bookmark" id="b_move" value="0"/>
	  <label for="b_move">Set bookmark on the newest comic page on redirect</label>
	  <input type="radio" h:holdBookmark="True" name="hold_bookmark" id="b_hold" value="1"/>
	  <label for="b_hold">Do not move the bookmark</label>
	</p>
	<p>
	  Bookmark sort order.
	</p>
	<p>
	  <ul class="unmarked">
	    <li>
	      <input type="radio" h:sortBookmark="0" name="bookmark_sort" id="b_desc" value="0"/>
	      <label for="b_desc">Most unread first</label>
	    </li>
	    <li>
	      <input type="radio" h:sortBookmark="1" name="bookmark_sort" id="b_asc" value="1"/>
	      <label for="b_asc">Fewest unread first</label>
	    </li>
	    <li>
	      <input type="radio" h:sortBookmark="2" name="bookmark_sort" id="b_alph" value="2"/>
	      <label for="b_alph">Alphabetical</label>
	    </li>
	    <li>
	      <input type="radio" h:sortBookmark="3" name="bookmark_sort" id="b_upd" value="3"/>
	      <label for="b_upd">Most recently updated first</label>
	    </li>
	    <li>
	      <input type="radio" h:sortBookmark="4" name="bookmark_sort" id="b_upd_desc" value="4"/>
	      <label for="b_upd_desc">Most recently updated last</label>
	    </li>
	  </ul>
	  <!-- </p> Somehow, the parser gets mad if I put the closing tag here. -->
	<p>
	  <input type="checkbox" name="offset_bookmark_by_one" h:offsetMode="True" id="b_offset" value="1"/>
	  <label for="b_offset">Offset bookmarks backwards by one page</label>
	</p>
	<p>
	  <input name="editacc_unpriv" value="Update" type="submit"/>
	</p>
      </h:csrfForm>
      <hr/>
      <div class="priv_settings">
	<h2>Account management</h2>
	<h:csrfForm method="post" action="account.html">
	  <input type="hidden" name="action" value="settings_priv"/>
	  <p>
	    The following settings are protected and need either a
	    password or OAuth2 verification to change.
	  </p>
	  <h:authenticateWith/>
	  <h3>Manage password</h3>
	  <p>
	    <label for="only_oauth2">Passwordless mode, use only OAuth2 for login </label>
	    <input type="checkbox" name="only_oauth2" h:hasNoPassword="checked" id="only_oauth2"/>
	  </p>
	  <p>
	    New password: <input type="password" name="new_passwd" h:hasNoPassword="disabled"/>
	  </p>
	  <p>
	    Retype new password: <input type="password" name="new_passwd_retype" h:hasNoPassword="disabled"/>
	  </p>
	  <h3>OAuth2 logins</h3>
	  OAuth2 is used solely for fetching a unique user ID from the provider.
	  <table>
	    <tr>
	      <th>Provider</th>
	      <th>ID</th>
	      <h:haveRemovableProviders>
		<th>Remove</th>
	      </h:haveRemovableProviders>
	      <h:haveAttachableProviders>
		<th>Attach (opens in a new window/tab)</th>
	      </h:haveAttachableProviders>
	    </tr>
	    <h:oauth2Providers>
	      <tr>
		<td><h:label/></td>
		<td><h:identification/></td>
		<h:haveRemovableProviders>
		  <td>
		    <h:hasIdentification>
		      <input type="checkbox" name="remove_oauth2" value="${h:name}"/>
		    </h:hasIdentification>
		  </td>
		</h:haveRemovableProviders>
		<h:haveAttachableProviders>
		  <td>
		    <h:hasIdentification check="False">
		      <a href="/s/attachProvider/${h:name}" target="_blank" class="oauth2_add">Attach</a>
		    </h:hasIdentification>
		  </td>
		</h:haveAttachableProviders>
	      </tr>
	    </h:oauth2Providers>
	  </table>
	  <h3>Change email address</h3>
	  <p>
	    New email: <input type="email" name="new_email" h:value="email"/>
	  </p>
	  <h3>Privacy settings</h3>
	  <p>
	    Who should see your profile?
	  </p>
	  <ul>
	    <li>
	      <input type="radio" id="profile-private" name="privacy" value="1" h:privacy="1"/>
	      <label for="profile-private">Only me</label>
	    </li>
	    <li>
	      <input type="radio" id="profile-protected" name="privacy" value="2" h:privacy="2"/>
	      <label for="profile-protected">People I have given the permission</label>
	    </li>
	    <li>
	      <input type="radio" id="profile-public" name="privacy" value="3" h:privacy="3"/>
	      <label for="profile-public">Everyone</label>
	    </li>
	  </ul>
	  <h4>Write up</h4>
	  <p>
	    Write anything that you'd like to show in your profile.
	    We reserve the right to moderate the content.
	  </p>
	  <p>
	    Allowed tags <tt>a p</tt>.  Allowed
	    attributes <tt>href</tt>.
	  </p>
	  <textarea cols="50" rows="8" name="writeup"><h:writeup/></textarea>
	  <p>
	    <input type="submit" name="editacc_priv" value="Update"/>
	  </p>
	</h:csrfForm>
      </div>
    </h:accountForm>
  </h:ifLoggedIn>
  <h:ifLoggedOut>
    <h2>Nothing to see here</h2>
    <p>
      This page would do something if you were logged in.
    </p>
  </h:ifLoggedOut>
</h:piperka>
