<h:piperka>
  <h:ifLoggedIn>
    <h:accountForm>
      <h:hasError>
	<maybeSqlError>
	  <p>
	    An error happened while trying to update account settings: <h:sqlError/>
	  </p>
	</maybeSqlError>
	<passwordMissing>
	  <p>
	    You tried to alter password protected settings, but
	    provided no password.  No changes made.
	  </p>
	</passwordMissing>
	<wrongPassword>
	  <p>
	    You tried to change protected settings, but failed to
	    provide the correct password.  No changes made.
	  </p>
	</wrongPassword>
	<passwordMismatch>
	  <p>
	    You tried to change your password, but the two password
	    entries didn't match.  No changes made.
	  </p>
	</passwordMismatch>
      </h:hasError>
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
	  <input type="radio" h:columns="1" name="set_columns" id="one" value="1"><label for="one">1</label>
	  <input type="radio" h:columns="2" name="set_columns" id="two" value="2"><label for="two">2</label>
	  <input type="radio" h:columns="3" name="set_columns" id="three" value="3"><label for="three">3</label>
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
	  <p>
	    Authenticate changes with
	    <select name="authenticate_with" id="authenticate_with">
	      <option value="Password" h:hasNoPassword="disabled">Password</option>
	      <h:oauth2Providers filter="True">
		<option value="${h:name}"><h:label/></option>
	      </h:oauth2Providers>
	    </select>
	  <p>
	    Password: <input type="password" name="_password"/>
	  </p>
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
