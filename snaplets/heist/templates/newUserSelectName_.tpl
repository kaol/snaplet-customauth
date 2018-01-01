<h:piperka>
  <h:oauth2Create>
    <h2>OAuth2 login success</h2>
    <p>
      You have successfully logged in via OAuth2.  If you are a new
      user, please select a user name and you're ready to start using
      Piperka.
    </p>
    <h:message>
      <h:duplicate>
	That user name is taken.  Please choose another.
      </h:duplicate>
      <h:invalid>
	That user name is invalid.  Please choose another.
      </h:invalid>
      <h:oldUser>
	If you already have a Piperka account, please log in to it
	first and then attach an OAuth2 provider identity to your
	account from your account settings.
      </h:oldUser>
    </h:message>
    <form action="/apiAuth/oauth2createaccount" method="post">
      <p>
	<label for="_new_login">User name:</label>
	<input id="_new_login" type="text" name="_new_login" maxlength="40"/>
      </p>
      <input type="submit" name="_action" value="Create account"/>
    </form>
  </h:oauth2Create>
</h:piperka>
