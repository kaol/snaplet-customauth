<h:piperka>
  <h2>Create account</h2>
  <h:ifLoggedIn>
    You already have an account.
  </h:ifLoggedIn>
  <h:ifLoggedOut>
    <form method="post">
      <h:paramAttrs>
	User name: <input type="text" name="_new_login" h:value="_new_login" maxlength="40"/>
	<p>Password: <input type="password" name="_new_password" maxlength="40"/></p>
	<p>Password again: <input type="password" name="_new_password_again" maxlength="40"/></p>
	<p>E-mail (optional): <input type="text" name="email" h:value="email"/></p>
	<p><input type="submit" name="action" value="Create account"/></p>
      </h:paramAttrs>
    </form>
  </h:ifLoggedOut>
</h:piperka>
