<h:piperka newUser="True">
  <h2>Create account</h2>
  <h:ifLoggedIn>
    You already have an account.
  </h:ifLoggedIn>
  <h:ifLoggedOut>
    <h:onCreateError>
      <p>Please try again.
	<h:on err="MissingName">Missing or invalid account name.</h:on>
	<h:on err="NameUsed">Account name reserved.</h:on>
	<h:on err="PasswordMismatch">Password does not match with the
	  verification field.</h:on>
	<h:on err="PasswordMissing">Password missing</h:on>
	<h:on err="AvailError">Error checking account name
	  availability. <h:sqlErr/></h:on>
	<h:otherwise>Error creating account. <h:sqlErr/></h:otherwise>
    </h:onCreateError>
    <form method="post" action="newuser.html">
      User name: <input type="text" name="_new_login" h:value="_new_login" maxlength="40"/>
      <p>Password: <input type="password" name="_new_password" maxlength="40"/></p>
      <p>Password again: <input type="password" name="_new_password_again" maxlength="40"/></p>
      <p>E-mail (optional): <input type="text" name="email" h:value="email"/></p>
      <p><input type="submit" name="action" value="Create account"/></p>
    </form>
  </h:ifLoggedOut>
</h:piperka>
