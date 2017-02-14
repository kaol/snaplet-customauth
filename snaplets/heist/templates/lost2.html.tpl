<h:piperka>
  <h:passwordRecovery>
    <h:success>
      <h2>Mail sent (possibly)</h2>
      <p>
	Assuming that the user name and email address you entered
	matched with the ones in the database, an email was sent.
	Note that your password will be changed only after you have
	used the hash given in the email.
      </p>
    </h:success>
    <h:otherwise>
      <h2>Password recovery email send confirmation page</h2>
      <p>
	This page would tell you about password reset email send.  But
	it doesn't look like you came here from the password reset
	page.  Nothing was done.
      </p>
    </h:otherwise>
  </h:passwordRecovery>
</h:piperka>
