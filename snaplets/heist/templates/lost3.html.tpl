<h:piperka>
  <h:usePasswordHash>
    <h:success>
      <h2>New password</h2>
      <p>You new password is <h:password/></p>
    </h:success>
    <h:otherwise>
      <h2>Password generation failed</h2>
      <p>
	There was a problem with the URL you used.  Please try again.
	<h:maybeSqlErr/>
      </p>
    </h:otherwise>
  </h:usePasswordHash>
</h:piperka>
