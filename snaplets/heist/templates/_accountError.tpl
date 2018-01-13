<h:accountValidationError>
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
</h:accountValidationError>
