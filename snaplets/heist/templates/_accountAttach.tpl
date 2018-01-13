<h:content>
  <h2>Attach new authentication method</h2>
  <p>
    This is the landing page for OAuth2 authentication method
    attachment.
  </p>
  <h:hasError/>
  <h:csrfForm method="post" action="account.html">
    <input type="hidden" name="action" value="attach_oauth2"/>
    <input type="hidden" name="attach_provider" value="1"/>
    <p>
      Please perform authentication with an existing authentication
      method to confirm the following change: Allow identification via
      <h:providerName/>.
    </p>
    <h:authenticateWith/>
    <p>
      <input type="submit" name="cancel_attach" value="Cancel"/>
      <input type="submit" name="editacc_priv" value="Confirm"/>
    </p>
  </h:csrfForm>
</h:content>
