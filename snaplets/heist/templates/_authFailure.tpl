<p>
  Please try again.
  <h:loginFailure>
    <h:on err="UsernameMissing">
      User name missing.
    </h:on>
    <h:on err="PasswordMissing">
      Password missing.
    </h:on>
  </h:loginFailure>
  <h:createFailure>
    <h:on err="MissingName">
      Missing or invalid account name.
    </h:on>
    <h:on err="NameUsed">
      Account name reserved.
    </h:on>
    <h:on err="PasswordFailure Mismatch">
      Password does not match with the verification field.
    </h:on>
    <h:on err="PasswordFailure Missing">
      Password missing
    </h:on>
    <h:on err="OAuth2Failure StateNotStored">
      Failed to retrieve stored state token.
    </h:on>
    <h:on err="OAuth2Failure StateNotReceived">
      Invalid or missing state token received from provider.
    </h:on>
    <h:on err="OAuth2Failure ExpiredState">
      State token expired.
    </h:on>
    <h:on err="OAuth2Failure BadState">
      Mismatch in generated and received state tokens.
    </h:on>
    <h:on err="OAuth2Failure IdExtractionFailed">
      Failed to extract user ID from the provider.
    </h:on>
    <h:on err="OAuth2Failure NoStoredToken">
      No state token stored in local session.
    </h:on>
    <h:on err="OAuth2Failure AlreadyUser">
      You had already logged in to Piperka and tried to create a new
      account via OAuth2.
    </h:on>
    <h:on err="OAuth2Failure AlreadyLoggedIn">
      You tried to log in via oauth2 but you are already logged in on
      Piperka.
    </h:on>
    <h:on err="OAuth2Failure AttachNotLoggedIn">
      You tried to attach an account via OAuth2 to a Piperka account
      but you are not logged in.
    </h:on>
    <h:on err="OAuth2Failure AlreadyAttached">
      You tried to attach an account via OAuth2 to a Piperka account
      but it was already attached.
    </h:on>
    <h:on err="OAuth2Failure AccessTokenFetchError">
      Failed to retrieve access token fron an OAuth2 provider.
    </h:on>
    <h:providerError>
      Error received from the provider: <h:error><i>No message received</i></h:error>
    </h:providerError>
  </h:createFailure>
  <h:actionFailure>
    <h:on err="ActionTimeout">
      Timeout validating action.
    </h:on>
    <h:on err="ActionDecodeError">
      Error decoding stored action.
    </h:on>
    <h:on err="ActionUserMismatch">
      Stored action is saved for a different user.
    </h:on>
  </h:actionFailure>
  <h:otherwise/>
</p>
