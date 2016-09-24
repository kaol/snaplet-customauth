<h:piperka>
  <h:ifLoggedOut>
    <h2>Why am I seeing this page?</h2>
    <p>
      You are most likely a webcomic author following site statistics.
      This page has bookmark links for logged in users.
    </p>
  </h:ifLoggedOut>
  <h:ifLoggedIn>
    <h:listing mode="Update"/>
  </h:ifLoggedIn>
</h:piperka>
