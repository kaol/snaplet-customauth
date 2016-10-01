<h:piperka>
  <h:listing mode="Profile">
    <h2>Comic picks of <h:profileName/></h2>
    <h:isPrivate>
      <p>Your profile is set private and no other people will see it.</p>
    </h:isPrivate>
    <h:isMine>
      <p>You have <a href="followers.html"><h:numFollowers/> and you
      follow <h:numFollowees/></a>.</p>
    </h:isMine>
    <h:mayAllowFollow>
      <p class="followcheck"><input type="checkbox" id="yourprofile" name="followee" value="${h:profileName}" h:checked=""/><label for="yourprofile">Allow <h:profileName/> to view your profile.</label></p>
    </h:mayAllowFollow>
    <h:requesting>
      <h:viewerIsPrivate>
	<p>Viewing protected profiles is disabled if your own profile is set as private.</p>
      </h:viewerIsPrivate>
      <h:interest>
	<p>You have requested to view the comic picks of
	<h:profileName/>.  The request is pending his/hers approval.</p>
	<p><span id="interest"><button id="cancel" name="${h:profileName}">Cancel request</button></span>
      </h:interest>
    </h:requesting>
    <h:mayInterest>
      <p><h:profileName/> has set his/hers profile as protected.</p>
      <h:ifLoggedIn>
	<p>You may request to view it.</p>
	<p><span if="interest"><button id="register" name="${h:profileName}">Request</button></span></p>
      </h:ifLoggedIn>
    </h:mayInterest>
    <h:publicFollow>
      <p class="followcheck"><input type="checkbox" id="myinterest" name="follow" value="${h:profileName}" h:checked=""/><label for="myinterest">Follow <h:profileName/>.</label></p>
    </h:publicFollow>
    <h:havePermission>
      <p>View <h:yourOrProfileName/> on <a href="${h:mapLink}">Piperka Map</a>.</p>
      <p>Total comic pages read by <h:youOrThisUser/>: <h:grandTotal/> in <h:nComics/> comics.</p>
      <div><h:writeUp/></div>
      <h:profileSortOptions/>
      <h:hilightButton/>
    </h:havePermission>
  </h:listing>
</h:piperka>
