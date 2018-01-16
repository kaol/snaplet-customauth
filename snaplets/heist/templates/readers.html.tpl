<h:piperka>
  <h:readers>
    <h:positive>
      <h:some>
	<h2>People subscribed to <h:title/></h2>
	<p>
	  These people have subscribed to <a
	  href="info.html?cid=${h:cid}"><h:title/></a> and have their
	  profiles set public<h:ifLoggedIn> or shared it with
	  you</h:ifLoggedIn>.
	</p>
	<ul>
	  <h:list>
	    <li><a href="profile.html?name=${h:userURL}" h:passive=""><h:user/></a></li>
	  </h:list>
	</ul>
	<p>
	  <h:total/> in all.
	</p>
      </h:some>
      <h:none>
	<h2>No readers for <h:title/></h2>
	<p>
	  Looks like nobody admits reading <a
	  href="info.html?cid=${h:cid}">this comic</a>.
	</p>
	<h:hidden>
	  <p>
	    Still, there are <h:total/> readers.
	  </p>
	</h:hidden>
      </h:none>
    </h:positive>
    <h:missing>
      <h2>No comic selected</h2>
      <p>
	Nothing to see here.
      </p>
    </h:missing>
  </h:readers>
</h:piperka>
