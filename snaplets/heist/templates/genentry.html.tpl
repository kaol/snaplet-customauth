<h:piperka ads="False">
  <h:submit mode="Genentry">
    <h:ifLoggedIn>
      <h:ifMod>
	<h2>Genentry</h2>
	<h:genentry>
	  <h:csrfForm id="genentry-form" method="post">
	    <input type="hidden" name="formtype" value="genentry"/>
	    <p>
	      cid <input type="text" readonly name="cid" value="${h:newCid}"/>
	    <div style="height: 60px" id="submission-banner"/>
	    <p>
	      Title: <input type="text" name="title" id="title"/>
	    </p>
	    <p>
	      Homepage: <input type="text" name="homepage" id="homepage">
	      <a id="homepage_link" target="_blank">Link</a>
	    </p>
	    <p>
	      Fixed head: <input type="text" name="fixed_head" id="fixed_head"/>
	    </p>
	    <p>
	      URL base: <input type="text" name="url_base" id="url_base"/>
	      URL tail: <input type="text" name="url_tail" id="url_tail"/>
	    </p>
	    <p>
	      Parser id: <input type="number" name="parser_id"/>
	    </p>
	    <p>
	      Bookmark regexp: <input type="text" name="bookmark_regexp"/>
	    </p>
	    <p>
	      Extra data: <input type="text" name="extra_data"/>
	      Extra URL: <input type="text" name="extra_url"/>
	    </p>
	    <h:submitForm/>
	    <h3>Precrawl</h3>
	    <ol start="0">
	      <h:precrawl>
		<li><h:page/></li>
	      </h:precrawl>
	    </ol>
	  </h:csrfForm>
	  <success>
	    Entry submitted <a href="${h:href}">link</a>.
	  </success>
	  <failure>
	    Failed. <h:message/>
	  </failure>
	</h:genentry>
      </h:ifMod>
    </h:ifLoggedIn>
  </h:submit>
  <h:notMod>
    <h2>403 Go away</h2>
    <p>
      This page is meant for moderators, not for mere mortals.
    </p>
  </h:notMod>
</h:piperka>
