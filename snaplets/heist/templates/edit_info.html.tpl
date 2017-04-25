<h:piperka>
  <h:submit mode="Edit">
    <h:hasCid>
      <h:found>
	<h2>Edit comic information</h2>
	Submissions will enter the moderation queue.
	<p>
	  If the comic has changed to another site, I would prefer to
	  receive an email about it.  I'll have to reprogram the crawler
	  and rebuild the index, which I need to do by hand anyway.
	  Thank you.
	</p>
	<p>
	  I'll expand this into some sort of a proper ticketing system
	  later on.
	</p>
	<h:csrfForm>
	  <input type="hidden" name="formtype" value="editinfo"/>
	  <input type="hidden" name="cid" value="${h:cid}"/>
	  Title: <h:title/>
	  <p>Home page: <h:homepage/></p>
	  <h:submitForm/>
	</h:csrfForm>
      </h:found>
      <h:notFound>
	<h2>Comic not found</h2>
	No comic found with this cid.  Sorry about that.
      </h:notFound>
    </h:hasCid>
    <h:noCid>
      <h2>Edit uninformation</h2>
      Insert a slightly humorous message about needing a cid here.
    </h:noCid>
  </h:submit>
</h:piperka>
