<h:submitAPI>
  <msg id="0">
    <h2>Can't do that</h2>
    <p>
      I'd like to see both a title and the home page URL for your
      submission.
    </p>
  </msg>
  <msg id="1">
    <h2>That looks familiar</h2>
    <p>
      There is already a comic named as <a href="${h:url}"><h:name/></a>
      listed on Piperka.  If you think that there's been some error or
      your comic just happens to have the same name as an existing comic
      listed on Piperka, then please send me an email about it.  Sorry
      for the inconvenience.
    </p>
  </msg>
  <msg id="2">
    <h2>Database error</h2>
    <p>
      An error occurred when trying to check the submission: <h:err/>
    </p>
  </msg>
  <msg id="3">
    No such comic
  </msg>
  <msg id="4">
    <h2>Database error</h2>
    <p>
      An error occurred when trying to check the comic: <h:err/>
    </p>
  </msg>
  <msg id="5">
    <h2>Upload error</h2>
    <p>
      There was an error in uploading a banner submission.
    </p>
  </msg>
  <msg id="6">
    <h2>Image missing or invalid</h2>
    <p>
      A banner file was submitted but it wasn't recognized as a valid
      image file.
    </p>
  </msg>
  <msg id="7">
    <h2>Animated banner detected</h2>
    <p>
      Seems like you submitted an animated banner.  The site policy
      prohibits them.  Sorry.
    </p>
  </msg>
  <msg id="8">
    <h2>Invalid image type</h2>
    <p>
      Only png, jpg or gif banner images are accepted.
    </p>
  </msg>
  <msg id="9">
    <h2>Invalid dimensions</h2>
    <p>
      The image size needs to be 468x60.  Your file was
      <h:width/>x<h:height/>.
    </p>
  </msg>
  <msg id="10">
    <h2>Unknown error in banner submission processing</h2>
    <p>
      This shouldn't happen. Banana.
    </p>
  </msg>
  <msg id="11">
    Your submission has been accepted and queued.
  </msg>
  <msg id="12">
    Your submission has been sent to our moderators for consideration.
    <a href="${h:url}">Return to the comic's info page</a>.
  </msg>
  <msg id="13">
    <span><a href="${h:url}">Entry</a> edited successfully.</span>
  </msg>
  <msg id="14">
    Invalid form parameters.  Something was off with your submission.
  </msg>
</h:submitAPI>
