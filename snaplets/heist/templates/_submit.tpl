<h3>Categories</h3>
<h:tags/>
<h3>Cross-references</h3>
<p>Piperka can have links to other sites containing information about this comic.</p>
<h:epedias/>
<h:ifMode mode="[Submit]">
  <h3>Email notification</h3>
  <label for="want_notify">Send me an email when the comics is added
  to Piperka.</label>
  <input type="checkbox" id="want_notify" name="want_notify"/>
  <p>Email: <input type="text" name="email" value="${h:email}"/></p>
  <p>
    If the comics hasn't been added after a while, feel free to ask
    about it.
  </p>
</h:ifMode>
<h:ifMode mode="[Edit,Submit]">
  <h3>Banner</h3>
  <p>
    If the comic has an unanimated, 468x60 banner, you can submit it
    too.  Yes, I like to keep that limit strict and I don't unanimate
    or resize any banners myself.
  </p>
  <p class="script noformdata">
    Uploading banners uses features that your browser doesn't seem to
    support.
  </p>
  <p>
    <span class="script hasformdata">
      <input type="file" name="banner" size="100"/>
    </span>
  </p>
</h:ifMode>
<h3>Description</h3>
<p>
  <textarea cols="50" rows="5" name="description">
    <h:ifMode mode="[Edit]"><h:description/></h:ifMode>
  </textarea>
</p>
<p>
  <span class="noscript">
    Submitting
    <h:ifMode mode="[Submit]">comics</h:ifMode>
    <h:ifMode mode="[Edit]">edits</h:ifMode>
    uses JavaScript.  Please enable it or use a capable browser.
    Sorry for the inconvenience.
  </span>
</p>
<h:ifMode mode="[Edit]">
  <div id="msgdiv"/>
</h:ifMode>
<h:ifMode mode="[Edit]">
  <span id="tagdiff" class="hideafterdone">
    <div id="removedtags" class="script">Tags to be removed: <span class="diff"/></div>
    <div id="addedtags" class="script">Tags to be added: <span class="diff"/></div>
  </span>
</h:ifMode>
<h:ifMode mode="[Moderate]">
  <span class="script hasbanner">
    <input type="checkbox" id="acceptbanner" name="acceptbanner"/>
    <label for="acceptbanner">Approve banner</label>
  </span>
</h:ifMode>
<span class="script show">
  <h:ifMode mode="[Edit]">
    <button class="hideafterdone" type="submit" disabled="1">Submit</button>
  </h:ifMode>
  <h:ifMode mode="[Submit]">
    <button type="submit" disabled="1">Submit</button>
  </h:ifMode>
</span>
<h:ifMode mode="[Moderate]">
  <span class="script show">
    <button type="button" id="removeedit">Drop submission</button>
  </span>
</h:ifMode>
