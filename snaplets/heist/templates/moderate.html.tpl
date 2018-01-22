<h:piperka ads="False">
  <h:submit mode="Moderate">
    <h:ifLoggedIn>
      <h:ifMod>
	<h2>Moderate</h2>
	<table id="user-edits">
	  <tr>
	    <th>sid</th>
	    <th>Comic</th>
	    <th>Submitted on</th>
	    <th>From IP</th>
	    <th>User</th>
	  </tr>
	  <h:ifLoggedIn>
	    <h:listOfEdits>
	      <tr id="${h:sidId}" h:class="youCare">
		<td><h:sid/></td>
		<td class="cid" id="${h:cidId}"><h:title/></td>
		<td><h:addedOn/></td>
		<td><h:fromIP/></td>
		<td><h:name/></td>
	      </tr>
	    </h:listOfEdits>
	  </h:ifLoggedIn>
	</table>
	<h2>Current Entry</h2>
	<div id="current-entry"/>
	<h:csrfForm class="submitcomic script" id="editinfo">
	  <input type="hidden" name="formtype" value="editinfo"/>
	  <input type="hidden" name="user_sid" value=""/>
	  <input type="hidden" name="cid" value=""/>
	  Title: <a id="info-title"></a>
	  <p>
	    CID: <span id="info-cid"/>
	  </p>
	  <div style="height: 60px" id="useredit-banner"/>
	  <h:submitForm/>
	</h:csrfForm>
      </h:ifMod>
    </h:ifLoggedIn>
    <h:notMod>
      <h2>403 Go away</h2>
      <p>
	This page is meant for moderators, not for mere mortals.
      </p>
    </h:notMod>
  </h:submit>
</h:piperka>
