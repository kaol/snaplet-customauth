<h:piperka ads="False">
  <h:ifLoggedIn>
    <h:ifMod>
      <h2>Submission</h2>
      <div id="submission-entry">
	<p>sid: <span id="sid"/> <a id="genentry-link" target="_blank">genentry</a></p>
	<p>Title: <span id="title"/></p>
	<p>Homepage: <a id="homepage" href=""/></p>
	<p>First page: <a id="first_page" href=""/></p>
	<p><textarea id="description" readonly="1"/></p>
	<p>User: <span id="user"/> From ip: <span id="from_ip"/></p>
	<p>
	  Email: <input type="text" readonly="1" id="email"/> Wants
	  email: <input id="want_email" type="checkbox" readonly="1"/>
	</p>
	<p>banner_url (old): <a id="banner_url" href=""/></p>
      </div>
      <div style="height: 60px" id="submission-banner"/>
      <div id="msgdiv"/>
      <h2>Submissions</h2>
      <table id="user-submits">
	<tr>
	  <th>sid</th>
	  <th>Title</th>
	  <th>Submitted on</th>
	  <th>From IP</th>
	  <th>User</th>
	</tr>
	<h:submissions>
	  <tr id="sid-${h:sid}">
	    <td><h:sid/></td>
	    <td><h:title/></td>
	    <td><h:submittedOn/></td>
	    <td><h:fromIP/></td>
	    <td><h:name/></td>
	  </tr>
	</h:submissions>
      </table>
    </h:ifMod>
  </h:ifLoggedIn>
  <h:notMod>
    <h2>403 Go away</h2>
    <p>
      This page is meant for moderators, not for mere mortals.
    </p>
  </h:notMod>
</h:piperka>
