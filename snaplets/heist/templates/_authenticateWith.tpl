<h:content>
  <p>
    Authenticate changes with
    <select name="authenticate_with" id="authenticate_with">
      <option value="Password" h:hasNoPassword="disabled">Password</option>
      <h:oauth2Providers filter="True">
	<option value="${h:name}"><h:label/></option>
      </h:oauth2Providers>
    </select>
  </p>
  <p>
    Password: <input type="password" name="_password"/>
  </p>
</h:content>
