<h:ifLoggedIn>
  <h:form>
    <input type="hidden" name="csrf_ham" value="${h:csrf}"/>
    <h:apply-form-content/>
  </h:form>
</h:ifLoggedIn>
<h:ifLoggedOut>
  <h:apply-form-content/>
</h:ifLoggedOut>
