<h:form>
  <h:ifLoggedIn>
    <input type="hidden" name="csrf_ham" value="${h:csrf}"/>
  </h:ifLoggedIn>
  <h:apply-form-content/>
</h:form>
