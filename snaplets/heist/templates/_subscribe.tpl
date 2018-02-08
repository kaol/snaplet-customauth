<h:csrfForm method="post" h:action="1">
  <input type="hidden" name="subscribe" value="${h:cid}"/>
  <input type="checkbox" id="start_at_first" name="start_at_first" value="1"/>
  <label for="start_at_first">Set bookmark at the first page</label>
  <input type="submit" name="action" value="Subscribe"/>
</h:csrfForm>
