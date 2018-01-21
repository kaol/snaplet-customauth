<h:subscribeControl>
  <h:ifLoggedIn>
    <div>
      <label for="atfirst">Bookmark the first comic</label>
      <input id="atfirst" type="checkbox" name="start_at_first" checked="checked"/>
    </div>
  </h:ifLoggedIn>
</h:subscribeControl>
<div class="paginate">
  <h:start><a class="start" h:href="href">First</a></h:start>
  |
  <h:prev><a class="prev" rel="prev" h:href="href">Last</a></h:prev>
  |
  <h:next><a class="next" rel="next" h:href="href">Next</a></h:next>
  |
  <h:end><a class="end" h:href="href">End</a></h:end>
</div>
<h:apply-content/>
<div class="paginate bottom">
  <h:start><a class="start" h:href="href">First</a></h:start>
  |
  <h:prev><a class="prev" rel="prev" h:href="href">Last</a></h:prev>
  |
  <h:next><a class="next" rel="next" h:href="href">Next</a></h:next>
  |
  <h:end><a class="end" h:href="href">End</a></h:end>
</div>
