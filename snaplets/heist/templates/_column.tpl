<h:listingMode type="[Top]">
  <ol start="${h:startNum}">
    <h:listingStdItem/>
  </ol>
</h:listingMode>
<h:listingMode type="[Browse,Profile]">
  <ul class="list">
    <h:listingStdItem/>
  </ul>
</h:listingMode>
<h:listingMode type="[Update]">
  <ul class="list">
    <h:item type="UpdateMode">
      <h:holdbookmark check="True">
	<li h:class="booklink" id="c${h:cid}">
	  <h:externA href="${h:directLink}"><h:title/></h:externA>
	  (<h:new/> new)
	</li>
      </h:holdbookmark>
      <h:holdbookmark check="False">
	<li h:class="">
	  <h:externA href="info.html?redir=${h:cid}&amp;csrf_ham=${h:csrf}${h:offsetBackParam}"><h:title/></h:externA>
	  (<h:new/> new)
	</li>
      </h:holdbookmark>
    </h:item>
  </ul>
</h:listingMode>
<h:listingMode type="[Graveyard]">
  <ul class="list">
    <h:item type="ListingMode">
      <li>
	<button class="script null" value="${h:cid}">&nbsp;</button>
	<a href="deadinfo.html?cid=${h:cid}"><h:title/></a>
      </li>
    </h:item>
  </ul>
</h:listingMode>
