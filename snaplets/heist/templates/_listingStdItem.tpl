<h:item type="UserMode">
  <li>
    <h:followee>
      <a href="readers.html?cid=${h:cid}" class="followee">F</a>
    </h:followee>
    <h:subscribed check="True">
      <button class="minus" name="unsubscribe" value="${h:cid}" type="submit">-</button>
    </h:subscribed>
    <h:subscribed check="False">
      <button class="plus" name="subscribe" value="${h:cid}" type="submit">+</button>
    </h:subscribed>
    <a href="info.html?cid=${h:cid}"><h:title/></a>
  </li>
</h:item>
<h:item type="ListingMode">
  <li>
    <a href="info.html?cid=${h:cid}"><h:title/></a>
  </li>
</h:item>
