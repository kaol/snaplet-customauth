<h:comicInfo>
  <h:banner>
    <p>
      <img src="${h:bannerUrl}"/>
    </p>
  </h:banner>
  <h2><h:title/></h2>
  <p>
    Subscriptions: <h:readersLink><h:subscriptions/></h:readersLink>
  </p>
  <p>
    Total pages: <h:hasFragments><h:fragmentCount/>/</h:hasFragments><h:pageCount/>
    <h:ifKnowPages>
      <true>
	| <h:externA href="${h:firstPageUrl}">First page</h:externA>
	| <h:externA href="${h:lastPageUrl}">Last known page</h:externA>
	<h:ifFixedHead>
	  (excluding front page)
	</h:ifFixedHead>
      </true>
      <false>
	| No pages in index
      </false>
    </h:ifKnowPages>
  </p>
  <p>
    Homepage: <h:externA href="${h:homepage}"><h:homepageText/></h:externA>
  </p>
  <h:ifExternLinks>
    <p>
      This comic on:
      <h:externLink>
	<h:externA href="${h:url}" title="${h:description}"><h:siteName/></h:externA>
      </h:externLink>
    </p>
  </h:ifExternLinks>
  <h:ifAddDate>
    <p>
      Added on: <h:addDate/>
    </p>
  </h:ifAddDate>
  <p>
    Categories:
    <h:categories>
      <span h:description="" h:class=""><h:name/></span>
    </h:categories>
  </p>
</h:comicInfo>
