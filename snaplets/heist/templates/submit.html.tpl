<h:piperka>
  <h:submit mode="Submit">
    <span class="hideafterdone">
      <h2>Policies</h2>
      <p>
	As I can't quite accomplish the impossible, I will have to lay down
	a few policies on what will be added to Piperka.  Or jump below to
	<a href="#submit">submit now</a>.
      </p>
      <h3>What does Piperka do?</h3>
      <p>
	Having a comic listed on Piperka means more than having just
	the banner and home page URL stored on the server.  Piperka
	will also build an index of all of the archive pages of a
	comic, and periodically check if more pages have been added to
	it.  As such, I will have to place a number of requirements on
	anything that I would add to Piperka.
      </p>
      <p>
	Building the index is done by initially giving the first page
	of the archives to the crawler and writing an HTML parsing
	script for seeking for a possible link to the next page.
	Updates to the archive are sought for by trying to find the
	next page from the newest known page.
      </p>
      <p>
	The parser used for this will often have to be custom tailored
	for a site.  There are no widely used standards for these
	kinds of pages.  The first priority of a web designer is to
	make their pages look good.  However, it doesn't have to be
	done at the cost of navigatibility for scripts too.
      </p>
      <h3>A request to comic authors</h3>
      <p>
	This doesn't really apply to you if your comic is hosted on a
	major host like Comicgenesis or Webtoons.  They've got their
	quirks, but the effort to take them into account has already
	been taken.  Not that I would mind if their administrators
	would apply the following advice.
      </p>
      <p>
	I'm not necessarily going to outright reject a comic if your's
	doesn't implement what I describe below, either.  But I do
	consider that having a comic listed on Piperka is a service
	for your readers and I would like to see my job be easier.
      </p>
      <p>
	There does exist a specification in HTML 4.01 standard (and
	later) for a sequential series of documents, like the pages of
	a comic.  I would like to point to the <tt>rel</tt> attribute
	in the definition of
	<a href="http://www.w3.org/TR/html4/struct/links.html#edef-A">A</a>
	and <a href="http://www.w3.org/TR/html4/struct/links.html#edef-LINK">LINK</a>
	tags.  If you look at
	the <a href="http://www.w3.org/TR/html4/types.html#type-links">link
	types</a> defined in the standard, you'll find ready
	definitions for <tt>start</tt>, <tt>prev</tt>, <tt>next</tt>
	and <tt>index</tt>.  These would neatly correspond to the
	usual set of links to the first, previous, next and today's
	page.
      </p>
      <p>
	If you would apply the <tt>rel</tt> attribute to your pages,
	at least the <tt>next</tt> one, I wouldn't have to try to
	match the text in the link to the next page or look for key
	words in the URLs of images or use any other of the tricks I'd
	need to find that elusive next page.  Thank you.
      </p>

      <h3>Other policies</h3>
      <dl>
	<dt>Needs to be crawlable</dt>

	<dd>
	  Piperka is not a generic webcomic list.  I'll pass over
	  comics that I can't write a crawler for.  Existing ones that
	  have removed their archives temporarily will still be listed
	  and kept under a watch.  Also, if an individual archive page
	  has no link to the next page, it'll remain out.
	</dd>

	<dt>Public archives</dt>

	<dd>
	  The archives will have to be publicly available.  Also, they
	  need to stay public too.  Exceptions might be made if the
	  comic is not actively removing archives older than <i>n</i>
	  days and has the newest archives available.
	</dd>

	<dt>Copyrights</dt>

	<dd>
	  As a general rule, if the comic doesn't have the copyright
	  to the material used or have a license of some sort for it,
	  it might be better to leave it off Piperka.  It's just a
	  clearer situation all around that way.
	</dd>

	<dt>Translations</dt>

	<dd>
	  Piperka will stick to tracking any official translation(s),
	  possibly an unofficial one to English too.  One reason for
	  this: see <i>Copyrights</i> above.
	</dd>

	<dt>Distinct URLs</dt>

	<dd>
	  Piperka stores URLs of the pages in the archive.  There's
	  any number of tricks to have different pages reside on the
	  same location pointed to by the URL, including using Flash,
	  Javascript or frames.  Also, I can't screen scrape Flash.
	</dd>

	<dt>Sites that use frames</dt>

	<dd>
	  If the site uses frames, I will have to unframe them and
	  link directly to the comic frames.  Sorry.  I may reject the
	  submission if this would break the layout too badly.  At the
	  minimum it would be good if there was a link in the comic
	  frame to the main site.
	</dd>

	<dt>Cookies</dt>

	<dd>The comic must be readable without having cookies set.</dd>

	<dt>One archive per submit</dt>

	<dd>
	  Some sites have multiple comics on them.  If you want to
	  have them all listed, please take the effort to submit each
	  one separately.
	</dd>

	<dt>Finished comics</dt>

	<dd>
	  Comics that have ceased to update for one reason or another
	  but that still have the archives available are OK, as long
	  as it is not just a handful of pages or a preview of some
	  sort.
	</dd>

	<dt>I get to decide</dt>

	<dd>
	  Ultimately, I may reject submissions just on a whim.  Feel
	  free to ask about it if I haven't added the comic after a
	  while.  Most likely the reason was technical or I have a bit
	  of a backlog.
	</dd>
      </dl>

      <h2 id="submit">Submit a comic</h2>
      <p>
	This all being said, I do add most requested comics to Piperka
	as well as some that haven't been asked for yet, too.
      </p>

      <p>
	<h:csrfForm class="submitcomic script show">
	  <input type="hidden" name="formtype" value="submit" />
	  Title: <input type="text" name="title">
	  <p>Home page: <input type="text" name="url" value="${h:preHomepage}">
	  <p>First page: <input type="text" name="first_page" />
	  <p>
	    You don't need to provide a first page, but if you want to
	    save me a few seconds, then feel free to.  If the site's
	    navigation makes the URL in your browser's location bar
	    stay the same when you get to the first page, then the
	    comic might not be suitable for Piperka.
	  </p>
	  <h:submitForm/>
	</h:csrfForm>

      <p class="script show">
	And if your comic begins like "The Comic", check around C
	first.  The ordering used is arguably somewhat arbitrary.
      </p>
    </span>

    <div class="noscript">
      Submitting comics requires JavaScript.  Please
      enable it.
    </div>

    <div id="msgdiv"/>
  </h:submit>
</h:piperka>
