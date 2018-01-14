(function($) {
    var qsearchRowsDefault = 10;
    var comics;

    $.fn.qsearch = function(options){
	var form = this.find('form');
	var qsearchButton = this.find('button').attr('disabled', 1);
	var lastSelect = null;
	var qsearchRows = qsearchRowsDefault;
	if (comics == null) {
	    comics = $.getJSON('/d/comics_ordered.json');
	}
	this.find('#showallresults').on('change', function(ev) {
	    qsearchRows = ev.target.checked ? 100000 : qsearchRowsDefault;
	});

	var qinput_source = function(req, resp){
	    var hits = [];
	    $.when(comics).then(function(rpy) {
		var matches = {};
		var reqlen = req.term.length;
		var req_lc = req.term.toLowerCase();
		// First, exact matches
		$.each(rpy, function() {
		    if (this[1].substr(0, reqlen).toLowerCase() == req_lc) {
			if (options.filterFunc && options.filterFunc(this[0]))
			    return true;
			hits.push({label: this[1], cid: this[0]});
			matches[this[0]] = 1;
			if (hits.length >= qsearchRows) {
			    return false;
			}
		    }
		});
		// Then, grab regexp results
		if (hits.length < qsearchRows) {
		    var req_regexp;
		    try {
			req_regexp = new RegExp(req.term, 'i');
		    } catch (err) {
			return false;
		    }
		    $.each(rpy, function() {
			if (options.filterFunc && options.filterFunc(this[0]))
			    return true;
			if (matches[this[0]] == null && req_regexp.test(this[1])) {
			    hits.push({label: this[1], cid: this[0]});
			}
			if (hits.length >= qsearchRows) {
			    return false;
			}
		    });
		}
		if (hits.length > 0) {
		    form.data('cid', hits[0].cid);
		    qsearchButton.removeAttr('disabled');
		} else {
		    form.data('cid', null);
		    qsearchButton.attr('disabled', 1);
		    lastSelect = null;
		}
		resp(hits);
	    }, function() {
		resp([]);
	    })};

	var optSelect = options.select;
	var optSubmit = options.submit;
	delete options.select;
	var qinput = this.find('input[type="input"]');
	var settings = $.extend({
	    minLength: 1,
	    delay: 100,
	    autoFocus: true,
	    select: function(event, ui) {
		lastSelect = ui;
		if (optSelect)
		    optSelect.apply(this, arguments);
	    },
	    source: qinput_source
	    }, options);

	qinput.autocomplete(settings);
	form.on('submit', function(ev){
	    if (lastSelect && optSubmit)
		optSubmit.call(qinput[0], ev, lastSelect);
	    return false;
	});
	return this;
    };
})(jQuery);
