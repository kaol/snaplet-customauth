var csrf_ham = /csrf_ham=([0-9a-z-]+)/.exec(document.cookie);
csrf_ham = csrf_ham ? csrf_ham[1] : null;
var cid = /\?.*cid=([0-9]+)/.exec(document.location.href)
cid = cid != null ? cid[1] : null;

(function( $ ){
    $.fn.tagSelect = function() {
	$(this).on('change', function(){
	    $('.maincategory').hide();
	    var id = 'cat-'+$(this).find(':selected').attr('value');
	    $('#'+id).show();
	});
	return this;
    };

    return this;
})( jQuery );

function submitSuccess(reply) {
    if (typeof reply != 'undefined') {
	$('#msgdiv').html('');
	if (reply.ok) {
	    $('.hideafterdone').slideUp();
	    $('.removeafterdone').remove();
	    $('#msgdiv').hide().html(reply.msg).slideDown();
	} else if (reply.errmsg) {
	    enableSubmitcomic();
	    $('.submitcomic button[type=submit]').removeAttr('disabled');
	    $('#msgdiv').hide().html(reply.errmsg).slideDown();
	}
    }
}

function enableSubmitcomic() {
    var submitButton = $('.submitcomic button[type=submit]');
    submitButton.removeAttr('disabled');
    $('.submitcomic').one('submit', function() {
	submitButton.attr('disabled', 1);
	var options = {url: '/s/submit',
		       type: 'POST',
		       dataType: 'json',
		       success: submitSuccess
		      };
	if (window.FormData) {
	    options.data = new FormData(this);
	    options.processData = false;
	    options.contentType = false;
	} else {
	    options.data = $(this).serialize();
	}
	$.ajax(options);
	return false;
    });
    if (window.FormData) {
	$('.script.hasformdata').show();
    } else {
	$('.script.noformdata').show();
    }
}

function setTagsEpedias(rpy) {
    var origtags = {};
    $.each(rpy.origtags, function(idx, tagid){
	origtags[tagid] = true;
    });
    $('.submitcomic').on('change', 'input[type=checkbox]', function() {
	var tagid = this.getAttribute('value');
	if ((origtags[tagid] && this.checked) || (!origtags[tagid] && !this.checked)) {
	    $('span#difftag-'+tagid).remove();
	} else {
	    var diffelem = '<span id="difftag-'+tagid+'">'+$('label[for='+this.getAttribute('id')+']').text()+'</span>';
	    if (origtags[tagid]) {
		$('#removedtags').show();
		$('#removedtags .diff').append(diffelem);
	    } else {
		$('#addedtags').show();
		$('#addedtags .diff').append(diffelem);
	    }
	}
    });
    var initialNewtags = {};
    $.each(rpy.tags, function(idx, tagid){
	$('#cat-sel-'+tagid).click();
	initialNewtags[tagid] = true;
    });
    $.each(rpy.origtags, function(idx, tagid){
	if (!initialNewtags[tagid])
	    $('#cat-sel-'+tagid).trigger('change');
    });
    $.each(rpy.epedias, function(epid, dat) {
	$('input[name=epedia-entry-'+epid+']').val(dat.entry);
    });
}

function resetSubmitForm() {
    $('.submitcomic input[type=text]').val('');
    $('.submitcomic textarea').val('');
    $('.submitcomic input[type=checkbox]').removeAttr('checked');
    $('.submitcomic').off('change');
    $('#tagdiff .diff').html('');
}

$(document).ready(function(){
    var repage;
    $('.noscript').hide();
    $('.script.show').show();
    $('.scripthidden.show').removeClass('scripthidden');
    $('.booklink').prepend($('<button type="button" class="bookmarkforward">&gt;</button>')
			   .click(function(){
			       var par = $(this).parent()
			       var cid = par.attr('id').substring(1)
			       $.post('/updates.html', { redir: cid, csrf_ham: csrf_ham, only_update: 1 },
				      function(){par.fadeOut('fast')});
			   }));
    if ($('.submitcomic').length > 0) {
	if (cid != null && $('.submitcomic input[name="cid"]').length > 0) {
	    $.ajax({url:'/s/cinfo/'+cid,
		    dataType: 'json',
		    success:function(rpy){
			if (rpy != null) {
			    rpy.origtags = rpy.tags;
			    setTagsEpedias(rpy);
			    enableSubmitcomic();
			}
		    }});
	} else {
	    enableSubmitcomic();
	}
    }

    var hilitedialog;
    var loadedtags = new Object;
    var tagcounts = new Object;
    $('form:has(#piperka_list)').on('hilite', '#piperka_list', function(){
	$('#piperka_list button').each(function(){
	    var cid = this.getAttribute('value');
	    if (tagcounts[cid] != undefined) {
		var li = $(this).parent();
		if (tagcounts[cid] > 0) {
		    li.addClass('hilite');
		} else {
		    li.removeClass('hilite');
		}
	    }
	});
    });
    $('#tagselect').tagSelect().trigger('change');
    $('button.hilitetags').on('click', function() {
	if (hilitedialog == undefined) {
	    $.ajax({url: '/d/tags.xml',
		    dataType: 'html',
		    success: function(content){
			hilitedialog = $('#hilitedialog').html(content);
			hilitedialog.find('#tagselect').tagSelect().trigger('change');
			var lastTop, lastLeft;
			hilitedialog.dialog({appendTo: '#notmuch', dialogClass: 'isover', minWidth:200, width:400, open: function(){
			    if (lastTop != undefined)  {
				var style = this.parentNode.style;
				style.top = lastTop;
				style.left = lastLeft;
			    }
			}, close: function(){
			    var style = this.parentNode.style;
			    lastTop = style.top;
			    lastLeft = style.left;
			}});
			hilitedialog.on('change', 'input', function() {
			    function updateTagcounts(tagid, diff) {
				$.each(loadedtags[tagid], function() {
				    if (tagcounts[this] == undefined) {
					tagcounts[this] = 0;
				    }
				    tagcounts[this] += diff;
				});
				$('#piperka_list').trigger('hilite');
			    }
			    var tagid = this.getAttribute('value');
			    var diff = this.checked ? 1 : -1;
			    if (loadedtags[tagid] == undefined) {
				hilitedialog.find('.tagselector').hide();
				$.ajax({url:'/s/tagslist/'+tagid,
					method:'GET',
					dataType:'json',
					success:function(rpy){
					    hilitedialog.find('.tagselector').show();
					    if (rpy != null) {
						if (rpy.cids) {
						    loadedtags[tagid] = rpy.cids;
						}
					    }
					    updateTagcounts(tagid, diff);
					}});
			    } else {
				updateTagcounts(tagid, diff);
			    }
			});
		    }});
	} else {
	    hilitedialog.dialog('open');
	}
    });

    // Reader history chart
    if ($('.chart').length > 0) {
	var rhist = $('#readerchart');
	if (cid != null && rhist.length > 0) {
	    $.when($.ajax({url:'/d/readershistory/'+cid, method:'GET', dataType:'json'}))
		.then(function(rpy){
		    chart = rpy;
		    if (rpy.length > 1) {
			var day = new Date(), dateTicks = [];
			for (var i=0; i < 5; ++i) {
			    dateTicks[4-i] = day.toISOString().substring(0,10);
			    day.setTime(day.getTime()-7*24*60*60*1000);
			}
			var maxReader = 0, minReader;
			$.each(rpy, function() {
			    var reader = this[1];
			    minReader = minReader == null || reader < minReader ? reader : minReader;
			    maxReader = maxReader < reader ? reader : maxReader;
			});
			var readerTicks = [minReader-1 < 0 ? 0 : minReader-1], lastTick = minReader-1, j=1;
			for (var i=0; i < 4; ++i) {
			    var num = Math.floor(minReader + (maxReader-minReader)/5*i);
			    if (num != lastTick) {
				lastTick = num;
				readerTicks[j++] = num;
			    }
			}
			readerTicks[j++] = maxReader;
			readerTicks[j] = maxReader+1;
			rhist.show();
			$.jqplot('readerchart', [chart],
				 {axes:{xaxis:{renderer: $.jqplot.DateAxisRenderer,
					       ticks:dateTicks,
					       tickRenderer: $.jqplot.CanvasAxisTickRenderer,
					       tickOptions: {angle: -30, formatString: '%m-%d'}
					      },
					yaxis:{ticks:readerTicks,
					       tickOptions:{formatString: '%i'}}
				       },
				  title:"Reader history"
				 });
		    }
		});
	}
    }

    if ($('#quicksearch').length > 0) {
	var form = $('#quicksearch form');
	form.on('submit', function() {
	    var cid = form.data('cid');
	    var sorttype = $('#quicksearch input[name=sorttype]').attr('value');
	    if (cid) {
		$.getJSON('/s/qsearch', {cid: cid, sorttype: sorttype}, function(rpy) {
		    if (rpy && rpy.offset) {
			if (repage != undefined) {
			    repage(rpy.offset);
			} else {
			    if (sorttype == 'top') {
				document.location = '/top.html?offset='+rpy.offset;
			    } else {
				document.location = '/browse.html?sort='+sorttype+'&offset='+rpy.offset;
			    }
			}
		    }
		});
	    }
	    return false;
	});
	$('#quicksearch').qsearch({
	    select: function(event, ui) {
		form.data('cid', ui.item.cid);
		form.submit();
	    }});
    }

    // AJAX subscribe buttons
    $('form:has(#piperka_list)').on('click', 'button', function(event) {
	event.preventDefault();
	function updateUnread(unread) {
	    if (unread.total_new > 0) {
		var newin = $('#newin').show();
		newin.text('('+unread.total_new+' new in '+unread.new_in+')');
	    } else {
		$('#newin').hide();
	    }
	}
	var name = this.getAttribute('name');
	var subscid = this.getAttribute('value');
	var button = this;
	if (name == 'subscribe') {
	    var startAtFirst = $('#atfirst:checked').length > 0;
	    $.ajax({url: '/s/uprefs',
		    method: 'POST',
		    data: {bookmark: [subscid, startAtFirst ? 0 : 'max'], csrf_ham: csrf_ham, getunread:1},
		    success: function(rpy) {
			if (rpy && rpy.ord != undefined) {
			    $(button).attr('name', 'unsubscribe').attr('class', 'minus').text('-');
			    updateUnread(rpy);
			}
		    }
		   });
	} else if (name == 'unsubscribe') {
	    $.ajax({url: '/s/uprefs',
		    method: 'POST',
		    data: {bookmark: [subscid, 'del'], csrf_ham: csrf_ham, getunread:1},
		    success: function(rpy) {
			if (rpy && rpy.ok) {
			    $(button).attr('name', 'subscribe').attr('class', 'plus').text('+');
			    updateUnread(rpy);
			}
		    }
		   });
	}
    });

    // AJAX paginate
    if (!!(window.history && history.pushState) && $('.paginate').length > 0) {
	var b = {start: $('.paginate .start'),
		 prev: $('.paginate .prev'),
		 next: $('.paginate .next'),
		 end: $('.paginate .end')};
	var getOffsetRegexp = /[?&].*offset=([0-9]+)/;
	var getOffset = function(url) {
	    var matches = getOffsetRegexp.exec(url);
	    if (matches != undefined && matches.length > 1)
		return parseInt(matches[1]);
	    else
		return null;
	}
	var getMaxcidRegexp = /&_max=([0-9]+)/;
	var getMaxcid = function(url) {
	    var matches = getMaxcidRegexp.exec(url);
	    if (matches && matches.length > 1)
		return parseInt(matches[1]);
	    else
		return null;
	};
	var offset = getOffset(window.location.href);
	var sorttype;
	if (window.location.pathname == '/top.html') {
	    sorttype = 'top';
	} else {
	    sorttype = /[?&]sort=([a-z]+)/.exec(window.location.search);
	    if (sorttype) {
		sorttype = sorttype[1];
		switch (sorttype) {
		case 'top': case 'name': case 'new': case 'update':
		    break;
		default:
		    sorttype = 'name';
		}
	    } else {
		sorttype = 'name';
	    }
	}
	var prev = getOffset(b.prev.attr('href'));
	var container = $('#piperka_list');
	if (offset == undefined) {
	    offset = 0;
	}
	var total, perPage = getOffset($(b.next).attr('href'));
	if (perPage == undefined) {
	    if (prev != undefined) {
		perPage = offset-prev;
		total = $('#piperka_list li').length+offset;
	    }
	} else {
	    perPage -= offset;
	    total = getOffset(b.end.attr('href'))+perPage;
	}
	if (total != undefined) {
	    var lastAjax;
	    repage = function(event) {
		var newOffset, newSorttype, href, maxcidMarker = '';
		if (typeof event == 'number') {
		    newOffset = event;
		    newSorttype = sorttype;
		} else {
		    event.stopPropagation();
		    event.preventDefault();
		    var forceUpdate = false;
		    if (event.type == 'popstate') {
			href = document.location.href;
			if (event.originalEvent.state) {
			    forceUpdate = true;
			}
		    } else {
			href = this.getAttribute('href');
			if (href == undefined)
			    return;
		    }
		    newOffset = getOffset(href);
		    newSorttype = /\?.*sort=([a-z]+)/.exec(href);
		    if (newSorttype)
			newSorttype = newSorttype[1];
		    if (newOffset == undefined) {
			newOffset = 0;
			var maxcid;
			if (newSorttype == 'new' && (maxcid = getMaxcid(href)))
			    maxcidMarker = '&_max='+maxcid;
		    }
		    if (newSorttype && window.location.pathname != '/top.html') {
			switch (newSorttype) {
			case 'top': case 'name': case 'new': case 'update':
			    if (newSorttype != sorttype) {
				if (newSorttype == 'name') {
				    $('#alphabet_index').show();
				} else if (sorttype == 'name') {
				    $('#alphabet_index').hide();
				}
				$('#quicksearch input[name=sorttype]').attr('value', newSorttype);
			    }
			    break;
			default:
			    newSorttype = sorttype;
			}
		    } else {
			if (window.location.pathname == '/top.html') {
			    newSorttype = 'top';
			} else {
			    newSorttype = 'name';
			}
		    }
		}
		newOffset = newOffset < 0 ? 0 : newOffset >= total ? (total-1) : newOffset;
		if (!forceUpdate && offset == newOffset && sorttype == newSorttype)
		    return;
		sorttype = newSorttype;
		offset = newOffset;
		if (window.location.pathname == '/top.html') {
		    href = '/top.html?offset='
		} else {
		    var search = window.location.search
			.replace(/[?&]offset=[^&#]*/, '')
			.replace(/[?&]sort=[^&#]*/, '')
			.replace(/[?&]_max=[^&#]*/, '');
		    if (search == '') {
			search = '?'
		    } else {
			search = search.replace(/^./, '?') + '&';
		    }
		    href = window.location.pathname + search + 'sort='+sorttype+'&offset=';
		}
		if (offset == 0) {
		    b.start.removeAttr('href');
		    b.prev.removeAttr('href');
		} else {
		    b.start.attr('href', href + '0');
		    var newPrev = offset-perPage;
		    if (newPrev < 0)
			newPrev = 0;
		    b.prev.attr('href', href + newPrev);
		}
		var newNext = offset+perPage;
		if (newNext >= total) {
		    b.next.removeAttr('href');
		    b.end.removeAttr('href');
		} else {
		    b.next.attr('href', href + newNext);
		    b.end.attr('href', href + (total-perPage));
		}
		container.fadeTo('slow', 0, 'linear');
		if (lastAjax)
		    lastAjax.abort();
		href = href + offset + maxcidMarker;
		lastAjax = $.ajax({url: href,
				   dataType: 'html',
				   data: {min: 1},
				   success: function(content) {
				       lastAjax = null;
				       container.stop();
				       var newContent = $(content);
				       var newContainer = newContent.filter('#piperka_list');
				       if (newContainer.length == 0)
					   newContainer = newContent.find('#piperka_list');
				       container.replaceWith(newContainer);
				       container = newContainer;
				       if (event.type != 'popstate')
					   history.pushState(true, null, href);
				       container.trigger('hilite');
				   }});
	    };
	    $('.paginate').on('click', 'a', repage);
	    $('#alphabet_index').on('click', 'a', repage);
	    $('#sort_by').on('click', 'a', repage);
	    $(window).on('popstate', repage);
	    href = undefined;
	    $('body').on('click', 'a', function(event) {
		history.replaceState(false, $('title').text());
		return true;
	    });
	}
    }

    $(".followcheck").on('change', 'input[type="checkbox"]', function(){
	var type = this.getAttribute('name');
	var name = this.getAttribute('value');
	var checkbox = this;
	var action;
	if (type == 'followee') {
	    action = this.checked ? 'permit' : 'deny';
	} else if (type == 'follow') {
	    action = this.checked ? 'follow' : 'unfollow';
	}
	if (action == undefined) {
	    this.checked = !this.checked;
	    return;
	}
	var docancel = function() {
	    checkbox.checked = (action == 'permit' || action == 'follow') ? false : true;
	};
	this.setAttribute('disabled', true);
	$.ajax({url: '/s/profile',
		method: 'POST',
		data: {action: action, name: name, csrf_ham: csrf_ham},
		success: function(rpy) {
		    if (!(rpy && rpy.rows && rpy.rows > 0)) {
			docancel();
		    }
		    checkbox.removeAttribute('disabled');
		},
		failure: function(rpy) {
		    docancel();
		    checkbox.removeAttribute('disabled');
		}
	       });
    });

    $('#followee').on('click', 'button', function(){
	var row = $(this).parents('tr');
	var button = this;
	this.setAttribute('disabled', true);
	$.ajax({url: '/s/profile',
		method: 'POST',
		data: {action: 'unfollow', name: this.getAttribute('name'),  csrf_ham: csrf_ham},
		success: function(rpy) {
		    if (rpy.rows != undefined && rpy.rows > 0) {
			row.fadeOut();
		    } else {
			button.removeAttribute('disabled');
		    }
		},
		failure: function(rpy) {
		    button.removeAttribute('disabled');
		}
	       });
    });

    $('span#interest').on('click', 'button', function() {
	var action = this.getAttribute('id') == 'cancel' ? 'unfollow' : 'follow';
	var button = this;
	button.setAttribute('disabled', true);
	$.ajax({url: '/s/profile',
		method: 'POST',
		data: {action: action, name: this.getAttribute('name'), csrf_ham: csrf_ham},
		success: function(rpy) {
		    if (rpy.rows != undefined && rpy.rows > 0) {
			window.location.href = window.location.href;
		    } else {
			button.removeAttribute('disabled');
		    }
		}
	       });
    });

    // Account management field disables and enables
    var oldPasswdInput = $('input[name="oldpasswd"]');
    $('#authenticate_with').on('change', function() {
	if ($(this).find(':selected').attr('value') == 'Password') {
	    oldPasswdInput.removeAttr('disabled');
	} else {
	    oldPasswdInput.attr('disabled', 1);
	}
    });
    var passwdInput = $('input[name="newpasswd"], input[name="newpasswd_again"]');
    $('#only_oauth2').on('change', function() {
	var checkbox = this;
	if (this.checked) {
	    passwdInput.attr('disabled', 1);
	} else {
	    passwdInput.removeAttr('disabled');
	}
    });

    // Moderator interface
    if ($.fn.pModerate) {
	$('#user-edits tbody').pModerate();
	$('#user-submits tbody').pSubmissions();
    }
});
