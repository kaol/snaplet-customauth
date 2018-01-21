var csrf_ham = /csrf_ham=([0-9a-z-]+)/.exec(document.cookie);
csrf_ham = csrf_ham ? csrf_ham[1] : null;
var cid, visiblepage;
var userreq, userdata, subscriptions;
var comictitlesreq, comictitles;

function maximizeReader() {
    $('body').height(window.innerHeight);
    $('#reader').height(window.innerHeight-$('#navigation').height());
}

function getPage(ord) {
    var info = $('#archivedialog').data();
    if (info.pages[ord][0] != undefined) {
	return info.url_base+info.pages[ord][0]+info.url_tail;
    } else {
	return info.fixed_head == undefined ? info.homepage : info.fixed_head;
    }
}

function loadPage(ord) {
    if (visiblepage == undefined) {
	visiblepage = ord;
    }
    var page = getPage(ord);
    if (page != undefined) {
	var fix = $('#fixiframe').prop('checked');
	var iframe = $('<iframe src="'+page+'" '+(fix ? ' scrolling="no"' : '')+'/>').data('ord', ord);
	return iframe;
    }
}

function refreshBookmark(rpy) {
    if (rpy && ((typeof rpy == 'object' && rpy.ord != undefined) || (typeof rpy == 'string' && rpy == 'keep'))) {
	if (subscriptions[cid] == undefined) {
	    subscriptions[cid] = [cid];
	}
	if (typeof rpy == 'object') {
	    subscriptions[cid][1] = parseInt(rpy.ord);
	}
	if (subscriptions[cid][1] != undefined) {
	    var unread = subscriptions[cid][2]-subscriptions[cid][1]+1;
	    $('#bookmarkmarker').show().appendTo($('#archivedialog tr:eq('+(subscriptions[cid][1]+1)+') .status'));
	    subscriptions[cid].uiRow.find('.unread').text(unread > 0 ? unread : '');
	}
    }
}

function setPage(ord, button, moveforward) {
    if (cid == undefined || (button != undefined && (visiblepage == undefined || $(button).prop('disabled')))) {
	return undefined;
    }
    var pages = $('#archivedialog').data('pages');
    var iframe;
    var dir = ord-visiblepage;
    dir = button != undefined && Math.abs(dir) == 1 ? dir : 0;
    if (ord <= 0) {
	ord = 0;
	$('#first,#prev').attr('disabled', 1);
    } else {
	$('#first,#prev').removeAttr('disabled');
    }
    if (ord+1 >= pages.length) {
	ord = pages.length-1;
	$('#next,#current').attr('disabled', 1);
    } else {
	$('#next,#current').removeAttr('disabled');
    }
    var next = ord+dir;
    if (dir == 1 && $('#reader #next').length > 0 && $('#reader #next').data('ord') == ord) {
	$('#reader iframe:not(#next)').remove();
	iframe = $('#reader #next').removeAttr('id');
	if (next < pages.length) {
	    loadPage(next).attr('id', 'next').appendTo($('#reader'));
	}
    } else if (dir == -1 && $('#reader #prev').length > 0 && $('#reader #prev').data('ord') == ord) {
	$('#reader iframe:not(#prev)').remove();
	iframe = $('#reader #prev').removeAttr('id');
	if (next >= 0) {
	    loadPage(next).attr('id', 'prev').appendTo($('#reader'));
	}
    } else {
	iframe = loadPage(ord);
	$('#reader').empty().append(iframe);
	var prevnext = dir == -1 ? 'prev' : 'next';
	if (dir && next >= 0 && next < pages.length) {
	    iframe.on('load', function() {
		loadPage(next).attr('id', prevnext).appendTo('#reader');
	    });
	}
    }
    visiblepage = ord;
    $('#currentpagemarker').show().appendTo($('#archivedialog tr:eq('+(ord+1)+') .status'));
    $('#pagenum').text(ord+1);
    if (userdata) {
	if (moveforward && $('#autoupdate').prop('checked')) {
	    $.post('/s/uprefs', {bookmark:[cid, ord+1], csrf_ham:csrf_ham}, refreshBookmark, 'json');
	} else {
	    refreshBookmark('keep');
	}
    }
    var page = getPage(ord);
    $('#url').attr('value', page).width(window.innerWidth-$('#preurl').get(0).offsetLeft-20);
    $('#tocomic').attr('href', page);
    if (button != undefined || moveforward) {
	centerDialog();
    }
    if ($('#fixiframe').prop('checked')) {
	iframe.css('height', $('#fixheight span').text());
	$('#reader').focus().scrollTop($('#lockselect').prop('checked') ? $('#lockheight').attr('value') : 0);
    } else {
	iframe.focus();
    }
}

function initcid(newcid) {
    if ($('#logo button').length == 0) {
	$('#welcome').appendTo($('body')).dialog({autoOpen: false, minwidth: 300, width: 500});
	$('<button type="button" id="welcomebutton">?</button>').on('click', function() {
	    // Workaround, see http://forum.jquery.com/topic/dialog-position-issue
	    $('#archivedialog,#mycomicsdialog').dialog('close');
	    $('#welcome').dialog('open');
	}).appendTo('#logo');
    }
    $('#lockheight').attr('value', 0);
    $('#reader').empty();
    $('.marker').hide().appendTo($('body'));
    iframes = [];
    visiblepage = undefined;
    cid = newcid;
    var archive = makePageDialog(cid);
    $.when(userreq, comictitlesreq, archive)
	.then(function(userresp, comictitlesresp, archive){
	    $('#title').text(comictitles[cid]);
	    $('#pagetotal').text(archive.pages.length);
	    $('#archive,#tocomic').removeAttr('disabled');
	    $('#archivedialog').on('click', 'tr:has(td)', function(){
		if ($(this).has('#currentpagemarker').length) {
		    if (subscriptions) {
			var thisrow = $(this);
			$.post('/s/uprefs', {bookmark:[cid, thisrow.data('ord')], csrf_ham:csrf_ham}, refreshBookmark, 'json');
		    }
		} else {
		    setPage($(this).data('ord'));
		}
	    });
	    $('#archive').on('click', function() {
		$('#welcome,#mycomicsdialog').dialog('close');
		$('#archivedialog').dialog('open');
	    });
	    var ord = 0;
	    var moveforward = 0;
	    if (subscriptions && subscriptions[cid]) {
		subscriptions[cid][2] = archive.pages.length-1;
		if (subscriptions[cid][1] != undefined) {
		    moveforward = 1;
		    ord = subscriptions[cid][1];
		}
	    }
	    setPage(ord, undefined, moveforward);
	});
}

$(document).ready(function(){
    maximizeReader();
    var navigationHeight = $('#navigation').innerHeight();
    $('#moreoptions').mouseleave(function(){
	$(this).hide()
	if ($('#fixiframe').prop('checked')) {
	    $('#reader').focus();
	}
    });
    $('#logo').mouseenter(function(){$('#moreoptions').show()});
    $(window).on('resize', maximizeReader).on('keydown', function(e) {
	if ($('#arrownavigate').prop('checked')) {
	    if (e.keyCode == $.ui.keyCode.LEFT) {
		$('#prev').click();
		return false;
	    } else if (e.keyCode == $.ui.keyCode.RIGHT) {
		$('#next').click();
		return false;
	    }
	}
    });
    if (csrf_ham) {
	userreq = $.ajax({url: '/s/uprefs',
			  method: 'POST',
			  dataType: 'json'
			 });
    }
    comictitlesreq = $.ajax({url: '/d/comictitles.json',
			     method: 'GET',
			     dataType: 'json'
			    });
    $.when(userreq, comictitlesreq).then(function(userresp, comictitlesresp){
	comictitles = comictitlesresp[0];
	if (userresp == undefined) {
	    $('.nouser').show();
	} else if (typeof userresp == 'object' && userresp[0].name != undefined) {
	    $('#namehere').text(userresp[0].name);
	    $('.user').show();
	    userdata = userresp[0];
	    subscriptions = {};
	    var dialog = $('<div title="My Comics" id="mycomicsdialog"><table><tr><th>Comic</th><th>Unread</th></tr></table></div>')
		.on('click', 'tr:has(td)', function() {
		    initcid($(this).data('cid'));
		});
	    var table = dialog.find('table');
	    $.each(userdata.subscriptions, function(idx) {
		subscriptions[this[0]] = this;
		this.uiRow = $('<tr><td>'+comictitles[this[0]]+'</td><td class="unread">'+(this[4] > 0 ? this[4] : '')+'</td><td class="status"/></tr>').data('cid', this[0]);
		table.append(this.uiRow);
	    });
	    dialog.dialog({autoOpen: false, minwidth: 300, height: 400});
	    $('#mycomics').on('click', function(){
		$('#archivedialog,#welcome').dialog('close');
		dialog.dialog('open');
	    });
	}
    });
    $('#first').on('click', function() {
	setPage(0, this);
    });
    $('#next').on('click', function(){
	setPage(visiblepage+1, this, 1);
    });
    $('#prev').on('click', function(){
	setPage(visiblepage-1, this);
    });
    $('#current').on('click', function(){
	setPage($('#archivedialog').data('pages').length-1, this);
    });
    var cidFromHref = /cid=([0-9]+)/.exec(window.location.href);
    if (cidFromHref != undefined) {
	initcid(cidFromHref[1]);
	$('#autoupdate').removeAttr('checked');
    }
    $('#fixiframe').on('change', function() {
	if ($(this).prop('checked')) {
	    $('#withfix').show();
	    $('#reader').css('overflow-y', 'auto');
	} else {
	    $('#withfix').hide();
	    $('#reader').css('overflow-y', 'visible');
	}
	$('#reader').empty();
	setPage(visiblepage);
    });
    $('#fixheight button').on('click', function(){
	var reader = $('#reader');
	var height = $('#fixheight input').val();
	if (height < reader.height()) {
	    height = reader.height();
	}
	$('#fixheight span').text(height);
	$('iframe').height(height);
	reader.focus();
    });
    $('#lockselect').on('change', function() {
	var reader = $('#reader');
	var lockheight = $('#lockheight');
	if ($(this).prop('checked')) {
	    reader.off('scroll');
	} else {
	    reader.on('scroll', function() {
		lockheight.attr('value', reader.scrollTop());
	    });
	}
	reader.focus();
    });
});
