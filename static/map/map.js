var csrf_ham = /csrf_ham=([0-9a-z-]+)/.exec(document.cookie);
if (!csrf_ham) {
    csrf_ham = /p_session=([0-9a-z-]+)/.exec(document.cookie)
}
csrf_ham = csrf_ham ? csrf_ham[1] : null;
var queryParms = {};
(function(){
    var re = /[?&](.*?)=([^&]*)/g;
    var match;
    while(match = re.exec(document.URL)) {
	queryParms[decodeURIComponent(match[1])] = decodeURIComponent(match[2]);
    }
})();
var mediumThreshold = 0.02, smallThreshold = 0.04;

function maximizeMap() {
    $('body').height(window.innerHeight);
}

function makeSVG(tag, attrs) {
    var el= document.createElementNS('http://www.w3.org/2000/svg', tag);
    for (var k in attrs)
	el.setAttribute(k, attrs[k]);
    return el;
}

function zoom(dir, screenX, screenY) {
    $('#selectionpalette').hide();
    var g = $('svg g#map')[0];
    var svg = $('svg');
    var view = svg[0].viewBox.baseVal;
    var trans = g.transform.baseVal.getItem(0);
    var oldScale = trans.matrix.a, scale;
    var scaleTo = false;
    if (oldScale > 0.2 && dir == 'in')
	return false;
    var x, y;
    if (typeof dir == 'number') {
	var comic = $('#c'+dir)[0];
	if (comic) {
	    var cx = comic.cx.baseVal.value, cy = comic.cy.baseVal.value, r = comic.r.baseVal.value;
	    scaleTo = true;
	    scale = 0.08/r;
	    x = scale*cx-0.5;
	    y = scale*cy-0.5;
	    dir = 'in';
	} else {
	    console.log('no comic found on map');
	    return false;
	}
    } else if (typeof screenX == 'undefined') {
	x = view.x+0.5;
	y = view.y+0.5;
    } else {
	var coord = svg.realToSvgCoord(screenX, screenY);
	x = coord.x;
	y = coord.y;
    }
    var preX = x/oldScale, preY = y/oldScale;
    if (!scaleTo) {
	switch (dir) {
	case 'in':
	    scale = oldScale*1.61803398874989484820;
	break;
	case 'out':
	    scale = oldScale/1.61803398874989484820;
	break;
	}
    }
    var postX = x/scale, postY = y/scale;
    if (scaleTo) {
	view.x = x;
	view.y = y;

    } else {
	var nudgeX = scale*(preX-postX), nudgeY = scale*(preY-postY);
	view.x = view.x+nudgeX;
	view.y = view.y+nudgeY;
    }
    displayVisibleTexts(scale, oldScale);
    trans.setScale(scale, scale);
    return true;
}

// Show and hide comic texts after zoom or pan
function displayVisibleTexts(scale, oldScale) {
    var viewBox = $('svg')[0].viewBox.baseVal;
    if (typeof scale === 'undefined') {
	scale = $('svg g#map')[0].transform.baseVal.getItem(0).matrix.a;
    }
    if (oldScale != undefined) {
	// Hide elements unconditionally on zoom out
	if (scale < oldScale) {
	    if (scale < mediumThreshold && oldScale >= mediumThreshold)
		$('text.medium').hide();
	    if (scale < smallThreshold && oldScale >= smallThreshold)
		$('text.small').hide();
	}
    }
    var texts;
    if (scale >= smallThreshold) {
	texts = $('svg g#map text.comic.medium,svg g#map text.comic.small');
    } else if (scale >= mediumThreshold) {
	texts = $('svg g#map text.comic.medium');
    } else {
	return;
    }
    var minX = (viewBox.x-3)/scale, minY = (viewBox.y-2.5)/scale;
    var maxX = (viewBox.x+2)/scale, maxY = (viewBox.y+2.5)/scale;
    texts = texts.filter(function(){
	var x = this.x.baseVal.getItem(0).value, y = this.y.baseVal.getItem(0).value;
	if (x <= maxX && x >= minX && y <= maxY && y >= minY) {
	    return true;
	} else {
	    return false;
	}
    });
    texts.show();
}

function moveblurb(ev){
    var x = ev.originalEvent.clientX, y = ev.originalEvent.clientY;
    var nameblurb = $('#comicnameblurb');
    var el = nameblurb[0];
    el.style.left = (x-(10+el.clientWidth))+"px";
    el.style.top = (y-(10+el.clientHeight))+"px";
}

(function($) {
    $.fn.realToSvgCoord = function(coordX, coordY){
	var view = this[0].viewBox.baseVal;
	var offsetX = 0, offsetY = 0;
	var container = this.parent()[0];
	var width = container.clientWidth, height = container.clientHeight, size;
	if (container.clientWidth > container.clientHeight) {
	    size = container.clientHeight;
	    offsetX = (coordX-(width-size)/2)/size;
	    offsetY = coordY/size;
	} else {
	    size = container.clientWidth;
	    offsetX = coordX/size;
	    offsetY = (coordY-(height-size)/2)/size;
	}
	return {x: view.x+offsetX, y: view.y+offsetY, size: size, rawX:offsetX, rawY:offsetY};
    };

    $.fn.clickPalette = function(options){
	var svg = this;
	var palette = $('#selectionpalette');
	var shadow = $('#selectionpalette .shadow')[0];
	var cid = {};
	var atfirst = $('#atfirst')[0];
	palette.on('mouseleave', function(ev){
	    palette.hide();
	});
	var uprefs = $(this).data('uprefs');
	$('#subscribe').on('click', function(){
	    var subcid = cid.cid;
	    var pos = atfirst.checked ? 0 : 'max';
	    var subreq = $.ajax({url: '/s/uprefs',
				 data: {bookmark:[subcid, pos], csrf_ham:csrf_ham},
				 type: 'POST',
				 dataType: 'json'
				});
	    $.when(subreq, uprefs).then(function(res, uprefs){
		if (res[1] == 'success' && res[0].ok) {
		    document.getElementById('c'+subcid).classList.add('subscribed');
		    uprefs.subscriptions[subcid] = true;
		    palette.hide();
		}
	    });
	});
	$('#unsubscribe').on('click', function(){
	    var subcid = cid.cid;
	    var subreq = $.ajax({url: '/s/uprefs',
				 data: {bookmark:[subcid, 'del'], csrf_ham:csrf_ham},
				 type: 'POST',
				 dataType: 'json'
				});
	    $.when(subreq, uprefs).then(function(res, uprefs){
		if (res[1] == 'success' && res[0].ok) {
		    document.getElementById('c'+subcid).classList.remove('subscribed');
		    delete uprefs.subscriptions[subcid];
		    palette.hide();
		}
	    });
	});
	$('#info').on('click', function(){
	    window.open('/info.html?cid='+cid.cid, 'piperka_info');
	});
	this.on('click', 'circle.comic', function(ev){
	    if (options.pan.moved)
		return;
	    var el = ev.target;
	    cid.cid = $(el).data().cid;
	    var coord = svg.realToSvgCoord(ev.clientX, ev.clientY);
	    palette[0].transform.baseVal.getItem(0).setTranslate(coord.x,coord.y);
	    var scale = 1/coord.size;
	    palette[0].transform.baseVal.getItem(1).setScale(scale, scale);
	    $.when(uprefs).then(function(dat){
		if (dat && dat.subscriptions) {
		    if (dat.subscriptions[cid.cid]) {
			$('#unsubscribe').show();
			$('#subscribe').hide();
		    } else {
			$('#subscribe').show();
			$('#unsubscribe').hide();
		    }
		}
		palette.show();
	    });
	});

	return this;
    };
})(jQuery);

$(document).ready(function(){
    $(window).on('resize', maximizeMap).trigger('resize');
    var pan = {};
    var svg = $('svg')[0];
    var g = $('svg g#map')[0];
    var svgparent = $('#svgcontainer')[0];
    var nameblurb = $('#comicnameblurb').hide();
    var world = {};
    var worldIndex = 0;
    var panning = function(ev) {
	var diffX = (pan.start.x-ev.clientX)/pan.size;
	var diffY = (pan.start.y-ev.clientY)/pan.size;
	svg.viewBox.baseVal.x = pan.start.panX+diffX;
	svg.viewBox.baseVal.y = pan.start.panY+diffY;
	if (ev.which == 0) {
	    $(document).off('mousemove', panning);
	}
    };
    $(document).on('mousedown', 'svg', function(ev) {
	if (ev.button == 0) {
	    pan.start = {x: ev.clientX, y: ev.clientY, panX: svg.viewBox.baseVal.x, panY: svg.viewBox.baseVal.y};
	    pan.size = svgparent.clientWidth > svgparent.clientHeight ? svgparent.clientHeight : svgparent.clientWidth;
	    $(document).on('mousemove', panning);
	}
    });
    var uprefs = new $.Deferred();
    $(svg).data('uprefs', uprefs);
    if (csrf_ham) {
	var userreq = $.ajax({url: '/s/uprefs',
			  method: 'POST',
			  dataType: 'json'
			 });
	userreq.done(function(dat){
	    var subscriptions = {};
	    $.each(dat.subscriptions, function(){
		subscriptions[this[0]] = true;
	    });
	    dat.subscriptions = subscriptions;
	    $(svg).data('uprefs').resolve(dat);
	    $('.script.useronly').removeClass('useronly');
	    $('#atfirst').removeAttr('disabled');
	});
    } else {
	$(svg).data('uprefs').resolve(null);
	$('#subscribe').hide();
	$('#unsubscribe').hide();
    }
    var otherUser = new $.Deferred();
    if (queryParms.profile) {
	var otherreq = $.ajax({url: '/s/profile',
			       data: {action: 'getsubs', name: queryParms.profile},
			       method: 'GET',
			       dataType: 'json'});
	$.when(otherreq).then(function(dat){
	    var subscriptions = {};
	    if (dat.subs) {
		$.each(dat.subs, function(){
		    subscriptions[this] = true;
		});
	    }
	    otherUser.resolve({subscriptions:subscriptions});
	});
    } else {
	otherUser.resolve(null);
    }

    $(svg).clickPalette({svg: svg, g: g, pan: pan});
    $(document).on('mouseup', function(ev) {
	$(document).off('mousemove', panning);
	pan.moved = pan.start && (pan.start.x != ev.clientX && pan.start.y != ev.clientY);
	delete pan.start;
	displayVisibleTexts();
    });
    if (typeof document.onwheel != 'undefined') {
	$(document).on('wheel', function(ev) {
	    var delta = ev.originalEvent.deltaY;
	    if (typeof delta != 'undefined')
		zoom(delta < 0 ? 'in' : 'out', ev.originalEvent.pageX, ev.originalEvent.pageY);
	});
    } else if (typeof document.onmousewheel != 'undefined') {
	$(document).on('mousewheel', function(ev) {
	    var delta = ev.originalEvent.wheelDelta;
	    zoom(delta < 0 ? 'out' : 'in', ev.pageX, ev.pageY);
	});
    }
    var titlesreq = $.ajax({url: '/d/comictitles_all.json',
			    method: 'GET',
			    dataType: 'json'});
    var loadWorld = function(titles, map, uprefs, oprefs) {
	var comics = titles[0];
	var foo;
	var minX, minY, maxX, maxY;
	minX = minY = Number.POSITIVE_INFINITY;
	maxX = maxY = Number.NEGATIVE_INFINITY;
	$.each(map[0], function(cid) {
	    if (this.radius > 1) {
		minX = this.x-this.radius < minX ? this.x-this.radius : minX;
		minY = this.y-this.radius < minY ? this.y-this.radius : minY;
		maxX = this.x+this.radius > maxX ? this.x+this.radius : maxX;
		maxY = this.y+this.radius > maxY ? this.y+this.radius : maxY;
	    }
	    var subscribeStyle = '';
	    if (uprefs && uprefs.subscriptions[cid]) {
		subscribeStyle = ' subscribed';
	    }
	    if (oprefs && oprefs.subscriptions[cid]) {
		subscribeStyle += ' otheruser';
	    }
	    var el = makeSVG('circle', {id:'c'+cid, class:'comic'+subscribeStyle, cx:this.x, cy:this.y, r:this.radius});
	    // Estimate some appropriate font height
	    var text = comics[cid], maxHeight = this.radius/3;
	    var textHeight;
	    if (text != undefined && text.length > 11)
		textHeight = 11/text.length*maxHeight;
	    else
		textHeight = maxHeight;
	    var textStyle = '';
	    if (textHeight < 0.2)
		textStyle = 'small';
	    else if (textHeight < 1.0)
		textStyle = 'medium';
	    var textEl = makeSVG('text', {class:'comic '+textStyle, x:this.x-this.radius*0.9, y:this.y+textHeight/3, textLength:this.radius*1.8, lengthAdjust:'spacingAndGlyphs'});
	    if (textStyle != '')
		textEl.style.display="none";
	    textEl.textContent = comics[cid];
	    textEl.style.fontSize = textHeight+'px';
	    $(el).data({cid:cid})
	    g.appendChild(el);
	    g.appendChild(textEl);
	});
	world.minX = minX;
	world.minY = minY;
	world.maxX = maxX;
	world.maxY = maxY;
	$(svg).on('mousemove', 'circle.comic', function(ev){
	    if (!pan.start) {
		var cid = $(this).data().cid;
		nameblurb.text(comics[cid]).show();
	    }
	});
	$(svg).on('mousemove', moveblurb);
    };
    $('#controls .solid').on('mouseenter', function(ev){
	nameblurb.hide();
    });
    $('#comicnameblurb').on('mousemove', moveblurb);
    $('#controls .zoom').on('click', function(){
	zoom(this.dataset.dir);
    });
    $('#fit').on('click', function(){
	var lenX = world.maxX-world.minX, lenY = world.maxY-world.minY;
	var imgAspect = lenX/lenY, svgAspect = svgparent.clientWidth/svgparent.clientHeight;
	var scale;
	if (imgAspect < svgAspect) {
	    scale = 1/lenY;
	} else {
	    scale = 1/lenX;
	}
	var trans = g.transform.baseVal.getItem(0);
	trans.setScale(scale, scale);
	svg.viewBox.baseVal.x = svg.viewBox.baseVal.y = -0.5;
    });
    var worldreq = $.ajax({url: '/d/world.json',
			   method: 'GET',
			   dataType: 'json'});
    var req = $.when(titlesreq, worldreq, uprefs, otherUser).then(loadWorld);
    req.then(function(){
	var doZoom = false;
	if (queryParms.cid) {
	    doZoom = zoom(parseInt(queryParms.cid));
	}
	if (!doZoom) {
	    $('#fit').trigger('click');
	}
    });
    $('#quicksearch').qsearch({
	select: function(event, ui) {zoom(ui.item.cid);},
	submit: function(event, ui) {zoom(ui.item.cid);}
    });
});
