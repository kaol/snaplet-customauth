function makePageDialog(cid) {
    $('#archivedialog').remove();
    var archivedialog = $('<div id="archivedialog" title="Archive"><table><tr><th>#</th><th class="page">Page</th></table></div>')
	.dialog({autoOpen: false, minwidth: 300, width: 400, height: 300, open: centerDialog});
    var table = archivedialog.find('table');
    return $.when($.ajax({url:'/s/archive/'+cid, method: 'GET', dataType: 'json'}))
	.then(function(rpy){
	    archivedialog.data(rpy);
	    $.each(rpy.pages, function(idx){
		$('<tr><td>'+(idx+1)+'</td></tr>').data('page', this[0]).data('ord', idx).data('maxsubord', this[1])
		    .append('<td class="page">'+(this[0] != null ? this[0] : 'Current page')+'</td>')
		    .append('<td class="status"/>')
		    .appendTo(table);
	    });
	    table.append($('<tr><td class="status"/></td>'));
	    return rpy;
	});
}

function centerDialog() {
    var row = $('#archivedialog tr:has(#currentpagemarker)');
    if (row.length > 0) {
	var ord = row.data('ord');
	$('#archivedialog').get(0).scrollTop = -($('#archivedialog').height()/2)+$('#archivedialog tr').get(ord).offsetTop;
    }
}
