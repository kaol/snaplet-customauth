var csrf_ham = /csrf_ham=([0-9a-z-]+)/.exec(document.cookie);
csrf_ham = csrf_ham ? csrf_ham[1] : null;

(function( $ ){
    $.fn.pModerate = function() {
	$(this).on('click', 'tr', function() {
	    $('#msgdiv').html('');
	    $('#addedtags').hide().find('.diff').html('');
	    $('#removedtags').hide().find('.diff').html('');
	    $('#currententry').html('');
	    $('.submitcomic').off('submit');
	    var thisrow = $(this).on('delete', function(){
		$('.submitcomic').hide();
		$(this).remove();
		$('#current-entry').html('');
		$('#nummod').text(($('#nummod').text())-1);
	    });
	    $('#editinfo').one('submit', function(){thisrow.trigger('delete')});
	    enableSubmitcomic();
	    $('.submitcomic').show();
	    var sid = thisrow.attr('id').substring(4);
	    var cid = thisrow.find('.cid').attr('id').substring(4);
	    $('#useredit-banner').html('<img src="/s/viewsubmitbanner/'+sid+'">');
	    $('#info-cid').text(cid);
	    $('#info-title').attr('href', 'info.html?cid='+cid).text(thisrow.find('.cid').text());
	    resetSubmitForm();
	    $('.submitcomic input[name=cid]').val(cid);
	    $('.submitcomic input[name=user_sid]').val(sid);
	    $('#user-edits tr').removeClass('hilite removeafterdone');
	    thisrow.addClass('hilite removeafterdone');
	    $('#removeedit').off('click');
	    $('#removeedit').one('click', function(){
		$.ajax({url:'/s/dropsubmit/'+sid,
			data:{'csrf_ham':csrf_ham},
			type:'POST',
			success:function(rpy){
			    if (rpy.ok) {
				thisrow.trigger('delete');
			    } else if (rpy.errmsg) {
				$('#msgdiv').hide().html(rpy.errmsg).slideDown();
			    }
			}});
	    });
	    $.ajax({url:'/include/cinfo',
		    data:{'cid':cid},
		    dataType:'html',
		    success:function(rpy){
			$('#current-entry').html(rpy);
		    }});
	    $.ajax({url:'/s/sinfo/'+sid,
		    dataType: 'json',
		    success:function(rpy){
			if (rpy != null) {
			    if (rpy.ok) {
				setTagsEpedias(rpy);
				if (rpy.oldbanner) {
				    $('.oldbanner').show().find('span').text(rpy.banner);
				} else {
				    $('.oldbanner').hide();
				}
				if (rpy.newbanner) {
				    $('.hasbanner.script').show();
				    $('.hasbanner input[type=checkbox]')[0].checked = true;
				} else {
				    $('.hasbanner').hide();
				}
				$('.submitcomic input[name=banner]').val(rpy.banner);
				$('.submitcomic textarea[name=description]').val(rpy.description);
			    } else if (rpy.errmsg) {
				$('#msgdiv').hide().html(rpy.errmsg).slideDown();
			    }
			}
		    }});
	});
	return this;
    };
    $.fn.setChildFields = function(rpy) {
	var mainField = $(this);
	$.each(rpy, function(k,v){
	    var field = mainField.find('#'+k)[0];
	    if (typeof field != 'undefined') {
		switch (field.tagName.toLowerCase()) {
		case 'span':
		case 'textarea':
		    field.innerText = v;
		    break;
		case 'a':
		    field.innerText = v;
		    field.setAttribute('href', v);
		    break;
		case 'input':
		    var type = field.getAttribute('type');
		    if (type == 'checkbox') {
			if (v) {
			    field.setAttribute('checked', 1);
			} else {
			    field.removeAttribute('checked');
			}
		    } else if (type == 'text') {
			field.setAttribute('value', v);
		    }
		    break;
		}
	    }
	});
	return this;
    };
    $.fn.pSubmissions = function() {
	$(this).on('click', 'tr', function() {
	    var sid = this.getAttribute('id').substring(4);
	    $('#submission-banner').html('<img src="/s/viewsubmitbanner/'+sid+'">');
	    $.ajax({url:'/s/sinfo2/'+sid,
		    dataType: 'json',
		    success: function(rpy){
			$('#genentry-link')[0].setAttribute('href', 'genentry.html?sid='+sid);
			$('#submission-entry').setChildFields(rpy);
		    }});
	});
	return this;
    };
})( jQuery );

$(document).ready(function(){
    var genentry = $('#genentry-form');
    if (genentry.length > 0) {
	genentry.show();
	$('#tagdiff .script').show();
	var sid = /sid=(\d+)/.exec(document.location.href)[1];
	$('#submission-banner').html('<img src="/s/viewsubmitbanner/'+sid+'">');
	$.ajax({url:'/s/genentry/'+sid,
		dataType: 'json',
		success: function(rpy) {
		    if (rpy.newbanner) {
			$('.hasbanner.script').show();
			$('.hasbanner input[type=checkbox]')[0].checked = true;
		    } else {
			$('.hasbanner').hide();
		    }
		    rpy.origtags = [];
		    setTagsEpedias(rpy);
		    if (!/^http/.test(rpy.homepage)) {
			rpy.homepage = 'http://'+rpy.homepage;
		    }
		    $('#genentry-form').setChildFields(rpy);
		    $('#homepage_link').attr('href', rpy.homepage);
		}});
    }
});
