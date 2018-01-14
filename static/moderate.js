var session = /p_session=([0-9a-z-]+)/.exec(document.cookie)
session = session != null ? session[1] : null;

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
			data:{'csrf_ham':session},
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
				if (rpy.banner) {
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
    $.fn.pSubmissions = function() {
	$(this).on('click', 'tr', function() {
	    var sid = this.getAttribute('id').substring(4);
	    $('#submission-banner').html('<img src="/s/viewsubmitbanner/'+sid+'">');
	    $.ajax({url:'/s/sinfo2/'+sid,
		    dataType: 'json',
		    success: function(rpy){
			if (rpy != null && rpy.ok) {
			    $.each(rpy.info, function(k,v){
				var field = $('#submission-entry #'+k)[0];
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
			}
		    }});
	});
	return this;
    };
})( jQuery );
