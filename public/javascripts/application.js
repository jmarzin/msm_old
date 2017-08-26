function deleteDialog(e) {
    var r = confirm("Confirmer la suppression");
    if (r == true) {
        return true;
    } else {
        e.preventDefault();
    }
}

function editeur() {
    tinymce.init({
        selector: '#mytextarea',
        plugins: [
            'advlist autolink link image lists charmap print preview hr anchor pagebreak spellchecker',
            'searchreplace wordcount visualblocks visualchars code fullscreen insertdatetime media nonbreaking',
            'save table contextmenu directionality emoticons template paste textcolor'
        ],
        file_picker_callback: function(callback, value, meta) {
            if(meta.filetype == 'image')
                $('#picker').trigger( "click" );
        }
    });
}