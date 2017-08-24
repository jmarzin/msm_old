function deleteDialog(e) {
    var r = confirm("Confirmer la suppression");
    if (r == true) {
        return true;
    } else {
        e.preventDefault();
    }
}