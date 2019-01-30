function full_head_ref(){
    // Instead of showing <user>:<branch> it shows <user>/<repo>:<branch>
    //
    // This can then be used to add the user as a remote
    el = document.querySelector('.gh-header-meta .head-ref');
    el.textContent = el.title;
}

full_head_ref();
