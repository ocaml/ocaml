// NaVigation helpers for the manual, especially in mobile mode.

// copyright 2020 San Vu Ngoc
//

// Permission to use, copy, modify, and/or distribute this software
// for any purpose with or without fee is hereby granted, provided
// that the above copyright notice and this permission notice appear
// in all copies.

// THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
// WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
// WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
// AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR
// CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS
// OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
// NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
// CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

// In mobile mode, both left navigation bar and top part menu are
// closed by default.

var MENU_HEIGHT = 0;

function closeSidebarExceptSearch (event) {
    if ( event && event.target && event.target.classList.contains("api_search") ) {
	false;
    } else {
	closeSidebar ();
	true;
    }
}

// This closes the sidebar in mobile mode. This should have no effect
// in desktop mode.
function closeSidebar () {
    let bar = document.getElementById("sidebar");
    let w = getComputedStyle(bar).width;
    bar.style.left = "-" + w;
    document.body.removeEventListener("click", closeSidebarExceptSearch); 
}

function toggleSidebar () {
    let bar = document.getElementById("sidebar");
    let l = getComputedStyle(bar).left;
    if (l == "0px") {
	closeSidebar ();
    } else {
	bar.style.left = "0px";
	setTimeout(function(){
	    // Any click anywhere but in search widget will close the sidebar
	    document.body.addEventListener("click", closeSidebarExceptSearch);
	}, 1000);
    }
}

function togglePartMenu () {
    let pm = document.getElementById("part-menu");
    let h = pm.offsetHeight;
    if ( h == 0 ) {
	pm.style.height = MENU_HEIGHT.toString() + "px";
    } else {
	pm.style.height = "0px";
    }
}
    
function partMenu () {
    let pm = document.getElementById("part-menu");
    if ( pm != null ) {
	MENU_HEIGHT = pm.scrollHeight; // This should give the true
	// height of the menu, even if
	// it was initialized to 0 in
	// the CSS (mobile view).
	// In desktop mode, the height is initially on "auto"; we
	// have to detect it in
	// order for the css animmations to work.
	// TODO update this when window is resized
	let currentHeight = pm.offsetHeight;
	pm.style.height = currentHeight.toString() + "px";
	let p = document.getElementById("part-title");
	if ( p != null ) {
	    p.onclick = togglePartMenu;
	}
    }
}

function sideBar () {
    closeSidebar();
    let btn = document.getElementById("sidebar-button");
    btn.onclick = toggleSidebar;
}
    
// We add it to the chain of window.onload
window.onload=(function(previousLoad){
    return function (){
	previousLoad && previousLoad ();
	partMenu ();
	sideBar ();
    }
})(window.onload);
	
    
