// Smooth scrolling only for near targets
// copyright 2019-2020 San Vu Ngoc
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


// Goal: if a link is located at distance larger than MAX_DISTANCE, we
// don't use a smooth scrolling.

const MAX_DISTANCE = 1000;

const url = window.location.pathname;
var filename = url.substring(url.lastIndexOf('/')+1);
if (filename == "") { filename = "index.html"; }

function localLink (link) {
    return (link.length > 0 &&
	    (link.charAt(0) == '#'
	     || link.substring(0,filename.length) == filename));
}

//aaa.html#coucou --> coucou
function getId (link) {
    return link.substring(link.lastIndexOf('#')+1);
}

// Get absolute y position of element.
// modified from:
// https://www.kirupa.com/html5/get_element_position_using_javascript.htm
// assuming effective licence CC0, see
// https://forum.kirupa.com/t/get-an-elements-position-using-javascript/352186/3
function getPosition(el) {
    let yPos = 0; 
    while (el) {
	yPos += (el.offsetTop + el.clientTop);
	el = el.offsetParent;
    }
    return yPos;
}

function setSmooth () {
    let x = document.getElementsByClassName("toc_title");
    let a = document.getElementsByTagName("a");
    let container = document.body.parentNode; 
    let i;
    for (i = 0; i < a.length; i++) {
	let href = a[i].getAttribute("href");
	if (localLink(href)) {
	    a[i].onclick = function () {
		let id = getId(href);
		let target = document.getElementById(id);
		if (! target) { target = document.body.parentNode; }
		let top = container.scrollTop;
		let dist = top - getPosition(target)
		if (Math.abs(dist) < MAX_DISTANCE) {
		    target.scrollIntoView({ block: "start", inline: "nearest", behavior: 'smooth' });
		    setTimeout(function () {
		    	location.href = href;
			// this will set the "target" property.
		    }, 600);
		    return false;
		    // so we don't follow the link immediately
		}
	    }
	}
    }
}

window.onload = function() {
    setSmooth();
};
