// Searching the OCAML API.
// Copyright 2019-2020 San VU NGOC

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

// Thanks @steinuil for help on deferred loading.
// Thanks @osener, @UnixJunkie, @Armael for very helpful suggestions
// Thanks to all testers!

const MAX_RESULTS = 20;
const MAX_ERROR = 10;
const DESCR_INDEX = 4; // index of HTML description in index.js
const SIG_INDEX = 6; // index of HTML signature in index.js
const ERR_INDEX = 8; // length of each line in index.js. This is used
		     // for storing the computed error, except if we
		     // don't want description and type signature,
		     // then ERR_INDEX becomes DESCR_INDEX.

let indexState = 'NOT_LOADED';

// return true if we are loading the index file
function loadingIndex (includeDescr) {
    switch (indexState) {
    case 'NOT_LOADED':
	indexState = 'LOADING';

	const script = document.createElement('script');
	script.src = 'index.js';
	script.addEventListener('load', () => {
	    indexState = 'HAS_LOADED';
	    mySearch(includeDescr);
	});
	document.head.appendChild(script);
	return true;

    case 'LOADING':
	return true;

    case 'HAS_LOADED':
	return false;
    }
}

// line is a string array. We check if sub is a substring of one of
// the elements of the array. The start/end of the string s are marked
// by "^" and "$", and hence these chars can be used in sub to refine
// the search. Case sensitive is better for OCaml modules. Searching
// within line.join() is slightly more efficient that iterating 'line'
// with .findIndex (my benchmarks show about 15% faster; except if we
// search for the value at the beginning of line). However it might
// use more memory.
function hasSubString (sub, line) {
    let lineAll = "^" + line.join("$^") + "$";
    return (lineAll.includes(sub));
}

// Check if one of the strings in subs is a substring of one of the
// strings in line.
function hasSubStrings (subs, line) {
    let lineAll = "^" + line.join("$^") + "$";
    return (subs.findIndex(function (sub) {
	return (lineAll.includes(sub))}) !== -1);
}
// Error of sub being a substring of s. Best if starts at 0. Except
// for strings containing "->", which is then best if the substring is
// at the most right-hand position (representing the "return type").
// markers "^" and "$" for start/end of string can be used: if they
// are not satisfied, the MAX_ERROR is returned.
function subError (sub, s) {
    let StartOnly = false;
    let EndOnly = false;
    if (sub.length>1) {
	if (sub[0] == "^") {
	    StartOnly = true;
	    sub = sub.substring(1);
	}
	if (sub[sub.length - 1] == "$") {
	    EndOnly = true;
	    sub = sub.substring(0, sub.length - 1);
	}
    }
    let err = s.indexOf(sub);
    if (err == -1 ||
	(StartOnly && err != 0) ||
	(EndOnly && err != s.length - sub.length)) {
	err = MAX_ERROR;
    } else {
	if ( sub.includes("->") ) {
	    err = Math.min(s.length - sub.length - err,1); // 0 or 1
	    // err = 0 if the substring is right-aligned
	} else {
	    err = Math.min(err,1); // 0 or 1
	    // err = 0 if the substring
	}
	err += Math.abs((s.length - sub.length) / s.length);}
    return (err)
    // between 0 and 2, except if MAX_ERROR
}

// Minimal substring error. In particular, it returns 0 if the string
// 'sub' has an exact match with one of the strings in 'line'.
function subMinError (sub, line) {
    let errs = line.map(function (s) { return subError (sub, s); });
    return Math.min(...errs); // destructuring assignment
}


function add (acc, a) {
    return acc + a;
}

// for each sub we compute the minimal error within 'line', and then
// take the average over all 'subs'. Thus it returns 0 if each sub has
// an exact match with one of the strings in 'line'.
function subsAvgMinError (subs, line) {
    let errs = subs.map(function (sub) { return subMinError (sub, line); });
    return errs.reduce(add,0) / subs.length;
}

function formatLine (line) {
    let li = '<li>';
    let html = `<code class="code"><a href="${line[1]}"><span class="constructor">${line[0]}</span></a>.<a href="${line[3]}">${line[2]}</a></code>`;
    if (line.length > 5) {
	if ( line[ERR_INDEX] == 0 ) {
	    li = '<li class="match">';
	}
	html = `<pre>${html} : ${line[SIG_INDEX]}</pre>${line[DESCR_INDEX]}`; }
    return (li + html + "</li>\n");
}

// Split a string into an array of non-empty words, or phrases
// delimited by quotes ("")
function splitWords (s) {
    let phrases = s.split('"');
    let words = [];
    phrases.forEach(function (phrase,i) {
	if ( i%2 == 0 ) {
	    words.push(...phrase.split(" "));
	} else {
	    words.push(phrase);
	}
    });
    return (words.filter(function (s) {
	return (s !== "")}));
}

// The initial format of an entry of the GENERAL_INDEX array is
// [ module, module_link,
//   value, value_link,
//   html_description, bare_description,
//   html_signature, bare_signature ]

// If includeDescr is false, the line is truncated to its first 4
// elements.  When searching, the search error is added at the end of
// each line.

// In order to reduce the size of the index.js file, one could create
// the bare_description on-the-fly using .textContent, see
// https://stackoverflow.com/questions/28899298/extract-the-text-out-of-html-string-using-javascript,
// but it would probably make searching slower (haven't tested).
function mySearch (includeDescr) {
    if (loadingIndex (includeDescr)) {
	return;
    }
    let text = document.getElementById('api_search').value;
    let results = [];
    let html = "";
    let count = 0;
    let err_index = DESCR_INDEX;

    if (text !== "") {
	if ( includeDescr ) {
	    err_index = ERR_INDEX;
	}

	let t0 = performance.now();
	let exactMatches = 0;
	results = GENERAL_INDEX.filter(function (line) {
	    // We remove the html hrefs and add the Module.value complete name:
	    let cleanLine = [line[0], line[2], line[0] + '.' + line[2]];
	    line.length = err_index; // This truncates the line:
	    // this removes the description part if includeDescr =
	    // false (which modifies the lines of the GENERAL_INDEX.)
	    if ( includeDescr ) {
		cleanLine.push(line[DESCR_INDEX+1]);
		cleanLine.push(line[SIG_INDEX+1]);
		// add the description and signature (txt format)
	    }
	    let error = MAX_ERROR;
	    if ( exactMatches <= MAX_RESULTS ) {
		// We may stop searching when exactMatches >
		// MAX_RESULTS because the ranking between all exact
		// matches is unspecified (depends on the construction
		// of the GENERAL_INDEX array)
		if ( hasSubString(text, cleanLine) ) {
		    error = subMinError(text, cleanLine);
		    // one could merge hasSubString and subMinError
		    // for efficiency
		}
		if ( error != 0 && includeDescr ) {
		    let words = splitWords(text);
		    if ( hasSubStrings(words, cleanLine) ) {
			// if there is no exact match for text and
			// includeDescr=true, we also search for all separated
			// words
			error = subsAvgMinError(words, cleanLine);
		    }
		}
		if ( error == 0 ) { exactMatches += 1; }
	    }
	    line[err_index] = error;
	    // we add the error as element #err_index
	    return ( error != MAX_ERROR );
	});
	// We sort the results by relevance:
	results.sort(function(line1, line2) {
	    return (line1[err_index] - line2[err_index])});
	count = results.length;
	console.log("Search results = " + (count.toString()));
	results.length = Math.min(results.length, MAX_RESULTS);
	html = "no results";
    }
    // inject new html
    if (results.length > 0) {
	html = "<ul>";
	function myIter(line, index, array) {
	    html = html + formatLine(line);
	}
	results.forEach(myIter);
	html += "</ul>";
	if (count > results.length) {
	    html += "(...)";
	}
    }
    document.getElementById("search_results").innerHTML = html;
}

function showHelp () {
    document.getElementById("search_help").classList.toggle("hide");
}
