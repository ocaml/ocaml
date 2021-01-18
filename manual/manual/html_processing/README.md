# HTML post-processing

This directory contains material for enhancing the html of the manual
and the API (from the `../htmlman` directory), including a quick
search widget for the API.

The process will create the `../webman` dir, and output the new html
files (and assets) in `../webman/manual` (the manual) and `../webman/api` (the
API).

## manual and api

There are two different scripts, `process_manual.ml` and
`process_api.ml`.  The first one deals with all the chapters of the
manual, while the latter deals with the api generated with `ocamldoc`.
They both use a common module `common.ml`.

## How to build

With dependencies to build the whole manual:
```
cd ..
make web
```

Or, much faster if you know that `htmlman` is already up-to-date, from
within the `html_processing` dir:

```
make
```

You need a working
[`sass`](https://sass-lang.com/) CSS processor (tested with version
"3.4.23").

## How to browse

From the `html_processing` directory:

`firefox ../webman/api/index.html`

`firefox ../webman/manual/index.html`

## Debug

```
make DEBUG=1
```

By default all html files are re-created by `make`, but the javascript
index `webman/api/index.js` and `webman/api/compilerlibref/index.js`
are kept if they already exist. You can use `make clean` to delete all
generated files.

The javascript files in the `html_processing/js` dir add functionality
but the web-manual is still browsable without them:

- `scroll.js`: adds smooth scrolling in the html page, but only for
  near targets. The reason is that when you jump to another place in a
  text, if the jump is immediate (no scrolling), you easily get lost;
  for instance you usually don't even realize that the target of the
  link is just half a page below! Thus smooth scrolling helps
  _understanding the structure_ of the document. However, when the
  target is very far, the browser will scroll a huge amount of text
  very quickly, and this becomes useless, and even painful for the
  eye. Hence we disable smooth scrolling for far targets.

- `search.js`: adds an 'as-you-type quick search widget', which
  recognize values, modules, and type signatures. It is very useful,
  but of course not strictly necessary.
