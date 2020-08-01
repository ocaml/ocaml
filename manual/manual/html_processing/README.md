# HTML post-processing

This directory contains material for enhancing the html of the manual
and the API (from the `../htmlman` directory), including a quick
search widget for the API.

The process will create the `../webman` dir, and output the new html
files (and assets) in `../webman/manual` (the manual) and `../webman/api` (the
API).

For processing the manual/api of OCaml versions <= 4.10, see
https://github.com/sanette/ocaml-tutorial and
https://github.com/sanette/ocaml-api

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

The scripts should run with `ocaml >= 4.05`.  You also need a working
[`sass`](https://sass-lang.com/) CSS processor (tested with version
"3.4.23").

## How to browse

From the `html_processing` directory:

`firefox ../webman/api/index.html`

`firefox ../webman/manual/index.html`

## Debug

Look at the `Makefile`: the `quiet` keyword is optional. Remove it to
have debug information.

By default all html files are re-created by `make`, but the javascript
index `webman/api/index.js` and `webman/api/compilerlibref/index.js`
are kept if they already exist. You can use `make clean` to delete all
generated files.
