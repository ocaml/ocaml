DUNE_CMD := $(if $(wildcard dune/dune.exe),dune/dune.exe,dune)
DUNE ?= $(DUNE_CMD)

DEBUG ?= 0
ifeq ($(DEBUG), 1)
    DBG=
else
    DBG=quiet
endif

WEBDIR = ../webman
WEBDIRMAN = $(WEBDIR)/manual
WEBDIRAPI = $(WEBDIR)/api
WEBDIRCOMP = $(WEBDIRAPI)/compilerlibref

# The "all" target generates the Web Manual in the directories
# ../webman/manual, ../webman/api, and ../webman/api/compilerlibref
all: css js img
	$(DUNE) exec --root=. src/process_manual.exe $(DBG)
	$(DUNE) exec --root=. src/process_api.exe overwrite $(DBG)
	$(DUNE) exec --root=. src/process_api.exe compiler overwrite $(DBG)

$(WEBDIR)/%:
	mkdir -p $@

$(WEBDIRMAN)/manual.css: scss/_common.scss scss/manual.scss | $(WEBDIRMAN)
	sass scss/manual.scss > $(WEBDIRMAN)/manual.css

$(WEBDIRAPI)/style.css: scss/_common.scss scss/style.scss | $(WEBDIRAPI) $(WEBDIRCOMP)
	sass scss/style.scss > $(WEBDIRAPI)/style.css
	cp $(WEBDIRAPI)/style.css $(WEBDIRCOMP)/style.css

css: $(WEBDIRMAN)/manual.css $(WEBDIRAPI)/style.css

# Just copy the JS files:
#
JS_FILES0 := scroll.js navigation.js
JS_FILES1 := $(JS_FILES0) search.js
JS_FILES := $(addprefix $(WEBDIRAPI)/, $(JS_FILES1)) $(addprefix $(WEBDIRCOMP)/, $(JS_FILES1)) $(addprefix $(WEBDIRMAN)/, $(JS_FILES0))

# There must be a more clever way
$(WEBDIRAPI)/%.js: js/%.js | $(WEBDIRAPI)
	cp $< $@

$(WEBDIRMAN)/%.js: js/%.js | $(WEBDIRMAN)
	cp $< $@

$(WEBDIRCOMP)/%.js: js/%.js | $(WEBDIRCOMP)
	cp $< $@

js: $(JS_FILES)

CURL = curl -s
# download images for local use
SEARCH := search_icon.svg
$(WEBDIRAPI)/search_icon.svg: | $(WEBDIRAPI)
	$(CURL) "https://ocaml.org/img/search.svg" > $(WEBDIRAPI)/$(SEARCH)

$(WEBDIRCOMP)/%: $(WEBDIRAPI)/% | $(WEBDIRCOMP)
	cp $< $@

$(WEBDIRMAN)/%: $(WEBDIRAPI)/% | $(WEBDIRMAN)
	cp $< $@

LOGO := colour-logo.svg
$(WEBDIRAPI)/colour-logo.svg: | $(WEBDIRAPI) $(WEBDIRMAN) $(WEBDIRCOMP)
	$(CURL) "https://raw.githubusercontent.com/ocaml/ocaml-logo/master/Colour/SVG/colour-logo.svg" > $(WEBDIRAPI)/$(LOGO)

ICON := favicon.ico
$(WEBDIRAPI)/favicon.ico: | $(WEBDIRAPI) $(WEBDIRMAN) $(WEBDIRCOMP)
	$(CURL) "https://raw.githubusercontent.com/ocaml/ocaml-logo/master/Colour/Favicon/32x32.ico" > $(WEBDIRAPI)/$(ICON)

IMG_FILES0 := colour-logo.svg
IMG_FILES := $(addprefix $(WEBDIRAPI)/, $(IMG_FILES0)) $(addprefix $(WEBDIRCOMP)/, $(IMG_FILES0)) $(addprefix $(WEBDIRMAN)/, $(IMG_FILES0)) 

img: $(WEBDIRAPI)/search_icon.svg $(WEBDIRAPI)/favicon.ico $(WEBDIRCOMP)/search_icon.svg $(WEBDIRCOMP)/favicon.ico $(IMG_FILES)

clean:
	rm -rf $(WEBDIR) src/.merlin _build

.PHONY: distclean
distclean:: clean

distclean::
	rm -f src/common.ml

distclean::
	rm -rf .sass-cache

# We need Dune and Lambda Soup; Markup.ml and Uutf are dependencies
DUNE_TAG = 3.4.0
LAMBDASOUP_TAG = 500
MARKUP_TAG = 1.0.3
UUTF_TAG = v1.0.3
RE_TAG = 1.10.4
CAMLP_STREAMS_TAG = v5.0.1

# Duniverse rules - set-up dune and the dependencies in-tree for CI
duniverse: dune/dune.exe re markup.ml uutf lambdasoup camlp-streams

dune/dune.exe: dune
	cd dune; ocaml bootstrap.ml

GIT_CHECKOUT = git -c advice.detachedHead=false checkout

dune:
	git clone https://github.com/ocaml/dune.git -n -o upstream
	cd dune; $(GIT_CHECKOUT) $(DUNE_TAG)

distclean::
	rm -rf dune

re:
	git clone https://github.com/ocaml/ocaml-re.git -n -o upstream
	cd ocaml-re; $(GIT_CHECKOUT) $(RE_TAG)
	sed -i.bak -e '/(libraries seq)/d' ocaml-re/lib/dune

distclean::
	rm -rf ocaml-re

lambdasoup:
	git clone https://github.com/dra27/lambdasoup.git -n -o dra27
	cd lambdasoup; $(GIT_CHECKOUT) $(LAMBDASOUP_TAG)

distclean::
	rm -rf lambdasoup

markup.ml:
	git clone https://github.com/aantron/markup.ml.git -n -o upstream
	cd markup.ml; $(GIT_CHECKOUT) $(MARKUP_TAG)

distclean::
	rm -rf markup.ml

uutf:
	git clone https://github.com/dbuenzli/uutf.git -n -o upstream
	cd uutf; $(GIT_CHECKOUT) $(UUTF_TAG)
	cd uutf; \
  mv opam uutf.opam; \
  echo '(lang dune 1.0)' > dune-project; \
  echo '(name uutf)' >> dune-project; \
  echo '(library (name uutf)(public_name uutf)(flags (:standard -w -3-27))(wrapped false))' > src/dune

distclean::
	rm -rf uutf

camlp-streams:
	git clone https://github.com/ocaml/camlp-streams.git -n -o upstream
	cd camlp-streams; $(GIT_CHECKOUT) $(CAMLP_STREAMS_TAG)

distclean::
	rm -rf camlp-streams

.PHONY: css js img duniverse
