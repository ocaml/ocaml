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

$(WEBDIR):
	mkdir -p $(WEBDIRMAN)
	mkdir -p $(WEBDIRCOMP)

$(WEBDIRMAN)/manual.css: scss/manual.scss $(WEBDIR)
	sass scss/manual.scss > $(WEBDIRMAN)/manual.css

$(WEBDIRAPI)/style.css: scss/style.scss $(WEBDIR)
	sass scss/style.scss > $(WEBDIRAPI)/style.css
	cp $(WEBDIRAPI)/style.css $(WEBDIRCOMP)/style.css

css: $(WEBDIRMAN)/manual.css $(WEBDIRAPI)/style.css

# Just copy the JS files
JS_FILES0 := scroll.js search.js
JS_FILES := $(addprefix $(WEBDIRAPI)/, $(JS_FILES0)) $(addprefix $(WEBDIRCOMP)/, $(JS_FILES0)) $(WEBDIRMAN)/scroll.js
# should be same as:
#JS_FILES := $(WEBDIRAPI)/scroll.js $(WEBDIRAPI)/search.js $(WEBDIRCOMP)/scroll.js $(WEBDIRCOMP)/search.js $(WEBDIRMAN)/scroll.js

# There must be a more clever way
$(WEBDIRAPI)/%.js: js/%.js
	cp $< $@

$(WEBDIRMAN)/%.js: js/%.js
	cp $< $@

$(WEBDIRCOMP)/%.js: js/%.js
	cp $< $@

js: $(WEBDIR) $(JS_FILES)

# Copy images
IMG_FILES0 := colour-logo-gray.svg favicon.ico
IMG_FILES1 := $(IMG_FILES0) search_icon.svg
IMG_FILES := $(addprefix $(WEBDIRAPI)/, $(IMG_FILES1)) $(addprefix $(WEBDIRCOMP)/, $(IMG_FILES1)) $(addprefix $(WEBDIRMAN)/, $(IMG_FILES0)) 

# There must be a more clever way
$(WEBDIRMAN)/%.svg: images/%.svg
	cp $< $@

$(WEBDIRMAN)/%.ico: images/%.ico
	cp $< $@

$(WEBDIRAPI)/%.svg: images/%.svg
	cp $< $@

$(WEBDIRAPI)/%.ico: images/%.ico
	cp $< $@

$(WEBDIRCOMP)/%.svg: images/%.svg
	cp $< $@

$(WEBDIRCOMP)/%.ico: images/%.ico
	cp $< $@

img: $(WEBDIR) $(IMG_FILES)

clean:
	rm -rf $(WEBDIR) src/.merlin _build

distclean::
	rm -rf .sass-cache

# We need Dune and Lambda Soup; Markup.ml and Uutf are dependencies
DUNE_TAG = 2.6.2
LAMBDASOUP_TAG = 0.7.1
MARKUP_TAG = 0.8.2
UUTF_TAG = v1.0.2

# Duniverse rules - set-up dune and the dependencies in-tree for CI
duniverse: dune/dune.exe markup.ml uutf lambdasoup

dune/dune.exe: dune
	cd dune; ocaml bootstrap.ml

GIT_CHECKOUT = git -c advice.detachedHead=false checkout

dune:
	git clone https://github.com/ocaml/dune.git -n -o upstream
	cd dune; $(GIT_CHECKOUT) $(DUNE_TAG)

distclean::
	rm -rf dune

lambdasoup:
	git clone https://github.com/aantron/lambdasoup.git -n -o upstream
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

.PHONY: css js img duniverse
