# You can set these variables from the command line.
SPHINXOPTS    = -j auto -aE -n -W --keep-going
SPHINXBUILD   = poetry run sphinx-build
SPHINXPROJ    = Tezos
SOURCEDIR     = .
TMPDOCDIR        = /tmp/tezosdoc
BUILDDIR      = _build
TOPBUILDDIR   = ../_build/default

DOCGENDIR     = doc_gen
DOCERRORDIR   = $(DOCGENDIR)/errors
P2PDOCEXE     = $(TOPBUILDDIR)/docs/$(DOCGENDIR)/p2p_doc.exe
RPCDOCEXE     = $(TOPBUILDDIR)/docs/$(DOCGENDIR)/rpc_doc.exe
ERRDOCEXE     = $(TOPBUILDDIR)/docs/$(DOCERRORDIR)/error_doc.exe

PROTOCOLS     = ithaca jakarta alpha

# The following variables names are lowercase, so their names can be computed
# from the names of the corresponding protocol directories
ithaca_long   = Psithaca2MLRFYargivpo7YvUr7wUDqyxrdhC5CQq78mRvimz6A
jakarta_long  = PtJakart2xVj7pYXJBXrqHgd82rdkLey5ZeeGwDgPp9rhQUbSqY
alpha_long    = ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK

ithaca_short  = 012-Psithaca
jakarta_short = 013-PtJakart
alpha_short   = alpha

SCRIPTSDIR    = scripts
CHECKXREFS    = $(SCRIPTSDIR)/check_proto_xrefs.py

# Build all documentation (with CLI manuals and odoc) but without old PROTOCOLS
all: odoc-lite
	# Html needs the API pages for Ocaml modules, generated by odoc, because of
	# the :package: custom roles in RST files; so run html *after* odoc
	${MAKE} html

# Build the full documentation including archived protocols; for publishing only
full: odoc
	# Same ordering constraint: html *after* odoc (see target "all")
	${MAKE} html

main:
	${MAKE} -C .. all

# Build the manuals for a given protocol
%/tezos-client.html: main
	@../tezos-client -protocol $($(@D)_long) man -verbosity 3 -format html | sed "s#${HOME}#\$$HOME#g" > $@
%/tezos-baker.html: main
	@../tezos-baker-$($(@D)_short) man -verbosity 3 -format html | sed "s#${HOME}#\$$HOME#g" > $@
%/tezos-accuser.html: main
	@../tezos-accuser-$($(@D)_short) man -verbosity 3 -format html | sed "s#${HOME}#\$$HOME#g" > $@

manuals: \
	 $(PROTOCOLS:%=%/tezos-client.html) \
	 $(PROTOCOLS:%=%/tezos-baker.html) \
	 $(PROTOCOLS:%=%/tezos-accuser.html)
	# generic
	@../tezos-admin-client man -verbosity 3 -format html | sed "s#${HOME}#\$$HOME#g" > api/tezos-admin-client.html
	@../tezos-signer man -verbosity 3 -format html | sed "s#${HOME}#\$$HOME#g" > api/tezos-signer.html
	@../tezos-codec man -verbosity 3 -format html | sed "s#${HOME}#\$$HOME#g" > api/tezos-codec.html
	@../tezos-snoop man -verbosity 3 -format html | sed "s#${HOME}#\$$HOME#g" > api/tezos-snoop.html

# artificially depend on docexes to avoid concurrent dunes
.PHONY: odoc
odoc: main docexes
	cd $$(pwd)/.. ; dune build @doc
	@rm -rf $$(pwd)/_build/api/odoc
	@mkdir -p $$(pwd)/_build/api
	@cp -r $$(pwd)/../_build/default/_doc $$(pwd)/_build/api/odoc

odoc-lite: main docexes
	rm -rf $(TMPDOCDIR)/
	mkdir -p $(TMPDOCDIR)/
	rsync --recursive --links --perms --exclude="src/proto_0*" \
		../src ../tezt ../vendors ../dune ../dune-project $(TMPDOCDIR)/
	# FIXME: https://gitlab.com/tezos/tezos/-/issues/2971
	# The new version of odoc (2.1.0) is stricter than the old version (1.5.3),
	# we temporarily deactivate the odoc checks.
	# cd $(TMPDOCDIR); ODOC_WARN_ERROR=true dune build @doc
	cd $(TMPDOCDIR); dune build @doc
	@rm -rf _build/api/odoc
	@mkdir -p _build/api
	@cp -r $(TMPDOCDIR)/_build/default/_doc _build/api/odoc

linkcheck:
	$(SPHINXBUILD) -b linkcheck "$(SOURCEDIR)" "$(BUILDDIR)"

.PHONY: redirectcheck
redirectcheck:
	@cd $$(pwd)/.. ; ./scripts/lint.sh --check-redirects

# xrefs should be checked:
# - on the active protocol using its numeric name (not the "active" symlink)
# - on each other protocol including alpha, also checking label defs (option -l)
xrefscheck:
	$(CHECKXREFS) ithaca
	$(CHECKXREFS) -l jakarta
	$(CHECKXREFS) -l alpha

scriptsindoccheck:
	introduction/test_howtoget.sh install-bin-bionic
	introduction/test_howtoget.sh install-bin-focal
	introduction/test_howtoget.sh install-bin-fedora34
	introduction/test_howtoget.sh install-bin-rc-bionic
	introduction/test_howtoget.sh install-bin-rc-focal
	introduction/test_howtoget.sh install-bin-rc-fedora34
	introduction/test_howtoget.sh install-opam-scratch
	introduction/test_howtoget.sh install-opam-bionic
	introduction/test_howtoget.sh install-opam-focal
	introduction/test_howtoget.sh compile-sources-buster
	#introduction/test_howtoget.sh use-docker # returns 1 (and eats alot of disk)

sanitycheck:
	@if grep -Rq alpha 0*; then					\
		echo "'alpha' occurs in the docs for released protocols:";	\
                grep -R alpha 0*;                                               \
		exit 1;								\
	fi

# build dune targets together to avoid concurrent dunes
docexes: main
	cd .. && dune build docs/$(DOCERRORDIR)/error_doc.exe docs/$(DOCGENDIR)/rpc_doc.exe docs/$(DOCGENDIR)/p2p_doc.exe

developer/metrics.csv: main
	../tezos-node dump-metrics > developer/metrics.csv

$(ERRDOCEXE): docexes
$(RPCDOCEXE): docexes
$(P2PDOCEXE): docexes

api/errors.rst: $(ERRDOCEXE)
	$(ERRDOCEXE) > api/errors.rst

%/rpc.rst: $(RPCDOCEXE)
	$(RPCDOCEXE) index $(@D) > $@

user/default-acl.json: $(RPCDOCEXE)
	$(RPCDOCEXE) acl > $@

rpc: user/default-acl.json shell/rpc.rst \
			$(PROTOCOLS:%=%/rpc.rst)

p2p: $(P2PDOCEXE) shell/p2p_usage.rst.inc
	@$(P2PDOCEXE) < shell/p2p_usage.rst.inc > shell/p2p_api.rst

.PHONY: install-dependencies
install-dependencies:
	@poetry install

user/node-config.json: user/node-config.sh
	cd ..; ./docs/user/node-config.sh >docs/$@

# Catch-all target: route all unknown targets to Sphinx using the new
# "make mode" option.  $(O) is meant as a shortcut for $(SPHINXOPTS).
html: api/errors.rst developer/metrics.csv user/node-config.json \
		rpc p2p manuals install-dependencies
	(echo ':orphan:'; echo ''; cat ../CHANGES.rst) > CHANGES-dev.rst
	@$(SPHINXBUILD) -b html "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS)
	@ln -s active "$(BUILDDIR)/ithaca" || true

# pylint does not support pyproject.toml configuration for now
# It is configured by ./.pylint.rc
# In pylint, we ignore the following additional errors:
# - C0330: pylint is wrong (https://github.com/psf/black/issues/48)
PYLINTRC?=../.pylintrc
PYLINT=PYLINTRC=${PYLINTRC} poetry run pylint --disable C0330

# pycodestyle does not support pyproject.toml configuration for now
# https://github.com/PyCQA/pydocstyle/issues/447
#
# In pycodestyle we ignore the following additional errors w.r.t to defaults
# (i.e. E121,E123,E126,E226,E24,E704,W503)
# - E203 :: pycodestyle is wrong (https://github.com/psf/black/issues/315)
#           and black takes care of it
# - E231 :: same as E203
#
# Other error codes are detailed at https://www.flake8rules.com/
PYCODESTYLE=poetry run pycodestyle
PYCODESTYLE_OPTIONS=\
	--ignore=E121,E123,E126,E226,E24,E704,W503,E203,E231 \
	--max-line-length=80

PYTHON_SRCS?=conf.py _extensions/*.py scripts/*.py

pylint:
	@echo "Linting with pylint, version:"
	@poetry run pylint --version | sed 's/^/  /'
	$(PYLINT) $(PYTHON_SRCS)

lint_black:
	@echo "Running black formatter as style checker"
	poetry run black --check $(PYTHON_SRCS)

pycodestyle:
	@echo "Linting with pycodestyle version `poetry run pycodestyle --version` (`poetry run which pycodestyle`)"
	$(PYCODESTYLE) $(PYCODESTYLE_OPTIONS) $(PYTHON_SRCS)

lint: pylint pycodestyle lint_black

clean:
	@-rm -Rf "$(BUILDDIR)"
	@-rm -Rf api/errors.rst developer/metrics.csv user/node-config.json active/rpc.rst ithaca/rpc.rst jakarta/rpc.rst alpha/rpc.rst shell/rpc.rst shell/p2p_api.rst user/default-acl.json CHANGES-dev.rst
	@-rm -Rf api/tezos-*.html api/tezos-*.txt active/tezos-*.html ithaca/tezos-*.html jakarta/tezos-*.html alpha/tezos-*.html
