#!/usr/bin/env bash

# clone smartparens somewhere for testing...
smartparensDir="${XDG_RUNTIME_DIR:-/tmp}/smartparens"
dashDir="${XDG_RUNTIME_DIR:-/tmp}/dash"

if [[ ! -d "$smartparensDir" ]]; then
    git clone --depth 1 https://github.com/Fuco1/smartparens "$smartparensDir"
fi

if [[ ! -d "$dashDir" ]]; then
    git clone --depth 1 https://github.com/magnars/dash.el "$dashDir"
fi


emacs -batch -q -l cpo-tree-walk.el \
      -l cpo-indent-tree.el -l test-cpo-indent-tree.el \
      -l "$dashDir/dash.el" -l "$smartparensDir/smartparens.el" -l cpo-smartparens.el -l test-cpo-smartparens.el \
      -l cpo-text-object-stuff.el -l test-cpo-text-object-stuff.el \
      -f ert-run-tests-batch-and-exit
