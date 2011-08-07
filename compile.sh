#!/bin/sh
set -e
pushd strips >& /dev/null
echo "Running make in 'strips...'"
make clean
make
popd >& /dev/null
echo "done, building rest"
ocamlfind ocamlopt -g -package pxp -package camlp4 -syntax camlp4o -c -I strips strips.cmxa objconvert.ml
ocamlfind ocamlopt -g -package pxp -package bigarray -linkpkg -I strips strips.cmxa -o objconvert.bin objconvert.cmx
