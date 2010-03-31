#!/bin/sh
set -e
pushd strips
make clean
make
popd
ocamlfind ocamlc -g -package pxp -package camlp4 -syntax camlp4o -c -I strips strips.cma objconvert.ml
ocamlfind ocamlc -g -package pxp -package bigarray -linkpkg -I strips strips.cma -o objconvert.bin objconvert.cmo
