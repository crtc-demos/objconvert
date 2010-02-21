#!/bin/sh
set -e
ocamlfind ocamlc -annot -g -package pxp -c objconvert.ml
ocamlfind ocamlc -g -package pxp -linkpkg -o objconvert objconvert.cmo
