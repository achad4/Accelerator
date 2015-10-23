#!/bin/bash   

ocamllex scanner.mll
ocamlyacc -v parser.mly
ocamlc -c parser.mli
ocamlc -c scanner.ml
ocamlc -c parser.ml
ocamlc -o a parser.cmo scanner.cmo