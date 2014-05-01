#!/bin/bash

# Compila Los archivos con knitr
Rscript -e 'knitr::knit("01_example1.Rnw")'
rm -f 01_example1.pdf

# Compila el tex
pdflatex '00_main.tex'

# Limpia
rm -f *.out
rm -f *.aux
rm -f *.log
