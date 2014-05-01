#!/bin/bash

# Compila Los archivos con knitr
Rscript -e 'knitr::knit("../R/01_example1.Rnw")'
rm -f 01_example1.pdf

# Compila el tex
pdflatex '00_main.tex'

# Abriendo
evince '00_main.pdf' &

# Limpia
rm -f *.out
rm -f *.aux
rm -f *.log
