paper:
	# Compila Los archivos con knitr
	Rscript -e 'knitr::knit("../R/01_example1.Rnw")'
	Rscript -e 'knitr::knit("../R/02_sumstats.Rnw")'
	rm -f 01_example1.pdf
	rm -f 02_sumstats.pdf

	# Compila el tex
	pdflatex '00_main.tex'
	
	# Abriendo
	evince '00_main.pdf' &
clean:	
	# Limpia
	rm -f *.out
	rm -f *.aux
	rm -f *.log
