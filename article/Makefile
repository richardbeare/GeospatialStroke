


NAME = geospatial-stroke


%.eps: Pics/%.tif
	convert $< $@


${NAME}.pdf: ${NAME}.tex references.bib
	latexmk -bibtex -pdf -pdflatex="pdflatex -interaction=nonstopmode" -use-make ${NAME}.tex

clean:
	latexmk -CA

