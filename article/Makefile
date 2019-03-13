


NAME = geospatial-stroke
SUPP = supplementary_geospatial_stroke

PNGS=$(notdir $(wildcard *.png))
JPGS=$(notdir $(wildcard *.jpg))
DIAS=$(notdir $(wildcard *.dia))
FIGS=$(notdir $(wildcard *.fig))
EPSS=$(notdir $(wildcard *.eps))
TIFS=$(notdir $(wildcard *.tif))

${NAME}.pdf: ${NAME}.tex references.bib ${PNGS}
	latexmk -bibtex -pdf -pdflatex="pdflatex -interaction=nonstopmode" -use-make ${NAME}.tex

${SUPP}.pdf: ${SUPP}.tex
	latexmk  -pdf -pdflatex="pdflatex -interaction=nonstopmode" -use-make ${SUPP}.tex

all: ${SUPP}.pdf ${NAME}.pdf
clean:
	latexmk -CA

