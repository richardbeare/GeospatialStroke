
.DEFAULT_GOAL := knitr

knitr:
	echo "Sys.setenv(R_KNITR_OPTIONS = 'knitr.chunk.tidy = FALSE', RSTUDIO_PANDOC='/usr/lib/rstudio/bin/pandoc');library(rmarkdown);rmarkdown::render_site()" | R --no-save -q


clean:
	rm -rf *.html *.png README_cache
