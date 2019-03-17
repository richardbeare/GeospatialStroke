LFILE = Googleway_Mapdeck

.DEFAULT_GOAL := knitr

knitr: $(LFILE).Rmd
	echo "Sys.setenv(R_KNITR_OPTIONS = 'knitr.chunk.tidy = FALSE', RSTUDIO_PANDOC='/usr/lib/rstudio/bin/pandoc');library(rmarkdown);rmarkdown::render('$(LFILE).Rmd')" | R --no-save -q


clean:
	rm -rf *.html *.png README_cache
