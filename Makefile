pdf: ./NAMESPACE
	R ${R_OPTS} -e 'library(roxygen2);roxygenize("~/ITHIM/")'
	R CMD Rd2pdf --no-preview --force ~/ITHIM/

html: ./man/*
	R ${R_OPTS} -e 'library(roxygen2);roxygenize("~/ITHIM/")'
	R CMD INSTALL --html --no-inst ~/ITHIM/

check:
	rm -v ITHIM_*.*.*.tar.gz
	R --vanilla < ~/GHI/R/check.R
	R CMD build .
	R CMD check --as-cran ITHIM_*.*.*.tar.gz
