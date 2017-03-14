pdf: ./NAMESPACE
	R ${R_OPTS} -e 'library(roxygen2);roxygenize("~/ITHIM/")'
	R CMD Rd2pdf --no-preview --force ~/ITHIM/

html: ./man/*
	R ${R_OPTS} -e 'library(roxygen2);roxygenize("~/ITHIM/")'
	R CMD INSTALL --html --no-inst ~/ITHIM/
# 	cp -v ~/R/x86_64-redhat-linux-gnu-library/3.0/ITHIM/html/* /ua/kendzior/public_html/CKGROUP/YOUNKIN/ITHIM-html/
# 	cp -v ~/R/x86_64-redhat-linux-gnu-library/3.0/ITHIM/DESCRIPTION /ua/kendzior/public_html/CKGROUP/YOUNKIN/
# cd /ua/kendzior/public_html/CKGROUP/YOUNKIN/ITHIM-html/; touch ./*.html

check:
	R --vanilla < ~/GHI/R/check.R
	R CMD build .
	R CMD check --as-cran ITHIM*.tar.gz
