pdf: ./R/functions.R ./R/functions2.R
	R ${R_OPTS} -e 'library(roxygen2);roxygenize("~/ITHIM/")'
	R CMD Rd2pdf --no-preview --force ~/ITHIM/
#	mv -v ~/ITHIM/ITHIM.pdf ~/ckgroup-html/

# html: ./man/*
# 	R ${R_OPTS} -e 'library(roxygen2);roxygenize("~/ITHIM/")'
# 	R CMD INSTALL --html --no-inst ~/ITHIM/
# 	cp -v ~/R/x86_64-redhat-linux-gnu-library/3.0/ITHIM/html/* /ua/kendzior/public_html/CKGROUP/YOUNKIN/ITHIM-html/
# 	cp -v ~/R/x86_64-redhat-linux-gnu-library/3.0/ITHIM/DESCRIPTION /ua/kendzior/public_html/CKGROUP/YOUNKIN/
# cd /ua/kendzior/public_html/CKGROUP/YOUNKIN/ITHIM-html/; touch ./*.html
