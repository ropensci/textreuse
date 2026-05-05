.PHONY : docs deploy-docs

docs :
	Rscript -e "pkgdown::clean_site(); pkgdown::build_site(run_dont_run = TRUE)"

deploy-docs :
	@echo "Documentation is published by rOpenSci at https://docs.ropensci.org/textreuse/"

