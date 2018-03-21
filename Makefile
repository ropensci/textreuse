.PHONY : docs deploy-docs

docs :
	Rscript -e "pkgdown::clean_site(); pkgdown::build_site(run_dont_run = TRUE)"

deploy-docs :
	rsync --checksum --delete -avz docs/* reclaim:~/www/lincolnmullen.com/software/textreuse/

