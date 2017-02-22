all:
	Rscript -e 'bookdown::render_book("index.Rmd", "bookdown::gitbook")'

results:
	rm -f results.db
	scripts/get_results.sh

site_info.rds:
	Rscript scripts/make_site_info.R
