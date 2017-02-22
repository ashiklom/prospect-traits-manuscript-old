all:
	Rscript -e 'bookdown::render_book("index.Rmd", "bookdown::gitbook")'

results:
	rm -f results.db
	scripts/get_results.sh
	sqlite3 results.db < results.sql
