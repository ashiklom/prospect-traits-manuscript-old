all: build deploy

deploy:
	./_deploy.sh

build:
	./_build.sh

results:
	rm -f results.db
	scripts/get_results.sh

site_info.rds:
	Rscript scripts/make_site_info.R
