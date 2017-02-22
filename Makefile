all: build deploy

deploy:
	./_deploy.sh

build:
	./_build.sh

results:
	scripts/get_results.sh

site_info.rds:
	Rscript scripts/make_site_info.R
