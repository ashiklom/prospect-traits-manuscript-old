#!/bin/bash
pushd _book
rm -rf .git
git init
git add * .nojekyll
git commit -m "Create website: `date`"
git checkout -b gh-pages
git push -f git@github.com:ashiklom/prospect-traits-manuscript gh-pages
popd
