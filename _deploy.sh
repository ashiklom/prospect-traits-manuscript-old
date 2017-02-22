#!/bin/bash
pushd _book
rm -rf *.git
git init
git add * .*
git commit -f "Create website: `date`"
git checkout -b gh-pages
git push -f git@github.com:ashiklom/prospect-traits-manuscript gh-pages
popd
