#!/bin/bash

git config --global user.email 'ashiklom@bu.edu'
git config --global user.name 'Alexey Shiklomanov'

#git clone -b gh-pages https://ashiklom@github.com/manuscript.git book-output
git clone -b gh-pages git@github.com:ashiklom/manuscript book-output

#git clone -b gh-pages \
    #https://${GITHUB_PAT}@github.com/${TRAVIS_REPO_SLUG}.git \
    #book-output

cp -r _book/* book-output

pushd book-output

git add --all *

git commit -m "Update book: `date`"

git push -q origin gh-pages

popd
