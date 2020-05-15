#!/usr/bin/zsh
set -x
# commit dev
git checkout dev
git push -f origin dev
stack build
stack exec hakyll clean
stack exec hakyll build
# commit master
git fetch --all
git branch -D master
git checkout -b master --track origin/master
cp -a _site/. .
git add --all & git commit -am "published on `date`"
git push -f origin master
# back to dev and delete master
git checkout -f dev
git clean -fdx
# sync to andrew
# rsync -r -a -vv --delete _site/ yeshengm@linux.andrew.cmu.edu:~/www/
