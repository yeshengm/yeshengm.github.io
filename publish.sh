#!/usr/bin/zsh
# commit dev
git checkout dev
git add --all & git commit -am "src updated on `date`"
stack exec site clean
stack exec site build
git push -f origin dev
# commit master
git checkout -b master
cp -a _site/. .
git add --all & git commit -am "published on `date`"
git push -f origin master
# back to dev and delete master
git checkout -f dev
git branch -D master

