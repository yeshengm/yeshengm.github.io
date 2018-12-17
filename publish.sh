#!/usr/bin/zsh
# commit dev
git checkout dev
git add --all & git commit -am "src updated on `date`"
stack exec site clean
stack exec site build
git push origin dev
# commit master
git checkout -b master --track origin/master
cp -a _site/. .
git add --all & git commit -am "published on `date`"
git push origin master
# back to dev and delete master
git checkout dev
git branch -D master

