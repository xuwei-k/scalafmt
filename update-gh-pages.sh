#!/bin/bash
set -e

git checkout master
SHA=`git rev-parse --verify HEAD`
sbt "readme/run --validate-links"

assets="assets"
mkdir -p ${assets}
cp -r readme/target/scalatex/* ${assets}
find . -not -name "${assets}*" -print0 | xargs -0 rm -rf --
mv ${assets}/* .
rmdir assets
git checkout gh-pages

# If there are no changes to the compiled out (e.g. this is a README update) then just bail.
if [ -z `git diff --exit-code` ]; then
    echo "No changes to the output on this push; exiting."
    exit 0
fi

git config user.name "Travis CI"
git config user.email "$COMMIT_AUTHOR_EMAIL"

git add .
git commit -m "Deploy to GitHub Pages: ${SHA}"

#eval `ssh-agent -s`
#ssh-add travis_deploy

#git push origin gh-pages
#git checkout master
echo "Done!"
