#!/bin/bash
# The lambda-blas project has a branch (gh-pages) that contains the content for
# the git hub web pages.   Since this branch contains no source code, only
# build artifacts, we have chosen to simply overwrite the existing commit
# each time a new artiact is generated.

# In order to protect the lambda-blas repository from inadvertant changes to
# The master or feature branches, all work on the gh-pages branch will be
# performed in a separate folder.

# Create the gh-pages folder and copy the haddock and benchmark artifacts from
# the latest build
rm -rf ../gh-pages
mkdir -p ../gh-pages
cp -R "$(stack path --local-doc-root)" ../gh-pages
cp benchmarks.html ../gh-pages
cd ../gh-pages

# Configure the new folder as a git repository.   Rather than cloning (which)
# could take a long time, we simply initialize it and set the remote to the
# lambda-blas remote.   Depending upon if the script is run on the CI machine
# or locally we may have a couple of different steps.
git init
if [ "$TRAVIS" == "true" ]; then
    git config user.email "travis@travis-ci.org"
    git config user.name "Travis"
    git remote add origin https://${GH_TOKEN}@github.com/dlewissandy/lambda-blas.git &> /dev/null
else
    git remote add origin git@github.com:dlewissandy/lambda-blas.git
fi
git checkout -B gh-pages
git add .
git commit -m "Haddocks updated"
git push --force origin gh-pages &> /dev/null
