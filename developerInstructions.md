# Developer instructions for `qFeature`

We assume that:

1. You have forked the [qFeature repository](http://github.com/pnnl/qFeature) to another group or user
2. You have cloned your fork to create a local copy
3. You have [configured](http://help.github.com/articles/configuring-a-remote-for-a-fork/) your local copy to sync with the [upstream](http://github.com/pnnl/qFeature)

Note there are two active branches on qFeature:  `master` and `gh-pages`.  The `master` branch contains all the package, and the `gh-pages` branch contains the content that is viewable from [http://pnnl.github.io/qFeature](http://pnnl.github.io/qFeature).  It is imperative you not confuse these branches.

## Summary of the editing process

Any time you plan to make edits on the `master` branch, please do the following:

1. Make sure you are on the `master` branch of your local copy using: `git checkout master`
2. [Sync your local copy](https://help.github.com/articles/syncing-a-fork/) with the upstream
3. Make edits
4. Process the package using [prepPackage](http://github.com/pnnl/prepPackage)
5. Commit your changes and push to the `master` branch of your fork on github
6. Issue the pull request on the `master` branch

## Edits to the vignette

If, during your edits on the `master` branch, you make edits to the vignette, you will also need to update the vignette on the `gh-pages` branch, since the vignette is the `index.html` on the `gh-pages` branch.  Please do the following:

1. Start off on the `master` branch using:  `git checkout master`
1. Copy the vignette file `inst/doc/qFeatureTechnical.html` to a temporary location
2. Change the branch to the 'gh-pages' branch using:  `git checkout gh-pages`
3. [Sync your local copy](https://help.github.com/articles/syncing-a-fork/) with the upstream.  Note that instead of using the `master` branch, use the `gh-pages` branch where appropriate.
4. Move (and rename) the temporary copy of the vignette to `index.html` at the root of the `gh-pages` branch
5. Commit and push to the `gh-pages` branch of your fork
6. Issue the pull request on the `gh-pages` branch