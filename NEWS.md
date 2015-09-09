Version 0.1.4, 2015-09-09
-----------------------------------------------------------------------------------

FEATURES / CHANGES

- Added `genDemoData()`, which can be called from other packages via the `qFeature`
  namespace.

- Added README.md 


BUG FIXES

- Corrected passing of `fitQ` arguments from higher-level arguments


Version 0.1.3, 2015-09-04
-----------------------------------------------------------------------------------

FEATURES / CHANGES

- Edits to make ready package for deployment to github and CRAN

BUG FIXES 

- `ddply_getFeatures()` was failing due to incorrect input to `Smisc::selectElements()`

Version 0.1.2, 2015-02-16
-----------------------------------------------------------------------------------

BUG FIXES

- `plyr::ddply()` produces a bogus warning when run in parallel (see https://github.com/hadley/plyr/issues/203). 
   Corrected the handling of these warnings so that they do not appear when using `ddply_getFeatures()` in parallel.

Version 0.1.1, 2015-02-12
-----------------------------------------------------------------------------------

FEATURES / CHANGES

- Added `check_getFeatures_args()` and `check_fitQ_args()` to minimize redundancy in the argument checking
- Added `centerScale` argument to `ddply_getFeatures()` and `getFeatures()` to easily center and scale continuous 
  variables prior to extracting features
- Removed `inst/validation_functions/calcIVTslow()` because it was a legacy function from `calcIVT` that no 
  longer has any relevance for the package


Version 0.1.0, 2015-01-29
-----------------------------------------------------------------------------------

FEATURES / CHANGES

- Initial version of `qFeature`, a revision of the `calcIVT` package

