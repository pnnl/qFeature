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

