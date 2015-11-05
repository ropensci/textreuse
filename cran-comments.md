This is a patch release of the `textreuse` package. It fixes two problems identified by Brian Ripley. It fixes a locale-related problem with one of the tests; it fixes a bad read identified by valgrind.

Thanks to the CRAN maintainers for identifying these problems, and apologies for patches in quick succession.

## Test environments

* local OS X 10.10.5 install, R 3.2.2
* Ubuntu 12.04 (on Travis-CI), R 3.2.2
* Win-builder (R-devel and R-release)

## R CMD check results

There were no ERRORs or WARNINGs. 

There was one NOTE about this being a updated version of the package. The NOTE mentions three possible misspelled words, all of which are correctly spelled proper nouns or terms of art.
