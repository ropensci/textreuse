This is a new release with bug fixes, documentation refreshes, and helper
functions added after a long maintenance interval.

This resubmission fixes a moved URL in README.md:
https://github.com/hadley/devtools was replaced with
https://github.com/r-lib/devtools.

This is also a maintainer change release. The maintainer has changed from
Lincoln Mullen <lincoln@lincolnmullen.com> to Yaoxiang Li
<liyaoxiang@outlook.com>. The previous maintainer, Lincoln Mullen, has
confirmed by email that he supports the maintainer transition. I can provide
the email thread if requested.

## Test environments

* local Windows 11 install: R 4.4.2

## R CMD check results

There were no ERRORs or WARNINGs.

Local checks were run with:

`R CMD check --no-manual textreuse_1.0.0.tar.gz`

`R CMD check --as-cran --no-manual textreuse_1.0.0.tar.gz`

The `--as-cran` check reported three NOTEs:

* This release changes the maintainer from Lincoln Mullen to Yaoxiang Li.
* The local Windows check was unable to verify the current time.
* The local Windows check reported that README.md or NEWS.md could not be
  checked without pandoc. README.md was regenerated locally with rmarkdown,
  and the pkgdown site was built locally with RStudio Pandoc before release.

There were no invalid URL NOTEs.

## Downstream dependencies

There are no known downstream dependency issues.
