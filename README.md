<!-- README.md is generated from README.Rmd. Please edit that file -->
textreuse: Detect Text Reuse and Document Similarity
----------------------------------------------------

**This package is in very early development and does little as of yet.**

This R package provides a set of functions for detecting passages which have been reused across documents, as well as for calculating similarity between documents based on those matches. These functions are intended in the first instance for historical research, but should be more broadly applicable.

This package is part of a collaboration between [Lincoln Mullen](http://lincolnmullen.com) and [Kellen Funk](http://kellenfunk.org). For more about the project we are working on, see the following blog posts (though these precede this implementation):

-   Funk, "[The Influence of the Field Code: An Introduction to the Critical Issues](http://kellenfunk.org/field-code/the-influence-of-the-field-code-an-introduction/)"
-   Funk, "[Density Plots: Predicting the Influence of One Jurisdiction's Code on Another’s](http://kellenfunk.org/field-code/density-plots/)"
-   Funk, "[Network Analysis: Finding the Regional Influences of Codification](http://kellenfunk.org/field-code/network-analysis/)"
-   Mullen, "[Detecting Text Reuse in Nineteenth-Century Legal Documents: Methods and Preliminary Results](http://lincolnmullen.com/blog/detecting-text-reuse-in-legal-documents/)"

The actual analyses are available in [this GitHub repository](https://github.com/lmullen/civil-procedure-codes).

### Installation

This package is not yet on CRAN. To install it from GitHub, use devtools. This package depends on the development version of [stringr](https://github.com/hadley/stringr), which in turn depends on [stringi](https://github.com/Rexamine/stringi/). You will also need to install them.

``` r
# install.packages("devtools")
devtools::install_github("Rexamine/stringi")
devtools::install_github("hadley/stringr")
devtools::install_github("lmullen/textreuse")
```
