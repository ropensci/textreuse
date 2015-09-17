<!-- README.md is generated from README.Rmd. Please edit that file -->
textreuse
---------

An R package for detecting text reuse and document similarity in a corpus.

**Author:** [Lincoln Mullen](http://lincolnmullen.com)<br> **License:** [MIT](http://opensource.org/licenses/MIT)<br> **Status:** In development

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/textreuse)](http://cran.r-project.org/package=textreuse) [![CRAN\_Downloads](http://cranlogs.r-pkg.org/badges/grand-total/textreuse)](http://cran.r-project.org/package=textreuse) [![Build Status](https://travis-ci.org/lmullen/textreuse.svg?branch=master)](https://travis-ci.org/lmullen/textreuse) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/lmullen/textreuse?branch=master)](https://ci.appveyor.com/project/lmullen/textreuse)

### Description

This R package provides a set of functions for measuring similarity among documents and detecting passages which have been reused. It implements shingled n-gram, skip n-gram, and other tokenizers; similarity/dissimilarity functions; pairwise comparisons; and minhash and locality sensitive hashing algorithms.

The package was written to facilitate historical research among legal texts, but it is broadly useful for, for example, detecting duplicate documents in a corpus prior to text analysis. The classes provides by this package follow the model of other natural language processing packages for R, especially the [NLP](https://cran.r-project.org/package=NLP) and [tm](https://cran.r-project.org/package=tm) packages. (However, this package has no dependency on Java, which should make it easier to install.)

### Installation

This package is not yet on CRAN. To install it from GitHub, use [devtools](https://github.com/hadley/devtools).

``` r
# install.packages("devtools")
devtools::install_github("lmullen/textreuse", build_vignettes = TRUE)
```

### Example

In this example we will load a tiny corpus of three documents. These documents are drawn from Kellen Funk's [research](http://kellenfunk.org/field-code/) into the propagation of legal codes of civil procedure in the nineteenth-century United States.

``` r
library(textreuse)
dir <- system.file("extdata/legal", package = "textreuse")
corpus <- TextReuseCorpus(dir = dir, meta = list(title = "Civil procedure"),
                          tokenizer = tokenize_ngrams, n = 7)
```

We have loaded the three documents into a corpus, which involves tokenizing the text and hashing the tokens. We can inspect the corpus as a whole or the individual documents that make it up.

``` r
corpus
#> TextReuseCorpus
#> Number of documents: 3 
#> hash_func : hash_string 
#> title : Civil procedure 
#> tokenizer : tokenize_ngrams
corpus[["ca1851-match"]]
#> TextReuseTextDocument
#> file : /Users/lmullen/Library/R/3.2/library/textreuse/extdata/legal/ca1851-match.txt 
#> hash_func : hash_string 
#> id : ca1851-match 
#> tokenizer : tokenize_ngrams 
#> content : § 4. Every action shall be prosecuted in the name of the real party
#> in interest, except as otherwise provided in this Act.
#> 
#> § 5. In the case of an assignment of a thing in action, the action by
#> the as
```

Now we can compare each of the documents to one another. The result is a matrix of scores. As we would expect, some documents are similar and others are not.

``` r
comparisons <- pairwise_compare(corpus, jaccard_similarity)
comparisons
#>                ca1851-match ca1851-nomatch ny1850-match
#> ca1851-match             NA              0    0.3842549
#> ca1851-nomatch           NA             NA    0.0000000
#> ny1850-match             NA             NA           NA
```

We can convert that matrix to a data frame of pairs and scores if we prefer.

``` r
pairwise_candidates(comparisons)
#>                a              b     score
#> 1   ca1851-match ca1851-nomatch 0.0000000
#> 2   ca1851-match   ny1850-match 0.3842549
#> 3 ca1851-nomatch   ny1850-match 0.0000000
```

Pairwise comparisons can be very time-consuming because they grow exponentially with the size of the corpus. (A corpus with 10 documents would require at least 45 comparisons; a corpus with 100 documents would require 4,950 comparisons; a corpus with 1,000 documents would require 499,500 comparisons.) That's why this package implements the minhash and locality sensitive hashing algorithms, which can detect candidate pairs much faster than pairwise comparisons in corpora of any significant size. For details, see the package vignettes.

``` r
vignette("introduction", package = "textreuse")
vignette("pairwise", package = "textreuse")
vignette("minhash", package = "textreuse")
```

### Citation

If you use this package for scholarly research, I would appreciate a citation.

``` r
citation("textreuse")
#> 
#> To cite package 'textreuse' in publications use:
#> 
#>   Lincoln Mullen (2015). textreuse: Detect Text Reuse and Document
#>   Similarity. R package version 0.0.1.9000.
#>   https://github.com/lmullen/textreuse
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {textreuse: Detect Text Reuse and Document Similarity},
#>     author = {Lincoln Mullen},
#>     year = {2015},
#>     note = {R package version 0.0.1.9000},
#>     url = {https://github.com/lmullen/textreuse},
#>   }
```
