<!-- README.md is generated from README.Rmd. Please edit that file -->

# textreuse

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/textreuse)](https://cran.r-project.org/package=textreuse)
[![CRAN\_Downloads](http://cranlogs.r-pkg.org/badges/grand-total/textreuse)](https://cran.r-project.org/package=textreuse)
[![Build
Status](https://travis-ci.org/ropensci/textreuse.svg?branch=master)](https://travis-ci.org/ropensci/textreuse)
[![Build
status](https://ci.appveyor.com/api/projects/status/9qwf0473xi8cyuoh/branch/master?svg=true)](https://ci.appveyor.com/project/lmullen/textreuse-6xljc/branch/master)
[![Coverage
Status](https://img.shields.io/codecov/c/github/ropensci/textreuse/master.svg)](https://codecov.io/github/ropensci/textreuse?branch=master)
[![rOpenSci
badge](https://badges.ropensci.org/20_status.svg)](https://github.com/ropensci/onboarding/issues/20)

## Overview

This [R](https://www.r-project.org/) package provides a set of functions
for measuring similarity among documents and detecting passages which
have been reused. It implements shingled n-gram, skip n-gram, and other
tokenizers; similarity/dissimilarity functions; pairwise comparisons;
minhash and locality sensitive hashing algorithms; and a version of the
Smith-Waterman local alignment algorithm suitable for natural language.
It is broadly useful for, for example, detecting duplicate documents in
a corpus prior to text analysis, or for identifying borrowed passages
between texts. The classes provides by this package follow the model of
other natural language processing packages for R, especially the
[NLP](https://cran.r-project.org/package=NLP) and
[tm](https://cran.r-project.org/package=tm) packages. (However, this
package has no dependency on Java, which should make it easier to
install.)

### Citation

If you use this package for scholarly research, I would appreciate a
citation.

    citation("textreuse")
    #> To cite package 'textreuse' in publications use:
    #> 
    #>   Li Y, Mullen L (2024). _textreuse: Detect Text Reuse and Document
    #>   Similarity_. https://docs.ropensci.org/textreuse,
    #>   https://github.com/ropensci/textreuse.
    #> 
    #> A BibTeX entry for LaTeX users is
    #> 
    #>   @Manual{,
    #>     title = {textreuse: Detect Text Reuse and Document Similarity},
    #>     author = {Yaoxiang Li and Lincoln Mullen},
    #>     year = {2024},
    #>     note = {https://docs.ropensci.org/textreuse,
    #> https://github.com/ropensci/textreuse},
    #>   }

## Installation

To install this package from CRAN:

    install.packages("textreuse")

To install the development version from GitHub, use
[devtools](https://github.com/hadley/devtools).

    # install.packages("devtools")
    devtools::install_github("ropensci/textreuse", build_vignettes = TRUE)

## Examples

There are three main approaches that one may take when using this
package: pairwise comparisons, minhashing/locality sensitive hashing,
and extracting matching passages through text alignment.

See the [introductory
vignette](https://cran.r-project.org/package=textreuse/vignettes/textreuse-introduction.html)
for a description of the classes provided by this package.

    vignette("textreuse-introduction", package = "textreuse")

### Pairwise comparisons

In this example we will load a tiny corpus of three documents. These
documents are drawn from Kellen Funk’s
[research](http://kellenfunk.org/field-code/) into the propagation of
legal codes of civil procedure in the nineteenth-century United States.

    library(textreuse)
    dir <- system.file("extdata/legal", package = "textreuse")
    corpus <- TextReuseCorpus(dir = dir, meta = list(title = "Civil procedure"),
                              tokenizer = tokenize_ngrams, n = 7)

We have loaded the three documents into a corpus, which involves
tokenizing the text and hashing the tokens. We can inspect the corpus as
a whole or the individual documents that make it up.

    corpus
    #> TextReuseCorpus
    #> Number of documents: 3 
    #> hash_func : hash_string 
    #> title : Civil procedure 
    #> tokenizer : tokenize_ngrams
    names(corpus)
    #> [1] "ca1851-match"   "ca1851-nomatch" "ny1850-match"
    corpus[["ca1851-match"]]
    #> TextReuseTextDocument
    #> file : C:/Users/bach/AppData/Local/Temp/RtmpecFDvh/temp_libpath4c4124c4b59/textreuse/extdata/legal/ca1851-match.txt 
    #> hash_func : hash_string 
    #> id : ca1851-match 
    #> minhash_func : 
    #> tokenizer : tokenize_ngrams 
    #> content : § 4. Every action shall be prosecuted in the name of the real party
    #> in interest, except as otherwise provided in this Act.
    #> 
    #> § 5. In the case of an assignment of a thing in action, the action by
    #> the as

Now we can compare each of the documents to one another. The
`pairwise_compare()` function applies a comparison function (in this
case, `jaccard_similarity()`) to every pair of documents. The result is
a matrix of scores. As we would expect, some documents are similar and
others are not.

    comparisons <- pairwise_compare(corpus, jaccard_similarity)
    comparisons
    #>                ca1851-match ca1851-nomatch ny1850-match
    #> ca1851-match             NA              0    0.3842549
    #> ca1851-nomatch           NA             NA    0.0000000
    #> ny1850-match             NA             NA           NA

We can convert that matrix to a data frame of pairs and scores if we
prefer.

    pairwise_candidates(comparisons)
    #> # A tibble: 3 × 3
    #>   a              b              score
    #> * <chr>          <chr>          <dbl>
    #> 1 ca1851-match   ca1851-nomatch 0    
    #> 2 ca1851-match   ny1850-match   0.384
    #> 3 ca1851-nomatch ny1850-match   0

See the [pairwise
vignette](https://cran.r-project.org/package=textreuse/vignettes/textreuse-pairwise.html)
for a fuller description.

    vignette("textreuse-pairwise", package = "textreuse")

### Minhashing and locality sensitive hashing

Pairwise comparisons can be very time-consuming because they grow
geometrically with the size of the corpus. (A corpus with 10 documents
would require at least 45 comparisons; a corpus with 100 documents would
require 4,950 comparisons; a corpus with 1,000 documents would require
499,500 comparisons.) That’s why this package implements the minhash and
locality sensitive hashing algorithms, which can detect candidate pairs
much faster than pairwise comparisons in corpora of any significant
size.

For this example we will load a small corpus of ten documents published
by the American Tract Society. We will also create a minhash function,
which represents an entire document (regardless of length) by a fixed
number of integer hashes. When we create the corpus, the documents will
each have a minhash signature.

    dir <- system.file("extdata/ats", package = "textreuse")
    minhash <- minhash_generator(200, seed = 235)
    ats <- TextReuseCorpus(dir = dir,
                           tokenizer = tokenize_ngrams, n = 5,
                           minhash_func = minhash)

Now we can calculate potential matches, extract the candidates, and
apply a comparison function to just those candidates.

    buckets <- lsh(ats, bands = 50, progress = FALSE)
    #> Warning: `gather_()` was deprecated in tidyr 1.2.0.
    #> ℹ Please use `gather()` instead.
    #> ℹ The deprecated feature was likely used in the textreuse package.
    #>   Please report the issue at <https://github.com/ropensci/textreuse/issues>.
    #> This warning is displayed once every 8 hours.
    #> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    #> generated.
    candidates <- lsh_candidates(buckets)
    scores <- lsh_compare(candidates, ats, jaccard_similarity, progress = FALSE)
    scores
    #> # A tibble: 1 × 3
    #>   a              b                      score
    #>   <chr>          <chr>                  <dbl>
    #> 1 remember00palm remembermeorholy00palm 0.701

For details, see the [minhash
vignette](https://cran.r-project.org/package=textreuse/vignettes/textreuse-minhash.html).

    vignette("textreuse-minhash", package = "textreuse")

### Text alignment

We can also extract the optimal alignment between to documents with a
version of the
[Smith-Waterman](https://en.wikipedia.org/wiki/Smith-Waterman_algorithm)
algorithm, used for protein sequence alignment, adapted for natural
language. The longest matching substring according to scoring values
will be extracted, and variations in the alignment will be marked.

    a <- "'How do I know', she asked, 'if this is a good match?'"
    b <- "'This is a match', he replied."
    align_local(a, b)
    #> TextReuse alignment
    #> Alignment score: 7 
    #> Document A:
    #> this is a good match
    #> 
    #> Document B:
    #> This is a #### match

For details, see the [text alignment
vignette](https://cran.r-project.org/package=textreuse/vignettes/textreuse-alignment.html).

    vignette("textreuse-alignment", package = "textreuse")

### Parallel processing

Loading the corpus and creating tokens benefit from using multiple
cores, if available. (This works only on non-Windows machines.) To use
multiple cores, set `options("mc.cores" = 4L)`, where the number is how
many cores you wish to use.

### Contributing and acknowledgments

Please note that this project is released with a [Contributor Code of
Conduct](https://github.com/ropensci/textreuse/blob/master/CONDUCT.md).
By participating in this project you agree to abide by its terms.

Thanks to [Noam Ross](http://www.noamross.net/) for his thorough [peer
review](https://github.com/ropensci/onboarding/issues/20) of this
package for [rOpenSci](https://ropensci.org/).

------------------------------------------------------------------------

[![rOpenSCi
logo](http://ropensci.org/public_images/github_footer.png)](http://ropensci.org)
