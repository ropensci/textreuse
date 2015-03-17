# WordNet list of words
library(stringr)

tarball <- "data-raw/wn3.1.dict.tar.gz"

if (!file.exists(tarball)) {
  download.file("http://wordnetcode.princeton.edu/wn3.1.dict.tar.gz", tarball)
}

untar(tarball, exdir = "data-raw")

indexes <- c("data-raw/dict/index.noun",
             "data-raw/dict/index.adj",
             "data-raw/dict/index.adv",
             "data-raw/dict/index.verb")

words_en <- indexes %>%
  lapply(readLines) %>%
  lapply(function(x) {
    x[30:length(x)]
  }) %>%
  lapply(word) %>%
  lapply(str_replace_all, "_", " ") %>%
  lapply(str_to_lower) %>%
  unlist() %>%
  sort() %>%
  unique()

writeLines(words_en, "data-raw/words_en.wordnet.txt")
use_data(words_en, overwrite = TRUE, compress = "xz")
