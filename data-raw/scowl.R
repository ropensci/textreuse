library(devtools)
library(stringr)

# Download word list from http://app.aspell.net/create
# This is not the hugest list, but it reduces the size considerably.
list <- "data-raw/scowl-huge.txt"
if (!file.exists(list)) {
  download.file("http://app.aspell.net/create?max_size=80&spelling=US&spelling=GBs&spelling=GBz&spelling=CA&max_variant=3&diacritic=both&special=hacker&special=roman-numerals&download=wordlist&encoding=utf-8&format=inline",
                list)
}

words_en <- readLines(list)
words_en <- words_en[-(1:45)] %>%
  str_to_lower() %>%
  sort() %>%
  unique()

use_data(words_en, overwrite = TRUE, compress = "xz")
