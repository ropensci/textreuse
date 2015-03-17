# Hide variables from R CMD check
if(getRversion() >= "2.15.1") {
  utils::globalVariables(c("words_en"))
}
