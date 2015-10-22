context("Alignment")

test_that("returns correct results with edits properly marked", {
  a <- "How can we tell if this is a match, he asked?"
  b <- "Surely, this is a good match, she replied."
  res <- align_local(a, b)
  expect_equal(res$a_edits, "this is a #### match")
  expect_equal(res$b_edits, "this is a good match")
  expect_is(res, "list")
  expect_is(res, "textreuse_alignment")
})

test_that("works with TextReuseTextDocuments", {
  file_a <- system.file("extdata/legal/ca1851-match.txt",
                         package = "textreuse")
  file_b <- system.file("extdata/legal/ny1850-match.txt",
                        package = "textreuse")
  doc_a <- TextReuseTextDocument(file = file_a)
  doc_b <- TextReuseTextDocument(file = file_b)
  doc_res <- align_local(doc_a, doc_b)
  expect_is(doc_res, "textreuse_alignment")
  expect_gt(wordcount(doc_res$a_edits), 200)
})
