

test_that("viewbox parsing works", {
  # this is a test for issue-03 in which the numbers in a viewbox statement
  # are separated by "," whereas I'd only ever seen whitespace separation in
  # the wild.
  # SVG spec says that comma or whitespace spearation are both valid

  res <- read_svg("svgs-for-issues/issue-03-minmal.svg")

  # Got here without an error!
  expect_true(TRUE)

})
