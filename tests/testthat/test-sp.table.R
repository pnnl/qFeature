context("Behavior of sp-table()")

test_that("Numerical values are as expected", {

  x <- sample(c(rep(1,5), rep(2,3)))

  expect_equal(sp.table(x, 1:2), c(5, 3))
  expect_equal(sp.table(x, 0:3), c(0, 5, 3, 0))
  expect_equal(sp.table(x, 1:3, pct = TRUE), c(0.625, 0.375, 0))

})

