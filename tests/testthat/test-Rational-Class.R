test_that("Rational Class", {
  rat <- new("Rational",
             numerator = as.integer(1),
             denominator = as.integer(2))
  expect_true(is(rat, "Rational"))
})

test_that("Exponential", {
  rat <- R(5, 9)
  rat2 <- rat^2 ## `^`(rat, 2)
  expect_true(is(rat2, "Rational"))
  expect_true(rat2@numerator == 5^2)
  expect_true(rat2@denominator == 9^2)
})
