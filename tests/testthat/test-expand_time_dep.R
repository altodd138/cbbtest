test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("time_dep() sorts correctly", {
  dat <- time_dep(
    c(2, 1, 3, -Inf),
    c(1, 2, 3, 4)
  )
  time <- dat$time
  value <- dat$value
  expect_identical(time, c(-Inf, 1, 2, 3))
  expect_identical(value, c(4, 2, 1, 3))
})
