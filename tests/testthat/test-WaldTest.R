testthat::test_that("Wald Works", {
  d <- 5
  n <- 10
  prob0 <-0.5
  bt <- stats::pnorm((d/n - prob0)/(
    ((d/n) * ((n - d)/n))^0.5 * n^(-0.5))
  )
  
  dt <- stats::pnorm((d/n - prob0)/(
    sqrt((1/n) *(d/n)* ((n - d)/n)))
  )
  testthat::expect_equal(bt, dt)
})
