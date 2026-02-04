test_that("output works", {
  expect_gt(myhdi(funame = qbeta , shape1 = 5, shape2 = 7)$L,0)
  expect_gt(myhdi(funame = qbeta , shape1 = 5, shape2 = 7)$U,0)
})
