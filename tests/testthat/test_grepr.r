# These are tests of 'grepRet'

context("All tests of 'grepr'")

test_that("Basic functionality", {
   x <- c("Henrik", "Kenneth", "Conny", "Johnny", "Herakleitos")
   expect_equivalent(grepr("h", x), c("Kenneth", "Johnny"))
   expect_equivalent(grepr("h|H", x), x[-3])
})
