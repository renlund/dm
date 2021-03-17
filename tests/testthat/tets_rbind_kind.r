test_that("'rbind_kind' works", {
    x <- data.frame(x = 1:2,
                    y = letters[1:2],
                    z = c(TRUE, FALSE))
    y <- data.frame(x = 3:4,
                    y = letters[3:4],
                    u = c("foo", "bar"))
    r1 <- data.frame(x = 1:4,
                     y = letters[1:4],
                     z = c(TRUE, FALSE, NA, NA),
                     u = c(NA, NA, "foo", "bar"))
    r2 <- data.frame(x = 1:4,
                     y = letters[1:4])
    expect_equal(rbind_kind(x, y, in.both = FALSE), r1)
    expect_equal(rbind_kind(x, y, in.both = TRUE), r2)
})
