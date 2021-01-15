test_that("'fix.single.cdate' works", {
    expect_equal(
        fix.single.cdate(x = "20110213"),
        as.Date("2011-02-13")
    )
    expect_equal(
        fix.single.cdate(x = "20011231"),
        as.Date("2001-12-31")
    )
    expect_equal(
        fix.single.cdate(x = "20120000"),
        as.Date("2012-07-01")
    )
    expect_equal(
        fix.single.cdate(x = "20120600"),
        as.Date("2012-06-15")
    )
    expect_equal(
        fix.single.cdate(x = "20120000", low.bound = as.Date("2012-01-01")),
        as.Date("2012-07-01")
    )
    expect_equal(
        fix.single.cdate(x = "20120000", low.bound = as.Date("2012-12-31")),
        as.Date("2012-12-31")
    )
    expect_equal(
        fix.single.cdate(x = "20120000", low.bound = as.Date("2012-10-03")),
        as.Date("2012-11-16")
    )
    expect_equal(
        fix.single.cdate(x = "20120500", low.bound = as.Date("2012-01-01")),
        as.Date("2012-05-16")
    )
    expect_equal(
        fix.single.cdate(x = "20120500", low.bound = as.Date("2012-05-31")),
        as.Date("2012-05-31")
    )
    expect_equal(
        fix.single.cdate(x = "20120500", low.bound = as.Date("2010-05-31")),
        as.Date("2012-05-16")
    )
    expect_equal(
        fix.single.cdate(x = "20120500", low.bound = as.Date("2010-05-12")),
        fix.single.cdate(x = "20120500")
    )
    expect_equal(
        fix.single.cdate(x = "20120500", low.bound = as.Date("2012-04-12")),
        fix.single.cdate(x = "20120500")
    )
    expect_equal(
        fix.single.cdate(x = "19890000"),
        fix.single.cdate(x = "19890000", low.bound = as.Date("1989-01-01"))
    )
})

test_that("'cdate' works", {
    x <- c("20090000" , "20100800", "20120506")
    y <- as.Date(c("2009-07-02" , "2010-08-16", "2012-05-06"))
    ## cdate(x)
    ## cdate(x[3])
    expect_equal(cdate(x, verbose = FALSE),y)
    L <- as.Date(c("2009-07-01" , "2010-08-10", "2012-05-06"))
    y <- as.Date(c("2009-10-01" , "2010-08-20", "2012-05-06"))
    ## cdate(x, low.bound = L)
    expect_equal(cdate(x, low.bound = L, verbose = FALSE), y)
    L <- as.Date(c("2010-07-01" , "2010-08-10", "2012-05-06"))
    expect_error(cdate(x, low.bound = L, verbose = FALSE))
    ## L <- as.Date(c("2009-01-01" , "2010-08-10", "2012-05-07"))
    ## expect_error(cdate(x, low.bound = L, verbose = FALSE))
    L <- as.Date(c("2009-07-01" , "2010-08-10", "2012-05-07"))
    expect_equal(cdate(x, low.bound = L, verbose = FALSE), y)
    expect_error(cdate(x, low.bound = L, bound4all = TRUE, verbose = FALSE))
})
