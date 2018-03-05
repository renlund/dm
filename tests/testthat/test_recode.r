test_that("'recode works", code = {

x <- letters[1:2]
expect_equal(recode(x), factor(x, levels = letters[1:2]))
expect_equal(recode(x, asFactor = FALSE), letters[1:2])
L <- list('b' = NULL)
expect_equal(recode(x, L), factor(x, levels = letters[2:1]))
expect_equal(recode(x, L, newFirst = FALSE), factor(x, levels = letters[1:2]))
expect_equal(recode(x, L, asFactor = FALSE), letters[1:2])
expect_equal(recode(x, L, asFactor = FALSE, newFirst = FALSE), letters[1:2])

L <- list('B' = 'b')
l <- c('a', 'B')
expect_equal(recode(x, L), factor(l, levels = l[2:1]))
expect_equal(recode(x, L, newFirst = FALSE), factor(l, levels = l[1:2]))
expect_equal(recode(x, L, asFactor = FALSE), l[1:2])
expect_equal(recode(x, L, asFactor = FALSE, newFirst = FALSE), l[1:2])

L <- list('B' = 'b', 'newA' = 'a')
l <- c('newA', 'B')
expect_equal(recode(x, L), factor(l, levels = l[2:1]))
expect_equal(recode(x, L, newFirst = FALSE), factor(l, levels = l[2:1]))
expect_equal(recode(x, L, asFactor = FALSE), l[1:2])
expect_equal(recode(x, L, asFactor = FALSE, newFirst = FALSE), l[1:2])

x <- c('b', 'a', 'c', NA, '', 'd')
L <- list('B' = c('b', 'c'), 'a' = NULL, 'missing' = c('', NA))
l <- c(names(L)[c(1, 2, 1, 3, 3)], "d")
expect_equal(recode(x, L), factor(l, unique(l)))
expect_equal(recode(x, L, asFactor = FALSE), l)
expect_equal(recode(x, L, newFirst = FALSE), factor(l, unique(l)[c(4,1:3)]))

L <- list(letters, c(NA, ''))
names(L) <- c('letters', NA)
l <- factor(rep('letters', 6))
l[4:5] <- NA
expect_equal(recode(x, L), l)

L <- list(letters, c(''))
names(L) <- c('letters', NA)
expect_equal(recode(x, L), l)

x <- c('b', Inf, 'a', NaN, '', NA)
L <- list('B' = c('b', 'c'), 'a' = NULL, 'weird' = c(NaN, '', NA), "LARGE!" = c(Inf, -Inf))
expect_equal(recode(x, L),
             factor(c('B', 'LARGE!', 'a', 'weird', 'weird','weird'),
                    levels = c('B', 'a', 'weird', 'LARGE!')))
})
