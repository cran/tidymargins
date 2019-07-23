#! This file was automatically produced by the testextra package.
#! Changes will be overwritten.

context('tests extracted from file `spread_each.R`')
#line 91 "C:/Users/u0092104/Box Sync/Projects/Personal/tidymargins/R/spread_each.R"
test_that('spread_each', {#@testing
    data <- expand.grid( x = c( 'a', 'b', 'c')
                        , y = c( 'd', 'e', 'f')
                        , .rep = 1:10
                        ) %>%
             mutate( v = rep(c(-1, 0, 1), length.out=90)) %>%
             select(-.rep)
    long <- data %>%
        group_by(x, y) %>%
        summarise(N=n(), sum=sum(v))
    val <- spread_each(long, y, N, sum)

    expect_equal(dim(val), c(3L,7L))
    expect_equal(names(val), c('x', 'd.N', 'd.sum', 'e.N', 'e.sum'
                              , 'f.N', 'f.sum'))

    val2 <- spread_each(long, y, N, sum, key.first=FALSE)
    expect_equal(dim(val2), c(3L,7L))
    expect_equal(names(val2), c('x'
                               , paste0('N', '.', c( 'd', 'e', 'f'))
                               , paste0('sum', '.', c( 'd', 'e', 'f'))
                               ))
})
#line 114 "C:/Users/u0092104/Box Sync/Projects/Personal/tidymargins/R/spread_each.R"
test_that('spread_each(fill=...)', {#@testing spread_each(fill=...)
    data <- expand.grid( x = c( 'a', 'b', 'c')
                       , y = c( 'd', 'e', 'f')
                       , .rep = 1:10
                       ) %>%
             mutate( v = rep(c(-1, 0, 1), length.out=90)) %>%
             select(-.rep)
    long <- data %>%
        group_by(x, y) %>%
        summarise(N=n(), sum=sum(v)) %>%
        filter(!(x=='b' & y=='e'))

    val <- spread_each(long, y, N, sum, fill=list(N='#N/A', sum='???'))
    expect_is(val, 'tbl')
    expect_equal( val[2,c('e.N', 'e.sum')]
                , tibble(e.N = '#N/A', e.sum = '???')
                )

    expect_error(spread_each(long, y, N, sum, fill=list(N='#N/A')))
    expect_error(spread_each(long, y, N, sum, fill=list('#N/A', '???', x='.')))

    val2 <- spread_each(long, y, N, sum, fill=list(N='#N/A', '???'))
    expect_is(val2, 'tbl')
    expect_equal( val2[2,c('e.N', 'e.sum')]
                , tibble(e.N = '#N/A', e.sum = '???')
                )
})
#line 151 "C:/Users/u0092104/Box Sync/Projects/Personal/tidymargins/R/spread_each.R"
test_that('levels2', {#@testing
    x <- ordered(c('b', 'a', 'c'), levels=c('c', 'b', 'a'))
    expect_equal(levels2(x), levels(x))

    x <- c('c', 'b', 'a')
    expect_equal(levels2(x), c('a', 'b', 'c'))

    x <- 1:3
    expect_equal(levels2(x), c('1','2','3'))
})
