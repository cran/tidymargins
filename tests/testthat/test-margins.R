#! This file was automatically produced by the testextra package.
#! Changes will be overwritten.

context('tests extracted from file `margins.R`')
#line 78 "R/margins.R"
test_that('with_margins', {#@testing
    x <- c( 'a', 'b', 'c')
    y <- c( 'd', 'e', 'f')
    data <- expand.grid( x = x
                       , y = y
                       , .rep = 1:10
                       , stringsAsFactors = FALSE) %>%
            mutate( v = rnorm(90)) %>%
            select(-.rep)


    ms <- with_margins(summarise)
    expect_is(ms, "function")
    val <- ms(group_by(data, x, y), N=n(), sum=sum(v))

    expect_equal( val[1:3]
                , data.frame( x = c(rep(x, each =length(y)), x, rep("(All)", length(y)+1))
                            , y = c(rep(y, length(x)), rep("(All)", length(x)), y, "(All)")
                            , N = c( rep(10L, length(x) * length(y))
                                   , rep(30L, length(x) + length(y))
                                   , 90L
                                   )
                            , stringsAsFactors = FALSE) %>% as_tibble
                )
})
#line 103 "R/margins.R"
test_that('with_margins with factors', {#@testing with_margins with factors
    x <- c( 'a', 'b', 'c')
    y <- c( 'd', 'e', 'f')
    data <- expand.grid( x = x
                       , y = y
                       , .rep = 1:10
                       , stringsAsFactors = TRUE) %>%
            mutate( v = rnorm(90)) %>%
            select(-.rep)

    ms <- with_margins(summarise)
    expect_is(ms, "function")
    val <- ms(group_by(data, x, y), N=n(), sum=sum(v))

    expect_equal( val[1:3]
                , data.frame( x = factor( c(rep(x, each =length(y)), x, rep("(All)", length(y)+1))
                                        , levels = c(x, '(All)'))
                            , y = factor( c(rep(y, length(x)), rep("(All)", length(x)), y, "(All)")
                                        , levels = c(y, '(All)'))
                            , N = c( rep(10L, length(x) * length(y))
                                   , rep(30L, length(x) + length(y))
                                   , 90L
                                   )
                            ) %>% as_tibble
                )
})
#line 129 "R/margins.R"
test_that('with_margins for non-character/factor variables.', {#@testing with_margins for non-character/factor variables.
    x <- c( 'a', 'b', 'c')
    data <- expand.grid( x = x
                       , y = 1:3
                       , .rep = 1:10
                       , stringsAsFactors = TRUE)
    val <- with_margins(count)(group_by(data, x, y))

    expect_is(val, 'tbl')
    expect_is(pull(val, 'y'), 'list')
    expect_equal(pull(val, 'y'), list(1,2,3,1,2,3,1,2,3
                                     ,'(All)','(All)','(All)'
                                     , 1,2,3,'(All)'))

})
