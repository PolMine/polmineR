library(polmineR)
library(data.table)

testthat::context("aggregate")

test_that(
  "aggregate partition and regions",
  {
    
    # the aggregate-method is defined for the slice virtual class which comprises
    # the partition and the regions class. Just to be sure, a test is performed
    # for both cases
    
    # aggregate a partition
    
    P <- new(
      "partition",
      cpos = matrix(data = c(1:10, 20:29), ncol = 2, byrow = TRUE),
      stat = data.table()
    )
    P2 <- aggregate(P)
    P2@cpos
    
    expect_equal(nrow(P2@cpos), 2)
    expect_equal(P2@cpos[,2] - P2@cpos[,1], c(9, 9))
    
    # aggregate a regions object
    
    P <- new(
      Class = "regions",
      cpos = matrix(data = c(1:10, 20:29), ncol = 2, byrow = TRUE)
    )
    P2 <- aggregate(P)
    P2@cpos
    
    expect_equal(nrow(P2@cpos), 2)
    expect_equal(P2@cpos[,2] - P2@cpos[,1], c(9, 9))
    
  }
)


test_that(
  "disaggregate region matrix / as_cpos_vector",
  {
    y <- cpos(matrix(c(1,5, 6,10), ncol = 2, byrow = TRUE))
    expect_equal(y, 1:10)
    y <- cpos(matrix(c(1,6, 7,10), ncol = 2, byrow = TRUE))
    expect_equal(y, 1:10)
  }
)