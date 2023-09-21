Sys.setenv(OMP_THREAD_LIMIT = 2)

library(testthat)
library(accessibility)

test_check("accessibility")
