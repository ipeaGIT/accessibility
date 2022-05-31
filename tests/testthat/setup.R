
if (Sys.getenv("NOT_CRAN") != "false") {
  data_path <- system.file("extdata/ttm_poa.csv", package = "accessibility")
  ttm <- read.csv(data_path)
}

