
if (Sys.getenv("NOT_CRAN") != "false") {
  data_path <- system.file("extdata/ttm_bho.rds", package = "accessibility")
  ttm <- readRDS(data_path)
}

