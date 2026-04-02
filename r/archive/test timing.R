library(readr)
library(data.table)
library(arrow)
library(fst)
library(bench)

#read in the data
euci = read_rds('./euclidean_distance_matrix/euci_2km.rds')
saveRDS(euci,'./euclidean_distance_matrix/euci_2km.rds',compress = F)

#reduce the data set to make the tests run faster
reduced = euci[,c(1:5)]

#test saving scripts
test_saveRDS <- function(reduced) {
  saveRDS(reduced, "test.RDS")
  return(TRUE)
}
test_RDSunc <- function(reduced) {
  saveRDS(reduced, "test_unc.RDS",compress = F)
  return(TRUE)
}
test_fwrite_uncomp <- function(reduced) {
  data.table::fwrite(reduced, "test_dt.csv")
  return(TRUE)
}
test_write_parquet <- function(reduced) {
  arrow::write_parquet(reduced, "test.parquet")
  return(TRUE)
}
test_write_fst <- function(reduced) {
  fst::write_fst(reduced, "test.fst",compress = 50)
  return(TRUE)
}

#bench mark the dataset
b = bench::mark(
  test_saveRDS(reduced),
  test_RDSunc(reduced),
  test_fwrite_uncomp(reduced),
  test_write_parquet(reduced),
  test_write_fst(reduced),
)

b

#save RDS uncompressed is the best option for speed/size followed by fst

