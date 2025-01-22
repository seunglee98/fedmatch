# Loading package info, set threads to 1 for CRAN submission acceptance

.onLoad <- function(libname, packagename) {
  # CRAN OMP THREAD LIMIT
  Sys.setenv("OMP_THREAD_LIMIT" = 1)
}
