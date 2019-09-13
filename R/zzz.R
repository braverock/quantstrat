.onUnload <- function(libpath) {
  library.dynam.unload("quantstrat", libpath)
}

.onLoad <- function(lib, pkg) {
  if(!exists('.strategy'))
    .strategy <<- new.env()
}