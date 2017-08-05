.onUnload <- function(libpath) {
  library.dynam.unload("quantstrat", libpath)
}
