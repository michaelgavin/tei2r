#' Setup sample docLists on new machine
#' 
#' @param object The name of the sample \code{docList} to be set up. Possible
#'               values are "natlaw" and "richardson".
#' 
#' @return A new \code{docList} object named "dl".
#' 
#' @examples
#' data(natlaw)
#' setup(natlaw)
#' @export
setup = function(object) {
  name <- deparse(substitute(object))
  object@directory <- system.file("extdata", name, package = "tei2r")
  object@indexFile <- system.file("extdata", paste(name, "/", name, ".csv", sep=""), package = "tei2r")
  object@paths <- paste(object@directory, "/", object@filenames, sep="")
  object <- importTexts(object)
  dl <<- object
  show(object)
  rm(list = name, pos = ".GlobalEnv")
}
