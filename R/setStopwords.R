#====================================================================
# Set stopwords. The argument is a filepath to a comma-separated list
# of stopwords. The output is a character vector of terms, which is 
# saved into the main environment.
#====================================================================
#
#
#' Create or change a vector of stopwords
#' 
#' @param filepath A path to the file that will be converted.
#' @param delimiter The kind of character used to delimit your file 
#'   of stopwords. The default is "," for comma-separated files. Special 
#'   characters are included here as regular expessions, such as "\\n" for
#'   new lines (every word is on its own line) and "\\t" for tabs.
#' @examples
#' stopwords = setStopwords("~/Desktop/stopwords.txt",delimiter=",")

setStopwords = function(filepath,delimiter=",") {
  stopwords = scan(filepath,what="character",sep=delimiter)
  return(stopwords)
}

