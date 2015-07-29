#====================================================================
# This function accepts one or more tei nodes and will return all
# values that match those nodes within a given file.
#====================================================================
#' Extract part of a TEI document
#'
#' This function will return the values of given \code{nodes} in a
#' given \code{filepath}.
#' 
#' @param filepath      A string that holds the location of the file
#'                      you want to extract values from.
#'                   
#' @param node          One or more node types that you want to extract
#'                      values from.
#'                   
#' @return wantedNodes  The values for all nodes matching \code{node} in
#'                      the file found at \code{filepath}.
#'                      
#' @examples
#' nodes = parseTEI("path/to/xml/file", node="hi") # Get values for all highlight nodes
#' @export
parseTEI = function(filepath, node="") {
  parsedText = xmlTreeParse(filepath,useInternalNodes = TRUE)
  parsedText = xmlRoot(parsedText)
  if(length(node) > 1) {
    for(i in 1:length(node)){
      nodes = paste("//d:", node[i], sep='')
      wantedNodes = c(wantedNodes, getNodeSet(parsedText, nodes, namespace = c(d = "http://www.tei-c.org/ns/1.0")))
    }
  } else {
    nodes = paste("//d:", node, sep="")
    wantedNodes = getNodeSet(parsedText, nodes, namespace = c(d = "http://www.tei-c.org/ns/1.0"))
  }
  return(sapply(wantedNodes, xmlValue))
}
