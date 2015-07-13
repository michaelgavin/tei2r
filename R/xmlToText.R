#====================================================================
# This Function converts the xml file found at filepath to plain
# text. Returns a string of the text.
#====================================================================
#'
#' This function converts the xml file found at \code{filepath} to 
#' plain text and, optionally, stores it at \code{target}.
#' 
#' @param filepath  A string that holds the path to the xml file
#'                  to convert to text.
#' 
#' @param target    A string that holds the path at which to store
#'                  the converted text file.
#'                  
#' @return text     The plain text extracted from the xml file.
#' 
#' @examples
#' fp = "path/to/file/text.xml"
#' text = xmlToText(fp)
#' text = xmlToText(fp, target="/alternate/path/for/new/file/")
xmlToText = function(filepath,target=NULL) {
  parsedText = xmlTreeParse(filepath,useInternalNodes = TRUE)
  nodes = getNodeSet(parsedText,"/d:TEI//d:text//d:div", 
                     namespace = c(d = "http://www.tei-c.org/ns/1.0"))
  text = sapply(nodes,addSpace)
  filename = paste(file_path_sans_ext(filepath),".txt",sep="")
  write.csv(text,filename)
  cleanup(filename)
  text = read.csv(filename)
  return(text)
}

addSpace = function(input) {
  return(paste(xmlValue(input), " ", sep=""))
}
