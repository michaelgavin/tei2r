#===========================================
# Text cleanup function. Enter file 
# path. Output is a vector of words, all
# lower case, with punctuation and stopwords 
# removed.
#
# This function is adapted from commands
# included in Jockers, "Text Analysis with R"
#===========================================

#' Convert a plain-text document into a character vector
#' 
#' @param filepath A path to the file that will be converted.
#' @param removeCaps A logical condition. If "TRUE", all words will be converted to lower case.
#' @param removeStopwords A logical condition. If "TRUE", all words contained in a pre-defined vector of stopwords will be excluded.
#' 
#' @examples
#' locke.path = "~/Desktop/locke2ndTreatise.txt"
#' textCleanup(locke.path, removeCaps = TRUE, removeStopwords = FALSE)
#' @name cleanup
NULL
#' @rdname cleanup
textCleanup = function(filepath, removeCaps = TRUE, stopwords = dl@stopwords, removeStopwords = TRUE) {
  # Need to create a test that checks if stopwords are defined.
  #if(!is.na(stopwords) || length(stopwords) < 1) {
  #  stopwords = setStopwords("defaultPath")
  #}
  if (length(grep(".txt",filepath)) == 1) {
    text = scan(filepath,what="character",sep="\n")
    text = paste(text, collapse= " ")
    if (removeCaps == TRUE) text = tolower(text)

  } else if (length(grep(".xml",filepath)) == 1) {
    parsedText = xmlTreeParse(filepath,useInternalNodes = TRUE)
    nodes = getNodeSet(parsedText,"/d:TEI//d:text", 
                       namespace = c(d = "http://www.tei-c.org/ns/1.0"))
    text = sapply(nodes,xmlValue)
  }
  text = strsplit(text,"\\W")
  text = unlist(text)
  text = text[text!=""]
  if (removeStopwords == TRUE) text = text[text %in% stopwords ==FALSE ]
  return(text)
}

#' @rdname cleanup
teiTextCleanup = function(filepath, stopwords) {
  parsedText = xmlTreeParse(filepath,useInternalNodes = TRUE)
  nodes = getNodeSet(parsedText,"/d:TEI//d:text", 
                     namespace = c(d = "http://www.tei-c.org/ns/1.0"))
  text = sapply(nodes,xmlValue)
  text = unlist(text)
  text = paste(text, collapse= " ")
  text = tolower(text)
  text = strsplit(text,"\\W")
  text = unlist(text)
  text = text[text!=""]
  text = text[text %in% stopwords ==FALSE ]
  return(text)  
}


