#===========================================
# Text cleanup function. Enter file 
# path. Output is a vector of words, all
# lower case, with punctuation and stopwords 
# removed.
#
# This function is adapted from commands
# included in Jockers, "Text Analysis with R"
#===========================================

#' Convert a plain-text or TEI document into a character vector
#' 
#' @param filepath A path to the file that will be converted.
#' @param removeCaps A logical condition. If "TRUE", all words will be converted to lower case.
#' @param removeStopwords A logical condition. If "TRUE", all words contained in a pre-defined vector of stopwords will be excluded.
#' @param normalizeLongS A logical condition. If "TRUE", all instances of '∫' and 'ſ' will be converted to 's'.
#' 
#' @examples
#' locke.path = "~/Desktop/locke2ndTreatise.txt"
#' cleanup(locke.path, removeCaps = TRUE, removeStopwords = FALSE)
#' @name cleanup
cleanup = function(filepath, stopwords = c(), removeCaps = TRUE, removeStopwords = TRUE, normalizeLongS = TRUE) {
  if (length(grep(".txt",filepath)) == 1) {
    text = scan(filepath,what="character",sep="\n", fileEncoding = "UTF-8")
    text = paste(text, collapse= " ")
    
  } else if (length(grep(".xml",filepath)) == 1) {
    parsedText = xmlTreeParse(filepath,useInternalNodes = TRUE)
    nodes = getNodeSet(parsedText,"/d:TEI//d:text//text()", 
                       namespace = c(d = "http://www.tei-c.org/ns/1.0"))
    text = lapply(nodes,xmlValue)
    text = paste(text, collapse = ",")
    names(text) = NULL
  }
  text = gsub("non-Latin alphabet", " ", text)
  if (normalizeLongS == TRUE) {
    text= gsub("∫", "s", text)
    text = gsub('ſ', "s", text) 
  }
  text = strsplit(text,"\\W")
  text = unlist(text)
  text = text[text!=""]
  if (removeCaps == TRUE) text = tolower(text)
  if (removeStopwords == TRUE) text = text[tolower(text) %in% stopwords ==FALSE ]
  return(text)
}
