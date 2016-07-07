#' Convert a plain-text or TEI document into a character vector
#' 
#' @param filepath A path to the file that will be converted.
#' 
#' @param normalize A logical condition. If "TRUE", text will be converterd to 
#'                  all lower case and stopwords will be removed. Also, all 
#'                  instances of '∫' and 'ſ' will be convertedto 's', all 
#'                  numeric characters will be removed, 'vv' will be converted to 'w', 
#'                  and ''d' and ''ring' will be converted to 'ed' and 'ering'
#'                  respectively, and all special characters will be removed.
#' 
#' @examples
#' locke.path = "~/Desktop/locke2ndTreatise.txt"
#' cleanup(locke.path, removeCaps = TRUE, removeStopwords = FALSE)
#' @export
cleanup = function(filepath, stopwords = c(), normalize = TRUE) {
  if (length(grep(".txt",filepath)) == 1) {
    text = scan(filepath,what="character",sep="\n", fileEncoding = "UTF-8")
    text = paste(text, collapse= " ")
    
  } else if (length(grep(".xml",filepath)) == 1) {
    #parsedText = xmlTreeParse(filepath,useInternalNodes = TRUE)
    #nodes = getNodeSet(parsedText,"/d:TEI//d:text", namespace = c(d = "http://www.tei-c.org/ns/1.0"))
    parsedText = htmlTreeParse(filepath,useInternalNodes = TRUE)
    nodes = getNodeSet(parsedText,"//text")
    text = lapply(nodes,xmlValue)
  }
  
  text = gsub("non-Latin alphabet", " ", text)
  text = gsub("1 page duplicate", " ", text)
  
  if (normalize == TRUE) {
    text = gsub("ſ", "s", text)
    text = gsub("[0-9]", "", text)
    text = gsub("vv", "w", text)
    text = gsub("'d ", "ed ", text)
    text = gsub("'ring ", "ering ", text)
  }
  
  text = strsplit(text,"\\W")
  text = unlist(text)
  text = text[text!=""]
  
  if (normalize == TRUE) {
    text = tolower(text)
    text = text[text %in% stopwords == FALSE]
    if (any(grep("[^\x20-\x7E]",text))) text = text[-grep("[^\x20-\x7E]",text)]
  }
  return(text)
}
