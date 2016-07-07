#' Search the TCP index
#' 
#' Performs basic keyword searching in the Text Creation Partnership index.
#' 
#' @param term A character string (i.e., the search term).
#' 
#' @param range A numeric vector representing the desired date range, usually 
#'             provided in the form 1600:1610. For a single year, just enter
#'             the year number.
#' 
#' @param field A character string (i.e., the search field). Must be identical
#'             to the name of a column in the TCP index. Valid field names include
#'             "TCP","EEBO","VID","STC","Status","Author","Date","Title",
#'             "Terms",and "Pages" (case-sensitive).
#'             
#' @param free  A logical value. If TRUE, will limit results to freely available texts.
#'             To see "Restricted" items, set this argument to FALSE.
#'             
#' @param write A logical value. If TRUE, will write the results to a .csv file, called
#'             "index.csv" in your working directory. BE CAREFUL WHEN USING, because
#'             it will over-write any existing "index.csv" file.
#'             
#' @section How to use it:
#' The most common situation for using \code{tcpSearch} is when you
#' want to find and download content from Text Creation Partnership. In this case, follow
#' these steps.
#' \enumerate{
#'   \item Create a folder where you'll store your XML files.
#'   \item Play with \code{tcpSearch} until you find results that look useful to you.
#'   \item Enter the command: \code{results = tcpSearch(YOUR SEARCH, write = T)}. This
#'         generates a 'results' object and writes it to .csv as "index.csv."
#'   \item Enter the command: \code{tcpDownload(results)}. This will download the selected
#'         files.
#' }
#' 
#' @examples
#'tcpSearch(term = "Robinson Crusoe", field = "Title")
#'tcpSearch(term = "Defoe", field = "Author") # search by author
#'tcpSearch(term = "Travel", field = "Terms") # search by subject terms
#'tcpSearch(range = 1700:1735, field = "Date") # search by date
#'results = tcpSearch(term = "Defoe", field = "Author") # creates a 'results' data frame in R
#'tcpSearch(term = "Robinson Crusoe", field = "Title", write = TRUE) # writes an index to .csv
#' 
#' @export
tcpSearch = function(term = NULL, range = NULL, field, free = TRUE, write = FALSE) {
  data(tcp)
  if (field %in% colnames(tcp) == FALSE) {
    stop("The 'field' you entered isn't among the TCP fields. Usually this is just
         a typo. Make sure that you've capitalized the field correctly. Valid field
         names include 'TCP','EEBO','VID','STC','Status','Author','Date','Title',
         'Terms',and 'Pages'.")
  }
  if (field == "Date") {
    hits = which(tcp$Date %in% range)
    results = tcp[hits,]
  }
  if (field != "Date") {
    col = which(colnames(tcp) == field)
    results = tcp[grep(term, tcp[,col]),]
  }
  if (free == TRUE) {
    results = results[which(results$Status == "Free"),]
  }
  if (write == T) {
    write.csv(results, file = "index.csv", row.names = F)
  }
  View(results)
  results$Date = as.integer(as.character(results$Date))
  return(results)
}
