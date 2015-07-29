#' Download TCP files from Github
#' 
#' Download raw XML files from the Text Creation Partnership's Github repository.
#' 
#' @slot results A dataframe, usually the search results generated using 
#'               \code{searchTCP}. Must contain a column of TCP numbers with the 
#'               name 'TCP'.
#'               
#' @section To download the entire TCP corpus:
#' If you'd like to download the entire TCP corpus, run two commands: 
#' \enumerate{
#'     \item \code{data(tcp)} (Activates the TCP data in your R environment.)
#'     \item \code{tcpDownload(tcp)} (Begins the download.)
#' }
#' 
#' Be careful: the entire TCP is several GBs, so the download will take awhile.
#' 
#' @examples
#' tcpDownload(results)
#'
#' @export
tcpDownload = function(results) {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("tei2r uses a package called 'httr' to download XML content from the web. Please install it
         using the command > install.packages('httr'), then re-try your download.")
  }
  selection = index$TCP
  urls = paste("https://raw.githubusercontent.com/textcreationpartnership/",
               selection,
               "/master/",
               selection,
               ".xml",
               sep="")
  files = paste(selection,".xml",sep="")
  for (i in 1:length(urls)) {
    data.r = httr::GET(url = urls[i])
    data.v = httr::content(data.r)
    filename = paste(dl@directory, selection[i],".xml",sep="")
    print(paste("Downloading file: ", selection[i], '.xml', sep=""))
    write.table(data.v,filename,quote = F,row.names = F, col.names = F)
  }
  print("File download complete.")
}
