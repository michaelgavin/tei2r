#' Build a topic model
#' 
#' Run \code{mallet} over a \code{tei2r} collection. 
#' 
#' @section Description:
#' Trains a basic topic model using the 'mallet' package and saves the output
#' into a single \code{docModel} object. For easy viewing, the resulting object 
#' contains a data frame of each topic's top words and each document's most
#' prevalent topics. 
#'
#' @param dl              A \code{docList} object, representing your collection (must include
#'                        a stopwords file and must have imported texts).
#' @param tnum            The number of topics you want \code{mallet} to search for.
#' @param train           The number of times \code{mallet} will iterate over your
#'                        collection.
#' @param maximize        A few extra iterations meant to pick the best topic for each word.
#' @param listLength      The number of words from each topic you'd like to see displayed.
#'
#' @return dmod           The completed \code{docModel} object for viewing and simple analysis.
#' 
#' @examples
#' dmod = buildModel(dl = dl, tnum = 50)
#' View(dmod@@topics) # To view a table showing the words of each topic
#' View(dmod@@frequencies) # To view the frequency of the topics in each document
#' @export
buildModel = function(dl, tnum, train = 100, maximize = 10, listLength = 100 ) {
  if (!requireNamespace("mallet", quietly = T)) {
    stop("You need to install MALLET for this function to work. To install MALLET,
         use install.packages('mallet') and then library(mallet).", call. = F)
  }
  model = docModel()
  model@directory = dl@directory
  model@index = dl@index
  files = names(dl@texts)
  texts = c()
  for (i in 1:length(dl@texts)) {
    texts[i] = paste(dl@texts[[i]], collapse = " ")
  }
  write.table(dl@stopwords, file = "tempStops.txt", append = F, quote = F, sep = "\n",row.names = F, col.names = F)
  mallet.instances = mallet.import(files,
                                   texts,
                                   "tempStops.txt",
                                   preserve.case = FALSE,
                                   token.regexp="[\\p{L}']+")
  file.remove("tempStops.txt")
  tmod = MalletLDA(num.topics = tnum)
  tmod$loadDocuments(mallet.instances)
  tmod$train(100)
  tmod$maximize(10)
  model@malletObj = tmod
  
  vocabulary = tmod$getVocabulary()
  twords = mallet.topic.words(tmod, normalized = T, smoothed = F)
  colnames(twords) = vocabulary
  model@termMatrix = twords
  
  tdocs = mallet.doc.topics(topic.model = tmod, normalized = T, smoothed = F)
  colnames(tdocs) = 1:ncol(tdocs)
  tdocs = round(tdocs, digits = 2)
  model@frequencies = tdocs
  
  tlist = list()
  for (i in 1:nrow(twords)) {
    t = mallet.top.words(tmod, twords[i,], listLength)
    tlist[[i]] = t 
  }
  model@weights = tlist
  
  tlist.df = data.frame(1:nrow(tlist[[1]])) # Creates single column matrix
  for (i in 1:length(tlist)) {
    topic = tlist[[i]]$words # Pulls words (not frequency) from each topic
    tlist.df = cbind(tlist.df,topic) # Binds each column to existing tdf
  }
  topnames = paste("Topic",1:length(tlist), sep=" ") # Makes column names
  names(tlist.df)[2:ncol(tlist.df)] = topnames # Adds them
  tlist.df = tlist.df[,-1] # Removes placeholder column 
  model@topics = tlist.df
  
  return(model)
}

