useWizard <- function(dl) {
  print("Thank you for choosing the wizard option.  This wizard
        is designed to walk you through setting up your corpus and
        docList object.")
  directory <- readline("To get started, what is the directory that you
                       have your files stored in? [full path] > ")
  if(substr(directory, 1, 1) == '\"') {
    directory = substr(directory, 2, nchar(directory)-1)
  }
  #browser()
  if(!is.na(directory) && file.info(directory)$isdir == TRUE) {
    dl@directory = directory
  } else {
    while(file.info(directory)$isdir == FALSE) {
      directory <- readline("That doesn't appear to be a directory, please
                            try again. > ")
    }
  }
  indexAnswer <- readline("Do you have an index file? [yes/no] > ")
  
  if(indexAnswer == "yes") {
    indexFile <- readline("Please enter the directory that your index file is stored in. [full path] > ")
    if(substr(indexFile, 1, 1) == '\"') {
      indexFile = substr(indexFile, 2, nchar(directory)-1)
    }
  } else if(indexAnswer == "no") {
      indexFile <- readline("Ok, we'll create one for you.  Where would you like it stored? [full path] > ")
      #browser()
      if(substr(indexFile, 1, 1) == '\"') {
        indexFile = substr(indexFile, 2, nchar(indexFile)-1)
      }
      if(!is.na(indexFile) && file.info(indexFile)$isdir == TRUE) {
        if(substr(indexFile, nchar(indexFile)-1,nchar(indexFile)-1) != "/") {
          indexFile = paste(indexFile, "/", sep="")
        }
        dl@indexFile = buildIndex(indexFile)
        dl@index = read.csv(dl@indexFile)
      }
  }
  
  stopwordsAnswer <- readline("Do you have a stopwords file? [yes/no] > ")
  if(stopwordsAnswer == "yes") {
    stopwordsFile = readline("Please enter the directory where your stopwords file is stored. [full path] > ")
    if(substr(stopwordsFile, 1, 1) == '\"') {
      stopwordsFile = substr(stopwordsFile, 2, nchar(stopwordsFile)-1)
    }
    if(!is.na(stopwordsFile) && file.info(stopwordsFile)$isdir == TRUE) {
      if(substr(stopwordsFile, nchar(stopwordsFile)-1,nchar(stopwordsFile)-1) != "/") {
        stopwordsFile = paste(stopwordsFile, "/", sep="")
      }
      dl@stopwordsFile = stopwordsFile
      dl@stopwords = setStopwords(stopwordsFile)
    }
  } else if(stopwordsAnswer == "no") {
    print("Ok, we'll supply one for you.")
    #need to look into integrating this data.
  }
  return(dl)
}
