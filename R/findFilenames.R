#' Find filenames in an index
#' 
#' Searches an index and returns the column that represents the names of each file.
#' @param dl The \code{docList} representing the collection
#' @param directory A string providing the filepath to the collection
#' 
#' @section What it does:
#' Users should rarely, if ever, need to use \code{findFilenames} directly. The \code{findFilenames} 
#' operation is a key component of the \code{buildDocList} function. Given an index in the form of
#' a .csv file, \code{findFilenames} searches through the index to find which column has a unique
#' identifier that matches the naming scheme of the files.
#' @examples
#' filenames = findFilenames(dl = dl, directory = "~/path/to/your/corpus/files")
findFilenames = function(dl,directory) {
  index = dl@index
  filenames = c()
  #locmat = matrix(NA,nrow(index),ncol(index))
  #locations = c()
  colNum = detectColumn(index, directory)
  if(class(colNum) == "character") {
    print(colNum)
  } else {
    print(paste("Using Column Number: ", colNum, " first test val: ", index[1, colNum], sep=""))
  }
  for (i in 1:nrow(index)) {
    print(paste("Checking Row Number: ", i, sep = ""))
    # Commenting this out for back-up purposes.
    #for (j in 1:ncol(index)) {
    # #print(paste("Checking Column Number: ", j, sep = ""))
    # loc = grep(index[i,j],dir(directory),value=T,fixed = T)
    # if (length(loc) == 1) {
    #    locmat[i,j] = loc
    #  }
    loc = grep(index[i,colNum],dir(directory),value=T,fixed = T)
    if (length(loc) == 1) {
      #browser()
      filenames[i] = loc
    }
  }
  errors = c()
  for(i in 1:length(filenames)) {
    if (all(is.na(filenames[i]) == T)) {
      # Will fail if 'filenames' is not complete.
      #browser()
      errors = c(errors, index[i,colNum])
    }
  }
  if(length(errors) > 0) {
    print("We seem to be missing the following files: ")
    for(i in 1:length(errors)) {
      print(errors[i])
    }
    download = readline("Would you like to download them? [yes/no] > ")
    if(download == "yes" || download == "Yes") {
      getFiles(dl=dl, ids = errors)
      convert = readline("Would you like to convert from XML to txt? [yes/no] > ")
      if(convert == "yes" || convert == "Yes") {
        for(i in 1:length(errors)) {
          xmlToText(paste(dl@directory, errors[i], ".xml", sep=""), dl@directory)
        }
      }
    }
  }
  return(filenames)
}

detectColumn <- function(index, directory) {
  colNum = 0
  for (i in 1:10) {
    print(paste("Checking Row Number: ", i, " of 10", sep=""))
    for (j in 1:ncol(index)) {
      if(!is.na(index[i,j])){
        loc = grep(index[i,j], dir(directory), value=T, fixed=T)
      }
      
      if(length(loc == 1) && !is.na(loc)) {
        #print(paste("LOCATION ", loc, " J: ", j, sep=""))
        #if(colNum != j && colNum > 0) {
        #  print(paste("Conflicting columns detected: ", colNum, " vs. ", j, sep=""))
        #} else {
        #  colNum = j
        #}
        fName = file_path_sans_ext(loc)
        #print(paste(" FNAME:  ", fName, sep=""))
        if(!is.na(index[i,j]) && index[i,j] == fName) {
          colNum = j
        }
      }
    }
  }
  if(colNum == 0) {
    colNum = as.character(colNum)
    colNum = "It looks like we have not found an appropriate column in your index file that
              matches up with your file structure.  Please ensure that your index file contains
              a column that matches your file naming schema."
  }
  return(colNum)
}
