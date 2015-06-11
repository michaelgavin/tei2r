#====================================================================
#
#
#
#
#====================================================================
#'
#' Placeholder description
#' 
#' @param dl
#' @param directory
#' 
#' @examples
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
      filenames[i] = loc
    }
  }
  errors = c()
  for(i in 1:length(filenames)) {
    if (all(is.na(filenames[i]) == T)) {
      # Will fail if 'filenames' is not complete.
      errors = c(errors, index[i][colNum])
    }
  }
  if(length(errors) > 0) {
    print("We seem to be missing the following files: ")
    for(i in 1:length(errors)) {
      print(errors[i])
    }
  }
  return(filenames)
}

detectColumn <- function(index, directory) {
  colNum = 0
  for (i in 1:10) {
    print(paste("Checking Row Number: ", i, " of 10", sep=""))
    for (j in 1:ncol(index)) {
      loc = grep(index[i,j], dir(directory), value=T, fixed=T)
      
      if(length(loc == 1)) {
        #print(paste("LOCATION ", loc, " J: ", j, sep=""))
        #if(colNum != j && colNum > 0) {
        #  print(paste("Conflicting columns detected: ", colNum, " vs. ", j, sep=""))
        #} else {
        #  colNum = j
        #}
        fName = file_path_sans_ext(loc)
        #print(paste(" FNAME:  ", fName, sep=""))
        if(index[i,j] == fName) {
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