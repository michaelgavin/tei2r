print.docList = function(dl) {
  print("Document list summary:")
  cat("Source: @directory",dl@directory,"\n")
  cat("..@filenames", dl@filenames[1],"... \n")
  cat("..@paths", dl@paths[1], "... \n")
  cat("..@indexFile", dl@indexFile,"\n")
  cat("..@index, 'data.frame':", dim(dl@index)[1], "obs. of", dim(dl@index)[2], "variables:","\n")
  cat(".... columns:", colnames(dl@index), "\n")
  cat("..@stopwordsFile:", dl@stopwordsFile, "\n")
  cat(".. @stopwords:", (dl@stopwords)[1:10], "...")
}

print.docTexts = function(dt) {
  print("Document texts summary:")
  cat("Source: @directory",dt@directory,"\n")
  cat("..@indexFile", dt@indexFile,"\n")
  cat("..@text, 'list':", length(dt@text), "documents","\n")
  for (i in 1:length(dt@text)) {
    cat("..$", names(dt@text)[i], ":", dt@text[[i]][1:8], "... \n")
  }
}
