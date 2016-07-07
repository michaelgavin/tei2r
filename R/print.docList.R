setMethod(f = "show", signature = "docList",
          definition = function(object) {
  print("Document list summary:")
  cat("@directory:", object@directory,"\n")
  cat("@filenames (first 5):",object@filenames[1:5],"\n")
  cat("@indexFile location:", object@indexFile, "\n")
  cat("@index of", nrow(object@index), "rows and", ncol(object@index), "columns.", "\n")
  cat(".... index columns:", colnames(object@index), "\n")
  cat("@stopwords: Character vector of", length(object@stopwords), "words.", "\n")
  cat("@text: List of imported character data.", length(object@text), "texts.")
          }
)

setMethod(f = "summary", signature = "docList",
          definition = function(object) {
            print("Document list summary:")
            cat("@directory:", object@directory,"\n")
            cat("@filenames (first 5):",object@filenames[1:5],"\n")
            cat("@indexFile location:", object@indexFile, "\n")
            cat("@index of", nrow(object@index), "rows and", ncol(object@index), "columns.", "\n")
            cat(".... index columns:", colnames(object@index), "\n")
            cat("@stopwords: Character vector of", length(object@stopwords), "words.", "\n")
            cat("@text: List of imported character data.", length(object@text), "texts.")
          }
)

ViewIndex = function(dl) { View(dl@index) }

ViewTexts = function(dl) {
  txt = lapply(dl@texts, function(x) paste(x[1:12], collapse = " "))
  ids = names(dl@texts)
  xy = data.frame(ids,unlist(txt))
  row.names(xy) = NULL
  colnames(xy) = c("ID","TEXT")
  print(xy)
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
