#====================================================================
#
#
#
#
#====================================================================
#' 
#' Placeholder
#' 
#' @param dt
#' @param limit
#' 
#' @examples
#' placeholder
getDocFrequencies = function(dt, limit=0) {
  df = docFrequencies()
  df@directory = dt@directory
  df@indexFile     = dt@indexFile
  df@raw = list()
  df@proportional = list()
  print("Calculating Straight Frequencies.  Accessible by df@raw")
  for (i in 1:length(dt@text)) {
    if (limit == 0) {
      df@raw[[i]] = rev(sort(table(dt@text[i])))
    } else {
      df@raw[[i]] = rev(sort(table(dt@text[i])))[1:limit]
    }
  }
  names(df@raw) = names(dt@text)
  print("Have calculated Straight Frequencies, moving on to proportional. Accessible by df@proportional.")
  for (i in 1:length(df@raw)) {
    total = sum(df@raw[[i]])
    df@proportional[[i]] = ((df@raw[[i]]) / total) #* 100
  }
  names(df@proportional) = names(dt@text)
  return(df)
}