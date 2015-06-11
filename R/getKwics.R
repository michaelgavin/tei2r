##############################################
# Function for generating a keyword-in-context 
# (KWIC) analysis of a given text. The arguments
# of the function are:
#
#   filepath  : the path to the primary document, 
#               saved as a txt or csv file. 
#   term      : a string, the key word you're 
#               analyzing.
#   context   : an integer that refers to how 
#               many words to the left and right 
#               the function will pull.
#
# Adapted from Jockers, "Text Analysis with R"

getKwics = function(dl, item, term, context = 10) {
  text = dl@text[[item]]
  hits = which(text == term)
  kwics = list()
  if (term %in% text) {
    for (i in 1:(length(hits))) {
      start = hits[i] - context
      end = hits[i] + context
      if (start < 1) {
        start = 1
      }
      else kwics[[i]] = text[start:end]
    }
  }
  else kwics = list(NA)
  return(kwics)
}

