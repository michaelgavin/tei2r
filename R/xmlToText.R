xmlToText = function(filepath,target=NULL) {
  parsedText = xmlTreeParse(filepath,useInternalNodes = TRUE)
  nodes = getNodeSet(parsedText,"/d:TEI//d:text//d:div", 
                     namespace = c(d = "http://www.tei-c.org/ns/1.0"))
  text = sapply(nodes,xmlValue)
  filename = paste(file_path_sans_ext(filepath),".txt",sep="")
  write.csv(text,filename)
  return(text)  
}
