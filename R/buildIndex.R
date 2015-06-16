buildIndex = function(directory) {
  print("Building your index file.")
  filenames = c()
  paths = c()
  author = c()
  title = c()
  date = c()
  eebo = c()
  stc = c()
  tcp = c()
  id = c()
  files = dir(directory)
  for(i in 1:length(files)){
    if (length(grep(".txt",files[i])) == 1) {
      id[i] = file_path_sans_ext(files[i])
      filenames[i] = files[i]
      paths[i] = paste(directory, files[i], sep="/")
      author[i] = ""
      title[i] = ""
      date[i] = ""
      eebo[i] = ""
      stc[i] = ""
      tcp[i] = ""
      
    } else if(length(grep(".xml", files[i])) == 1) {
      ns = c(d = "http://www.tei-c.org/ns/1.0")
      filepath = paste(directory, files[i], sep="/")
      filenames[i] = files[i]
      paths[i] = filepath
      parsedText = xmlTreeParse(filepath,useInternalNodes = TRUE)
      parsedText = xmlRoot(parsedText)
      # Get title
      tempTitle = getNodeSet(parsedText, "//d:fileDesc/d:titleStmt/d:title", 
                             namespace = c(d = "http://www.tei-c.org/ns/1.0"))
      tempTitle = unlist(sapply(tempTitle, xmlValue))
      title[i] = tempTitle
      
      # Get Author
      auth = getNodeSet(parsedText, "//d:biblFull/d:titleStmt/d:author", 
                        namespace = c(d = "http://www.tei-c.org/ns/1.0"))
      if(length(auth) >= 1) {
        auth = unlist(sapply(auth, xmlValue))
        author[i] = paste(auth, collapse = "; ")
      } else {
        author[i] = ""
      }
      
      # Get date
      tempDate = getNodeSet(parsedText, "//d:biblFull/d:publicationStmt/d:date",
                            namespace = c(d = "http://www.tei-c.org/ns/1.0"))
      tempDate = unlist(sapply(tempDate, xmlValue))
      # Here I remove punctuation from the date except for ?
      # and - because both are meaningful indicators of 
      # the TCP & STCs involvement in the records
      # (i.e. date ranges and supplied dates).
      date[i] = sub("([?-])|[[:punct:]]","", tempDate)
      
      # Get eebo
      eeboTemp = getNodeSet(parsedText, "//d:fileDesc/d:publicationStmt/d:idno[@type='EEBO-CITATION']",
                            namespace = c(d = "http://www.tei-c.org/ns/1.0"))
      if(!is.na(eeboTemp) && length(eeboTemp) > 1) {
        eeboTemp = unlist(sapply(eeboTemp, xmlValue))
        if(!is.null(eeboTemp)) {
          eebo[i] = eeboTemp
        } else {
          eebo[i] = ""
        }
      } else {
        eebo[i] = ""
      }
      
      # Get STC/ESTC
      stcTemp = getNodeSet(parsedText, "//d:fileDesc/d:publicationStmt/d:idno[@type='STC']",
                           namespace = c(d = "http://www.tei-c.org/ns/1.0"))
      stcTemp = unlist(sapply(stcTemp, xmlValue))
      stcTemp = paste(stcTemp, collapse = "; ")
      stc[i]  = stcTemp
      
      # Get TCP
      tcpTemp = getNodeSet(parsedText, "//d:fileDesc/d:publicationStmt/d:idno[@type='DLPS']",
                           namespace = c(d = "http://www.tei-c.org/ns/1.0"))
      tcpTemp = unlist(sapply(tcpTemp, xmlValue))
      tcp[i] = tcpTemp
      
      # Set ID
      id[i] = file_path_sans_ext(files[i])
    }
  }
  #browser()
  index = data.frame(id, stc, tcp, eebo, author, date, title, filenames, paths)
  write.csv(index, paste(directory, "index.csv", sep=""))
  return(paste(directory, "index.csv", sep=""))
}
