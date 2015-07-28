#====================================================================
#  This function builds an index file for use with docList objects
#  given a directory to search through.  It's primary purpose is to
#  compile meta-data and to serve as a reference or overview for the
#  user's corpus.
#====================================================================
# 
# Create a .csv index of your collection
# 
# @section Description:
# This function accepts a directory and reads the files in that directory
# to build a spreadsheet of metadata for that collection.  It will work for
# either \code{.txt} or \code{.xml} files.  The resulting spreadsheet is
# more robust for \code{.xml} files, but these \emph{must} be encoded
# according to the \code{TEI} guidelines.  For \code{.txt} files,
# the algorithm assumes that the filenames (sans extension) are also
# the \code{id}s for the files and these will be used throughout the
# package to link data to particular documents.  The \code{.txt} based
# index will feature all of the same fields as the \code{.xml} one,
# but they will all be blank and the user may fill them accordingly.
# 
# @param directory The directory in which to search for documents and
#                  to store the index file.  If you are using the wizard,
#                  this function will create the file in the same directory
#                  as the documents and then move it to the path you supply
#                  through the wizard.
#                  
# @return indexFile The current path to the document that is stored in the
#                   \code{docList} object.
# 
# @examples
# indexFile = buildIndex("~/path/to/corpus/documents")
# @export
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
  last = 0
  for(i in 1:length(files)){
    total = length(files)
    percent = (i/total) * 100
    percents = c(10,20,30,40,50,60,70,80,90,100)
    if (round(percent) %in% percents && round(percent) != last) {
      print(paste(round(percent), "% complete!", " Reading document ", i, " of ", total, ".", sep=""))
      last = round(percent)
    }
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
      
    } else if (length(grep(".xml", files[i])) == 1) {
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
      if(!is.na(eeboTemp) && length(eeboTemp) > 0) {
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
      if(!is.na(tcpTemp) && length(tcpTemp) > 0) {
        tcpTemp = unlist(sapply(tcpTemp, xmlValue))
        if(!is.null(tcpTemp)) {
          tcp[i] = tcpTemp
        } else {
          tcp[i] = ""
        }
      } else {
        tcp[i] = ""
      }
      
      # Set ID
      id[i] = file_path_sans_ext(files[i])
    }
  }
  index = data.frame(id, stc, tcp, eebo, author, date, title, filenames, paths)
  # browser()
  not_blanks = c()
  for (i in 1:ncol(index)) {
    not_blanks = c(not_blanks, any(index[,i] != ""))
  }
  index = index[,not_blanks]
  write.csv(index, paste(directory, "/", "index.csv", sep=""))
  indexFile = paste(directory, "/", "index.csv", sep="")
  return(indexFile)
}
