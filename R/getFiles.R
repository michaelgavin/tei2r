getFiles = function(src = "TCP", ids = "", dl = NA, append=F){
  if (src == "TCP" || src == "tcp") {
    TCP = read.csv("./data/TCP.csv")
    TCP$Date = as.character(TCP$Date)
    TCP$Date = as.numeric(TCP$Date)
    if(ids == "") {
      typeAnswer = ""
      selection = c()
      answers = c("date", "authors", "author", "title", "titles", "status", "subject")
      while(!(typeAnswer) %in% answers) {
        typeAnswer = readline("How would you like to subset the TCP Index? [date, authors, titles, status, subject] >")
      }
      if(typeAnswer != "status") {
        status = readline("Would you also like to limit by status? [yes/no] > ")
        if(status == "yes") {
          status = readline("Which status do you want to limit your selection to? [free/restricted] > ")
        }
      }
      if(typeAnswer == "date") {
        dates = 1473:1800
        startDate = readline("Please enter the start year > ")
        while(!(startDate %in% dates)){
          startDate = readline("Please enter the start year > ")
        }
        endDate = readline("Please enter the end year > ")
        while(!(endDate %in% dates)){
          endDate = readline("Please enter the start year > ")
        }
        selection = TCP$TCP[which(TCP$Date >= startDate & TCP$Date <= endDate & tolower(TCP$Status) == tolower(status))]
      } else if(typeAnswer == "authors" || typeAnswer == "author") {
        authors = readline("Which author(s) would you like to include? [Last, First (Seperate by ;)] > ")
        authors = strsplit(authors, split = ';')
        authors = unlist(authors)
        for(i in 1:length(authors)) {
          selection = c(selection, grep(tolower(authors[i]), tolower(TCP$Author)))
        }
        #browser()
        selection = TCP$TCP[selection]
        selection = selection[which(selection %in% TCP$TCP[which(tolower(TCP$Status) == tolower(status))])]
        #browser()
      } else if(typeAnswer == "titles" || typeAnswer == "title") {
        titles = readline("Which title(s) would you like to include? [Seperate by ;] > ")
        titles = strsplit(titles, split=';')
        titles = unlist(titles)
        for(i in 1:length(titles)) {
          selection = c(selection, grep(tolower(titles[i]), tolower(TCP$Title)))
        }
        selection = TCP$TCP[selection]
        selection = selection[which(selection %in% TCP$TCP[which(tolower(TCP$Status) == tolower(status))])]
      } else if(typeAnswer == "status") {
        status = readline("Which status would you like to include?")
        selection = TCP$TCP[which(tolower(tcp$Status) == tolower(status))]
      } else if(typeAnswer == "subject" || typeAnswer == "subjects") {
        subjects = readline("Which subject(s) would you like to include? [Seperate by ;] > ")
        subjects = strsplit(subjects, split=';')
        subjects = unlist(subjects)
        for(i in 1:length(subjects)){
          selection = c(selection, grep(tolower(subjects[i]), tolower(TCP$Terms)))
        }
        selection = TCP$TCP[selection]
        selection = selection[which(selection %in% TCP$TCP[which(tolower(TCP$Status) == tolower(status))])]
      }
#       browser()
      print(paste("We have selected the following ", length(selection), " TCP numbers: ", sep=""))
      print(selection)
      
      getAnswer = readline("Would you like to download these files? [yes/no] > ")
      if(getAnswer == "yes") {
        print(paste("Saving in ", dl@directory, "data", sep =""))
        urls = paste("https://raw.githubusercontent.com/textcreationpartnership/",
                     selection,
                     "/master/",
                     selection,
                     ".xml",
                     sep="")
        files = paste(selection,".xml",sep="")
        library(httr)
        dir.create(paste(dl@directory, "data", sep=""), showWarnings = F)
        for (i in 1:length(urls)) {
          data.r = GET(url = urls[i])
          data.v = content(data.r)
          filename = paste(dl@directory, "data/", selection[i],".xml",sep="")
          print(paste("Downloading file: ", selection[i], '.xml', sep=""))
          write.table(data.v,filename,quote = F,row.names = F, col.names = F)
        }
        
        if(!is.na(dl) && append == FALSE) {
          write.csv(TCP[which(TCP$TCP %in% selection),], paste(dl@directory, "data/index.csv", sep=""))
          dl@indexFile = paste(dl@directory, "data/index.csv", sep="")
          dl@index = read.csv(dl@indexFile)
          dl@directory = paste(dl@directory, "data", sep="")
          return(dl)
        } else if(!is.na(dl) && append == TRUE) {
          index = TCP[which(TCP$TCP %in% selection),]
          dl@index = cbind(dl@index, index)
          write.csv(dl@index, dl@indexFile)
          dl@directory = paste(dl@directory, "data", sep="")
          return(dl)
        } else {
          index = TCP[which(TCP$TCP %in% selection),]
          write.csv(index, paste(getwd(), "data/index.csv", sep="/"))
          dl = builDocList(directory = paste(getwd(), "data", sep=""), indexFile=paste(getwd(), "data/index.csv", sep="/"))
        }
      } else {
        index = TCP[which(TCP$TCP %in% selection),]
        write.csv(index, paste(dl@directory, "index.csv", sep=""))
        dl@indexFile = paste(dl@directory, "index.csv", sep="")
        dl@index = read.csv(dl@indexFile)
        return(dl)
      }
    } else {
      selection = TCP$TCP[which(TCP$TCP %in% ids)]
      urls = paste("https://raw.githubusercontent.com/textcreationpartnership/",
                   selection,
                   "/master/",
                   selection,
                   ".xml",
                   sep="")
      files = paste(selection,".xml",sep="")
      library(httr)
      #dir.create(paste(dl@directory, "data", sep=""), showWarnings = F)
      if(dl@directory == "") {
        dl@directory = getwd()
      }
      #browser()
      for (i in 1:length(urls)) {
        data.r = GET(url = urls[i])
        data.v = content(data.r)
        filename = paste(dl@directory, selection[i],".xml",sep="")
        print(paste("Downloading file: ", selection[i], '.xml', sep=""))
        write.table(data.v,filename,quote = F,row.names = F, col.names = F)
      }
      
#       if(!is.na(dl)) {
#         dl@index = cbind(dl@index, tCP[which(TCP$TCP %in% selection),])
#         write.csv(dl@index, dl@indexFile)
#         return (dl)
#       }
    }
  }
}
