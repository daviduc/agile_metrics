prototype<-function(x)
{  
  process_sprint_string<-function(sprint_string)
  {
    sprint_string<-tolower(sprint_string)
    sprint_string<-sub(".","",sprint_string,fixed=TRUE)
    sprint_string<-sub(".","",sprint_string,fixed=TRUE)
    
    if(nchar(sprint_string)<NUM.CHARS.SPRINT.FILE.LESS.10) 
      final_sprint_number<-paste("0",substr(sprint_string,
        STRT.CHAR.PSTN.SPRINT.PERIOD,STRT.CHAR.PSTN.SPRINT.PERIOD),sep="") #this is the 1 digit final sprint number
    else
      final_sprint_number<-substr(sprint_string,
        STRT.CHAR.PSTN.SPRINT.PERIOD,STRT.CHAR.PSTN.SPRINT.PERIOD+1) #this is the 2 digit final sprint number
  }
  directory<-paste(".\\data analysis\\process metrics","\\",sep="")
  filenames<-list.files(".\\data analysis\\process metrics")
  list_agile_files<-lapply(filenames,function(x) loadWorkbook(paste(directory,x,sep="")))
  list_agile_sheets<-lapply(list_agile_files, function(x) readWorksheet(x,"Derived Sprint Requirements"))
  col_cleanup<-function(raw_df)
  {
    #change SOW.. to SOW #
    agile_wks_cnames <- c( 
      "SOW.Number",
      "Nested.ID",
      "Start.Sprint.Number",
      "Finish.Sprint.Number",
      "Reqt.ID",
      "Description",
      "Resource",
      "Percent.Complete",
      "Spent.Hours",
      "Estimate.Total.Hours",
      "Active",
      "Parent.ID",
      "Creation.Date",
      "Modified.Date",
      "Start.Date",
      "Finish.Date",
      "CoS",
      "Status.Notes")
    colnames(raw_df)<-agile_wks_cnames
    raw_df[,1:18]
  }
  list_sprints<-lapply(list_agile_sheets, function(x) x=col_cleanup(x))
  #find basic task rows thar are largely complete
  find_complete_tasks<-function(agile_data)
  {
    #filter agile sheet by finding basic complete rows
    #find non-NA SOW #
    agile_data <- agile_data[!is.na(agile_data[1]),]
    #find non-NA start sprint
    agile_data <- agile_data[!is.na(agile_data[3]),]
    #find non-NA finish sprint  
    agile_data <- agile_data[!is.na(agile_data[4]),]
    #find active tasks
    agile_data <- agile_data[which(agile_data[11]==TRUE),]
  }
  
  list_basic_sprints<-lapply(list_sprints, function(x) x=find_complete_tasks(x))

  list_agile_sheets.rollup<-
    lapply(list_agile_files, 
           function(x) readWorksheet(x,paste("Sprint",
           as.numeric(process_sprint_string(substr(x@filename,33,nchar(x@filename)))))))
  
  #print(length(list_sprints))
  #add name of file to sprint data frame

  index<- 1
  for(iter in filenames)
  {
    #if(nrow(list_basic_sprints[[index]])>0)
    list_basic_sprints[[index]]$Sourcefile <- iter
    list_basic_sprints[[index]]$Start.Date <-as.POSIXct(list_basic_sprints[[index]]$Start.Date)
    list_basic_sprints[[index]]$Sprint.Start <- 
      as.POSIXct(list_agile_sheets.rollup[[index]]$Start.Date[1])
    list_basic_sprints[[index]]$Sprint.End <- 
      as.POSIXct(list_agile_sheets.rollup[[index]]$End.Date[1])
    index<-index+1
  }
  
  
  list_basic_sprints
  
}

