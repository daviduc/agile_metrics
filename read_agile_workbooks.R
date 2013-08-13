read_agile_workbooks<-function(directory,filenames)
{
  #loads in the "Dervied Sprint Requirements" tab of Excel workbook files
  #filenames should be character vector
  directory<-paste(directory,"\\",sep="")
  list_agile_files<-lapply(filenames,function(x) loadWorkbook(paste(directory,x,sep="")))
  list_agile_sheets<-lapply(list_agile_files, function(x) readWorksheet(x,"Derived Sprint Requirements"))
  # function for cleaning up column names
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
  
  list_agile_sheets.projsummary<-lapply(list_agile_files, function(x) readNamedRegion(x,"ProjectSummary"))
  summary.out<-lapply(list_agile_sheets.projsummary, function(x) data.frame(x))
  
  #print(length(list_agile_sheets.projsummary))
  print(str(list_agile_sheets.projsummary[1]))
  
  #list_agile_sheets.projsummary<-lapply(list_agile_sheets.projsummary, 
  #                                      function(x) lapply(colnames(x), function(y) { 
  #                                        sub("X..Complete.in.Sprint","Percent.Complete.in.Sprint",y)
  #                                        }))
  
  #list.s.updates<-lapply(list_agile_sheets.sofsummary, function(x) s.updates<-colnames(x))
  #print(str(list_agile_sheets.sofsummary))
  #print(length(list_sprints))
  #add name of file to sprint data frame
  
  index<- 1
  for(iter in filenames)
  {
    #if(nrow(list_basic_sprints[[index]])>0)
    list_basic_sprints[[index]]$Sourcefile <- iter
    list_basic_sprints[[index]]$Start.Date <-as.POSIXct(list_basic_sprints[[index]]$Start.Date)
    list_basic_sprints[[index]]$Sprint.Start.Date <- 
      as.POSIXct(list_agile_sheets.rollup[[index]]$Start.Date[1])
    list_basic_sprints[[index]]$Sprint.End.Date <- 
      as.POSIXct(list_agile_sheets.rollup[[index]]$End.Date[1])
    index<-index+1
  }
  
  #iterate over size of $Sourcefile to save data object file
  save_to_file<- function(sprint)
  {
    filename<-tolower(sprint[1,]$Sourcefile)
    
    filename<-sub(".","",filename,fixed=TRUE)
   
    filename<-sub(".","",filename,fixed=TRUE)
    
    
    if(nchar(filename)<NUM.CHARS.SPRINT.FILE.LESS.10)
    {
      filename<-paste(substr(filename,1,3),"s","0",substr(filename,
        STRT.CHAR.PSTN.SPRINT.PERIOD,STRT.CHAR.PSTN.SPRINT.PERIOD+2),sep="")
      save(sprint,file=filename)
    }
    else
    {
      filename<-paste(substr(filename,1,3),"s",substr(filename,
        STRT.CHAR.PSTN.SPRINT.PERIOD,STRT.CHAR.PSTN.SPRINT.PERIOD+3),sep="")
      save(sprint,file=filename)
    }
  }
  #list_basic_sprints<-add_sprint_variables(list_basic_sprints)
  lapply(list_basic_sprints, save_to_file)
  list_basic_sprints
}

