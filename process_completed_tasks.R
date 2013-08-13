process_completed_tasks<-function(complete_tasks,final_sprint_name="ocas0721")
{
  #complete_tasks should be an output from the find_completed_agile_task function
  #complete_tasks should be a variable row, 2 column character matrix, col1 = reqtid, col2=sprint2
  process_sprint_string<-function(sprint_string)
  {
    sprint_string<-tolower(sprint_string)
    sprint_string<-sub(".","",sprint_string,fixed=TRUE)
    sprint_string<-sub(".","",sprint_string,fixed=TRUE)
    
    if(nchar(sprint_string)<3) 
      final_sprint_number<-paste("0",substr(sprint_string,2,2),sep="") #this is the 1 digit final sprint number
    else
      final_sprint_number<-substr(sprint_string,2,3) #this is the 2 digit final sprint number
  }
  Completed.Sprint<-unname(sapply(complete_tasks[,2],process_sprint_string))
  Start.Sprint<-unname(sapply(complete_tasks[,1], function(x) { substr(x,2,3)}))
  Start.Sprint<-unname(sapply(Start.Sprint,function(x) 
    {if(as.integer(as.character(x))<10) paste("0",x,sep="") else x}))
  Duration.Sprint<-as.integer(as.character(Completed.Sprint))-as.integer(as.character(Start.Sprint))
  Duration.Sprint<-sapply(Duration.Sprint,function(x) {if(x<0) x=0 else x+1})
  complete_tasks<-cbind(complete_tasks,Start.Sprint,Completed.Sprint,
                        Duration.Sprint)
  #use sprint object
  print(paste("using ,",final_sprint_name," as final Sprint period"))
  load(paste(".\\",final_sprint_name,sep="")) 
  last.sprint.data<-sprint
                      
  Resource<-sapply(complete_tasks[,1], function(x) 
    {as.character(last.sprint.data[last.sprint.data$Reqt.ID==x,7]) }) #7 is Resource column
  Spent.Hours<-sapply(complete_tasks[,1], function(x) 
  {last.sprint.data[last.sprint.data$Reqt.ID==x,9] }) #9 is Spent.Hours column
  Est.Hours<-sapply(complete_tasks[,1], function(x) 
    {last.sprint.data[last.sprint.data$Reqt.ID==x,10]}) #10 is Estimated Hours column
  InActive.Periods<-sapply(complete_tasks[,1],function(x)
    {last.sprint.data[last.sprint.data$Reqt.ID==x,22]}) #22 is Number of InActive Periods
  Active.Periods<-sapply(complete_tasks[,1],function(x)
  {last.sprint.data[last.sprint.data$Reqt.ID==x,23]}) #23 is Number of Active Periods
  
  complete_tasks<-cbind(complete_tasks,Resource,Spent.Hours,Est.Hours,InActive.Periods, Active.Periods)
  dimnames(complete_tasks)[[1]]<-c(1:nrow(complete_tasks))
  complete_tasks<-data.frame(complete_tasks)
  complete_tasks[,5]<-as.numeric(as.character(complete_tasks[,5]))
  complete_tasks[,7]<-as.numeric(as.character(complete_tasks[,7]))
  complete_tasks[,8]<-as.numeric(as.character(complete_tasks[,8]))
  complete_tasks[,9]<-as.numeric(as.character(complete_tasks[,9]))
  complete_tasks[,10]<-as.numeric(as.character(complete_tasks[,10]))
  complete_tasks
}