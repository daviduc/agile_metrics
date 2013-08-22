#load all files
do_job_primary<-function(phase)
{
  #prior to loading rJava, XLConnectJars, and XLConnect
  #set parameters
  options(java.parameters="-Xmx2048m")
  library("rJava")
  library("XLConnectJars")
  library("XLConnect")
  library("randomForest")
  source("read_agile_workbooks.R")
  source("find_completed_agile_task.R")
  source("load_agile_sprints_into_frames.R")
  source("across_all_sprints.R")
  source("process_completed_tasks.R")
  source("agile_graphs.R")
  NUM.CHARS.SPRINT.FILE.LESS.10<<-36 #char length minus period chars, plus one, of filename of 1 digit sprint sub-string for less than conditional
  STRT.CHAR.PSTN.SPRINT.PERIOD<<-28 #char position of start char of sprint sub-string in filename

  #directory as of 8/22/2013 should return 56 files
  TOTAL.STATUS.FILES<<-56 #update this when a file is added
  #############FOR phase 1#############
  
  fnames<-list.files(".\\data analysis\\process metrics")
 
  if(phase==1)
  {
    load.seq<-c(TOTAL.STATUS.FILES:TOTAL.STATUS.FILES)
    output<-sapply(load.seq, function(x) {read_agile_workbooks(".\\data analysis\\process metrics",fnames[x])})
    print("Finished Loading Workbooks")
    load.fnames<-list.files(".\\","ocas")
    output<-lapply(list.files(".\\","ocas"),load_agile_sprints_into_frames)
    output<-add_sprint_variables(output)
    print("Finished Adding variables")
    output<-find_completed_tasks(output)
    print("Finished Finding Completed Tasks")
    output<-process_completed_tasks(output,load.fnames[length(load.fnames)])
    print("Finished Processing of Completed Tasks")
    sprints<-lapply(list.files(".\\","ocas"),load_agile_sprints_into_frames)
    plot_graphs(output,1)
    plot_graphs(output,2)
    plot_graphs(output,3)
    plot_graphs(output,4)
    results<-plot_graphs(sprints,7)
    results<-list(output,sprints)
    output<-plot_graphs(sprints,8)
    output<-plot_graphs(sprints,11)
    results<-read_code_metrics(sprints,".\\data analysis\\code metrics")
    output<-plot_graphs(results[[1]],9)
    output<-plot_graphs(results[[2]],10)
  }  
}

process_sprint_week<-function(sprint_string)
{
  sprint_string<-tolower(sprint_string)
  sprint_string<-sub(".","",sprint_string,fixed=TRUE)
  sprint_string<-sub(".","",sprint_string,fixed=TRUE)
  
  if(nchar(sprint_string)<NUM.CHARS.SPRINT.FILE.LESS.10) 
    sprint_week_number<-as.numeric(substr(sprint_string,STRT.CHAR.PSTN.SPRINT.PERIOD+1,STRT.CHAR.PSTN.SPRINT.PERIOD+1)) 
  else
    sprint_week_number<-as.numeric(substr(sprint_string,STRT.CHAR.PSTN.SPRINT.PERIOD+2,STRT.CHAR.PSTN.SPRINT.PERIOD+2))
}

process_sprint_period<-function(sprint_string)
{
  sprint_string<-tolower(sprint_string)
  sprint_string<-sub(".","",sprint_string,fixed=TRUE)
  sprint_string<-sub(".","",sprint_string,fixed=TRUE)
  
  if(nchar(sprint_string)<NUM.CHARS.SPRINT.FILE.LESS.10) 
    sprint_week_number<-as.numeric(substr(sprint_string,STRT.CHAR.PSTN.SPRINT.PERIOD,STRT.CHAR.PSTN.SPRINT.PERIOD)) 
  else
    sprint_week_number<-as.numeric(substr(sprint_string,STRT.CHAR.PSTN.SPRINT.PERIOD,STRT.CHAR.PSTN.SPRINT.PERIOD+1))
}
