add_sprint_variables<-function(x)
{
  sprints<-x
  #initializing previous_spent_hours to first sprint data
  #skipping first sprint data
  #initialize new variable Num.Zero.Change  
  for(iter_reqt in sprints[[length(sprints)]]$Reqt.ID)
  {
    sprint_index<-1
    for(iter_sprint in sprints)
    {
      dup.row<-iter_sprint_has_reqtid<-nrow(iter_sprint[iter_sprint$Reqt.ID == iter_reqt,])>1  
      if(dup.row)
      {
        delete.rows<-which(iter_sprint$Reqt.ID==iter_reqt)
        sprints[[sprint_index]]<-sprints[[sprint_index]][-delete.rows[2:length(delete.rows)],]
      }
      sprint_index<-sprint_index+1       
    }
  }  
  #adding new variables
  sprints<-lapply(sprints, function(x) { x$InActive.Periods<-0;x})
  sprints<-lapply(sprints, function(x) { x$Active.Periods<-0;x})
  sprints<-lapply(sprints, function(x) { x$Start.Sprint<-0;x})
  sprints<-lapply(sprints, function(x) { x$Completed.Sprint<-0;x})
  sprints<-lapply(sprints, function(x) { x$Duration.Sprint<-0;x})
  
  for(iter_reqt in sprints[[length(sprints)]]$Reqt.ID)
  {
    # check for any updates for those tasks that existed in the previous sprint
    #total.zero.changes<-sapply(prior_sprint$Reqt.ID,function(x)
    sprint_index<-1
    for(iter_sprint in sprints)
    {
      
      if(sprint_index>1)
        prior_sprint<-sprints[[sprint_index-1]]
      else
        prior_sprint<-sprints[[1]]
      
      prior_sprint_has_reqtid<-nrow(prior_sprint[prior_sprint$Reqt.ID == iter_reqt,])>0
      iter_sprint_has_reqtid<-nrow(iter_sprint[iter_sprint$Reqt.ID == iter_reqt,])>0
      if(prior_sprint_has_reqtid & iter_sprint_has_reqtid)
      {
        left.compare<-iter_sprint[iter_sprint$Reqt.ID == iter_reqt,]$Spent.Hours
        right.compare<-prior_sprint[prior_sprint$Reqt.ID ==iter_reqt,]$Spent.Hours
        task.not.complete<-iter_sprint[iter_sprint$Reqt.ID==iter_reqt,]$Percent.Complete != 1
        dummy_InActive.Periods<-prior_sprint[prior_sprint$Reqt.ID==iter_reqt,]$InActive.Periods
        dummy_Active.Periods<-prior_sprint[prior_sprint$Reqt.ID==iter_reqt,]$Active.Periods
        if(length(dummy_InActive.Periods)==0) dummy_InActive.Periods=0
        if(length(dummy_Active.Periods)==0) dummy_Active.Periods=0
        if(length(left.compare) != 0 & length(right.compare !=0 ) & task.not.complete )
        {
          if(left.compare==right.compare )
          {
            dummy_InActive.Periods<-dummy_InActive.Periods+1
          }
          else
          {
            dummy_Active.Periods<-dummy_Active.Periods+1
          }
        }
        else if(!prior_sprint_has_reqtid & iter_sprint_has_reqtid)
        {
          #a new task just showed up in the current period, previous period did not have task
        }
        
        iter_sprint[iter_sprint$Reqt.ID == iter_reqt,]$InActive.Periods<-dummy_InActive.Periods
        iter_sprint[iter_sprint$Reqt.ID==iter_reqt,]$Active.Periods<-dummy_Active.Periods
        sprints[[sprint_index]]<-iter_sprint
        
      } 
      sprint_index<-sprint_index+1
    }    
  }
  sprints<-lapply(sprints, function(x) {process_sprint(x)})      
  
  save_to_file<- function(sprint)
  {
    filename<-tolower(sprint[1,]$Sourcefile)
    
    filename<-sub(".","",filename,fixed=TRUE)
    
    filename<-sub(".","",filename,fixed=TRUE)
    
    if(nchar(filename)<NUM.CHARS.SPRINT.FILE.LESS.10)
    {
      filename<-paste(substr(filename,1,3),"s","0",substr(filename,STRT.CHAR.PSTN.SPRINT.PERIOD,
                                                          STRT.CHAR.PSTN.SPRINT.PERIOD+2),sep="")
      save(sprint,file=filename)
    }
    else
    {
      filename<-paste(substr(filename,1,3),"s",substr(filename,STRT.CHAR.PSTN.SPRINT.PERIOD,
                                                      STRT.CHAR.PSTN.SPRINT.PERIOD+3),sep="")
      save(sprint,file=filename)
    }
  }
  #list_basic_sprints<-add_sprint_variables(list_basic_sprints)
  lapply(sprints, save_to_file)
  
  sprints
}

process_sprint<-function(a.sprint)
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
  
  for(iter_reqt in a.sprint[,"Reqt.ID"])
  {
    combined_res<-mapply(function(x,y) { x==iter_reqt & y==1.00},a.sprint[,"Reqt.ID"], a.sprint[,"Percent.Complete"])
    Start.Sprint<-unname(substr(iter_reqt,2,3))
    Start.Sprint<-unname(if(as.integer(Start.Sprint)<10) paste("0",Start.Sprint,sep="") else Start.Sprint)
    Completed.Sprint<-unname(substr(a.sprint[a.sprint$Reqt.ID == iter_reqt,]$Finish.Sprint.Number,2,3))
    Completed.Sprint<-unname(if(as.integer(Completed.Sprint)<10) paste("0",Completed.Sprint,sep="") else Completed.Sprint)
    Duration.Sprint<-as.integer(Completed.Sprint)-as.integer(Start.Sprint)
    Duration.Sprint<-if(Duration.Sprint<0) Duration.Sprint=0 else Duration.Sprint+1
    a.sprint[a.sprint$Reqt.ID == iter_reqt,]$Start.Sprint<-Start.Sprint
    a.sprint[a.sprint$Reqt.ID == iter_reqt,]$Completed.Sprint<-Completed.Sprint
    a.sprint[a.sprint$Reqt.ID == iter_reqt,]$Duration.Sprint<-Duration.Sprint    
  }
  #print(a.sprint)
  a.sprint
}