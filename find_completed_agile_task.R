find_completed_tasks<-function(sprints)
{
  find_task<-function(sprint, reqt_id)
  {
    #search through sprint to find reqt id to 100% completion
    combined_res<-mapply(function(x,y) { x==reqt_id & y==1.00},sprint[,"Reqt.ID"], sprint[,"Percent.Complete"])
    return_val<-sprint[combined_res,]
  }
 
  completed_task_list<-matrix(c("Reqt.ID","Finish.Sprint"),nrow=1,ncol=2)
  colnames(completed_task_list)<-c("Reqt.ID","Finish.Sprint")
  
  for(iter_sprint in sprints)
  {
    for(iter_reqt in iter_sprint[,"Reqt.ID"])
    {
      #this finds the first task id match against the list of completed task matches
      #to ensure that we go through processing for only those tasks that have yet
      #to be determinted complete or not
      if(!(sum(mapply(function(x) {x==iter_reqt},completed_task_list[,1]))>0))
      {
        sprint_list<-lapply(sprints,find_task, iter_reqt)
        sprint_list<-sapply(sprint_list,function(x) {nrow(x)})
        first_sprint_completed_index<-which(sprint_list>0)[1]
        
        
        if(!is.na(first_sprint_completed_index))
        {
          this.sprint<-sprints[[first_sprint_completed_index]]
          first_sprint_completed<-this.sprint[this.sprint$Reqt.ID==iter_reqt,"Finish.Sprint.Number"]
          if(completed_task_list[1]=="Reqt.ID")
          {
            completed_task_list<-rbind(completed_task_list,
                                   matrix(c(iter_reqt,first_sprint_completed),ncol=2,nrow=1))
          }
          else
          {
            completed_task_list[1] <- iter_reqt
            completed_task_list[2] <- first_sprint_completed
          }
        }
      }
    }
  }
  completed_task_list<-completed_task_list[2:nrow(completed_task_list),]
}

