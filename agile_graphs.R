plot_graphs<-function(x,type=1)
{
  #x be data frame of processed and completed agile tasks for types 1-5
  #such as idt.ctasks.to3
  
  #x be data frame of a sprint backlog snapshot for type 6
  
  windows(width=10,height=10)
  
  if(type==1)
  {
    par(mfrow=c(1,1))
    par(mar=c(2,2,2,1),mgp=c(1,0,0)) 
    color.plot<-sapply(x$InActive.Periods,function(a) {if(a>summary(x$InActive.Periods)[3]) 2 else 1}) # 3 is Median of summaryDefault
    plot(jitter(x$Duration.Sprint),pch=19,ylab="",xlab="",xaxt="n",yaxt="n",cex.lab=.8, col=color.plot) # use main="IDT Sprint Tasks: Duration" for plot title
    #points(1,.29,pch=21, cex=3, col="red")
    #text(1.5,.29, cex=1, col="red", labels="xxx", adj=0)
    axis(1,line=0,at=c(1:length(x$Duration.Sprint)), cex.axis=.6, tck=-.01)
    
    par(mgp=c(1,0.5,0)) #changing start line of y-axis from 0 to 0.5
    axis(2,line=0,at=x$Duration.Sprint, cex.axis=.6, tck=-.01)
    mtext("completed sprint tasks",side=1,line=1,cex=.8,las=0)
    mtext("duration # sprints", side=2,line=1.1,cex=.8,las=0)
    #legend("bottomright", pch=19, legend=c("very low","low","middle","high","very high"), cex=.8, col=c(1,2,3,4,5), text.col=c(1,2,3,4,5))
    
  }
  else if(type==2)
  {
    max.hor<-summary(as.numeric(as.character(x$Start.Sprint)))[6]
    if(max.hor < summary(as.numeric(as.character(x$Completed.Sprint)))[6]) max.hor = summary(as.numeric(as.character(x$Completed.Sprint)))[6]
    hor.lab<-c(0:max.hor)
    par(mfrow=c(2,1))
    par(mar=c(2,2,2,1),mgp=c(1,.3,0))     

    hist(as.numeric(as.character(x$Start.Sprint)),ylab="",xlab="",cex.axis=.6, tck=-.01,main="", right=FALSE,col="gray") # use main="IDT Sprint Tasks: Duration" for plot title
    par(mgp=c(1,0.5,0)) #changing start line of y-axis from 0 to 0.5
    mtext("started tasks", side=2,line=1.1,cex=.8,las=0)
    mtext("sprint", side=1,line=1,cex=.8,las=0)
    
    par(mar=c(2,2,2,1),mgp=c(1,.3,0))     
    hist(as.numeric(as.character(x$Completed.Sprint)),ylab="",xlab="", cex.axis=.6, tck=-.01,main="",right=FALSE,col="gray") # use main="IDT Sprint Tasks: Duration" for plot title
    par(mgp=c(1,0.5,0)) #changing start line of y-axis from 0 to 0.5
    mtext("completed tasks",side=2,line=1.1,cex=.8,las=0)
    mtext("sprint", side=1,line=1,cex=.8,las=0)
    
  }
  else if(type==3)
  {
    par(mfrow=c(1,1))
    par(mar=c(2,2,2,1),mgp=c(1,.3,0))     
    plot(jitter(x$Spent.Hours/x$Est.Hours),jitter(x$Duration.Sprint), 
         ylab="", xlab="", pch=19, cex.axis=.6, tck=-.01)
    par(mgp=c(1,0.5,0)) #changing start line of y-axis from 0 to 0.5
    mtext("task duration (sprints)", side=2,line=1.1,cex=.8,las=0)
    mtext("actual / estimated task hours", side=1,line=1,cex=.8,las=0)
  }
  else if(type==4)
  {
    par(mfrow=c(1,1))
    par(mar=c(2,2,2,1),mgp=c(1,.3,0))     
    
    plot(jitter(x$InActive.Periods),jitter(x$Duration.Sprint), 
         ylab="", xlab="", pch=19, cex.axis=.6, tck=-.01)
    par(mgp=c(1,0.5,0)) #changing start line of y-axis from 0 to 0.5
    mtext("task duration (sprints)", side=2,line=1.1,cex=.8,las=0)
    mtext("latent status periods", side=1,line=1,cex=.8,las=0)
  }
  else if (type==5)
  {
    x1.start<-as.numeric(as.character(x$Start.Sprint))
    x2.hours<-x$Est.Hours
    y.completed<-as.numeric(as.character(x$Completed.Sprint))
    
    par(mfrow=c(1,1))
    par(mar=c(2,2,2,1),mgp=c(1,.3,0)) 
    plot(jitter(x1.start),jitter(y.completed),pch=19,ylab="", xlab="", cex.axis=.6, tck=-.01)
    par(mgp=c(1,0.5,0)) #changing start line of y-axis from 0 to 0.5
    mtext("completed sprint", side=2,line=1.1,cex=.8,las=0)
    mtext("started sprint", side=1,line=1,cex=.8,las=0)
    ls<-lm(y.completed~x1.start + x2.hours)
    #y.predict<-predict(ls,newdata=x.start)
    lines(x=x1.start,y=ls$fitted.values)
    
  }
  else if(type==6)
  {
    par(mfrow=c(1,1))
    par(mar=c(2,2,2,1),mgp=c(1,0,0)) 
    color.plot<-sapply(x$InActive.Periods,function(a) {if(a>summary(x$InActive.Periods)[3]) 2 else 1}) # 3 is Median of summaryDefault
    plot(jitter(x$Duration.Sprint),pch=19,ylab="",xlab="",xaxt="n",yaxt="n",cex.lab=.8, col=color.plot) # use main="IDT Sprint Tasks: Duration" for plot title
    axis(1,line=0,at=c(1:length(x$Duration.Sprint)), cex.axis=.6, tck=-.01)
    
    par(mgp=c(1,0.5,0)) #changing start line of y-axis from 0 to 0.5
    axis(2,line=0,at=x$Duration.Sprint, cex.axis=.6, tck=-.01)
    mtext("all sprint tasks",side=1,line=1,cex=.8,las=0)
    mtext("duration # sprints", side=2,line=1.1,cex=.8,las=0)    
  }
  # x needs to be a list of all the status periods, i.e. lapply(list.files(".\\","ocas"),load_agile_sprints_into_frames)
  else if(type==7) 
  {
    dev.off()
    results<-list()
    #iterate over all the status periods
    sprints<-x
    last.status<-sprints[[length(sprints)]]
    total.model.data.raw.complete<-data.frame()
    total.model.data.raw.incomplete<-data.frame()
    for(iter_sprint in sprints){
      xActive<-iter_sprint$Active
      x0.id<-iter_sprint$Reqt.ID
      x1.duration<-as.numeric(as.character(iter_sprint$Duration.Sprint))
      x2.est<-iter_sprint$Estimate.Total.Hours
      x3.active<-iter_sprint$Active.Periods
      x4.inactive<-iter_sprint$InActive.Periods
      x5.res<-iter_sprint$Resource
      x6.implement.count<-sapply(gregexpr("implement",iter_sprint$Description,ignore.case=TRUE,fixed=FALSE),
                                 function(v) {if (attributes(v)$match.length[1]==-1) 0 else length(attributes(v)$match.length)})
      x7.perc<-sapply(iter_sprint$Percent.Complete,function(y) {
        if(y==0) 0.001 else y})
      x8.actual<-iter_sprint$Spent.Hours
      y.actual.hrs.for.completed.task<-NA
      
      model.set.all<-data.frame(xStatus=xActive, x0=x0.id,x1=x1.duration,x2=x2.est,x3=x3.active,x4=x4.inactive,x5=x5.res,
                                x6=x6.implement.count,x7=x7.perc, x8=x8.actual,y.pred=y.actual.hrs.for.completed.task)
      if(iter_sprint[1,"Sourcefile"]==last.status[1,"Sourcefile"])
      {
        model.set.incomplete.last<-model.set.all[model.set.all$x7!=1.00,]
        model.set.incomplete.last<-model.set.incomplete.last[model.set.incomplete.last$xStatus==TRUE,]
        model.set.incomplete.last<-model.set.incomplete.last[model.set.incomplete.last$x2 != 0,]
        total.model.data.raw.incomplete<-rbind(total.model.data.raw.incomplete,model.set.incomplete.last)
      }
      model.set.incomplete<-model.set.all[model.set.all$x7!=1.00,]
      model.set.incomplete<-model.set.incomplete[model.set.incomplete$xStatus==TRUE,]
      #model.set.incomplete<-model.set.incomplete[model.set.incomplete$x8!=0,]
      model.set.incomplete<-model.set.incomplete[model.set.incomplete$x2 != 0,]
      for(iter_reqt in model.set.incomplete$x0){
        completes<-mapply(function(x,y) { x==iter_reqt & y==1.00},last.status[,"Reqt.ID"], last.status[,"Percent.Complete"])
        if(sum(completes)>0) {
          model.set.incomplete[model.set.incomplete$x0==iter_reqt,"y.pred"]<-last.status[completes,"Spent.Hours"]
        }
      }      
      
                                             #model.set.incomplete[is.na(model.set.incomplete$y.pred),])
      #only keep tasks that are incomplete but end up being complete by the last status period
      model.set.incomplete<-model.set.incomplete[!is.na(model.set.incomplete$y.pred),]
      total.model.data.raw.complete<-rbind(total.model.data.raw.complete,model.set.incomplete)
    }
    #remove dups on reqt id and actual hours (i.e. no change)
    
    total.model.data.raw.incomplete<-total.model.data.raw.incomplete[!duplicated(total.model.data.raw.incomplete[,c(2,9)]),]
    total.model.data.raw.complete<-total.model.data.raw.complete[!duplicated(total.model.data.raw.complete[,c(2,9)]),] 

    total.test.data<-data.frame( id=total.model.data.raw.incomplete$x0,
                                 x0=total.model.data.raw.incomplete$x1+total.model.data.raw.incomplete$x3,
                                 x1=total.model.data.raw.incomplete$x6,
                                 x2=total.model.data.raw.incomplete$x2,
                                 #x3=abs(total.model.data.raw.incomplete$x8/total.model.data.raw.incomplete$x7 - total.model.data.raw.incomplete$x2),
                                 x4=total.model.data.raw.incomplete$x5,
                                 x5=total.model.data.raw.incomplete$x8)
    
    total.model.data<-data.frame(y.pred=total.model.data.raw.complete$y.pred,
                                 x0=total.model.data.raw.complete$x1+total.model.data.raw.complete$x3,
                                 x1=total.model.data.raw.complete$x6,
                                 x2=total.model.data.raw.complete$x2,
                                 #x3=abs(total.model.data.raw.complete$x8/total.model.data.raw.complete$x7 - total.model.data.raw.complete$x2),
                                 x4=total.model.data.raw.complete$x5,
                                 x5=total.model.data.raw.complete$x8)
    
    glm.actual<-glm(y.pred~x0 + x1 + x2 + x4+x5,data=total.model.data)
    y.predict<-predict(glm.actual,newdata=total.test.data) 
    corr.predict<-mapply(function(z1,z2,z3) { if(z1<z2) z2 else z1},
                         y.predict,
                         total.test.data$x5)
    predict.results<-data.frame(id=total.test.data$id, 
                                raw.predict=y.predict,
                                hours.predict=corr.predict)
    
    forestDuration<-randomForest(y.pred~x0 + x1 + x2 + x4+x5, data=total.model.data,prox=TRUE)
    predict.forestDuration<-predict(forestDuration,total.test.data)
    
    task.summary<-data.frame("total tasks"=nrow(last.status),
                             complete.tasks=nrow(last.status[last.status$Percent.Complete==1.00,]), 
                             incomplete.tasks=nrow(last.status[last.status$Percent.Complete!=1.00,]),
                             total.actual.hours=sum(last.status$Spent.Hours),
                             #hours.prediction.incomplete.tasks=sum(predict.results$hours.predict),
                             hours.prediction.incomplete.tasks=sum(predict.forestDuration),
                             hours.estimation.by.owners=sum(last.status[last.status$Percent.Complete!=1.00,"Estimate.Total.Hours"]),
                             hours.actual.incomplete.tasks=sum(last.status[last.status$Percent.Complete!=1.00,"Spent.Hours"]))
                             
    incomplete.tasks.summary<-data.frame(total.model.data.raw.incomplete)
    incomplete.tasks.summary<-incomplete.tasks.summary[c(-1,-5,-6,-8)] #remove active status variable/column
    names(incomplete.tasks.summary)<-c("Reqt ID", "Current Task Duration (Sprints)","Current Estimate To Complete (hours)",
                                       "Resource", "Percent Complete","Current Actual (hours)", "Predicted (hours)")
    incomplete.tasks.summary$"Predicted (hours)"<-predict.forestDuration
    
    results<-list("linear model"=glm.actual,
                  total.model.data,
                  total.test.data,
                  predict.results,
                  task.summary,
                  forestDuration,
                  predict.forestDuration,
                  incomplete.tasks.summary)
    
    write.table(results[[8]],file="incomplete task summary.csv",sep=",",row.names=F)
    write.table(results[[5]],file="all task summary.csv",sep=",",row.names=F)
    save(results,file=paste("results_",list.files(".\\","ocas")[length(list.files(".\\","ocas"))],sep=""))
    
  }
  # x needs to be a list of all the status periods, i.e. lapply(list.files(".\\","ocas"),load_agile_sprints_into_frames)
  else if(type==8)
  {
    #iterate over all the status periods
    #dev.off()
    burn.rate.data<-data.frame()
    sprints<-x
    actual.hours.by.status.period<-sapply(sprints,function(a) { sum(a$Spent.Hours)})
    status.period.end.date<-sapply(sprints, function(a) { 
      if     (process_sprint_week(a$Sourcefile[1])<2)   as.Date(a$Sprint.Start.Date[1]) +7
      else if(process_sprint_week(a$Sourcefile[1])<3)   as.Date(a$Sprint.Start.Date[1]) +14
      else                                              as.Date(a$Sprint.Start.Date[1])+21 } ) 
    sprint.start.date<-sapply(sprints, function(a) { as.Date(a$Sprint.Start.Date[1])})
    value<-sapply(sprints,function(a) { process_sprint_week(a$Sourcefile[1])})
    lo<-loess(actual.hours.by.status.period~status.period.end.date) #localized and weighted regressions
    predicts.loess<-predict(lo)
    
    burn.rate.data<-as.data.frame(cbind(actual.hours.by.status.period,status.period.end.date,sprint.start.date,value,predicts.loess))

    burn.rate.data<-burn.rate.data[-which(duplicated(burn.rate.data$status.period.end.date,fromLast=TRUE)),]
    
    weekend.days<-(diff(burn.rate.data$status.period.end.date)%/%7)*2  # doing integer division to calc 2 weekend days every 7 days
    weekend.days<-append(((burn.rate.data$status.period.end.date[1]-burn.rate.data$sprint.start.date[1])%/%7)*2,weekend.days) # add in first weekend count
    off.fridays<-append(0,diff(burn.rate.data$status.period.end.date)%/%14)  # doing integer division to calc off fridays every 14 days
    nonwork.days<-weekend.days+off.fridays
    
    daily.burn.rate<-append(0,diff(burn.rate.data$actual.hours.by.status.period)/diff(burn.rate.data$status.period.end.date))
    moving.daily.burn.rate<-append(0,burn.rate.data$actual.hours.by.status.period/
                                    (burn.rate.data$status.period.end.date-burn.rate.data$sprint.start.date[1]-nonwork.days))
    print(moving.daily.burn.rate)
    
    date.vector<-as.character(as.Date(burn.rate.data$status.period.end.date,origin="1970-01-01"))
    
    par(mar=c(4,2,2,2))   
    
    plot(burn.rate.data$actual.hours.by.status.period, type="l", lwd=2,
         ,pch=19,ylab="",xlab="",xaxt="n",yaxt="n",cex.lab=.8) # use main="Example Title" for plot title
    axis(1,line=0,at=c(1:length(burn.rate.data$status.period.end.date)), cex.axis=.6, tck=-.01, labels=date.vector,las=2) #hort-axis
    
    par(mgp=c(1,0.5,0)) #changing start line of y-axis tick labels from 0 to 0.5
    axis(2,line=0,at=burn.rate.data$actual.hours.by.status.period, cex.axis=.6, tck=-.01) #vert-axis left side
    #mtext("date",side=1,line=1,cex=.8,las=0) #hort-axis
    mtext("cumulative actual hours", side=2,line=1.1,cex=.8,las=0)   #vert-axis
    #lines(burn.rate.data$predicts.loess,col='red',lwd=2)
    par(new=TRUE)
    par(mgp=c(1,0.1,0)) #changing start line of y-axis tick labels from 0.5 to 0.1
    plot(moving.daily.burn.rate, pch=19, col='blue',type="l",xaxt="n",yaxt="n",xlab="",ylab="", lwd=2,col.lab='blue')
    axis(4,line=0,cex.axis=.6,tck=-.01, col='blue',col.lab='blue') #vert-axis, right side
    mtext("moving daily burn rate (hrs/day)",side=4,line=1.1,cex=.8,las=0,col='blue')
    legend("topleft",col=c("black","blue"),lty=1,legend=c("actuals","mov. avg. daily rate"),lwd=2)
    #lines(daily.burn.rate,col='blue',lwd=2)
    burn.rate.data
  }
  #dev.off()
  
}