read_code_metrics<-function(sprints, directory)
{
  fnames<-list.files(directory)
  directory<-paste(directory,"\\",sep="")
  list_agile_sheets<-lapply(fnames,function(x) readWorksheet(loadWorkbook(paste(directory,x,sep="")),"Sheet1"))
  vect.sloc<-sapply(list_agile_sheets, function(x) {df<-data.frame(x); df[df$Scope=="Project","Lines.of.Code"]})
  vect.test.complexity<-sapply(list_agile_sheets, function(x) { df<-data.frame(x);df[df$Scope=="Project","Cyclomatic.Complexity"]})
  fnames<-sapply(fnames, function(x) paste("ver ", substr(x,5,12),sep="")) # 5 is end char of "oca " part, 12 is last char of version
  #vector of Namespaces
  list.Namespaces<-sapply(list_agile_sheets, function(x) {
    df<-data.frame(x); ns<-dimnames(table(df$Namespace))[[1]]; ns<-ns[ns!=" "]
                                            }
  )
  #vector of Types
  list.Types<-sapply(list_agile_sheets, function(x) {
    df<-data.frame(x); ts<-dimnames(table(df$Namespace))[[1]]; ts<-ts[ts!=" "]
  }
  )
  #sub-total sloc by Namespace
  list.subtot<-lapply(list_agile_sheets,function(x,y) { 
      df<-data.frame(x); 
      ns<-as.vector(y); 
      sapply(ns,function(z) {ns.sloc<-df[df$Namespace==z & df$Type==" ","Lines.of.Code"]});
    }, list.Namespaces[[length(list.Namespaces)]])
  
  vect.subtotals<-numeric(0)
  for(iter_build in c(1:length(list.subtot[[length(list.subtot)]])))
    vect.subtotals<-c(vect.subtotals,c=sapply(list.subtot,function(x) {if(length(x[[iter_build]])==0) 0 else x[[iter_build]]}))
  #every length(list.subtot) elements is set of sloc changes for one namespace
  #vector of namespace names
  vect.Namespaces<-attributes(list.subtot[[1]])$names #numbered element doesnt matter since all are the same
  
  # sloc per hour
  # ver 0.1.0.2 built on 2/19/2013 --> Sprints 0-5
  # ver 0.1.0.3 built on 3/6/2013  --> Sprint 6
  # ver 0.1.0.7 built on 3/21/2013 --> Sprint 7
  # following versions track with sprint
  vect.sprint.num<-c(1:length(fnames))+4
  # find total hours by Sprint
  vect.sprint.sum<-rep(0,length(fnames))
  index<-1
  
  for(iter_matching_sprint in vect.sprint.num)
  {
    for(iter_sprint in sprints)
    {
      sprint.num<-as.numeric(process_sprint_period(iter_sprint$Sourcefile[1]))
      if(iter_matching_sprint+1 == sprint.num)
      {
        vect.sprint.sum[index]<-sum(iter_sprint[iter_sprint$Finish.Sprint.Number
                                           ==paste("S",iter_matching_sprint,sep=""),"Spent.Hours"])
        break
      }
    }
    index=index+1
  }
  #re-do only first element to include all sprints up to 5 for hours for first build
  
  for(iter_sprint in sprints)
  {
    sprint.num<-as.numeric(process_sprint_period(iter_sprint$Sourcefile[1]))
    if(sprint.num>5)
    {
      vect.sprint.sum[1]<-sum(iter_sprint[iter_sprint$Finish.Sprint.Number=="S5" |
                                          iter_sprint$Finish.Sprint.Number=="S4" |
                                          iter_sprint$Finish.Sprint.Number=="S3" |
                                          iter_sprint$Finish.Sprint.Number=="S2" |
                                          iter_sprint$Finish.Sprint.Number=="S1" |
                                          iter_sprint$Finish.Sprint.Number=="S0", "Spent.Hours"])
      break
    }
  }
  
  vect.sloc.change<-append(vect.sloc[1],diff(vect.sloc))
  vect.sloc.per.hour<-vect.sloc.change/vect.sprint.sum
  #adjusted for primay dev only, not possible to count existing sloc changes
  adj.vect.sloc.per.hour<-sapply(vect.sloc.per.hour, function(x) {if(x<0 | x == Inf) 0 else x})  
  
  arr.total.slocs<-array(c(fnames,
                           vect.sloc,
                           vect.test.complexity,
                           vect.sprint.num,
                           vect.sprint.sum,
                           vect.sloc.per.hour,
                           adj.vect.sloc.per.hour),c(length(fnames),7))
  arr.subtotals.slocs<-array(vect.subtotals,c(length(fnames),length(vect.Namespaces)),list(fnames,vect.Namespaces))
  out<-list(arr.total.slocs,arr.subtotals.slocs)
  write.table(out[[2]],file="sloc sub-totals.csv",sep=",",row.names=T)
  out
}