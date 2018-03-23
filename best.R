best<-function(state, criteria){
      
      state<-casefold(state, upper=T)
      criteria<-casefold(criteria, upper=F)
      
      outcome<-read.csv("outcome-of-care-measures.csv", colClasses ='character')
      
      
      
      out_come<-c("heart attack", "heart failure", "pneumonia")
      
      
      if(!is.element(criteria,out_come)) stop("outcome not valid", call.=T)
            
      if(!is.element(state, outcome[,7])) stop("state not valid", call.=T)
            

      if(criteria=='heart attack')colno<-11
      if(criteria=='pneumonia')colno<-23
      if(criteria=='heart failure') colno<-17
      
      s1<-split(outcome[,colno],outcome[, 7])
      s2<-split(outcome[,2],outcome[, 7])
      
      
      x1<-as.numeric(s1[[state]])
      x2<-s2[[state]]
      
      
      good<-!is.na(x1)
      
      y1<-x1[good]
      y2<-x2[good]
      
      r<-order(y1, y2)
      
      #out<-data.frame(Hospital=y2[r], rank=1:length(r), point=y1[r])
      #print(out)
      
     return(print(y2[r[1]]))

      
      
}