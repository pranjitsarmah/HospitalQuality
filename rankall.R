rankall<-function( criteria, rank_entry){
      
     
      criteria<-casefold(criteria, upper=F)
      
      outcome<-read.csv("outcome-of-care-measures.csv", colClasses ='character')
      
      
      
      out_come<-c("heart attack", "heart failure", "pneumonia")
      
      
      if(!is.element(criteria,out_come)) stop("outcome not valid", call.=T)
      
      if(criteria=='heart attack')colno<-11
      if(criteria=='pneumonia')colno<-23
      if(criteria=='heart failure') colno<-17
      
      
      state_col<-unique(outcome[,7])
      state1<-NULL
      hospital_name<-vector()
      state_name<-vector()
      
      
      s1<-split(outcome[,colno],outcome[, 7])
      s2<-split(outcome[,2],outcome[, 7])
      
      for(i in seq_along(state_col)){
            
            
         state1=state_col[i]
      
      
      
         x1<-as.numeric(s1[[state1]])
         x2<-s2[[state1]]
      
      
         good<-!is.na(x1)
      
         y1<-x1[good]
         y2<-x2[good]
      
         r<-order(y1, y2)
    
         if(casefold(rank_entry)=='best')rank1<-1
         if(casefold(rank_entry)=='worst')rank1<-length(r)
         else rank1<-rank_entry
        
      
         hospital_name[i]<-y2[r[rank1]]
         state_name[i]<-state1
         rank1<-NULL
      }
         data.frame(state=state_name, hospital=hospital_name)
         
         

      
      
      
}