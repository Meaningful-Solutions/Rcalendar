require(Hmisc)
createCalendarTable<-
  function(years, mo=NULL, q=NULL){
    response<-
      merge(month.abb, years, all=T);
    
    names(response)<-
      c("month", "year");
    
    qtr<-
      c(1,1,1,2,2,2,3,3,3,4,4,4);
    
    wkday<-
      c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun");
    
    response$month.nbr<-
      match(response$month, month.abb);
    
    day<-c();
    month.nbr<-c();
    year<-c();
    quarter<-c();
    month<-c();
    wday<-c();
    wday.nbr<-c();
    wk.nbr<-c();
    
    #expand response to have one row for every calendar day
    for(i in 1:nrow(response)){
      dt<-
        as.Date(paste(response$year[i], response$month.nbr[i], "1", sep="-"));
      
      nbr<-
        monthDays(dt);
      
      month.nbr<-
        c(month.nbr, rep(response$month.nbr[i],nbr));
      
      month<-
        c(month, rep(month.abb[response$month.nbr[i]], nbr));
      
      year<-
        c(year, rep(response$year[i], nbr));
      
      quarter<-
        c(quarter, rep(qtr[response$month.nbr[i]], nbr));
      
      day<-
        c(day, 1:nbr);
    }
    
    response<-
      data.frame(year, quarter, month, month.nbr, day);
    
    #Determin the week information for each day
    for(i in 1:nrow(response)){
      dt<-
        as.Date(paste(response$year[i], response$month.nbr[i], response$day[i], sep="-"));
      
      w<-
        weekdays(dt, abbreviate=TRUE);
      
      wn<-
        match(w, wkday);
      
      wday<-
        c(wday, w);
      
      wday.nbr<-
        c(wday.nbr, wn);
      
      if(response$day[i]==1){
        nbr<-1;
      } else if(wn==1){
        nbr<-nbr+1;
      }
      
      wk.nbr<-
        c(wk.nbr, nbr);
        
    }
    
    response<-
      cbind(response, wday, wday.nbr, wk.nbr);
    
    #factors for ggplot facets
    
    response$monthf<-
      factor(response$month, levels=month.abb, ordered=TRUE);
    
    response$wdayf<-
      factor(response$wday, levels=wkday, ordered=TRUE);
    
    if(!is.null(q)){
      response<-
        response[response$quarter == q, ];
    }
    
    if(!is.null(mo)){
      response<-
        response[response$month == mo, ];
    }
    
    return(response);
}