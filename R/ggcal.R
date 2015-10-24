require(ggplot2);
ggcal<-
  function(years, mo=NULL, q=NULL, print.days=FALSE){
    cal<-
      createCalendarTable(years=years, mo=mo, q=q);
    
    gg<-
      ggplot( cal );
    
    gg<-
      (gg
       + geom_rect(aes(xmin=wday.nbr, xmax=wday.nbr+1, ymin=wk.nbr*-1, ymax=(wk.nbr*-1)+1), color="white")
       + scale_x_continuous( name="", breaks=NULL)
       + scale_y_continuous( name="", breaks=NULL)
       + facet_grid(monthf ~ year)
        );
    
    if(print.days){
      gg<-
        (gg + geom_text(aes(label=day, x=wday.nbr, y=(wk.nbr * -1)), color="white", size=3, vjust= -.75, hjust= -.35))
    }
    
    return(gg);
  }