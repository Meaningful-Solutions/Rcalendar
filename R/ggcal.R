require(ggplot2);
ggcal<-
  function(years, mo=NULL, q=NULL){
    cal<-
      createCalendarTable(years=years, mo=mo, q=q);
    
    gg<-
      ggplot( cal );
    
    gg<-
      (gg
       + geom_rect(aes(xmin=wday.nbr, xmax=wday.nbr+1, ymin=wk.nbr*-1, ymax=(wk.nbr*-1)+1), color="white")
       + scale_x_continuous( label="", breaks=NULL)
       + scale_y_continuous( label="", breaks=NULL)
       + facet_grid(monthf ~ year)
        );
    
    return(gg);
  }