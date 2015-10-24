\name{Rcalendar-package}
\alias{Rcalendar-package}
\alias{Rcalendar}
\docType{package}
\title{
R package for plotting calendars
}
\description{
Plot data on a calendar
}
\details{
\tabular{ll}{
Package: \tab Rcalendar\cr
Type: \tab Package\cr
Version: \tab 0.1\cr
Date: \tab 2015-10-22\cr
License: \tab Apache 2.0\cr
}
~~ An overview of how to use the package, including the most important functions ~~
}
\author{
Bill -Chopper- Genne

Maintainer: Developer <developer@meaningful-solutions.com>

}
\references{
  
}
\keyword{ package }
\seealso{
  \url{http://meaningful-solutions.com}
}
\examples{
#plot a calendar
ggcal(years=2014:2016);

#table used to build the calendar plot
cal<-createCalendarTable(2014:2016);

}
