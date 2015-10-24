\name{ggcal}
\alias{ggcal}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create a calendar plot using ggplot2
}
\description{
Returns a calendar as ggplot2 object.
}
\usage{
ggcal(years, mo = NULL, q = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{years}{
A single year, or a vector of years. They do not need to be sequential.
}
  \item{mo}{
Optional. The 3 character abbreviation in month.abb, which will limit the result to the specific month.
}
  \item{q}{
Optional. The integer for a quarter which will limit the result to the 3 months in the specified quarter.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\url{http://meaningful-solutions.com}
}
\examples{
gc<-ggcal(2014:2016);
print(gc);
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
