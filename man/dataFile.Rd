\name{datafile}
\alias{datafile}
\title{Write an ASCII data file to the working directory.}
\description{
  Invoking this function writes one or more nominated files to the
  working directory.  In particular, it may be used to write the
  files 'fuel.txt' and 'fuel.csv' that are used in Chapter 1 of
  DAAGUR, and the files 'oneBadRow.txt' and 'scan-demo.txt' that
  are used in Chapter 14 of DAAGUR.
}
\usage{
datafile(file = c("fuel", "travelbooks"), datastore = DAAGxdb,
altstore = zzDAAGxdb, showNames = FALSE)
}
\arguments{
  \item{file}{character; with the defaults for \code{datastore} and
    \code{altstore} the options are
    "fuel", for fuel.txt;
 "fuel.csv", for fuel.csv;
 "oneBadRow", for oneBadRow.txt;
 "scan-demo", for scan-demo.txt;
 "molclock1", for molclock1.txt;
 "molclock2", for molclock2.txt;
 "travelbooks", for travelbooks.txt;
 "bestTimes", for bestTimes.txt;
 "bostonc", for bostonc.txt
}
  \item{datastore}{Each element of this list is a character vector
that holds the rows of a file.}
  \item{altstore}{An alternative list.  The default alternative list 
is used for the two files that are more than a few lines.}
  \item{showNames}{if \code{TRUE}, returns the names of available datasets.}
}
\value{  
An ASCII file is output to the current working directory. The names of
all available datasets are returned invisibly.
}
\author{J.H. Maindonald}
\examples{
datafile(file="", showNames=TRUE)
}
\keyword{models}
