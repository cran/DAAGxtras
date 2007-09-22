\name{DAAGxdb}
\alias{DAAGxdb}
\docType{data}
\title{List, each of whose elements hold rows of a file, in character format}
\description{
  This is the default database for use with the function
  \code{datafile}, which uses elements of this list to place files
  in the working directory. 
}
\usage{data(DAAGxdb)}
\format{  
  Successive elements in this list hold character vectors from which the
  corresponding files can be generated.  The names of the list elements
  are fuel, fuel.csv, oneBadRow, scan-demo, molclock1, molclock2, and
  travelbooks. 
}
\details{
  The files fuel.txt and fuel.csv are used in Chapter 1 of DAAGUR, while
  the files oneBadRow.txt and scan-demo.txt are used in Chapter 14 of
  DAAGUR.}
\source{
  Marland, G., T.A. Boden, and R. J. Andres. 2007. Global, Regional, and
  National Fossil Fuel CO2 Emissions. In Trends: A Compendium of Data on
  Global Change. Carbon Dioxide Information Analysis Center, Oak Ridge
  National Laboratory, U.S. Department of Energy, Oak Ridge, Tenn.,
  U.S.A. (fuel.txt)\\
\url{http://cdiac.ornl.gov/trends/trends.htm}

Rodriguez-Trelles, F., Tarrio, R., and Ayala, F.J. 2001. Erratic
  overdispersion of three molecular clocks: GPDH, SOD and XDH.
  PNAS 98: 11405-11410. (molclock1.txt and molclock2.txt)
}
\references{
  Maindonald, J.H. and Braun, W.J. 2007. Data Analysis and Graphics
  Using R: An Example-Based Approach.  2nd edn, Cambridge University
  Press (DAAGUR).
  }
\examples{
data(DAAGxdb)
names(DAAGxdb)
}
\keyword{datasets}
