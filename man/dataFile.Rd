\name{dataFile}
\alias{dataFile}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Create one or more ASCII data file}
\description{
  Creates, in the working directory, one or more of three data files
  that are useful for demonstrating the use of \code{read.table}
}
\usage{
dataFile(file = c("molclock1", "molclock2", "travelbooks"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{character; "molclock1" for molclock1.txt, "molclock2"
    for molclock2.txt, "travelbooks" for travelbooks.txt.}
}
\value{
     One or more of three ASCII files is produced, and output to the current
     working directory.
}
\author{J.H. Maindonald}
\examples{
dataFile(file="travelbooks")
  
## The function is currently defined as
  function(file=c("molclock1", "molclock2", "travelbooks")){
    datastore <- list(molclock1=c(
                        "Gpdh  Sod  Xdh AvRate  Myr",                          
                        "\"Drosophila subgroups\"  1.50 25.7 30.4   22.4   55",
                        "\"Drosophila subgenera\"  2.00 30.7 29.2   22.3   60",
                        "\"Drosophial genera\"     4.40 34.9 31.7   24.9   65",
                        "\"dipteran families\"     9.25 33.7 25.3   22.0  120",
                        "\"mammalian orders\"     11.60 46.0 17.1   18.7   70",
                        "'animal phyla'         13.20 19.2 19.2   17.5  600",
                        "fungi                40.00 24.9 13.7   21.4  300",
                        "kingdoms             13.00 12.6 11.5   11.9 1100"),
                      molclock2=c("'Level of variation' Gpdh  Sod  Xdh AvRate  Myr",                          
                        "\"Drosophila subgroups\"  1.50 25.7 30.4   22.4   55",
                        "\"Drosophila subgenera\"  2.00 30.7 29.2   22.3   60",
                        "\"Drosophial genera\"     4.40 34.9 31.7   24.9   65",
                        "\"dipteran families\"     9.25 33.7 25.3   22.0  120",
                        "\"mammalian orders\"     11.60 46.0 17.1   18.7   70",
                        "'animal phyla'         13.20 19.2 19.2   17.5  600",
                        "fungi                40.00 24.9 13.7   21.4  300",
                        "kingdoms             13.00 12.6 11.5   11.9 1100"),
                      travelbooks=c("\"thickness\" \"width\" \"height\" \"weight\" \"volume\" \"type\"",
                        "\"Aird\'s Guide to Sydney\" 1.3 11.3 23.9 250 351 \"Guide\"", 
                        "\"Moon\'s Australia handbook\" 3.9 13.1 18.7 840 955 \"Guide\"",    
                        "\"Explore Australia Road Atlas\" 1.2 20 27.6 550 662 \"Roadmaps\"",
                        "\"Australian Motoring Guide\" 2 21.1 28.5 1360 1203 \"Roadmaps\"",
                        "\"Penguin Touring Atlas\" 0.6 25.8 36 640 557 \"Roadmaps\"",
                        "\"Canberra - The Guide\" 1.5 13.1 23.4 420 460 \"Guide\""))
for(nam in file){
    fnam <- paste(nam, ".txt", sep="")
    cat("\nData written to file:", fnam, "\n")
    cat(datastore[[nam]], sep="\n", file=fnam)
  }
    invisible()
  }
  }
\keyword{models}

