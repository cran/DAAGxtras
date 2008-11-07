% --- Source file: bomregions.Rd ---
\name{bomregions}
\alias{bomregions}
\title{Southern Oscillation Index Data}
\usage{bomregions}
\description{
The Southern Oscillation Index (SOI) is the difference in barometric
pressure at sea level between Tahiti and Darwin.  Annual SOI, Australian
regional rainfall data, and Australian regional temperature data are
given for the years 1900-2007.  The regional rainfall and temperature
data are area-weighted averages for the respective regions.
}
\format{
  This data frame contains the following columns:
  \describe{
\item{Year}{Year}
\item{eastRain}{east rain}
\item{mdRain}{Murray-Darling basin rainfall}
\item{northRain}{north rain}
\item{seRain}{southeast rain}
\item{southRain}{south rain}
\item{swRain}{southwest rain}
\item{eastAVt}{east temperature}
\item{mdAVt}{Murray-Darling basin temperature}
\item{northAVt}{north temperature}
\item{seAVt}{southeast temperature}
\item{southAVt}{south temperature}
\item{swAVt}{southwest temperature}
\item{SOI}{Annual average Southern Oscillation Index}
\item{sunspot}{Annual average sunspot counts}
\item{auAVrain}{Australian average rainfall, area weighted}
\item{auAVt}{Australian average temperature, area-weighted mean}
  } 
} 
\source{
Australian Bureau of Meteorology web pages:

\url{http://www.bom.gov.au/climate/change/} 
\url{http://www.bom.gov.au/climate/current/soihtm1.shtml}

Regions are identified on a map that can be found at:
\url{http://www.bom.gov.au/silo/products/cli_chg/rain_timeseries.shtml}

Sunspot data is from \url{http://sidc.oma.be/sunspot-data/}
} 
\references{
Lavery, B., Joung, G. and Nicholls, N. 1997. An extended high-quality
historical rainfall dataset for Australia. Australian Meteorological
Magazine, 46, 27-38.

Nicholls, N., Lavery, B., Frederiksen, C.\ and Drosdowsky, W. 1996.
Recent apparent changes in relationships between the El Nino --
southern oscillation and Australian rainfall and temperature.
Geophysical Research Letters 23: 3357-3360.
} 
\examples{ 
plot(ts(bomregions[, c("mdRain","SOI")], start=1900), 
     panel=function(y,...)panel.smooth(bomregions$Year, y,...))
avrain <- bomregions[,"mdRain"]
xbomsoi <- with(bomregions, data.frame(SOI=SOI, cuberootRain=avrain^0.33))
xbomsoi$trendSOI <- lowess(xbomsoi$SOI, f=0.1)$y
xbomsoi$trendRain <- lowess(xbomsoi$cuberootRain, f=0.1)$y
xbomsoi$detrendRain <-
  with(xbomsoi, cuberootRain - trendRain + mean(trendRain))    
xbomsoi$detrendSOI <-
  with(xbomsoi, SOI - trendSOI + mean(trendSOI))
## Plot time series avrain and SOI: ts object xbomsoi
plot(ts(xbomsoi[, c("cuberootRain","SOI")], start=1900), 
     panel=function(y,...)panel.smooth(1900:2007, y,...),
     xlab = "Year", main="", ylim=list(c(250, 800),c(-20,25)))
par(mfrow=c(1,2))
rainpos <- pretty(xbomsoi$cuberootRain^3, 6)
plot(cuberootRain ~ SOI, data = xbomsoi, 
     ylab = "Rainfall (cube root scale)", yaxt="n")
axis(2, at = rainpos^0.33, labels=paste(rainpos))
mtext(side = 3, line = 0.8, "A", adj = -0.025)
with(xbomsoi, lines(lowess(cuberootRain ~ SOI, f=0.75)))
plot(detrendRain ~ detrendSOI, data = xbomsoi,
     xlab="Detrended SOI", ylab = "Detrended rainfall", yaxt="n")
axis(2, at = rainpos^0.33, labels=paste(rainpos)) 
with(xbomsoi, lines(lowess(detrendRain ~ detrendSOI, f=0.75)))
mtext(side = 3, line = 0.8, "B", adj = -0.025)
par(mfrow=c(1,1))
} 
\keyword{datasets}
