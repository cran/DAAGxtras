% --- Source file: bomregions.Rd ---
\name{bomregions}
\alias{bomregions}
\title{Australian and Related Historical Annual Climate Data, by region}
\usage{bomregions}
\description{
  Australian regional temperature data, Australian regional rainfall data,
and Annual SOI, are given for the years 1900-2008.  The regional
rainfall and temperature data are area-weighted averages for the
respective regions.  The Southern Oscillation Index (SOI) is the
difference in barometric pressure at sea level between Tahiti and
Darwin.
}
\format{
  This data frame contains the following columns:
  \describe{
\item{Year}{Year}
\item{eastAVt}{Eastern temperature}
\item{seAVt}{Southeastern region average temperature (degrees C)}
\item{southAVt}{Southern temperature}
\item{swAVt}{Southwestern temperature}
\item{westAVt}{Western temperature}
\item{northAVt}{Northern temperature}
\item{mdbAVt}{Murray-Darling basin temperature}
\item{auAVt}{Australian average temperature, area-weighted mean}
\item{eastRain}{Eastern rainfall}
\item{seRain}{Southeast Australian annual rainfall (mm)}
\item{southRain}{Southern rainfall}
\item{swRain}{Southwest rainfall}
\item{westRain}{Western rainfall}
\item{northRain}{Northern rainfall}
\item{mdbRain}{Murray-Darling basin rainfall}
\item{auRain}{Australian average rainfall, area weighted}
\item{SOI}{Annual average Southern Oscillation Index}
\item{co2mlo}{Moana Loa CO2 concentrations, from 1959}
\item{co2law}{Moana Loa CO2 concentrations, 1900 to 1978}
\item{CO2}{CO2 concentrations, composite series}
\item{sunspot}{Annual average sunspot counts}
  } 
} 
\source{
Australian Bureau of Meteorology web pages:

\url{http://www.bom.gov.au/climate/change/} 
\url{http://www.bom.gov.au/climate/current/soihtm1.shtml}

Regions are identified on a map that can be found at:
\url{http://www.bom.gov.au/silo/products/cli_chg/rain_timeseries.shtml}

The CO2 series \code{co2law}, from
\url{http://cdiac.ornl.gov/trends/co2/lawdome.html}, is from Law Dome
ice core data.

The CO2 series \code{co2mlo} is from Dr. Pieter Tans, NOAA/ESRL
(\url{www.esrl.noaa.gov/gmd/ccgg/trends/})

The series \code{CO2} is a composite series, obtained by adding 0.46 to
he Law data for 1900 to 1958, then following this with the Moana Loa
data that is avaiable from 1959.  The addition of 0.46 is designed so
that the averages from the two series agree for the period 1959 to
1968

Sunspot data is from \url{http://sidc.oma.be/sunspot-data/}
} 
\references{
D.M. Etheridge, L.P. Steele, R.L. Langenfelds, R.J. Francey,
J.-M. Barnola and V.I. Morgan, 1998, \emph{Historical CO2 records from the
Law Dome DE08, DE08-2, and DSS ice cores}, in Trends: A Compendium of
Data on Global Change, on line at Carbon Dioxide Information Analysis
Center, Oak Ridge National Laboratory, U.S. Department of Energy, Oak
Ridge, Tenn., U.S.A. \url{http://cdiac.ornl.gov/trends/co2/lawdome.html}
  
Lavery, B., Joung, G. and Nicholls, N. 1997. An extended high-quality
historical rainfall dataset for Australia. Australian Meteorological
Magazine, 46, 27-38.

Nicholls, N., Lavery, B., Frederiksen, C.\ and Drosdowsky, W. 1996.
Recent apparent changes in relationships between the El Nino --
southern oscillation and Australian rainfall and temperature.
Geophysical Research Letters 23: 3357-3360.
} 
\examples{ 
plot(ts(bomregions[, c("mdbRain","SOI")], start=1900), 
     panel=function(y,...)panel.smooth(bomregions$Year, y,...))
avrain <- bomregions[,"mdbRain"]
xbomsoi <- with(bomregions, data.frame(Year=Year, SOI=SOI,
                cuberootRain=avrain^0.33))
xbomsoi$trendSOI <- lowess(xbomsoi$SOI, f=0.1)$y
xbomsoi$trendRain <- lowess(xbomsoi$cuberootRain, f=0.1)$y
xbomsoi$detrendRain <-
  with(xbomsoi, cuberootRain - trendRain + mean(trendRain))    
xbomsoi$detrendSOI <-
  with(xbomsoi, SOI - trendSOI + mean(trendSOI))
## Plot time series avrain and SOI: ts object xbomsoi
plot(ts(xbomsoi[, c("cuberootRain","SOI")], start=1900), 
     panel=function(y,...)panel.smooth(xbomsoi$Year, y,...),
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
