\name{fishRivers}
\alias{fishRivers}
\docType{data}
\title{
Characteristics of river reference sites
}
\description{
  Data on river sites was matched with data on fish caught at
  nearby fishing sites.  Fishing sites were classified according
  to type of fish (\code{Group} = fish river type).  
}
\usage{data(fishRivers)}
\format{
  A data frame with 128 observations on the following 23 variables.
  \describe{
    \item{\code{Group}}{Fish river types; a factor with levels \code{F1}
      \code{F2} \code{F3} \code{F4} \code{F5} \code{F6}
    }
    \item{\code{Alk}}{mgm/l of CaCO3}
    \item{\code{Avrain}}{mean annual rainfall, mm}
    \item{\code{Bedrock}}{\% cover on river edge attached
      to substratum}
    \item{\code{Boulder}}{numeric, \% cover of stones \eqn{>} 200mm diameter}
    \item{\code{Cobble}}{numeric, \% cover of stones between 60 \& 200mm}
    \item{\code{Cond}}{electrical conductivity (\eqn{mu S/cm})}
    \item{\code{Dis}}{maximum distance from source (m)}
    \item{\code{DO}}{numeric, dissolved O2, \% saturation}
    \item{\code{Elev}}{nearest contour line (m) below site}
    \item{\code{Fine}}{numeric, \% cover of particles \eqn{<} 0.02mm}
    \item{\code{Gravel}}{numeric, \% cover of particles between 2 \& 20mm}
    \item{\code{Lat}}{latitude}
    \item{\code{Long}}{longitude}
    \item{\code{NoxN}}{total oxidised Nitrogen (mgl/l)}
    \item{\code{Pebble}}{numeric, \% cover of stones between 20 \& 60mm}
    \item{\code{Ph}}{a numeric vector}
    \item{\code{Sand}}{numeric, \% cover of particles between 0.02 \& 2mm}
    \item{\code{Slope}}{numeric, elevatioon difference (m) between site
    \&
  a point 1km upstream}
    \item{\code{Tkn}}{total Kjeldahl Nitrogen (mgl/l)}
    \item{\code{Tpl}}{total Phosphorus (mgl/l)}
    \item{\code{Turb}}{turbidity (NTU)}
    \item{\code{Width}}{modal river width (m), assessed visually}
  }
}
\details{
  To what extent can the fish river type be predicted, based on: (i) all
  explanatory variables; (ii) the variables \code{Avrain}, \code{Dis},
  \code{Elev}, \code{Lat}, \code{Long} and \code{Slope}.  The second set
  comprises the variables that would be used in practice to predict
  the fish river type at other sites.
}
\source{
Data relate to Turak and Koop (2007).
}
\references{
  Turak, E and Koop, K. Multi-attribute ecological river typology for
  assessing ecological condition and conservation
  planning. Hydrobiologica 603:83-104, 2007.
}
\examples{
data(fishRivers)
library(MASS)
fish.lda <- lda(Group ~ Avrain + Dis +Elev + Lat + Long + Slope,
               data=fishRivers)
}
\keyword{datasets}
