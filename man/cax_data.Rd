\name{cax_data}
\alias{cax_data}
\docType{data}
\title{
Conspecific Attraction Experiment (cax) Data for Fruit-eating Birds
}
\description{
The dataframe is a series of 80 experiments with control and treatment periods. Control periods did not include a speaker. During treatment periods bird species' tracks were broadcasted in various orders and lengths (min). Whether or not the experiment was conducted during the bird's breeding season (0 = no, 1 = yes), on a native plant species (0 = no, 1 = yes), and the behavioral response of fruit-eating birds (0 = no response, 1 = attracted) are included. Japanese white-eye (jawe), Red-billed leiothrix (rble), Red-vented bulbul (rvbu), Red-whiskered bulbul (rwbu), and all non-fruit eating species (other).
}
\usage{data("cax_data")}
\format{
  A data frame with 624 observations on the following 12 variables.
  \describe{
    \item{\code{experiment}}{a character vector}
    \item{\code{track.order}}{a character vector}
    \item{\code{track.spp}}{a character vector}
    \item{\code{track.type}}{a character vector}
    \item{\code{track.length}}{a numeric vector}
    \item{\code{breeding.season}}{a numeric vector}
    \item{\code{native.plant}}{a numeric vector}
    \item{\code{jawe}}{a numeric vector}
    \item{\code{rble}}{a numeric vector}
    \item{\code{rvbu}}{a numeric vector}
    \item{\code{rwbu}}{a numeric vector}
    \item{\code{other}}{a numeric vector}
  }
}
\source{Data collected by Sean E. MacDonald; erroll4@illinois.edu}
\examples{
data(cax_data)
str(cax_data)
}
\keyword{datasets}
