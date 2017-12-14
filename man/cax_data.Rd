\name{cax_data}
\alias{cax_data}
\docType{data}
\title{
Conspecific Attraction Experiment (cax) Data for Fruit-eating Birds
}
\description{
The dataset is a product of a series of 80 experiments conducted on the Island of Oahu, Hawaii, USA from 06/2016 - 07/2017. These experiments were designed to investigate the efficacy of attracting fruit-eating birds to fruiting plants. More specifically, on the island of Oahu all fruit eating birds are extinct, but several exotic birds have established populations across the islands. The study investigated whether these introduced birds could be enticed to consume fruit from exotic, native, and endangered plants. Sampling took place across 26 plant species with each experiment on a different individual. Birds were only recorded within 10 meters of focal plant due to low visibility in the cluttered forest. Bird breeding season ranges from approximately March - August in Hawaii. Many more data were collected that are not present in this dataset. Initial experiments utilized track species White-rumped shama (wrsh) and Zebra dove (zedo), but will be removed due to low sampling effort. Low samples were taken due to diet analysis determining that these species are primarily insectivorous or don't respond to conspecific vocalizations. Additionally, experiments with track lengths not equal to 15 minutes will be removed due to low sampling effort. Preliminary experiments utilized various track lengths until a standardized methodology was implemented. Control periods are divided into four, 15-min trials for proper comparision when applying a generalized linear mixed model (GLMM). Applying this type of model is necessary as the data is not normally distributed and that trials were not independent of one another. More specifically, the broadcasting of the first playback influences each subsequent trial and their respective birds' behavioral response strength. As such, track order must be accounted for as a random effect hence the mixed model approach. Fixed effects that were accounted for and of particular interest include breeding season, plant origin, and track species. The latter being the more important in order to determine if birds utilize social information only from their own species' vocalizations or others within their dietary guild (i.e. frugivores). 
}
\usage{data("cax_data")}
\format{
  This dataset contains 80 experiments across 624 rows and 12 columns. Each experiment consists of a control (con) and treatment (treat) period specified under track type. Control and treatment periods have specified lengths found under track length. Treatment trials have certain bird species calls associated under the track species column as either: Japanese white-eye (jawe), Japanese white-eye and Red-billed leiothrix (jawe.rble), Red-billed leiothrix (rble), Red-vented bulbul (rvbu), and Red-whiskered bulbul (rwbu). Control periods are denoted as 'con' under track species. Each trial is associated with an order found under track order. Whether or not an experiment was conducted during birds' breeding season (0 = no, 1 = yes) is denoted under the breeding.season column. Additionally, data regarding the plant species origin is denoted in the native plant column (0 = no, 1 = yes). Lastly, behavioral response to track species playback is denoted by each focal bird species (0 = no response, 1 = attracted): Japanese white-eye (jawe), Red-billed leiothrix (rble), Red-vented bulbul (rvbu), Red-whiskered bulbul (rwbu), and all non-fruit eating species (other). The dataframe includes the following variables:
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
\source{Data collected by Sean E. MacDonald for master's thesis in the Department of Natural Resources and Environmental Sciences, University of Illinois at Urbana-Champaign; Contact author @ erroll4@illinois.edu}
\examples{
data(cax_data)
class(cax_data)
str(cax_data)
}
\keyword{datasets}
