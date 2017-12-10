\name{CAXanal}
\alias{Remove.WRSH.ZEDO}
\alias{Keep.15.Track}
\alias{GLMM.RM.Bin}
\title{Conspecific Attraction Data Cleaning and Analysis}
\description{
The package contains three functions that clean the data of any unwanted experiments. Firstly, data containing trials of the species White-rumped shama and Zebra dove are removed due to low sampling effort. Secondly, data containing trials not equal to 15 minutes are removed due to low sampling effort. Lastly, a generalized linear mixed model is applied to the data.
}
\usage{
Remove.WRSH.ZEDO(cax_data)
Keep.15.Track(cax_data_1)
GLMM.RM.Bin(cax_data_2)
}
\arguments{
  \item{cax_data}{
Dataframe containing all conspecific attraction experiment (cax) data collected on birds in Hawaii
}
  \item{cax_data_1}{
Dataframe containing conspecific attraction experiment (cax) data collected on birds in Hawaii minus experiments containing wrsh and zedo playbacks
}
  \item{cax_data_2}{
Dataframe containing conspecific attraction experiment (cax) data collected on birds in Hawaii minus experiments containing wrsh and zedo playbacks and trials not equaling 15 minutes
}
}
\details{This package can be applied to any playback study with a binary response variable.}
\value{
Remove.WRSH.ZEDO(cax_data) will return a dataframe with all experiments containing track species wrsh and zedo removed

Keep.15.Track(cax_data_1) will return a dataframe with only experiments containing track lengths equal to 15 minutes

GLMM.RM.Bin(cax_data_2) will return a summary of coefficients from a generalized linear mixed model applied to track species jawe
}
\references{https://github.com/Osean-4/Final.Project}
\author{Sean E. MacDonald}
\note{Contact author at erroll4@illinois.edu}
\examples{
data(cax_data)
## Remove all experiments with wrsh or zedo as track species
Remove.WRSH.ZEDO <- function (cax_data){
  cax_data \%>\%
  group_by(experiment) \%>\%
  filter(!any(track.spp == "wrsh")) \%>\%
  filter(!any(track.spp == "zedo"))
  return(cax_data_1)
}

## Keep only experiments with all trials of track lengths equal to 15 minutes
Keep.15.Tracks <- function(cax_data_1){
  cax_data_1 <- Remove.WRSH.ZEDO(cax_data)
  cax_data_1 %>%
  group_by(experiment) %>%
  filter(!any(track.length != "15"))
  return(cax_data_2)
}

## Apply a generalized linear mixed model with repeated measures and a binomial distribution
## For each glmm have bird species as response variable; track species, breeding season, and plant origin as fixed effects; order as random effect
GLMM.RM.Bin <- function(species){
  cax_data_2 <- Keep.15.Tracks(cax_data_1)
  species <- cax_data_2$jawe
  model <- glmer(species ~ track.spp + breeding.season + native.plant + (1 | track.order), data = cax_data_2, family = binomial)
  summary(model)
}
}
