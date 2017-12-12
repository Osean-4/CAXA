\name{Keep.15.Tracks}
\alias{Keep.15.Tracks}
\alias{Remove.WRSH.ZEDO}
\title{Keep All Experiments with a Track Length of 15 Minutes}
\description{
The Remove.WRSH.ZEDO function removes experiments containing trials of the track species White-rumped shama (WRSH) and Zebra dove (ZEDO) due to low sampling effort. The Keep.15.Tracks function keeps experiments containing trials equal to 15 minutes in order to standardize methodology and low sampling efforts with track lengths not equal to 15 minutes.
}
\usage{
Remove.WRSH.ZEDO(cax_data)
Keep.15.Tracks(cax_data_1)
}
\arguments{
  \item{cax_data}{
A dataframe containing all conspecific attraction experiments (cax) collected on fruit-eating birds on Oahu, Hawaii, USA. 
}
  \item{cax_data_1}{
A cleaned dataframe containing conspecific attraction experiments on fruit-eating birds minus experiments containing certain track species (i.e. wrsh, zedo).
}
}
\value{
The function Remove.WRSH.ZEDO(cax_data) will return a dataframe with all experiments containing track species wrsh and zedo removed.

The function Keep.15.Tracks(cax_data_1) will utilize the output of the Remove.WRSH.ZEDO function and return a dataframe with only experiments containing track lengths equal to 15 minutes.
}
\references{https://github.com/Osean-4/Final.Project}
\author{Sean E. MacDonald; University of Illinois at Urbana-Champaign}
\note{Contact author @ erroll4@illinois.edu}
\examples{
library(CAXA)
library(dplyr)
data("cax_data")
## Remove all experiments with track species containing wrsh or zedo. The newly cleaned dataframe (cax_data_1) should contain 573 observations now.
cax_data_1 <- Remove.WRSH.ZEDO(cax_data)

## Utilizing the cleaned dataframe (cax_data_1) from Remove.WRSH.ZEDO, this function will now Keep only experiments with all trials of track lengths equal to 15 minutes. The newly cleaned dataframe (cax_data_2) should contain 448 observations now.
cax_data_2 <- Keep.15.Tracks(cax_data_1)
}