\name{Remove.WRSH.ZEDO}
\alias{Remove.WRSH.ZEDO}
\title{Remove White-rumped shama (WRSH) and Zebra dove (ZEDO) experiments}
\description{
The Remove.WRSH.ZEDO function removes experiments containing trials of the track species White-rumped shama (WRSH) and Zebra dove (ZEDO) due to low sampling effort.
}
\usage{
Remove.WRSH.ZEDO(cax_data)
}
\arguments{
  \item{cax_data}{
A dataframe containing all conspecific attraction experiments (cax) collected on fruit-eating birds on Oahu, Hawaii, USA. 
}
}
\value{
The function Remove.WRSH.ZEDO(cax_data) will return a dataframe with all experiments containing track species wrsh and zedo removed.
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
}