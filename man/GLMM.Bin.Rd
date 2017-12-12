\name{GLMM.Bin}
\alias{GLMM.Bin}
\alias{Keep.15.Tracks}
\alias{Remove.WRSH.ZEDO}
\title{Generalized Linear Mixed Model (GLMM) with a Binary (Bin) Distribution}
\description{
The GLMM.Bin function applys a generalized linear mixed model with a binary distribution to the data in order to see what bird species significantly respond to track species.
}
\usage{
Remove.WRSH.ZEDO(cax_data)
Keep.15.Tracks(cax_data_1)
GLMM.Bin(cax_data_2)
}
\arguments{
  \item{cax_data}{
A dataframe containing all conspecific attraction experiments (cax) collected on fruit-eating birds on Oahu, Hawaii, USA. 
}
  \item{cax_data_1}{
A cleaned dataframe containing conspecific attraction experiments on fruit-eating birds minus experiments containing certain track species (i.e. wrsh, zedo).
}
  \item{cax_data_2}{
A cleaned dataframe containing conspecific attraction experiments on fruit-eating birds minus experiments containing certain track species (i.e. wrsh, zedo) and trials with track lengths not equaling 15 minutes. 
}
}
\value{
GLMM.Bin(cax_data_2) will utilize the cleaned dataframe created by functions Remove.WRSH.ZEDO and Keep.15.Bin and return a summary of coefficients from a generalized linear mixed model applied to the track species Japanese white-eye (jawe).
}
\references{https://github.com/Osean-4/Final.Project}
\author{Sean E. MacDonald; University of Illinois at Urbana-Champaign}
\note{Contact author @ erroll4@illinois.edu}
\examples{
library(CAXA)
library(lme4)
library(dplyr)
data("cax_data")
## Remove all experiments with track species containing wrsh or zedo. The newly cleaned dataframe (cax_data_1) should contain 573 observations now.
cax_data_1 <- Remove.WRSH.ZEDO(cax_data)

## Utilizing the cleaned dataframe (cax_data_1) from Remove.WRSH.ZEDO, this function will now Keep only experiments with all trials of track lengths equal to 15 minutes. The newly cleaned dataframe (cax_data_2) should contain 448 observations now.
cax_data_2 <- Keep.15.Tracks(cax_data_1)

## Utilizing the latest cleaned dataframe (cax_data_2), this function applys a generalized linear mixed model with a binomial distribution
## Bird species is the response variable; track species, breeding season, and plant origin as fixed effects; order as the one random effect. The output shows that the track species call's jawe, jawe.rble, rble, and rvbu significantly attract Japanese white-eyes in comparison to the intercept (i.e. control). The breeding season and plant origin did not signigicantly influence strength of response.
GLMM.Bin(cax_data_2)
}