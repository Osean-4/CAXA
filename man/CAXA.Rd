\name{CAXA}
\alias{CAXA}
\alias{Remove.WRSH.ZEDO}
\alias{Keep.15.Tracks}
\alias{GLMM.Bin}
\title{Conspecific Attraction Data Analysis (CAXA)}
\description{
The package contains three functions that help clean and analyze data. The Remove.WRSH.ZEDO function removes experiments containing trials of the track species White-rumped shama (WRSH) and Zebra dove (ZEDO) due to low sampling effort. The Keep.15.Tracks function keeps experiments containing trials equal to 15 minutes in order to standardize methodology and low sampling efforts with track lengths not equal to 15 minutes. Lastly, the GLMM.Bin function applys a generalized linear mixed model with a binary distribution to the data in order to see what bird species significantly respond to track species.
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
\details{This package can be applied to any study utilizing a behavioral stimulus to investigate an organism's response.See code{link{caxa_data}}}
\value{
The function Remove.WRSH.ZEDO(cax_data) will return a dataframe with all experiments containing track species wrsh and zedo removed.

The function Keep.15.Tracks(cax_data_1) will utilize the output of the Remove.WRSH.ZEDO function and return a dataframe with only experiments containing track lengths equal to 15 minutes.

GLMM.Bin(cax_data_2) will utilize the cleaned dataframe created by functions Remove.WRSH.ZEDO and Keep.15.Bin and return a summary of coefficients from a generalized linear mixed model applied to the track species Japanese white-eye (jawe).
}
\references{https://github.com/Osean-4/Final.Project}
\author{Sean E. MacDonald; University of Illinois at Urbana-Champaign}
\note{Contact author @ erroll4@illinois.edu}
\examples{
## Load the required packages and dataset
library(CAXA)
data("cax_data")

## Change class of track order to factor since the model treats 
## it as a 
cax_data$track.order <- as.factor(cax_data$track.order)
View(cax_data)
## Remove all experiments with track species containing wrsh or 
## zedo. The newly cleaned dataframe (cax_data_1) should contain 
## 573 observations now.
cax_data_1 <- Remove.WRSH.ZEDO(cax_data)
print(cax_data_1)

## Utilizing the cleaned dataframe (cax_data_1) from 
## Remove.WRSH.ZEDO, this function will now Keep only experiments 
## with all trials of track lengths equal to 15 minutes. The newly 
## cleaned dataframe (cax_data_2) should contain 448 observations now.
cax_data_2 <- Keep.15.Tracks(cax_data_1)
print(cax_data_2)

## Utilizing the latest cleaned dataframe (cax_data_2), this function 
## applys a generalized linear mixed model with a binomial distribution.
## Bird species is the response variable; track species, breeding season, 
## and plant origin as fixed effects; order as the one random effect. 
## The output shows that the track species call's jawe, jawe.rble, rble, 
## and rvbu significantly attract Japanese white-eyes in comparison to 
## the intercept (i.e. control). The breeding season and plant origin did 
## not signigicantly influence strength of response.
GLMM.Bin(cax_data_2)
}