\name{CAXA}
\alias{CAXA}
\alias{Remove.WRSH.ZEDO}
\alias{Keep.15.Tracks}
\alias{GLMM.Bin}
\title{Conspecific Attraction Data Analysis (CAXA)}
\description{
The package contains three functions that help clean and analyze a specific dataset pertaining to a series of conspecific attraction experiments. These series of 80 experiments were designed to investigate the efficacy of attracting fruit-eating birds to fruiting plants. More specifically, these experiments tease apart whether or not birds in this system utilize social information (i.e. vocalizations) when making foraging decisions. And if so, do they rely on conspecific cues, heterospecific cues, or both? Do members within the frugivory dietarty guild utilize eavesdropping when exploiting resources? These are the questions this small study aimed to answer. Birds within 10 meters of the focal were recorded. The three functions are as follows:

The Remove.WRSH.ZEDO function removes experiments containing trials of the track species White-rumped shama (WRSH) and Zebra dove (ZEDO) due to low sampling effort. Low samples were taken due to diet analysis determining that these species are primarily insectivorous or don't respond to conspecific vocalizations.

The Keep.15.Tracks function keeps experiments containing trials equal to 15 minutes in order to standardize methodology and low sampling efforts with track lengths not equal to 15 minutes. Preliminary experiments utilized various track lengths until a standardized methodology was implemented.

The GLMM.Bin function applys a generalized linear mixed model (GLMM) with a binary (Bin) distribution to the data in order to see what bird species significantly respond to track species. Applying this type of model is necessary as the data is not normally distributed (generalized). Additionally, trials were not independent of one another meaning track order must be categorized as a random effect. Therefore, this model will contain both fixed and random effects, hence mixed.

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
\details{This package utilizes a specific dataset in order to run properly. For better understanding regarding this package's functionality please see code{link{caxa_data}}
}
\value{
The function Remove.WRSH.ZEDO(cax_data) will return a dataframe with all experiments containing track species wrsh and zedo removed. This function will utilize the track.spp column primarily, but will remove all variables associated. 

The function Keep.15.Tracks(cax_data_1) will utilize the output of the Remove.WRSH.ZEDO function and return a dataframe with only experiments containing track lengths equal to 15 minutes. This function focuses on the track.length variable.

GLMM.Bin(cax_data_2) will utilize the cleaned dataframe created by functions Remove.WRSH.ZEDO and Keep.15.Bin. Additionally, the function will apply a generalized linear mixed model (GLMM) to the track species Japanese white-eye (jawe), Red-billed leiothrix (rble), Red-vented bulbul (rvbu), Red-whiskered bulbul (rwbu), and all non-fruit eating species (other). Lastly, the function will return a summary of coefficients from each model respectively called model1, model2, model3, model4, and model5. 
}
\references{https://github.com/Osean-4/Final.Project}
\author{Sean E. MacDonald; University of Illinois at Urbana-Champaign}
\note{Contact author @ erroll4@illinois.edu}
\examples{
## Load the required package and dataset
library(CAXA)
data("cax_data")

## Check out the dataframe
View(cax_data)

## Investigate track.spp to see how many experiments utilized 
## each species; notice how few used wrsh and zedo; let's 
## remove those few experiments
table(cax_data$track.spp)

## Remove all experiments with track species containing wrsh or 
## zedo. The newly cleaned dataframe (cax_data_1) should contain 
## 573 observations now
cax_data_1 <- Remove.WRSH.ZEDO(cax_data)
print(cax_data_1)

## Investigate track.length to see how many experiments utilized 
## different lengths; notice how most are 15 minutes long;
## let's keep only those experiments
table(cax_data$track.length)

## Utilizing the cleaned dataframe (cax_data_1) from 
## Remove.WRSH.ZEDO, this function will now Keep only experiments 
## with all trials of track lengths equal to 15 minutes. The newly 
## cleaned dataframe (cax_data_2) should contain 448 observations now
cax_data_2 <- Keep.15.Tracks(cax_data_1)
print(cax_data_2)

## Utilizing the latest cleaned dataframe (cax_data_2), this function 
## applys a generalized linear mixed model with a binomial distribution 
## and will create five models total; one for each bird species or group.
## Bird species is the response variable; track species, breeding season, 
## and plant origin are fixed effects; order is the one random effect. 
GLMM.Bin(cax_data_2)

## The output shows that Japanese white-eye (jawe), when present in the 
## immediate area are significantly attracted to playbacks of jawe, 
## jawe.rble, rble, and rvbu in comparison to the intercept (i.e. control). 
## The breeding season or plant origin did not signigicantly influence 
## strength of response. Interpretation: this species may use both 
## conspecific and heterospecific vocalizations when making foraging 
## decisions.
summary(model1)

## The output shows that Red-billed leiothrix (rble), when present in the 
## immediate area are significantly attracted to playbacks of jawe.rble 
## and rble in comparison to the intercept (i.e. control). The breeding 
## season did not signigicantly influence strength of response, but 
## plant origin did. Interpretation: this species may use conspecific 
## vocalizations when making foraging decisions and prefer exotic plants.
summary(model2)

## The output shows that Red-vented bulbul (rvbu), when present in the 
## immediate area are significantly attracted to playbacks of jawe.rble 
## and rvbu in comparison to the intercept (i.e. control). Plant origin 
## did not signigicantly influence strength of response, but breeding 
## season did. Interpretation: this species may use both 
## conspecific and heterospecific vocalizations when making foraging 
## decisions.
summary(model3)

## The output shows that Red-whiskered bulbul (rwbu), when present in the 
## immediate area are significantly attracted to playbacks of jawe, 
## jawe.rble, rble, and rvbu in comparison to the intercept (i.e. control). 
## The breeding season and plant origin did not signigicantly influence 
## strength of response. Interpretation: this species may use both 
## conspecific and heterospecific vocalizations when making foraging 
## decisions.
summary(model4)

## The output shows that non-frugivorous birds (other), when present in the 
## immediate area are not significantly attracted to any playback in 
## comparison to the intercept (i.e. control). The breeding season and 
## plant origin did not signigicantly influence 
## strength of response. Interpretation: this species may use both 
## conspecific and heterospecific vocalizations when making foraging 
## decisions.
summary(model5)
}