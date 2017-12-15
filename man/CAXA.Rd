\name{CAXA}
\alias{CAXA}
\alias{Remove.WRSH.ZEDO}
\alias{Keep.15.Tracks}
\alias{GLMM.Bin}
\alias{Con.Treat.test}
\title{Conspecific Attraction Data Analysis (CAXA)}
\description{
The package contains three functions that help clean and analyze a specific dataset pertaining to a series of conspecific attraction experiments. These series of 80 experiments were designed to investigate the efficacy of attracting fruit-eating birds to fruiting plants. More specifically, these experiments tease apart whether or not birds in this system utilize social information (i.e. vocalizations) when making foraging decisions. And if so, do they rely on conspecific cues, heterospecific cues, or both? Do members within the frugivory dietarty guild utilize eavesdropping when exploiting resources? These are the questions this small study aimed to answer. Birds within 10 meters of the focal were recorded. The three functions are as follows:

The Remove.WRSH.ZEDO function removes experiments containing trials of the track species White-rumped shama (WRSH) and Zebra dove (ZEDO) due to low sampling effort. Low samples were taken due to diet analysis determining that these species are primarily insectivorous or don't respond to conspecific vocalizations.

The Keep.15.Tracks function keeps experiments containing trials equal to 15 minutes in order to standardize methodology and low sampling efforts with track lengths not equal to 15 minutes. Preliminary experiments utilized various track lengths until a standardized methodology was implemented.

The GLMM.Bin function applys a generalized linear mixed model (GLMM) with a binary (Bin) distribution to the data in order to see what bird species significantly respond to track species. Applying this type of model is necessary as the data is not normally distributed (generalized). Additionally, trials were not independent of one another meaning track order must be categorized as a random effect. Therefore, this model will contain both fixed and random effects, hence mixed.

The Con.Treat.test function mutates the cax_data_2 dataframe created by the Remove.WRSH.ZEDO and Keep.15.Tracks functions into a matrix that contains overall frugivore behavioral response to broadcasted playbacks (treatment) in comparision to control periods. This function achieves this by creating a new column by combining all responses of Japanese white-eye (jawe), Red-billed leiothrix (rble), Red-vented bulbul (rvbu), and Red-whiskered bulbul (rwbu). Then this function creates a new dataframe with a column of track type (control or treatment) and frugivore response (0 = absence, 1 = presence). Following this the function creates a table of four categories including absent & absent, absent & present, present & absent, and present & present then turns it into a matrix. Finally, the McNemar's Chi-squared test is applied to this matrix and its output generated.
}
\usage{
Remove.WRSH.ZEDO(cax_data, track.spp.remove = "wrsh" | "zedo")
Keep.15.Tracks(cax_data_1, track.length.keep = "15")
GLMM.Bin(bird.spp)
Con.Treat.test(cax_data_2)
}
\arguments{
  \item{cax_data}{
A dataframe containing all conspecific attraction experiments (cax) collected on fruit-eating birds on Oahu, Hawaii, USA.
}
  \item{cax_data_1}{
A cleaned dataframe containing conspecific attraction experiments on fruit-eating birds minus experiments containing certain track species (i.e. wrsh, zedo).
}
  \item{cax_data_2}{
A cleaned dataframe containing conspecific attraction experiments on fruit-eating birds minus experiments containing certain track species (i.e. wrsh, zedo) and track lengths not equal to 15 minutes.
}
  \item{track.spp.remove}{
An object containing the track species to be removed from the dataframe cax_data.
}
  \item{track.length.keep}{
An object containing track lengths to be kept from the dataframe caxa_data_1.
}
  \item{bird.spp}{
An object containing bird species from the dataframe caxa_data_2.
}
}
\details{This package utilizes a specific dataset in order to run properly. For better understanding regarding this package's functionality please see code{link{caxa_data}}
}
\value{
Remove.WRSH.ZEDO(cax_data) will return a dataframe with all experiments containing track species wrsh and zedo removed. This function will utilize the track.spp column primarily, but will remove all variables associated. 

Keep.15.Tracks(cax_data_1) will utilize the output of the Remove.WRSH.ZEDO function and return a dataframe with only experiments containing track lengths equal to 15 minutes. This function focuses on the track.length variable.

GLMM.Bin(bird.spp) will utilize the cleaned dataframe created by functions Remove.WRSH.ZEDO and Keep.15.Bin. Additionally, the function will apply a generalized linear mixed model (GLMM) to the track species Japanese white-eye (jawe), Red-billed leiothrix (rble), Red-vented bulbul (rvbu), Red-whiskered bulbul (rwbu), and all non-fruit eating species (other). Lastly, the function will return a summary of coefficients from each model respectively called model1, model2, model3, model4, and model5. 

Con.Treat.test(cax_data_2) will return the output of the McNemar's Chi-squared Test including its test statistic, degrees of freedom of the approximate chi-squared distribution of the test statistic, and the p-value of the test. The test is applied to a 2 x 2 matrix, which was created from the cax_data_2 dataframe created by the Remove.WRSH.ZEDO and Keep.15.Tracks functions.
}
\references{https://github.com/Osean-4/Final.Project}
\author{Sean E. MacDonald; University of Illinois at Urbana-Champaign}
\note{Contact author @ erroll4@illinois.edu}
\examples{
## Load the required package and dataset
library(CAXA)
data("cax_data")

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

## Mutate cax_data_2 into a new df with only the presence and absence of 
## frugivores during control and treatment periods; first we make a new
## cloumn combining all presence/absence data from all the frugivores;
## next we make a df with columns track type and frugivore response;
## then we make a matrix with just values of frugivore response as 
## either absent & absent, absent & present, present & absent,
## and present & present between control and treatment periods
## Apply a Wilcoxon signed-rank test to frug.con.treat dataframe to test
## if more frugivores where present during control or treatment periods
Con.Treat.test(cax_data_2)

## Utilizing the latest cleaned dataframe (cax_data_2), this function 
## applys a generalized linear mixed model to Japanese white-eye (jawe)
model1 <- GLMM.Bin(cax_data_2$jawe)

## The output shows that Japanese white-eye (jawe), when present in the 
## immediate area are significantly attracted to playbacks of jawe, 
## jawe.rble, rble, and rvbu in comparison to the intercept (i.e. control). 
## The breeding season or plant origin did not signigicantly influence 
## strength of response. Interpretation: this species may use both 
## conspecific and heterospecific vocalizations when making foraging 
## decisions.
summary(model1)

## Utilizing the latest cleaned dataframe (cax_data_2), this function 
## applys a generalized linear mixed model to Red-billed leiothrix (rble)
model2 <- GLMM.Bin(cax_data_2$rble)

## The output shows that Red-billed leiothrix (rble), when present in the 
## immediate area are significantly attracted to playbacks of jawe.rble 
## and rble in comparison to the intercept (i.e. control). The breeding 
## season did not signigicantly influence strength of response, but 
## plant origin did. Interpretation: this species may use conspecific 
## vocalizations when making foraging decisions and prefer exotic plants.
summary(model2)

## Utilizing the latest cleaned dataframe (cax_data_2), this function 
## applys a generalized linear mixed model to Red-vented bulbul (rvbu)
model3 <- GLMM.Bin(cax_data_2$rvbu)

## The output shows that Red-vented bulbul (rvbu), when present in the 
## immediate area are significantly attracted to playbacks of jawe.rble 
## and rvbu in comparison to the intercept (i.e. control). Plant origin 
## did not signigicantly influence strength of response, but breeding 
## season did. Interpretation: this species may use both 
## conspecific and heterospecific vocalizations when making foraging 
## decisions and be more gregarious during the breeding season.
summary(model3)

## Utilizing the latest cleaned dataframe (cax_data_2), this function 
## applys a generalized linear mixed model to Red-whiskered bulbul (rwbu)
model4 <- GLMM.Bin(cax_data_2$rwbu)

## The output shows that Red-whiskered bulbul (rwbu), when present in the 
## immediate area are significantly attracted to playbacks of rwbu in 
## comparison to the intercept (i.e. control). Plant origin did not 
## signigicantly influence strength of response, but breeding season did.
## Interpretation: this species may use conspecific vocalizations when 
## making foraging decisions and be more grergarious during the breeding
## season.
summary(model4)

## Utilizing the latest cleaned dataframe (cax_data_2), this function 
## applys a generalized linear mixed model to non-frugivorous birds (other)
model5 <- GLMM.Bin(cax_data_2$other)

## The output shows that non-frugivorous birds (other), when present in 
## the immediate area ara significantly attracted to playbacks of 
## jawe.rble in comparison to the intercept (i.e. control). Plant origin 
## did not signigicantly influence strength of response, but breeding 
## season did. Interpretation: these species may respond to alarm 
## vocalizations of heterospecifics.
summary(model5)
}