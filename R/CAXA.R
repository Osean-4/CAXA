## Remove all experiments with track species containing wrsh or 
## zedo. The newly cleaned dataframe (cax_data_1) should contain 
## 573 observations now.
Remove.WRSH.ZEDO <- function(cax_data, track.spp.remove = "wrsh" | "zedo"){
  experiment <- track.spp <- NULL
  cax_data_1 <- cax_data %>%
  dplyr::group_by(experiment) %>%
  dplyr::filter(!any(track.spp == "wrsh")) %>%
  dplyr::filter(!any(track.spp == "zedo"))
  return(cax_data_1)
}
  
## Utilizing the cleaned dataframe (cax_data_1) from 
## Remove.WRSH.ZEDO, this function will now Keep only experiments 
## with all trials of track lengths equal to 15 minutes. The newly 
## cleaned dataframe (cax_data_2) should contain 448 observations now.
Keep.15.Tracks <- function(cax_data_1, track.length.keep = "15"){
  experiment <- track.length <- NULL
  cax_data_2 <- cax_data_1 %>%
  dplyr::group_by(experiment) %>%
  dplyr::filter(!any(!track.length %in% track.length.keep))
  return(cax_data_2)
}

## Utilizing the latest cleaned dataframe (cax_data_2), this function 
## applys a generalized linear mixed model with a binomial distribution.
## Bird species is the response variable; track species, breeding season, 
## and plant origin as fixed effects; order as the one random effect. 
GLMM.Bin <- function(bird.spp){
  model <- glmer(bird.spp ~ track.spp + breeding.season + native.plant + 
            (1 | track.order), data = cax_data_2, family = "binomial")
  summary(model)
}

## Mutate cax_data_2 into a new df with only the presence and absence of 
## frugivores during control and treatment periods; first we make a new
## cloumn combining all presence/absence data from all the frugivores;
## next we make a df with columns track type and frugivore response;
## then we make a matrix with just values of frugivore response as 
## either absent & absent, absent & present, present & absent,
## and present & present between control and treatment periods
## Apply a Wilcoxon signed-rank test to frug.con.treat dataframe to test
## if more frugivores where present during control or treatment periods
Con.Treat.test <- function(cax_data_2){
  cax_data_2$frugivore <- as.integer(cax_data_2$jawe|cax_data_2$rble|
                        cax_data_2$rvbu|cax_data_2$rwbu)
  cax_data_3 <- cax_data_2[,c(4,13)]
  control <- subset(cax_data_3, track.type == "con")
  treatment <- subset(cax_data_3, track.type == "treat")
  cax_data_4 <- cbind(control, treatment)
  frug.con.treat <- cax_data_4[,c(2,4)]
  colnames(frug.con.treat) <- c("Control", "Treatment")
  table(frug.con.treat)
  cax_data_5 <- matrix(c(42, 22, 93, 67), nrow = 2, ncol = 2, byrow = FALSE,
                dimnames = list(c("Control.absence", "Control.presence"),
                c("Treatment.absence", "Treatment.presence")))
mcnemar.test(cax_data_5)
}
