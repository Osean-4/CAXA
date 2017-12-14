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
## The output shows that the track species call's jawe, jawe.rble, rble, 
## and rvbu significantly attract Japanese white-eyes in comparison to 
## the intercept (i.e. control). The breeding season and plant origin did 
## not signigicantly influence strength of response.
GLMM.Bin <- function(cax_data_2){
  model1 <- glmer(jawe ~ track.spp + breeding.season + native.plant + 
                    (1 | track.order), data = cax_data_2, family = "binomial")
  model2 <- glmer(rble ~ track.spp + breeding.season + native.plant + 
                    (1 | track.order), data = cax_data_2, family = "binomial")
  model3 <- glmer(rvbu ~ track.spp + breeding.season + native.plant + 
                    (1 | track.order), data = cax_data_2, family = "binomial")
  model4 <- glmer(rwbu ~ track.spp + breeding.season + native.plant + 
                    (1 | track.order), data = cax_data_2, family = "binomial")
  model5 <- glmer(other ~ track.spp + breeding.season + native.plant + 
                    (1 | track.order), data = cax_data_2, family = "binomial")
  return(c(model1, model2, model3, model4, model5))
}
