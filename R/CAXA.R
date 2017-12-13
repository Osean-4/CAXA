# Remove all experiments that include a track species of "wrsh" and "zedo"
Remove.WRSH.ZEDO <- function(cax_data, track.spp.remove = "wrsh" | "zedo"){
  experiment <- track.spp <- NULL
  cax_data_1 <- cax_data %>%
  dplyr::group_by(experiment) %>%
  dplyr::filter(!any(track.spp == "wrsh")) %>%
  dplyr::filter(!any(track.spp == "zedo"))
  return(cax_data_1)
}
  
# Keep only experiments where all trials have a track length = 15 minutes
Keep.15.Tracks <- function(cax_data_1, track.length.keep = "15"){
  experiment <- track.length <- NULL
  cax_data_2 <- cax_data_1 %>%
  dplyr::group_by(experiment) %>%
  dplyr::filter(!any(!track.length %in% track.length.keep))
  return(cax_data_2)
}

# Apply a generalized linear mixed model (GLMM) with a binomial distribution
# For each GLMM bird species is the response variable; track species, 
# breeding season, and plant origin are fixed effects; order is random effect
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
