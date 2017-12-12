# Remove all experiments that include a track species of "wrsh" and "zedo"
Remove.WRSH.ZEDO <- function(cax_data){
  experiment <- track.spp <- NULL
  cax_data_1 <- cax_data %>%
  dplyr::group_by(experiment) %>%
  dplyr::filter(!any(track.spp == "wrsh")) %>%
  dplyr::filter(!any(track.spp == "zedo"))
  print(cax_data_1)
}
  
# Keep only experiments where all trials have a track length = 15 minutes
Keep.15.Tracks <- function(cax_data_1){
  experiment <- track.length <- NULL
  cax_data_2 <- cax_data_1 %>%
  dplyr::group_by(experiment) %>%
  dplyr::filter(!any(track.length != "15"))
  print(cax_data_2)
}

# Apply a generalized linear mixed model with repeated measures and a binomial distribution
# For each glmm have bird species as response variable; track species, breeding season, and plant origin as fixed effects; order as random effect
GLMM.Bin <- function(cax_data_2){
  species <- cax_data_2$jawe
  model <- glmer(species ~ track.spp + breeding.season + native.plant + (1 | track.order), data = cax_data_2, family = "binomial")
  summary(model)
}
