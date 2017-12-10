# Remove all experiments that include a track species of "wrsh" and "zedo"
Remove.WRSH.ZEDO <- function(cax_data){
  cax_data %>%
  group_by(experiment) %>%
  filter(!any(track.spp == "wrsh")) %>%
  filter(!any(track.spp == "zedo"))
  return(cax_data_1)
}

# Keep only experiments where all trials have a track length = 15 minutes
Keep.15.Tracks <- function(cax_data_1){
  cax_data_1 <- Remove.WRSH.ZEDO(cax_data)
  cax_data_1 %>%
  group_by(experiment) %>%
  filter(!any(track.length != "15"))
  return(cax_data_2)
}

# Apply a generalized linear mixed model with repeated measures and a binomial distribution
# For each glmm have bird species as response variable; track species, breeding season, and plant origin as fixed effects; order as random effect
GLMM.RM.Bin <- function(species){
  cax_data_2 <- Keep.15.Tracks(cax_data_1)
  species <- cax_data_2$jawe
  model <- glmer(species ~ track.spp + breeding.season + native.plant + (1 | track.order), data = cax_data_2, family = binomial)
  summary(model)
}
