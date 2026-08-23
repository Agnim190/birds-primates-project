# 01_statistical_analysis.R
# Statistical analysis for the birds-primates interactions project
# Run this script from the repository root.

library(tidyverse)
library(glmmTMB)
library(DHARMa)
library(performance)
library(emmeans)

# Load and prepare data

birds_raw <- read.csv(
  "data/processed/Birds_primates_interactions_species_wide_ABCDEFGH.csv",
  stringsAsFactors = FALSE
)

birds <- birds_raw %>%
  separate_wider_delim(
    time,
    delim = ":",
    names = c("time_hour", "time_minute"),
    cols_remove = FALSE
  ) %>%
  mutate(
    time_hour = as.numeric(time_hour),
    time_minute = as.numeric(time_minute),
    time_min = time_hour * 60 + time_minute,
    time_z = as.numeric(scale(time_min))
  )

birds_primate <- birds %>%
  filter(experiment == "primate") %>%
  mutate(
    condition = factor(
      condition,
      levels = c("control", "post_playback")
    ),
    playback_species = factor(
      playback_species,
      levels = c("CH", "GM", "BC", "RC")
    ),
    monkey_presence = ifelse(
      monkey_present == "0",
      "absent",
      "present"
    ),
    monkey_presence = factor(
      monkey_presence,
      levels = c("absent", "present")
    )
  )

table(birds_primate$playback_species, birds_primate$condition)
table(birds_primate$monkey_presence, useNA = "ifany")


# AIM 1
# Does primate playback have an overall effect on bird species richness?

mod1_poisson <- glmmTMB(
  species_richness_total ~
    condition +
    time_z +
    monkey_presence +
    (1 | day:net),
  family = poisson,
  data = birds_primate
)

summary(mod1_poisson)

set.seed(123)

res_mod1_poisson <- simulateResiduals(
  mod1_poisson,
  n = 1000
)

plot(res_mod1_poisson)
testUniformity(res_mod1_poisson)
testDispersion(res_mod1_poisson)
testOutliers(res_mod1_poisson)

# The Poisson model showed underdispersion, so the final model
# uses a COM-Poisson distribution.
mod1 <- glmmTMB(
  species_richness_total ~
    condition +
    time_z +
    monkey_presence +
    (1 | day:net),
  family = compois,
  data = birds_primate
)

summary(mod1)
VarCorr(mod1)

performance::check_convergence(mod1)
performance::check_singularity(mod1)

set.seed(123)

res_mod1 <- simulateResiduals(
  mod1,
  n = 5000
)

plot(res_mod1)
testUniformity(res_mod1)
testDispersion(res_mod1)
testOutliers(res_mod1)

# AIM 2
# Does the playback effect differ among primate species?

mod2 <- glmmTMB(
  species_richness_total ~
    condition * playback_species +
    time_z +
    monkey_presence +
    (1 | day:net),
  family = compois,
  data = birds_primate
)

summary(mod2)
VarCorr(mod2)

performance::check_convergence(mod2)
performance::check_singularity(mod2)

set.seed(123)

res_mod2 <- simulateResiduals(
  mod2,
  n = 5000
)

plot(res_mod2)
testUniformity(res_mod2)
testDispersion(res_mod2)
testOutliers(res_mod2)

mod2_no_interaction <- glmmTMB(
  species_richness_total ~
    condition +
    playback_species +
    time_z +
    monkey_presence +
    (1 | day:net),
  family = compois,
  data = birds_primate
)

summary(mod2_no_interaction)

performance::check_convergence(mod2_no_interaction)
performance::check_singularity(mod2_no_interaction)

set.seed(123)

res_mod2_no_interaction <- simulateResiduals(
  mod2_no_interaction,
  n = 5000
)

plot(res_mod2_no_interaction)
testUniformity(res_mod2_no_interaction)
testDispersion(res_mod2_no_interaction)
testOutliers(res_mod2_no_interaction)

# Overall test of the condition x playback species interaction
anova(mod2_no_interaction, mod2)
AIC(mod2_no_interaction, mod2)

# Estimated marginal means
emm_species <- emmeans(
  mod2,
  ~ condition | playback_species,
  type = "response"
)

emm_species

# Within-species control vs post-playback comparisons
within_species <- pairs(emm_species)

summary(
  within_species,
  by = NULL,
  adjust = "holm"
)

# Pairwise comparisons of the playback effect between primate species
emm_species_interaction <- emmeans(
  mod2,
  ~ condition * playback_species
)

contrast(
  emm_species_interaction,
  interaction = c("pairwise", "pairwise"),
  adjust = "holm"
)

# AIM 3
# Does the playback effect differ among bird feeding guilds?

birds_guild <- birds_primate %>%
  select(
    sample_id,
    day,
    net,
    condition,
    time_z,
    monkey_presence,
    species_richness_i,
    species_richness_f,
    species_richness_o
  ) %>%
  pivot_longer(
    cols = c(
      species_richness_i,
      species_richness_f,
      species_richness_o
    ),
    names_to = "feeding_guild",
    values_to = "guild_richness"
  ) %>%
  mutate(
    feeding_guild = recode(
      feeding_guild,
      species_richness_i = "Insectivores",
      species_richness_f = "Frugivores",
      species_richness_o = "Omnivores"
    ),
    feeding_guild = factor(
      feeding_guild,
      levels = c(
        "Insectivores",
        "Frugivores",
        "Omnivores"
      )
    )
  )

birds_guild %>%
  group_by(feeding_guild, condition) %>%
  summarise(
    mean_richness = mean(guild_richness),
    sd_richness = sd(guild_richness),
    .groups = "drop"
  )

birds_guild %>%
  group_by(feeding_guild) %>%
  summarise(
    min = min(guild_richness),
    max = max(guild_richness),
    mean = mean(guild_richness),
    variance = var(guild_richness),
    zeros = sum(guild_richness == 0),
    .groups = "drop"
  )

mod3 <- glmmTMB(
  guild_richness ~
    condition * feeding_guild +
    time_z +
    monkey_presence +
    (1 | day:net) +
    (1 | sample_id),
  family = compois,
  data = birds_guild
)

summary(mod3)
VarCorr(mod3)

performance::check_convergence(mod3)
performance::check_singularity(mod3)

set.seed(123)

res_mod3 <- simulateResiduals(
  mod3,
  n = 5000
)

plot(res_mod3)
testUniformity(res_mod3)
testDispersion(res_mod3)
testOutliers(res_mod3)

mod3_no_interaction <- glmmTMB(
  guild_richness ~
    condition +
    feeding_guild +
    time_z +
    monkey_presence +
    (1 | day:net) +
    (1 | sample_id),
  family = compois,
  data = birds_guild
)

summary(mod3_no_interaction)
VarCorr(mod3_no_interaction)

performance::check_convergence(mod3_no_interaction)
performance::check_singularity(mod3_no_interaction)

set.seed(123)

res_mod3_no_interaction <- simulateResiduals(
  mod3_no_interaction,
  n = 5000
)

plot(res_mod3_no_interaction)
testUniformity(res_mod3_no_interaction)
testDispersion(res_mod3_no_interaction)
testOutliers(res_mod3_no_interaction)

# Overall test of the condition x feeding guild interaction
anova(mod3_no_interaction, mod3)
AIC(mod3_no_interaction, mod3)

# Estimated control and post-playback richness within each guild
emm_condition <- emmeans(
  mod3,
  ~ condition | feeding_guild,
  type = "response"
)

emm_condition

pairs(
  emm_condition,
  adjust = "holm"
)

# Pairwise comparisons of the playback effect between guilds
emm_interaction <- emmeans(
  mod3,
  ~ condition * feeding_guild
)

contrast(
  emm_interaction,
  interaction = c("pairwise", "pairwise"),
  adjust = "holm"
)

# Overall differences in richness among feeding guilds
emm_guild <- emmeans(
  mod3_no_interaction,
  ~ feeding_guild,
  type = "response"
)

emm_guild

pairs(
  emm_guild,
  adjust = "holm"
)

