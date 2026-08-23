# 02_figures.R
# Figures for the bird-primate playback project
# Run this script from the repository root.
#
# This script sources the analysis script so that the model objects
# and estimated marginal means are available.

library(tidyverse)

source("scripts/01_statistical_analysis.R")


# Shared colours

primate_colours <- c(
  "Chimpanzee"              = "#D76A5B",
  "Grey-cheeked mangabey"   = "#D99B32",
  "Black-and-white colobus" = "#4C8DAE",
  "Red colobus"             = "#6C9A62"
)

guild_colours <- c(
  "Insectivores" = "#2A9D8F",
  "Frugivores"   = "#E9A03B",
  "Omnivores"    = "#8E6BBE"
)

# FIGURE 1
# Raw species richness data by primate playback

birds_aim1_plot <- birds_primate %>%
  mutate(
    condition = factor(
      condition,
      levels = c("control", "post_playback"),
      labels = c("Control", "Post-playback")
    ),
    playback_species = factor(
      playback_species,
      levels = c("CH", "GM", "BC", "RC"),
      labels = c(
        "Chimpanzee",
        "Grey-cheeked mangabey",
        "Black-and-white colobus",
        "Red colobus"
      )
    )
  )

birds_aim1_mean <- birds_aim1_plot %>%
  group_by(playback_species, condition) %>%
  summarise(
    mean_richness = mean(species_richness_total, na.rm = TRUE),
    se_richness = sd(species_richness_total, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

fig_aim1_raw <- ggplot() +

  geom_line(
    data = birds_aim1_plot,
    aes(
      x = condition,
      y = species_richness_total,
      group = pair_id
    ),
    colour = "grey78",
    linewidth = 0.6,
    alpha = 0.7
  ) +

  geom_point(
    data = birds_aim1_plot,
    aes(
      x = condition,
      y = species_richness_total
    ),
    colour = "grey65",
    size = 2.3,
    alpha = 0.7
  ) +

  geom_errorbar(
    data = birds_aim1_mean,
    aes(
      x = condition,
      ymin = mean_richness - se_richness,
      ymax = mean_richness + se_richness,
      colour = playback_species
    ),
    width = 0.07,
    linewidth = 0.85
  ) +

  geom_line(
    data = birds_aim1_mean,
    aes(
      x = condition,
      y = mean_richness,
      group = playback_species,
      colour = playback_species
    ),
    linewidth = 1.25,
    alpha = 0.9
  ) +

  geom_point(
    data = birds_aim1_mean,
    aes(
      x = condition,
      y = mean_richness,
      fill = playback_species
    ),
    shape = 21,
    size = 4.5,
    stroke = 1.1,
    colour = "white"
  ) +

  facet_wrap(
    ~ playback_species,
    nrow = 1
  ) +

  scale_colour_manual(
    values = primate_colours
  ) +

  scale_fill_manual(
    values = primate_colours
  ) +

  scale_y_continuous(
    limits = c(0, 11),
    breaks = 0:11,
    expand = expansion(mult = c(0, 0.02))
  ) +

  labs(
    x = NULL,
    y = "Species richness"
  ) +

  theme_minimal(base_size = 12) +

  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      colour = "grey90",
      linewidth = 0.45
    ),
    strip.background = element_rect(
      fill = "grey96",
      colour = NA
    ),
    strip.text = element_text(
      face = "bold",
      size = 11,
      margin = margin(t = 8, b = 8)
    ),
    axis.text.x = element_text(
      size = 10,
      colour = "grey20"
    ),
    axis.text.y = element_text(
      size = 10,
      colour = "grey30"
    ),
    axis.title.y = element_text(
      size = 11.5,
      margin = margin(r = 10)
    ),
    panel.spacing = unit(1.2, "lines"),
    legend.position = "none",
    plot.margin = margin(
      t = 10,
      r = 15,
      b = 10,
      l = 10
    )
  )

fig_aim1_raw

ggsave(
  "figures/aim1_raw_species_richness.png",
  fig_aim1_raw,
  width = 24,
  height = 8,
  units = "cm",
  dpi = 300,
  bg = "white"
)

ggsave(
  "figures/aim1_raw_species_richness.pdf",
  fig_aim1_raw,
  width = 24,
  height = 8,
  units = "cm",
  bg = "white"
)

# FIGURE 2
# Estimated playback responses by primate species

emm_species_plot <- as.data.frame(emm_species) %>%
  mutate(
    condition = factor(
      condition,
      levels = c("control", "post_playback"),
      labels = c("Control", "Post-playback")
    ),
    playback_species = factor(
      playback_species,
      levels = c("CH", "GM", "BC", "RC"),
      labels = c(
        "Chimpanzee",
        "Grey-cheeked mangabey",
        "Black-and-white colobus",
        "Red colobus"
      )
    )
  )

fig_aim2 <- ggplot(
  emm_species_plot,
  aes(
    x = condition,
    y = response,
    group = playback_species,
    colour = playback_species
  )
) +

  geom_line(
    linewidth = 1.25,
    alpha = 0.9
  ) +

  geom_errorbar(
    aes(
      ymin = asymp.LCL,
      ymax = asymp.UCL
    ),
    width = 0.07,
    linewidth = 0.85
  ) +

  geom_point(
    aes(fill = playback_species),
    shape = 21,
    size = 4.5,
    stroke = 1.1,
    colour = "white"
  ) +

  facet_wrap(
    ~ playback_species,
    nrow = 1
  ) +

  scale_colour_manual(
    values = primate_colours
  ) +

  scale_fill_manual(
    values = primate_colours
  ) +

  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0.02, 0.10))
  ) +

  labs(
    x = NULL,
    y = "Estimated species richness"
  ) +

  theme_minimal(base_size = 12) +

  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      colour = "grey90",
      linewidth = 0.45
    ),
    strip.background = element_rect(
      fill = "grey96",
      colour = NA
    ),
    strip.text = element_text(
      face = "bold",
      size = 11,
      margin = margin(t = 8, b = 8)
    ),
    axis.text.x = element_text(
      size = 10,
      colour = "grey20"
    ),
    axis.text.y = element_text(
      size = 10,
      colour = "grey30"
    ),
    axis.title.y = element_text(
      size = 11.5,
      margin = margin(r = 10)
    ),
    panel.spacing = unit(1.2, "lines"),
    legend.position = "none",
    plot.margin = margin(
      t = 10,
      r = 15,
      b = 10,
      l = 10
    )
  )

fig_aim2

ggsave(
  "figures/aim2_primate_species_emmeans.png",
  fig_aim2,
  width = 24,
  height = 8,
  units = "cm",
  dpi = 300,
  bg = "white"
)

ggsave(
  "figures/aim2_primate_species_emmeans.pdf",
  fig_aim2,
  width = 24,
  height = 8,
  units = "cm",
  bg = "white"
)

# FIGURE 3
# Estimated playback responses by feeding guild

emm_guild_plot <- as.data.frame(emm_condition) %>%
  mutate(
    condition = factor(
      condition,
      levels = c("control", "post_playback"),
      labels = c("Control", "Post-playback")
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

fig_aim3 <- ggplot(
  emm_guild_plot,
  aes(
    x = condition,
    y = response,
    group = feeding_guild,
    colour = feeding_guild
  )
) +

  geom_line(
    linewidth = 1.2,
    alpha = 0.85
  ) +

  geom_errorbar(
    aes(
      ymin = asymp.LCL,
      ymax = asymp.UCL
    ),
    width = 0.08,
    linewidth = 0.8
  ) +

  geom_point(
    aes(fill = feeding_guild),
    shape = 21,
    size = 4.3,
    stroke = 1,
    colour = "white"
  ) +

  facet_wrap(
    ~ feeding_guild,
    nrow = 1
  ) +

  scale_colour_manual(
    values = guild_colours
  ) +

  scale_fill_manual(
    values = guild_colours
  ) +

  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0.02, 0.10))
  ) +

  labs(
    x = NULL,
    y = "Estimated species richness"
  ) +

  theme_minimal(base_size = 12) +

  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      colour = "grey90",
      linewidth = 0.45
    ),
    strip.background = element_rect(
      fill = "grey96",
      colour = NA
    ),
    strip.text = element_text(
      face = "bold",
      size = 12,
      margin = margin(t = 8, b = 8)
    ),
    axis.text.x = element_text(
      size = 10.5,
      colour = "grey20"
    ),
    axis.text.y = element_text(
      size = 10,
      colour = "grey30"
    ),
    axis.title.y = element_text(
      size = 11.5,
      margin = margin(r = 10)
    ),
    panel.spacing = unit(1.4, "lines"),
    legend.position = "none",
    plot.margin = margin(
      t = 10,
      r = 15,
      b = 10,
      l = 10
    )
  )

fig_aim3

ggsave(
  "figures/aim3_feeding_guild_emmeans.png",
  fig_aim3,
  width = 18,
  height = 8,
  units = "cm",
  dpi = 300,
  bg = "white"
)

ggsave(
  "figures/aim3_feeding_guild_emmeans.pdf",
  fig_aim3,
  width = 18,
  height = 8,
  units = "cm",
  bg = "white"
)
