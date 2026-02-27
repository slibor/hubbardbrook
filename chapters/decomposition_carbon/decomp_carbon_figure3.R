library(tidyverse)
library(plotly)

data <- read_csv("w1reciprocal_litter_decomp.csv")

data_sum <- data |> 
  group_by(TreeSpecies, ElapsedDays) |> 
  summarise(
    mean_masslost = mean(PctMassLost, na.rm = TRUE),
    se = sd(PctMassLost, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

data_sum <- data_sum |> 
  mutate(SpeciesName = recode(TreeSpecies,
                              AB = "Beech",
                              SM = "Maple",
                              WA = "Ash",
                              YB = "Y Birch"
  ))

plot1 <- ggplot(data_sum,
                aes(ElapsedDays, mean_masslost,
                    linetype = SpeciesName,
                    shape = SpeciesName)) +
  
  geom_line() +
  geom_point(size = 2.5) +
  geom_errorbar(aes(
    ymin = mean_masslost - se,
    ymax = mean_masslost + se
  ),
  width = 0) +
  labs(
    x = "Days of Incubation",
    y = "% Mass Lost",
    linetype = "Species",
    shape = "Species"
  ) +
  theme_classic()

plot1


plot1_plotly <- ggplotly(plot1, margin=m)
plot1_plotly

htmlwidgets::saveWidget(as_widget(plot1_plotly), "Fig3_masslost.html")

