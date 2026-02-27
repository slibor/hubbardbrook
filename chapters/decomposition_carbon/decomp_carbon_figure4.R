library(tidyverse)
library(plotly)

SoilMassData <- read_csv("chapters/decomposition_carbon/HubbardBrook_ForestFloor_SoilMass_W6.csv")

# basic cleaning
SoilMassData2 <- SoilMassData |>
  select("Year", "OM_TM", "OM_OM", "OM_LOI") |>
  # where OM_LOI = -9999.99, other OM values are zero
  filter(OM_LOI != -9999.99)

# one value for each year
SoilMassData3 <- SoilMassData2 |>
  group_by(Year) |>
  summarize(
    mean_OM = mean(OM_OM),
    min_OM = min(OM_OM),
    max_OM = max(OM_OM)
  )

# PLOTS
theme_set(theme_classic())

plot1 <- ggplot(SoilMassData2, aes(Year, OM_OM)) +
  geom_point() +
  labs(x = "Year", y = "Forest floor OM mass (Mg/ha)")
plot1

plot2 <- ggplot(SoilMassData3, aes(Year, mean_OM)) +
  geom_point() +
  labs(x = "Year", y = "Forest floor OM mass (Mg/ha)") +
  geom_errorbar(aes(ymin = min_OM, ymax = max_OM))
plot2

# convert to plotly graph
plot1_plotly <- ggplotly(plot2)
plot1_plotly

htmlwidgets::saveWidget(
  widget = plot1_plotly,
  here::here("chapters", "decomposition_carbon", "Fig4_OrganicMatter.html"),
  selfcontained = TRUE
)

