library(tidyverse)
library(plotly)

SoilMassData <- read_csv(
  "https://pasta.lternet.edu/package/data/eml/knb-lter-hbr/172/3/775e243b7a2b67c0047498533bf5b9d1"
)

# basic cleaning
SoilMassData2 <- SoilMassData |>
  select("Year", "Plot", "Horizon", "OM_TM", "OM_OM", "OM_LOI") |>
  mutate(OM_LOI = na_if(OM_LOI, -9999.99)) |> 
  filter(Horizon != "min")

# sum organic matter per plot per year & convert units
SoilMassData3 <- SoilMassData2 |>
  group_by(Year, Plot) |>
  summarize(OM = sum(OM_OM, na.rm = TRUE), .groups = "drop") |>
  mutate(Mg_ha = OM * 10)

# calculate SE
st.err <- function(x) {
  sd(x, na.rm = TRUE) / sqrt(length(x))
}

# mean across plots to get yearly values
SoilMassData4 <- SoilMassData3 |>
  group_by(Year) |>
  summarize(
    OM = mean(Mg_ha, na.rm = TRUE),
    se = st.err(Mg_ha),
    .groups = "drop"
  )

# PLOTS
theme_set(theme_bw())

plot1 <- ggplot(SoilMassData4, aes(Year, OM)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = OM - se, ymax = OM + se), width = 2) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 120),
    breaks = c(0, 20, 40, 60, 80, 100)
  ) +
  scale_x_continuous(expand = c(0, 0), limits = c(1970, 2020)) +
  labs(x = "Year", y = "Forest floor OM mass (Mg/ha)") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    text = element_text(size = 24)
  )
plot1

# convert to plotly graph
plotly1 <- ggplotly(plot1, margin = m) |> layout(modebar = list(
  bgcolor = "white",
  color = "black",
  activecolor = "#1B5E20"
))
plotly1

htmlwidgets::saveWidget(
  widget = plotly1,
  # builds file path
  here::here(
    "chapters",
    "decomposition_carbon",
    "Fig4_OrganicMatter.html"
  ),
  selfcontained = TRUE
)
