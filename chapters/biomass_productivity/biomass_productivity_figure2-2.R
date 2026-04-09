library(tidyverse)
library(lubridate)
library(plotly)

# install.packages("devtools")
library(devtools)
# devtools::install_github('kearutherford/HubbardBrookForestAnalytics')
library(HubbardBrookForestAnalytics)

W1 <- read_csv(
  "https://pasta.lternet.edu/package/data/eml/knb-lter-hbr/446/1/95498fa6cf9986255fae32c1924182d9"
) |>
  mutate(watershed = "Watershed 1")

W6 <- read_csv(
  "https://pasta.lternet.edu/package/data/eml/knb-lter-hbr/448/1/ac44ba6b0f0aac1d8e5e270a63d0e009"
) |>
  mutate(watershed = "Watershed 6")

all <- rbind(W1, W6)

# reformat columns for package
all_format <- all |>
  mutate(year = as.character(year)) |>
  mutate(plot = as.character(plot)) |>
  mutate(vigor = as.character(vigor))

# use package to calculate above ground biomass per plot
all_agb <- HBEFBiomass(data_type = "external",
                       external_data = all_format,
                       results = "by_plot")

st.err <- function(x) {
  sd(x, na.rm = TRUE) / sqrt(length(x))
}

# mean across plots to get yearly values
all_agb_sum <- all_agb |>
  group_by(watershed, year) |>
  summarize(
    agb = mean(above_L_Mg_ha, na.rm = TRUE),
    se = st.err(above_L_Mg_ha),
    .groups = "drop"
  ) |>
  mutate(year = as.numeric(year))

# PLOT

plot1 <- ggplot(all_agb_sum, aes(year, agb, color = watershed, group = watershed)) +
  geom_line() +
  geom_point() +
  # ice storm in 1998
  geom_vline(xintercept = 1998, linetype = "dashed") +
  # error bars w/ 95% conf interval
  geom_errorbar(aes(ymin = agb - se, ymax = agb + se)) +
  scale_y_continuous(breaks = c(125, 150, 175, 200)) +
  scale_x_continuous(
    expand = c(0.1, 0),
    # 10% padding
    breaks = c(1965, 1975, 1985, 1995, 2005, 2015, 2025)
  ) +
  # scale_color_discrete(labels = c("Watershed 1", "Watershed 6")) +
  labs(x = "Year", y = "Aboveground Live Tree Biomass (Mg/ha⁻¹)") +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.position = c(0.1, 0.8),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
plot1

# convert to plotly graph
plotly1 <- ggplotly(plot1, margin = m) |> layout(
  modebar = list(
    bgcolor = "white",
    color = "black",
    activecolor = "#1B5E20"
  ),
  legend = list(title = list(text = NULL))
)
plotly1

# use temp dir so only html is saved
output_file <- "chapters/biomass_productivity/Aboveground_Biomass.html"
fname <- tools::file_path_sans_ext(basename(output_file))

plotly1 |>
  config(toImageButtonOptions = list(format = "png", filename = fname))

tmp_html <- tempfile(fileext = ".html")
htmlwidgets::saveWidget(plotly1, file = tmp_html, selfcontained = TRUE)

file.copy(tmp_html, output_file, overwrite = TRUE)
unlink(tmp_html)
