library(tidyverse)
library(plotly)
library(EDIutils)

# Alternate method to pull most recent data
# setwd to folder in which this script resides
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# source the fetch table from EDI function
source("../../functions/getEDItable-function.R")

# fetch the most recent version of the table from EDI
SoilMassData <- get_edi_table(identifier = "172", entity_seq = 5)
str(SoilMassData)

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

setwd("../../") # jumps up 2 folders 
output_file <- "chapters/decomposition_carbon/OrganicMatter.html"
fname <- tools::file_path_sans_ext(basename(output_file))

plotly1 |>
  config(toImageButtonOptions = list(format = "png", filename = fname))

# Write to temp dir where libdir can be relative
tmp_html <- tempfile(fileext = ".html")
htmlwidgets::saveWidget(plotly1, file = tmp_html, selfcontained = TRUE)

# Copy the single file to your desired output location
file.copy(tmp_html, output_file, overwrite = TRUE)
unlink(tmp_html)
