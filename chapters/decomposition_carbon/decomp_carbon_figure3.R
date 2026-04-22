library(tidyverse)
library(plotly)
library(EDIutils)

# Pull most recent data

# setwd to folder in which this script resides
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# source the fetch table from EDI function
source("../../functions/getEDItable-function.R")

# fetch the most recent version of the table from EDI
data <- get_edi_table(identifier = "220", entity_seq = 1)
str(data)

# calculate average mass loss and standard error for each tree over incubation time
data_sum <- data |>
  group_by(TreeSpecies, ElapsedDays) |>
  summarise(
    mean_masslost = mean(PctMassLost, na.rm = TRUE),
    se = sd(PctMassLost, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# rename trees for easier analysis
data_sum <- data_sum |>
  mutate(SpeciesName = recode(
    TreeSpecies,
    AB = "Beech",
    SM = "Maple",
    WA = "Ash",
    YB = "Y Birch"
  ))

# create the plot
plot1 <- ggplot(data_sum,
                aes(
                  ElapsedDays,
                  mean_masslost,
                  linetype = SpeciesName,
                  shape = SpeciesName
                )) +
  geom_line() +
  geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = mean_masslost - se, ymax = mean_masslost + se), width = 0) +
  labs(
    x = "Days of Incubation",
    y = "% Mass Lost",
    linetype = "Species",
    shape = "Species"
  ) +
  theme_classic()


# convert the plot to plotly to be interactive
plot1_plotly <- ggplotly(plot1, margin = m) |>
  layout(modebar = list(
    bgcolor = "white",
    color = "black",
    activecolor = "#1B5E20"
  ))


# This chunk creates the output

# set working directory to main repository
setwd("../../")
output_file <- "chapters/decomposition_carbon/LongTerm_MassLost.html"

fname <- tools::file_path_sans_ext(basename(output_file))

# set image button so downloaded png of plot has correct name
p <- plot1_plotly |>
  config(toImageButtonOptions = list(format = "png", filename = fname))

# Write to temp dir where libdir can be relative
tmp_html <- tempfile(fileext = ".html")
htmlwidgets::saveWidget(p, file = tmp_html, selfcontained = TRUE)

# Copy the single file to your desired output location
file.copy(tmp_html, output_file, overwrite = TRUE)
unlink(tmp_html)
