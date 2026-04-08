library(tidyverse)
library(plotly)

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-hbr/220/1/62f0a2575303eb4bc81064b9ec70274c" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


data <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "TreeSpecies",     
                 "SourceSite",     
                 "PlacementSite",     
                 "Collection",     
                 "ElapsedDays",     
                 "Count",     
                 "PctMassRemain",     
                 "PctMassLost",     
                 "PctC",     
                 "PctN",     
                 "Ca",     
                 "Mg",     
                 "K",     
                 "P"    ), check.names=TRUE)

unlink(infile1)


# Alternate method to pull most recent data

# setwd to folder in which this script resides
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# source the fetch table from EDI function
# source("../../functions/getEDItable-function.R")

# fetch the most recent version of the table from EDI
# data <- get_edi_table(identifier = "220", entity_seq = 2)
# Str(data)


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


plot1_plotly <- ggplotly(plot1, margin=m) |> 
  layout(
    modebar = list(
      bgcolor = "white",
      color = "black",
      activecolor = "#1B5E20"
    )
  )
plot1_plotly



output_file <- "chapters/decomposition_carbon/LongTerm_MassLost.html"

fname <- tools::file_path_sans_ext(basename(output_file))

p <- plot1_plotly |>
  config(toImageButtonOptions = list(format = "png", filename = fname))

# Write to temp dir where libdir can be relative
tmp_html <- tempfile(fileext = ".html")
htmlwidgets::saveWidget(p, file = tmp_html, selfcontained = TRUE)

# Copy the single file to your desired output location
file.copy(tmp_html, output_file, overwrite = TRUE)
unlink(tmp_html)



