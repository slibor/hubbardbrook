library(tidyverse)
library(plotly)

inUrl2 <- "https://pasta.lternet.edu/package/data/eml/knb-lter-hbr/208/14/024b6acc5cb2e03a14fff5558bbffc0c"
infile2 <- tempfile()
try(download.file(inUrl2,infile2,method="curl"))
if (is.na(file.size(infile2))) download.file(inUrl2,infile2,method="auto")

dt <- read.csv(infile2, header=F, skip=1, sep="", quot="")
dt <-read.csv(infile2,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "site",     
                 "date",     
                 "timeEST",     
                 "barcode",     
                 "pH",     
                 "DIC",     
                 "spCond",     
                 "temp",     
                 "ANC960",     
                 "ANCMet",     
                 "gageHt",     
                 "hydroGraph",     
                 "flowGageHt",     
                 "fieldCode",     
                 "notes",     
                 "uniqueID",     
                 "waterYr",     
                 "Ca",     
                 "Mg",     
                 "K",     
                 "Na",     
                 "TMAl",     
                 "OMAl",     
                 "Al_ICP",
                 "Al_ferron",
                 "NH4",     
                 "SO4",     
                 "NO3",     
                 "Cl",     
                 "PO4",     
                 "DOC",     
                 "TDN",     
                 "DON",     
                 "SiO2",     
                 "Mn",     
                 "Fe",     
                 "F",     
                 "cationCharge",     
                 "anionCharge",     
                 "ionError",     
                 "duplicate",     
                 "sampleType",     
                 "ionBalance",     
                 "canonical",     
                 "pHmetrohm"    ), check.names=TRUE)

unlink(infile2)
dt <- dt |> 
  filter(site == "W6")

dt <- dt |>
  mutate(
    AL = coalesce(Al_ICP, 0) + coalesce(Al_ferron, 0),
    AL = na_if(AL, 0)
  )


dtSums <-aggregate(list(annSO4=dt$SO4, 
                         annNO3=dt$NO3,
                         annpH=dt$pH, 
                         annANC=dt$ANC960,
                         annAl=dt$TMAl, 
                         annDOC=dt$DOC), 
                               by=list(waterYr=dt$waterYr,site=dt$site ),
                               FUN="mean", na.rm=TRUE)

dtSums <- dtSums |> 
  pivot_longer(cols = c(annSO4, annNO3, annpH, annANC, annAl, annDOC),
               names_to = "vars",
               values_to = "value")


dtSums <- dtSums |>
  mutate(vars = factor(vars,
                           levels = c("annSO4",
                                      "annNO3",
                                      "annpH",
                                      "annANC",
                                      "annAl",
                                      "annDOC")))

new_labels1 <- c(
  `annSO4` = "SO4 (µeq L)",
  `annNO3` = "NO3 (µeq L)",
  `annpH` = "pH",
  `annANC` = "ANC (µeq L)",
  `annAl` = "Al (µmol L)",
  `annDOC` = "DOC (µmol L)"
)


plot5 <- ggplot(dtSums, aes(waterYr, value)) +
  geom_line(size = 0.4) +
  geom_point() +
  geom_vline(xintercept = 1999,
             linetype = "dashed",
             size = 0.2,
             color = "black") +
  facet_wrap(~ vars, nrow = 6, scales = "free_y", labeller = as_labeller(new_labels1)) +
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(1960, 2025),
    breaks = seq(1960, 2030, 10)
  ) +
  labs(x = NULL, y = NULL, color = NULL) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(),
    axis.ticks = element_line(size = 0.3),
    strip.text = element_text(size = 8, margin = margin(b = 2)),
    axis.text = element_text(size = 7),
    panel.spacing = unit(0.4, "lines")
  )


plot5
plot5_plotly <- ggplotly(plot5, tooltip = c("x", "y", "color", height = 500)) |>
  layout(
    modebar = list(
      bgcolor = "white",
      color = "black",
      activecolor = "#1B5E20"
    ),
    (margin = list(l=0, r=0, t=0, b=0)
  ))

plot5
plot5_plotly
htmlwidgets::saveWidget(as_widget(plot5_plotly), "Fig5_streams.html")
