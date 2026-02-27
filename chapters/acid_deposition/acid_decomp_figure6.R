library(tidyverse)
library(plotly)

inUrl2 <- "https://pasta.lternet.edu/package/data/eml/knb-lter-hbr/208/14/024b6acc5cb2e03a14fff5558bbffc0c"
infile2 <- tempfile()
try(download.file(inUrl2,infile2,method="curl"))
if (is.na(file.size(infile2))) download.file(inUrl2,infile2,method="auto")

dt2 <- read.csv(infile2, header=F, skip=1, sep="", quot="")
dt2 <-read.csv(infile2,header=F 
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
dt2 <- dt2 |> 
  filter(site == "W1" | site == "W6")

dt2 <- dt2 |>
  mutate(
    AL = coalesce(Al_ICP, 0) + coalesce(Al_ferron, 0),
    AL = na_if(AL, 0)
  )


dt2Sums <-aggregate(list(annCa=dt2$Ca, annSiO2=dt2$SiO2, 
                                    annpH=dt2$pH, annANC=dt2$ANC960, 
                                    annAl=dt2$AL, annDOC=dt2$DOC), 
                               by=list(waterYr=dt2$waterYr,site=dt2$site ),
                               FUN="mean", na.rm=TRUE)

dt2Sums <- dt2Sums |> 
  pivot_longer(cols = c(annCa, annSiO2, annpH, annANC, annAl, annDOC),
               names_to = "vars",
               values_to = "value")


dt2Sums <- dt2Sums |>
  mutate(vars = factor(vars,
                           levels = c("annCa",
                                      "annSiO2",
                                      "annpH",
                                      "annANC",
                                      "annAl",
                                      "annDOC")))

new_labels <- c(
  `annCa` = "Ca2+ (µeq L)",
  `annSiO2` = "SiO2 (µmol L)",
  `annpH` = "pH",
  `annANC` = "ANC (µeq L)",
  `annAl` = "Al (µmol L)",
  `annDOC` = "DOC (µmol L)"
)


plot6 <- ggplot(dt2Sums, aes(waterYr, value, color = site)) +
  geom_line(size = 0.4) +
  geom_point() +
  geom_vline(xintercept = 1999,
             linetype = "dashed",
             size = 0.2,
             color = "black") +
  facet_wrap(~ vars, nrow = 6, scales = "free_y", labeller = as_labeller(new_labels)) +
  scale_color_manual(values = c(
    "W1" = "grey",
    "W6" = "black"
  )) +
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(1960, 2025),
    breaks = seq(1960, 2030, 10)
  ) +
  labs(x = NULL, y = NULL, color = NULL) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(size = 0.4),
    strip.background = element_blank(),
    strip.text = element_text(size = 9),
    axis.text = element_text(size = 8),
    axis.ticks = element_line(size = 0.3),
    legend.position = c(1, 0.9),
    legend.text = element_text(size = 8),
    legend.background = element_blank()
  )


plot6_plotly <- ggplotly(plot6, tooltip = c("x", "y", "color")) |>
  layout(
    modebar = list(
      bgcolor = "white",
      color = "black",
      activecolor = "#1B5E20"
    )
  )

plot6_plotly
htmlwidgets::saveWidget(as_widget(plot6_plotly), "Fig6_streams.html")

# figure 5
streamDataW6 <- streamData |> 
  filter(site == "W6")



