library(tidyverse)
library(plotly)
library(dplyr)

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
                                    annAl=dt2$TMAl, annDOC=dt2$DOC), 
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



dt2Sums <- dt2Sums |> 
  mutate(vars_lab = new_labels[as.character(vars)])

vars_unique <- unique(dt2Sums$vars_lab)
n <- length(vars_unique)

fig_list <- lapply(seq_along(vars_unique), function(i) {
  
  v <- vars_unique[i]
  
  df_sub <- dt2Sums |> 
    filter(vars_lab == v)
  
  ann <- NULL
  if (v == "DOC (µmol L)") {
    ann <- list(
      list(
        x = 1999,
        y = 1,
        xref = "x",
        yref = "paper",
        text = "<b>Calcium treatment</b>",
        showarrow = FALSE,
        xanchor = "left",
        yanchor = "top",
        font = list(size = 9),
        xshift = 5
      )
    )
  }
  
  plot_ly(df_sub,
          x = ~waterYr,
          y = ~value,
          color = ~site,
          colors = c("W1" = "grey", "W6" = "black"),
          type = "scatter",
          mode = "lines+markers",
          line = list(width = 0.4),
          marker = list(size = 5),
          showlegend = (i == 1)) |>
    
    layout(
      shapes = list(
        list(
          type = "line",
          x0 = 1999, x1 = 1999,
          y0 = 0, y1 = 1,
          xref = "x",
          yref = "paper",
          line = list(color = "black", width = 0.8, dash = "dash")
        )
      ),
      annotations = ann,
      xaxis = list(
        range = c(1960, 2025),
        tickvals = seq(1960, 2030, 10),
        showgrid = FALSE,
        zeroline = FALSE,
        showline = TRUE,
        linecolor = "black",
        linewidth = 0.4,
        ticks = "outside",
        tickfont = list(size = 12)
      ),
      yaxis = list(
        showgrid = FALSE,
        zeroline = FALSE,
        showline = TRUE,
        linecolor = "black",
        linewidth = 0.4,
        ticks = "outside",
        tickfont = list(size = 12)
      ),
      margin = list(l = 40, r = 10, t = 20, b = 30)
    )
})

fig <- subplot(fig_list,
               nrows = 6,
               shareX = FALSE,
               shareY = FALSE)


annotations <- lapply(seq_len(n), function(i) {
  y_top <- 1 - (i - 1) / n
  y_pos <- y_top - 0.04
  
  if (vars_unique[i] == "Ca2+ (µeq L)") {
    y_pos <- y_top - 0.1
  }
  
  list(
    x = 0.01,
    y = y_pos,
    xref = "paper",
    yref = "paper",
    text = paste0("<b>", vars_unique[i], "</b>"),
    showarrow = FALSE,
    xanchor = "left",
    yanchor = "top",
    font = list(size = 12),
    align = "left"
  )
})

fig <- fig |> 
  layout(
    annotations = annotations,
    plot_bgcolor = "white",
    paper_bgcolor = "white",
    legend = list(
      x = 1,
      y = 0.9,
      font = list(size = 8),
      bgcolor = "rgba(0,0,0,0)"
    ),
    modebar = list(
      bgcolor = "white",
      color = "black",
      activecolor = "#1B5E20"
    ),
    xaxis = list(matches = "x"),
    xaxis2 = list(matches = "x"),
    xaxis3 = list(matches = "x"),
    xaxis4 = list(matches = "x"),
    xaxis5 = list(matches = "x"),
    xaxis6 = list(matches = "x")
  )

output_file <- "StreamChem-W1W6_longtermTrends.html"
fname <- tools::file_path_sans_ext(basename(output_file))

fig |>
  config(toImageButtonOptions = list(format = "png", filename = fname)) |>
  htmlwidgets::saveWidget(file = output_file)


