library(tidyverse)
library(plotly)
library(EDIutils)

# Pull most recent data

# setwd to folder in which this script resides
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# source the fetch table from EDI function
source("../../functions/getEDItable-function.R")

# fetch the most recent version of the table from EDI
dt <- get_edi_table(identifier = "208", entity_seq = 2)
str(dt)

# filter data to only include watershed 6 for this plot
dt <- dt |>
  filter(site == "W6")

# edit aluminum counts
dt <- dt |>
  mutate(AL = coalesce(Al_ICP, 0) + coalesce(Al_ferron, 0),
         AL = na_if(AL, 0))

# create annual mean counts of plotted features
dtSums <- aggregate(
  list(
    annSO4 = dt$SO4,
    annNO3 = dt$NO3,
    annpH = dt$pH,
    annANC = dt$ANC960,
    annAl = dt$TMAl,
    annDOC = dt$DOC
  ),
  by = list(waterYr = dt$waterYr, site = dt$site),
  FUN = "mean",
  na.rm = TRUE
)

# pivot data to plot easier
dtSums <- dtSums |>
  pivot_longer(
    cols = c(annSO4, annNO3, annpH, annANC, annAl, annDOC),
    names_to = "vars",
    values_to = "value"
  )

# convert variables to factors
dtSums <- dtSums |>
  mutate(vars = factor(
    vars,
    levels = c("annSO4", "annNO3", "annpH", "annANC", "annAl", "annDOC")
  ))

# edit labels to clean visualiation
new_labels1 <- c(
  `annSO4` = "SO4 (µeq L)",
  `annNO3` = "NO3 (µeq L)",
  `annpH` = "pH",
  `annANC` = "ANC (µeq L)",
  `annAl` = "Al (µmol L)",
  `annDOC` = "DOC (µmol L)"
)

# create column of new labels
dtSums <- dtSums |>
  mutate(vars_lab = new_labels1[as.character(vars)])
vars_unique <- unique(dtSums$vars_lab)
n <- length(vars_unique)


# this function creates subplots for each variable
fig_list <- lapply(vars_unique, function(v) {
  df_sub <- dtSums |>
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
  plot_ly(
    df_sub,
    x = ~ waterYr,
    y = ~ value,
    type = "scatter",
    mode = "lines+markers",
    line = list(width = 0.4, color = "black"),
    marker = list(size = 5, color = "black"),
    showlegend = FALSE
  ) |>
    
    layout(
      shapes = list(
        list(
          type = "line",
          x0 = 1999,
          x1 = 1999,
          y0 = 0,
          y1 = 1,
          xref = "x",
          yref = "paper",
          line = list(
            color = "black",
            width = 0.8,
            dash = "dash"
          )
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
      margin = list(
        l = 40,
        r = 10,
        t = 20,
        b = 30
      )
    )
})


# create the layout for the plot
fig <- subplot(fig_list,
               nrows = 6,
               shareX = FALSE,
               shareY = FALSE)


# apply the annotations for the labels for each subplot
annotations <- lapply(seq_len(n), function(i) {
  y_top <- 1 - (i - 1) / n
  y_pos <- y_top - 0.04
  
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

# create the plot with annotations and shared axes
fig <- fig |>
  layout(
    annotations = annotations,
    plot_bgcolor = "white",
    paper_bgcolor = "white",
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


# This chunk creates the output

# set working directory to main repository
setwd("../../")
output_file <- "chapters/acid_deposition/StreamChem-W6_longtermTrends.html"

fname <- tools::file_path_sans_ext(basename(output_file))

# set image button so downloaded png of plot has correct name
p <- fig |>
  config(toImageButtonOptions = list(format = "png", filename = fname))

# Write to temp dir where libdir can be relative
tmp_html <- tempfile(fileext = ".html")
htmlwidgets::saveWidget(p, file = tmp_html, selfcontained = TRUE)

# Copy the single file to your desired output location
file.copy(tmp_html, output_file, overwrite = TRUE)
unlink(tmp_html)
