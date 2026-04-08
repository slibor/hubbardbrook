library(tidyverse)
library(plotly)
library(dplyr)

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-hbr/446/1/95498fa6cf9986255fae32c1924182d9" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")
ws1_data <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "watershed",     
                 "plot",     
                 "plot_area_ha",     
                 "year",     
                 "forest_type",     
                 "sample_class",     
                 "species",     
                 "dbh_cm",     
                 "status",     
                 "vigor",     
                 "exp_factor",     
                 "elev_m",     
                 "steep_deg",     
                 "aspect_deg",     
                 "hli"    ), check.names=TRUE)
unlink(infile1)



inUrl6  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-hbr/448/1/ac44ba6b0f0aac1d8e5e270a63d0e009" 
infile6 <- tempfile()
try(download.file(inUrl6,infile6,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile6))) download.file(inUrl6,infile6,method="auto")
ws6_data <-read.csv(infile6,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "watershed",     
                 "plot",     
                 "plot_area_ha",     
                 "year",     
                 "forest_type",     
                 "sample_class",     
                 "species",     
                 "dbh_cm",     
                 "status",     
                 "vigor",     
                 "exp_factor",     
                 "elev_m",     
                 "steep_deg",     
                 "aspect_deg",     
                 "hli"    ), check.names=TRUE)
unlink(infile6)


df <- bind_rows(ws1_data, ws6_data)

df <- df |> 
  filter(status == "Live")

exclude_w1 <- c(1:7, 14:15, 23, 31, 32, 39, 41, 44, 48, 51, 55, 168)
exclude_w6 <- c(1, 2, 4, 8, 9, 15:18, 24:27, 34, 35, 42)

df <- df |>
  filter(status == "Live",
         sample_class == "tree",
         !(watershed == "W1" & plot %in% exclude_w1),
         !(watershed == "W6" & plot %in% exclude_w6),
         !(watershed == "W1" & year < 1996),
         species %in% c("ACSA","BEAL","FAGR"))

df <- df |>
  mutate(ba_m2 = pi * (dbh_cm / 200)^2 * exp_factor)

plot_ba <- df |>
  group_by(watershed, plot, year, species) |>
  summarise(ba = sum(ba_m2, na.rm=TRUE), .groups="drop")

plot_total <- plot_ba |>
  group_by(watershed, plot, year) |>
  summarise(total_ba = sum(ba), .groups="drop")

plot_frac <- plot_ba |>
  left_join(plot_total, by=c("watershed","plot","year")) |>
  mutate(frac = ba / total_ba)

summary_df <- plot_frac |>
  group_by(watershed, year, species) |>
  summarise(mean_frac = mean(frac, na.rm=TRUE),
            se = sd(frac, na.rm=TRUE)/sqrt(n()),
            .groups="drop")

summary_df <- summary_df |>
  mutate(watershed = factor(watershed, levels = c("W6","W1")))


colors <- c("ACSA" = "#F8766D",
            "BEAL" = "#00BA38",
            "FAGR" = "#619CFF")

fig_W1 <- summary_df |> 
  filter(watershed == "W1") |> 
  plot_ly(
    x = ~year,
    y = ~mean_frac,
    color = ~species,
    colors = colors,
    type = "scatter",
    mode = "lines+markers",
    error_y = ~list(type="data", array=se, visible=TRUE)
  ) |> 
  layout(
    xaxis = list(title = "Year", range = c(1990, NA)),
    yaxis = list(title = "Relative Dominance"),
    showlegend = TRUE
  )

fig_W6 <- summary_df |> 
  filter(watershed == "W6", year >= 1990) |> 
  plot_ly(
    x = ~year,
    y = ~mean_frac,
    color = ~species,
    colors = colors,
    type = "scatter",
    mode = "lines+markers",
    error_y = ~list(type="data", array=se, visible=TRUE)
  ) |> 
  layout(
    xaxis = list(title = "Year", range = c(1995, NA)),
    yaxis = list(title = "Relative Dominance")
  )


n_W6 <- length(fig_W6$x$data)


fig <- subplot(
  fig_W6, fig_W1,
  nrows = 1,
  shareY = FALSE,
  titleX = TRUE,
  titleY = TRUE,
  margin = 0.08) |> 
  layout(
    legend = list(title = list(text = "Species"), x = 0.47, y = 1, xanchor = "center", orientation = "r"),
    annotations = list(
      list(text = "Watershed 6", x = 0.15, y = 1, xref = "paper", yref = "paper",
           showarrow = FALSE, font = list(size = 16)),
      list(text = "Watershed 1", x = 0.83, y = 1, xref = "paper", yref = "paper",
           showarrow = FALSE, font = list(size = 16))),
    margin = list(
      l = 60,
      r = 60,
      t = 60,
      b = 60),
    xaxis = list(
      showgrid = FALSE,
      showline = TRUE,
      linecolor = "black",
      linewidth = 1,
      mirror = FALSE
    ),
    xaxis2 = list(   # second subplot
      showgrid = FALSE,
      showline = TRUE,
      linecolor = "black",
      linewidth = 1,
      mirror = FALSE
    ),
    yaxis = list(
      showgrid = FALSE,
      showline = TRUE,
      linecolor = "black",
      linewidth = 1
    ),
    yaxis2 = list(   # second subplot
      showgrid = FALSE,
      showline = TRUE,
      linecolor = "black",
      linewidth = 1
    ),
    modebar = list(
      bgcolor = "white",
      color = "black",
      activecolor = "#1B5E20"
    )) |> 
  style(showlegend = TRUE) |> 
  style(showlegend = FALSE, traces = seq(3, length(fig_W1$x$data)*2))

fig


output_file <- "chapters/forest_composition/BasalArea-W1W6_Trends.html"

fname <- tools::file_path_sans_ext(basename(output_file))

p <- fig |>
  config(toImageButtonOptions = list(format = "png", filename = fname))

# Write to temp dir where libdir can be relative
tmp_html <- tempfile(fileext = ".html")
htmlwidgets::saveWidget(p, file = tmp_html, selfcontained = TRUE)

# Copy the single file to your desired output location
file.copy(tmp_html, output_file, overwrite = TRUE)
unlink(tmp_html)
