library(tidyverse)
library(lubridate)
library(plotly)
library(EDIutils)

# pull most recent data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("../../functions/getEDItable-function.R")

# read in and combine watersheds
W2 <- get_edi_table(identifier = "4", entity_seq = 1) |>
  mutate(Watershed = "W2")

W4 <- get_edi_table(identifier = "6", entity_seq = 1) |>
  mutate(Watershed = "W4")

W5 <- get_edi_table(identifier = "7", entity_seq = 1) |>
  mutate(Watershed = "W5")

W6 <- get_edi_table(identifier = "8", entity_seq = 1) |>
  mutate(Watershed = "W6")

All <- rbind(W2, W4, W5, W6) |>
  mutate(across(where(is.double), ~ na_if(., -888.88)))
head(All)

# format date and add water year
All <- All |> 
  # add day to year month string 
  mutate(DATE = ymd(paste0(Year_Month, "-01"))) |> 
  # water year starts in June 
  mutate(wyear = year(DATE), 
         wyear = if_else(month(DATE) < 6, wyear - 1, wyear)) 

# only use complete water year 
complete_wyear <- All |> 
  count(wyear) |> 
  mutate(Use = if_else(n < 40, "incomplete", "complete")) 

All_complete <- 
  right_join(All, complete_wyear, by = "wyear") |> 
  filter(Use == "complete") 

# sum calcium flux and flow 
annual_sum <- All_complete |>
  group_by(Watershed, wyear) |>
  summarize(
    Ca_flux = sum(Ca_flux, na.rm = TRUE),
    flow_mm = sum(flow_mm, na.rm = TRUE),
    .groups = "drop"
  )

# convert units
annual_sum <- annual_sum |>
  mutate(flow_m = flow_mm / 1000) |>
  mutate(Ca_mg = Ca_flux * 1000)

# add in areas for conversions
watershed_areas = c(
  W2 = 15.6,
  W4 = 36.1, 
  W5 = 21.9,
  W6 = 13.2
)

# multiply by area and convert to final concentration 
annual_sum <- annual_sum |>
  mutate(
    area_ha = watershed_areas[Watershed], 
    total_Ca_mg = Ca_mg * area_ha, 
    flow_m3 = flow_m * area_ha * 10000
    ) |> 
  mutate(
    flow_L = flow_m3 * 1000, 
    Ca_mgL = total_Ca_mg / flow_L)

# comparing all to watershed 6 
compare_watersheds = annual_sum |>
  select(Watershed, wyear, Ca_mgL) |> 
  pivot_wider(
    names_from = Watershed,
    values_from = Ca_mgL
  )

# PLOTS

# plot function to simplify code 
make_plot <- function(data, watershed, label_text, y_max) {
  
  ggplot(data) +
    
    # Reference watershed (W6)
    geom_line(aes(x = wyear, y = W6), linewidth = 0.8, color = "black") +
    geom_point(aes(x = wyear, y = W6), shape = 21, size = 4,
               stroke = 1, color = "black", fill = "white") + 
    geom_text(aes(x = 2008, y = y_max * 0.7, label = "reference"),
              size = 6, color = "black") + 
    annotate("point", x = 2004, y = y_max * 0.7, size = 4, shape = 21,
             stroke = 1, color = "black", fill = "white") +
    
    # Treatment watershed (W2, W4, or W5)
    geom_line(aes(x = wyear, y = watershed), linewidth = 0.8, color = "black") +
    geom_point(aes(x = wyear, y = watershed), shape = 21, size = 4,
               stroke = 1, color = "black", fill = "black") +
    geom_text(aes(x = 2008, y = y_max * 0.8, label = "treatment"),
              size = 6, color = "black") + 
    annotate("point", x = 2004, y = y_max * 0.8, size = 4, shape = 21,
             stroke = 1, color = "black", fill = "black") + 
    
    # labels and formatting 
    labs(x = "Water Year (June 1)", y = "Ca (mg/L)") +
    
    scale_x_continuous(
      limits = c(1960, 2023),
      breaks = seq(1960, 2020, 5),
      expand = c(0, 0)
    ) +
    
    coord_cartesian(ylim = c(0, y_max), clip = "off") + # doesn't cut off text labels 
    
    annotate("text", x = 1975, y = y_max * 0.90, label = label_text, size = 6) +
    
    theme_bw() +
    theme(
      panel.grid = element_blank(),
      axis.text = element_text(size = 16),
      axis.title = element_text(size = 18)
    )
}

g1 <- make_plot(compare_watersheds, compare_watersheds$W2, "W2: devegetated in 1965-1968", 9) +
  scale_y_continuous(limits = c(0, 9), breaks = seq(0, 8, 2)) # fixes breaks 
g1

g2 <- make_plot(compare_watersheds, compare_watersheds$W4, "W4: strip cut in 1970, 1972, 1974", 3.5) 
g2

g3 <- make_plot(compare_watersheds, compare_watersheds$W5, "W5: whole tree harvest in 1983, 1984", 3.5) 
g3

# convert to plotly graph
p1 <- ggplotly(g1) |>
  layout(modebar = list(
    bgcolor = "white",
    color = "black",
    activecolor = "#1B5E20"
  ))
p2 <- ggplotly(g2) |>
  layout(modebar = list(
    bgcolor = "white",
    color = "black",
    activecolor = "#1B5E20"
  ))
p3 <- ggplotly(g3) |>
  layout(modebar = list(
    bgcolor = "white",
    color = "black",
    activecolor = "#1B5E20"
  ))

# combine the plots into a grid
pfinal <- subplot(
  p1,
  p2,
  p3,
  nrows = 3,
  shareX = TRUE,
  titleY = TRUE,
  margin = 0.01
)
pfinal

# use temp dir so only html is saved
setwd("../../")
output_file <- "chapters/forest_management/StreamwaterCalcium.html"
fname <- tools::file_path_sans_ext(basename(output_file))

pfinal |>
  config(toImageButtonOptions = list(format = "png", filename = fname))

tmp_html <- tempfile(fileext = ".html")
htmlwidgets::saveWidget(pfinal, file = tmp_html, selfcontained = TRUE)

file.copy(tmp_html, output_file, overwrite = TRUE)
unlink(tmp_html)