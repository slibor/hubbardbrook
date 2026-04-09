library(tidyverse)
library(lubridate)
library(plotly)

# read in and combine watersheds
W2 <- read_csv(
  "https://pasta.lternet.edu/package/data/eml/knb-lter-hbr/4/19/a6aeef15070be913ee2f06f431b9b7a7"
) |>
  mutate(Watershed = "W2")

W4 <- read_csv(
  "https://pasta.lternet.edu/package/data/eml/knb-lter-hbr/6/19/54b3ae4a45a2bb6c7006c2ab45cf63b9"
) |>
  mutate(Watershed = "W4")

W5 <- read_csv(
  "https://pasta.lternet.edu/package/data/eml/knb-lter-hbr/7/19/c08ebaccab4fee5fb60f4eee77f06cb3"
) |>
  mutate(Watershed = "W5")

W6 <- read_csv(
  "https://pasta.lternet.edu/package/data/eml/knb-lter-hbr/8/19/3312389e77cc5fd06bc8a7c9019de0ed"
) |>
  mutate(Watershed = "W6")

All <- rbind(W2, W4, W5, W6) |>
  mutate(across(where(is.double), ~ na_if(., -888.88)))
head(All)

# add in date
All$DATE <- paste0(All$Year_Month, "-01")  # add day to year month string
All$DATE <- ymd(All$DATE) # change how R interprets Date to be a date

# add in water year
w_year <- as.numeric(format(All$DATE, "%Y"))

june_july_sept <- as.numeric(format(All$DATE, "%m")) < 6
w_year[june_july_sept] <- w_year[june_july_sept] - 1
All$wyear <- w_year

# make sure you are only using complete years for the record
monchem <- as.data.frame(table(All$wyear))

monchem$wys <- paste(monchem$Var1)
monchem[monchem$Freq < 40, "Use"] <- "incomplete wyear" # incomplete is less then 12 months
monchem[is.na(monchem$Use), "Use"] <- "complete"
All$Use <- monchem$Use[match(All$wyear, monchem$wys)]
All_complete <- All[All$Use == "complete", ]

manage <- All_complete |>
  group_by(Watershed, wyear) |>
  summarize(
    Ca_flux = sum(Ca_flux, na.rm = TRUE),
    flow_mm = sum(flow_mm, na.rm = TRUE),
    .groups = "drop"
  )

# units
manage <- manage |>
  mutate(flow_m = flow_mm / 1000) |>
  mutate(Ca_mg = Ca_flux * 1000)

# spread manage, then subset each watershed.
Ca <- spread(manage, "Watershed", "Ca_mg")
Fl <- spread(manage, "Watershed", "flow_m")

# Ca conc  (is in grams, multiply by hectares to just get grams)
Ca <- Ca |>
  mutate(W2 = W2 * 15.6) |>
  mutate(W4 = W4 * 36.1) |>
  mutate(W5 = W5 * 21.9) |>
  mutate(W6 = W6 * 13.2)

# Water is in meters, multiply by area of watershed in meters
Fl <- Fl |>
  mutate(W2 = W2 * 15.6 * 10000) |>
  mutate(W4 = W4 * 36.1 * 10000) |>
  mutate(W5 = W5 * 21.9 * 10000) |>
  mutate(W6 = W6 * 13.2 * 10000)

cag <- gather(Ca, "Watershed", "Ca_mg", 5:8)
flag <- gather(Fl, "Watershed", "flow_m3", 5:8)

flag <- flag |>
  mutate(flow_L = flow_m3 * 1000)

cag <- cag |>
  mutate(flow_L = flag$flow_L) |>
  mutate(camgL = Ca_mg / flow_L)


fa <- spread(cag, "Watershed", "camgL")

fa1 <- gather(fa, "Watershed", "camgL", c(7, 10))
fa2 <- gather(fa, "Watershed", "camgL", c(8, 10))
fa3 <- gather(fa, "Watershed", "camgL", c(9, 10))

fa1 <- fa1[, c(1, 9, 10)]
fa1 <- na.omit(fa1)
fa1 <- spread(fa1, "Watershed", "camgL")

fa2 <- fa2[, c(1, 9, 10)]
fa2 <- na.omit(fa2)
fa2 <- spread(fa2, "Watershed", "camgL")

fa3 <- fa3[, c(1, 9, 10)]
fa3 <- na.omit(fa3)
fa3 <- spread(fa3, "Watershed", "camgL")

# PLOTS
theme_set(theme_bw())

g1 <- ggplot(fa1) +
  geom_line(aes(x = wyear, y = W6), size = 0.8, color = "black") +
  geom_line(aes(x = wyear, y = W2), size = 0.8, color = "black") +
  geom_point(
    aes(x = wyear, y = W6),
    shape = 21,
    size = 4,
    stroke = 1.5,
    color = "black",
    fill = "white"
  ) +
  geom_point(
    aes(x = wyear, y = W2),
    shape = 21,
    size = 4,
    stroke = 1.5,
    color = "black",
    fill = "black"
  ) +
  theme(
    axis.text.x = element_text(size = 17),
    axis.text.y = element_text(size = 17),
    axis.title.x = element_text(size = 20),
    axis.title.y     = element_text(size = 20),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.text      = element_text(size = 22),
    plot.title = element_text(size = 26)
  ) +
  geom_text(aes(x = 2008, y = 7, label = "treatment"),
            size = 6 ,
            color = "black") +
  geom_text(aes(x = 2008, y = 6, label = "reference"),
            size = 6,
            color = "black") + ylab("Ca (mg/L)") + xlab("Water Year (June 1)") +
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(1960, 2023),
    breaks = seq(1960, 2020, 5)
  ) +
  scale_y_continuous(limits = c(0, 9), breaks = seq(0, 8, 2)) +
  annotate(
    "point",
    x = 2004,
    y = 7,
    size = 4,
    shape = 21,
    stroke = 1.5,
    color = "black",
    fill = "black"
  ) +
  annotate(
    "point",
    x = 2004,
    y = 6,
    size = 4,
    shape = 21,
    stroke = 1.5,
    color = "black",
    fill = "white"
  ) +
  geom_text(
    aes(x = 1971, y = 8.5, label = "W2: devegetated in 1965-1968"),
    size = 7,
    color = "black"
  )
g1

g2 <- ggplot(fa2) +
  geom_line(aes(x = wyear, y = W6), size = 0.8, color = "black") +
  geom_line(aes(x = wyear, y = W4), size = 0.8, color = "black") +
  geom_point(
    aes(x = wyear, y = W6),
    shape = 21,
    size = 4,
    stroke = 1.5,
    color = "black",
    fill = "white"
  ) +
  geom_point(
    aes(x = wyear, y = W4),
    shape = 21,
    size = 4,
    stroke = 1.5,
    color = "black",
    fill = "black"
  ) +
  theme(
    axis.text.x = element_text(size = 17),
    axis.text.y = element_text(size = 17),
    axis.title.x = element_text(size = 20),
    axis.title.y     = element_text(size = 20),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.text      = element_text(size = 22),
    plot.title = element_text(size = 26)
  ) +
  geom_text(aes(x = 2008, y = 3, label = "treatment"),
            size = 6 ,
            color = "black") +
  geom_text(aes(x = 2008, y = 2.6, label = "reference"),
            size = 6,
            color = "black") + ylab("Ca (mg/L)") + xlab("Water Year (June 1)") +
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(1960, 2023),
    breaks = seq(1960, 2020, 5)
  ) +
  annotate(
    "point",
    x = 2004,
    y = 3,
    size = 4,
    shape = 21,
    stroke = 1.5,
    color = "black",
    fill = "black"
  ) +
  annotate(
    "point",
    x = 2004,
    y = 2.6,
    size = 4,
    shape = 21,
    stroke = 1.5,
    color = "black",
    fill = "white"
  ) +
  ylim(0, 3.5) +
  geom_text(
    aes(x = 1972, y = 3.2, label = "W4: strip cut in 1970, 1972, 1974"),
    size = 7 ,
    color = "black"
  )
g2


g3 <- ggplot(fa3) +
  geom_line(aes(x = wyear, y = W6), size = 0.8, color = "black") +
  geom_line(aes(x = wyear, y = W5), size = 0.8, color = "black") +
  geom_point(
    aes(x = wyear, y = W6),
    shape = 21,
    size = 4,
    stroke = 1.5,
    color = "black",
    fill = "white"
  ) +
  geom_point(
    aes(x = wyear, y = W5),
    shape = 21,
    size = 4,
    stroke = 1.5,
    color = "black",
    fill = "black"
  ) +
  theme(
    axis.text.x = element_text(size = 17),
    axis.text.y = element_text(size = 17),
    axis.title.x = element_text(size = 20),
    axis.title.y     = element_text(size = 20),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.text      = element_text(size = 22),
    plot.title = element_text(size = 26)
  ) +
  geom_text(aes(x = 2008, y = 3, label = "treatment"),
            size = 6 ,
            color = "black") +
  geom_text(aes(x = 2008, y = 2.6, label = "reference"),
            size = 6,
            color = "black") + ylab("Ca (mg/L)") + xlab("Water Year (June 1)") +
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(1960, 2023),
    breaks = seq(1960, 2020, 5)
  ) +
  annotate(
    "point",
    x = 2004,
    y = 3,
    size = 4,
    shape = 21,
    stroke = 1.5,
    color = "black",
    fill = "black"
  ) +
  annotate(
    "point",
    x = 2004,
    y = 2.6,
    size = 4,
    shape = 21,
    stroke = 1.5,
    color = "black",
    fill = "white"
  ) +
  ylim(0, 3.5) +
  geom_text(
    aes(x = 1973, y = 3.2, label = "W5: whole tree harvest in 1983, 1984"),
    size = 7 ,
    color = "black"
  )
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

# use temp dir so only html is saved 
output_file <- "chapters/forest_management/StreamwaterCalcium.html"
fname <- tools::file_path_sans_ext(basename(output_file))

pfinal |>
  config(toImageButtonOptions = list(format = "png", filename = fname))

tmp_html <- tempfile(fileext = ".html")
htmlwidgets::saveWidget(pfinal, file = tmp_html, selfcontained = TRUE)

file.copy(tmp_html, output_file, overwrite = TRUE)
unlink(tmp_html)