library(tidyverse)
library(lubridate)

# read in and combine watersheds
W2 <- read_csv(
  "https://pasta.lternet.edu/package/data/eml/knb-lter-hbr/4/19/a6aeef15070be913ee2f06f431b9b7a7"
) |>
  mutate(Watershed = "W2")

W4 <- read_csv(
  "https://pasta.lternet.edu/package/data/eml/knb-lter-hbr/6/19/54b3ae4a45a2bb6c7006c2ab45cf63b9"
)|>
  mutate(Watershed = "W4")

W5 <- read_csv(
  "https://pasta.lternet.edu/package/data/eml/knb-lter-hbr/7/19/c08ebaccab4fee5fb60f4eee77f06cb3"
)|>
  mutate(Watershed = "W5")

W6 <- read_csv(
  "https://pasta.lternet.edu/package/data/eml/knb-lter-hbr/8/19/3312389e77cc5fd06bc8a7c9019de0ed"
)|>
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


