library(tidyverse)
library(lubridate)
library(plotly)

# install and load devtools
install.packages("devtools")
library(devtools)
# install and load HubbardBrookForestAnalytics
devtools::install_github('kearutherford/HubbardBrookForestAnalytics')
library(HubbardBrookForestAnalytics) 

# HB Forest Analytics with R package 
#install.packages("FAwR")
library(FAwR) 

W1 <- read_csv(
  "https://pasta.lternet.edu/package/data/eml/knb-lter-hbr/446/1/95498fa6cf9986255fae32c1924182d9"
)

W6 <- read_csv(
  "https://pasta.lternet.edu/package/data/eml/knb-lter-hbr/448/1/ac44ba6b0f0aac1d8e5e270a63d0e009"
)

# filter for live trees 
W1_live <- W1 |> filter(status == "Live") 
W6_live <- W6 |> filter(status == "Live") 

# use FAwR to calculate above ground biomass per tree 
W1_live |> mutate(agb_tree = SSallometric(dbh_cm, alpha, beta)) 
W6_live |> mutate(agb_tree = SSallometric(dbh_cm, alpha, beta)) 

# calc agb per plot, total plot agb = sum(agb_tree * expansion factor) 
  
# calc agb per hectare, agb_ha = agb_plot/plot area 

# graph 

