# Load necessary libraries
pacman::p_load(tidyverse, ggplot2, glmnet, caret, partykit, rsample)
options(scipen = 999)

synthpop_total_sample <- readRDS("~/Desktop/Uni/Hertie School/4th Semester/Thesis/Data/Derived data/synthpop_total_sample.rds")
  # select(-c(n.adults, n.add.adults, area, population, co2.sqm.dh.160, co2.sqm.oil, co2.sqm.gas, space.mid, co2.dh.160, co2.oil, co2.gas, tj.district.160, dhp.base.160kW, dhp.work.160kW, dhp.mixed.160kW, dhp.base.15kW, dhp.work.15kW, tj.heating.oil, tj.heating.gas, pj.heating.oil, pj.heating.gas)) |> 

# Data transformation and cleaning
sample <- synthpop_total_sample %>%
  filter(heating.energy %in% c(2, 3, 5)) %>%
  mutate(
    co2.scale = scale(co2.heating),
    inc.disp.adult.eq.scale = scale(inc.disp.adult.eq),
    inc.disp.adult.eq.dec = cut(
      inc.disp.adult.eq, 
      breaks = quantile(inc.disp.adult.eq, probs = seq(0, 1, by = 0.1)),
      labels = paste0(1:10), 
      include.lowest = TRUE
    )
  ) %>%
  mutate(inc.disp.adult.eq.quart = ntile(inc.disp.adult.eq, 4)) %>%
  mutate(
    hsize = factor(hsize, levels = 1:6, ordered = TRUE),
    htype = factor(htype, ordered = FALSE),
    space = factor(space, levels = 1:10, ordered = TRUE),
    inc.disp.adult.eq.dec = factor(inc.disp.adult.eq.dec, levels = 1:10, ordered = TRUE),
    heating.energy = factor(heating.energy, levels = c("2", "3", "5"), ordered = FALSE),
    building.type = factor(building.type),
    building.age = factor(building.age),
    owner = factor(owner),
    edu = factor(edu),
    hid = as.numeric(hid),
    lau = as.numeric(lau),
    heating.system = factor(heating.system),
  )

sample$inc.disposable.log <- log(sample$inc.disposable + 1)
sample$inc.disp.adult.eq.log <- log(sample$inc.disp.adult.eq + 1)

# Select relevant columns for sample_tree
sample_tree <- sample |> 
  mutate(across(c(inc.disposable.log, inc.disp.adult.eq, inc.disposable.log, inc.disp.adult.eq.log), ~round(., digits = 2))) |> 
  dplyr::select(-c(nuts3_name, eui.tj, co2.heating.sqm, co2.sqm.dh.160, co2.sqm.oil, co2.sqm.gas,
         co2.dh.160, co2.oil, co2.gas, tj.district.160, dhp.base.160kW, dhp.work.160kW, dhp.mixed.160kW, dhp.base.15kW,
         dhp.work.15kW, pj.heating.gas, pj.heating.oil, tj.heating.gas, tj.heating.oil))

saveRDS(sample_tree, "~/Desktop/Uni/Hertie School/4th Semester/Thesis/Data/Derived data/sample_tree.rds")
