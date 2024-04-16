##### Giorgio Coppola #####
### Hertie School
### AY 2023/2024 
### Master Thesis
## Data Manipulation: Full script

### Load packages:
pacman::p_load(tidyverse, ggplot2, knitr, kableExtra, readxl)
options(scipen = 999)

sample_reduced_synthpop <- readRDS("~/Desktop/sample_synthpop_D.rds")

synthpop <- as.data.frame(sample_reduced_synthpop)

##### 
##### Script 1
##### 
synthpop <- synthpop |>
  mutate(across(starts_with("exp."), ~ .x * 4), # Annualize quarterly data
         across(starts_with("inc."), ~ .x * 4))

# Expand the factor levels of 'heating.energy' and recode as specified.
synthpop$heating.energy <- fct_expand(synthpop$heating.energy, "6")

synthpop <- synthpop |>
  dplyr::rename(heating.system = heating) |> 
  mutate(heating.energy = case_when(
    heating.energy == "5" ~ "6", # Existing 'other' now becomes "6"
    heating.system == 1 ~ "5",   # If heating.system is district heating, set heating.energy to "5"
    TRUE ~ as.character(heating.energy)))


# Code adapted from Johannes code: p/projects/bymarka/synthpop/6_1_simulate_carbontax_final.R
synthpop <- synthpop |> 
  mutate(
    exp.heating.gas = case_when(heating.energy == 2 ~ exp.heating,
                                heating.energy != 2 ~ 0),
    exp.heating.oil = case_when(heating.energy == 3 ~ exp.heating,
                                heating.energy != 3 ~ 0),
    exp.heating.district = case_when(heating.energy == 5 ~ exp.heating,
                                     heating.energy != 5 ~ 0),
    pj.heating.gas = case_when(heating.energy == 2 ~ 3.6e-9 * (1/0.063) * exp.heating, 
                               # 6.3 cents per kW/h times price, conversion from kW/h to Pj (2019 prices)
                               heating.energy != 2 ~ 0),
    pj.heating.oil = case_when(heating.energy == 3 ~ 3.53e-8 * (1/0.81) * exp.heating, 
                               #.8.1 cents per kW/h times price
                               heating.energy != 3 ~ 0),
    tj.heating.gas = pj.heating.gas * 1000,
    tj.heating.oil = pj.heating.oil * 1000)

# Estimate heating expenditure and energy consumption for district heating with federal prices
dh.prices <- read_excel("~/Desktop/Uni/Hertie School/4th Semester/Thesis/Data/Primary data/price_data_district_heating.xlsx") 

dh.prices <-  dh.prices |>
  mutate(
    dhp.mixed.15kW = as.numeric(as.character(rhp.mixed.15kW)),
    dhp.work.15kW = as.numeric(as.character(rhp.work.15kW)),
    dhp.mixed.160kW = as.numeric(as.character(rhp.mixed.160kW)),
    dhp.work.160kW = as.numeric(as.character(rhp.work.160kW)),
    dhp.base.15kW = dhp.mixed.15kW - dhp.work.15kW,
    dhp.base.160kW = dhp.mixed.160kW - dhp.work.160kW,
    state = factor(state)) |> 
  dplyr::select(state, dhp.mixed.15kW, dhp.work.15kW, dhp.base.15kW, dhp.mixed.160kW, dhp.work.160kW, dhp.base.160kW, dhp.base.160kW)

synthpop <- left_join(synthpop, dh.prices, by = "state")

### Calculate Remote Heating Consumption in physical units for each household:
### U = (Y - base)/ P_work
synthpop <- synthpop |>
  group_by(state) |>
  mutate(
    tj.district.160 = ifelse(heating.system == 1, 
                           ((exp.heating) / dhp.work.160kW) 
                           * 0.0036, # to convert from MWh to TJ
                           0)) |> 
  ungroup()

### Calculate Emissions
### Gas Oil and RMH
# data from UBA
#https://www.umweltbundesamt.de/sites/default/files/medien/479/publikationen/cc_29-2022_emission-factors-fossil-fuels.pdf
# and https://www.bafa.de/SharedDocs/Downloads/DE/Energie/eew_infoblatt_co2_faktoren_2021.html (and converted)

# Emission factors
emission.factor.gas <- 56   # in tCO2/TJ for natural gas: https://www.climatiq.io/data/emission-factor/f255635c-3b18-4900-9297-f3312b4b3a30
emission.factor.oil <- 74   # in tCO2/TJ for oil (light heating oil): https://www.climatiq.io/data/emission-factor/9040e7ce-3774-4031-9e98-5281a134831a
emission.factor.rm.avg <- 72.25  # tCO2e/TJ for district heating mix (average), form:https://www.climatiq.io/data/emission-factor/b41b2b9a-db90-48a1-9770-af6649d366cf
# DH emission factors will be at the Bundesländer level

# Conversion:
synthpop <- synthpop |>
  mutate(co2.gas = tj.heating.gas * emission.factor.gas,
         co2.oil = tj.heating.oil * emission.factor.oil,
         co2.dh.160 = tj.district.160 * emission.factor.rm.avg,
         tj.heating = tj.heating.gas + tj.heating.oil + tj.district.160,
         co2.heating = co2.gas + co2.oil + co2.dh.160)
  
# Factors expressed in tons of CO2 per household
# Emissions are in tCO2/unit of time of consumption

### Normalization: Emissions per sqm
# Careful because space is categorical
# We have to use mid points of each category

space_mid_points <- c(30, 50, 70, 90, 110, 130, 150, 170, 190, 220) # midpoint +220 is arbitrary

### Emissions per sqm
synthpop <- synthpop |>
  mutate(
    # Find the mid-point for each space category
    space.mid = case_when(
      space == 1 ~ space_mid_points[1],
      space == 2 ~ space_mid_points[2],
      space == 3 ~ space_mid_points[3],
      space == 4 ~ space_mid_points[4],
      space == 5 ~ space_mid_points[5],
      space == 6 ~ space_mid_points[6],
      space == 7 ~ space_mid_points[7],
      space == 8 ~ space_mid_points[8],
      space == 9 ~ space_mid_points[9],
      space == 10 ~ space_mid_points[10],
      TRUE ~ NA_real_
    ),
    # Normalize CO2 emissions by the mid-point of space categories
    co2.sqm.gas = co2.gas / space.mid,
    co2.sqm.oil = co2.oil / space.mid,
    co2.sqm.dh.160 = co2.dh.160 / space.mid,
    co2.heating.sqm = co2.sqm.gas + co2.sqm.oil + co2.sqm.dh.160,
    eui.tj= tj.heating / space.mid, # energy use intensity by space category
    eui = ((tj.heating * 277777.78) / space.mid)) |> 
  mutate(eui = as.numeric(eui))

#####
##### Script 2
##### Add HDD data

### HDD NUTS3
hdd_nuts3 <- readxl::read_excel("~/Desktop/Uni/Hertie School/4th Semester/Thesis/Data/Primary data/hdd-nuts3.xltx") |>
  as.data.frame(hdd_nuts3) |> 
  dplyr::rename('nuts3' = 'GEO (Codes)', 'nuts3_name' = 'GEO (Labels)')

conversion <-  readxl::read_excel("~/Desktop/Uni/Hertie School/4th Semester/Thesis/Data/Primary data/lau-nuts-conv.xlsx") |> 
  as.data.frame() |> 
  dplyr::select('NUTS 3 CODE', 'LAU CODE', 'LAU NAME NATIONAL', 'POPULATION', 'TOTAL AREA (km2)', 'DEGURBA') |> 
  dplyr::rename('nuts3' = 'NUTS 3 CODE', 'lau' = 'LAU CODE', 'lau.name' = 'LAU NAME NATIONAL', 'population' = 'POPULATION', 'area' = 'TOTAL AREA (km2)', 'degurba' = 'DEGURBA')

conversion$lau.name <- as.character(conversion$lau.name)

hdd_nuts3 <- hdd_nuts3 |>
  dplyr::left_join(conversion, by = "nuts3")

hdd_nuts3 <- hdd_nuts3 |> # Only considering NUTS3 codes of Lower Saxony
  dplyr::rename(reg_name = lau.name)

hdd_nuts3_distinct <- hdd_nuts3 %>% 
  group_by(reg_name) %>%
  slice(1) %>%  # This keeps the first row for each reg_name. Adjust this as necessary.
  ungroup()

# Now perform the left join
synthpop <- synthpop %>%
  left_join(hdd_nuts3_distinct, by = "reg_name") %>%
  ungroup() 

#####
##### Scrip 3
##### 

### Disposable income / adult equivalent and dummies for household composition
# Assuming synthpop is already loaded with inc.disposable present.

# Calculate the household composition dummies.
synthpop <- synthpop |> 
  mutate(
    head.household = if_else(relation == 1, 1, 0),
    adult = if_else(relation %in% c(2, 4, 5), 1, 0),
    child = if_else(relation == 3, 1, 0),
    over65 = if_else(relation %in% c(1, 2, 4, 5) & age %in% c(6, 7), 1, 0)
  )

# Summarize data by household ID (hid), and create a temporary summary dataset.
household_summary <- synthpop |> 
  group_by(hid) |>
  summarise(
    n.adults = sum(adult) + sum(head.household),
    n.children = sum(child),
    n.add.adults = sum(adult),
    n.over65 = sum(over65),
    .groups = 'drop'
  )

# Join the summary back to the original dataset to preserve all original columns.
synthpop <- synthpop |> 
  left_join(household_summary, by = "hid")

# Perform the final mutations involving inc.disposable.
synthpop <- synthpop |> 
  mutate(
    equivalence.size = 1 + n.add.adults * 0.5 + n.children * 0.3,
    inc.disp.adult.eq = inc.disposable / equivalence.size,
    children.dummy = if_else(n.children > 0, 1, 0),  
    over65.dummy = if_else(n.over65 > 0, 1, 0)
  )

# synthpop <- synthpop |>   
#   dplyr::select(-c(n.adults, n.add.adults, area, population, co2.sqm.dh.160, co2.sqm.oil, co2.sqm.gas, space.mid, co2.dh.160, co2.oil, co2.gas, tj.district.160, dhp.base.160kW, dhp.work.160kW, dhp.mixed.160kW, dhp.base.15kW, dhp.work.15kW, tj.heating.oil, tj.heating.gas, pj.heating.oil, pj.heating.gas)) |> 
#   mutate(across(c(eui, inc.disposable), ~round(., digits = 2)))

# saveRDS(synthpop, file = "~/Desktop/Uni/Hertie School/4th Semester/Thesis/Data/Derived data/synthpop_total_sample.rds")