# Predict the terminal node assignment for each observation
node_assignments <- predict(ctree_model, newdata = dat, type = "node") 

# Split the data by node assignments
subgroups_data <- split(dat, node_assignments)

# Add the node assignment as a new variable to the processed dataset
sample_tree <- dat %>%
  mutate(cluster = as.factor(node_assignments))

library(giscoR)
library(httr2)
library(sf)

### NUTS3 Level
germany_nuts3 <- giscoR::gisco_get_nuts(
  nuts_level = 3,
  year = "2016",  # Choose the year of interest; "2016" is used here as an example
  epsg = "4326",
  country = "DE",  # DE for Germany
  cache = TRUE,
  update_cache = TRUE,  # This will update the cache if you've previously fetched data
  verbose = TRUE  # This enables messages that might help in troubleshooting
)

germany_nuts_geometries <- germany_nuts3 |> 
  select(c(NUTS_ID, URBN_TYPE, NUTS_NAME, geometry)) |> 
  rename(geometry_nuts = 'geometry')

nuts_lau <- read.csv("/Users/giocopp/Desktop/Uni/Hertie School/4th Semester/Thesis/Analysis/Code/Germany-LAU-2019-NUTS-2016.csv")

nuts_lau <- nuts_lau %>% 
  mutate(LAU.CODE = as.character(LAU.CODE))

# Now perform the left join
sample_tree_cluster <- sample_tree %>% 
  mutate(lau = as.character(lau)) %>% 
  left_join(nuts_lau, by = c("lau" = "LAU.CODE"))

sample_tree_geom <-  sample_tree_cluster |> 
  left_join(germany_nuts_geometries, by = c("NUTS.3.CODE" = "NUTS_ID"))

# saveRDS(sample_tree_geom, "sample_tree_geom.rds")


