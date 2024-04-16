library(viridis)
library(RColorBrewer)

sample_tree_nuts <- sample_tree_geom %>%
  group_by(NUTS.3.CODE) %>%
  mutate(mean_co2_heating_nuts = mean(co2.heating, na.rm = TRUE)) %>%
  ungroup()

sample_tree_nuts_sf <- sf::st_as_sf(sample_tree_nuts, sf_column_name = "geometry_nuts")

### Plot Emissions by NUTS3
map_nuts_emissions <- ggplot(sample_tree_nuts_sf) +
  geom_sf(aes(fill = mean_co2_heating_nuts)) +
  scale_fill_viridis_c(option = "inferno", direction = -1) +  # Use inferno color palette for continuous variable
  labs(title = "Average CO2 Emissions from Heating by NUTS3 Region in Germany",
       fill = "Mean of CO2 Emissions from Hearing") +
  theme_minimal()

map_nuts_emissions
ggsave("map_nuts_emissions.png", plot = map_nuts_emissions, width = 16, height = 12, dpi = 300)


### Plot Cluster mode by NUTS3
#
# First, calculate the mean CO2 heating per cluster
cluster_means <- sample_tree_geom %>%
  group_by(cluster) %>%
  summarize(mean_co2_heating_cluster = mean(co2.heating, na.rm = TRUE)) %>%
  ungroup()

# Arrange the clusters from least to most emitting
cluster_means <- cluster_means %>%
  arrange(mean_co2_heating_cluster)

sample_tree_geom <- sample_tree_geom %>%
  mutate(cluster = factor(cluster, levels = cluster_means$cluster))

# To find the mode cluster for each NUTS 3 region, we can count the occurrences of each cluster
cluster_mode <- sample_tree_geom %>%
  group_by(NUTS.3.CODE, cluster) %>%
  summarise(count_cluster_nuts = n(), .groups = 'drop') %>%
  arrange(NUTS.3.CODE, desc(count_cluster_nuts)) %>%
  group_by(NUTS.3.CODE) %>%
  slice(1) %>%
  ungroup() |> 
  select(c(NUTS.3.CODE, cluster)) |> 
  rename(cluster_mode_nuts = cluster)

# Perform the join
sample_tree_joined <- sample_tree_geom %>%
  left_join(cluster_mode, by = "NUTS.3.CODE")

cluster_means <- as_tibble(cluster_means) |> 
  select(c(cluster, mean_co2_heating_cluster))

sample_tree_joined <- sample_tree_joined |> 
  left_join(cluster_means, by = "cluster")

# Converting back to an sf object correctly
# This step is crucial for ensuring that the spatial data is recognized for plotting or spatial analysis
sample_tree_sf <- st_as_sf(sample_tree_joined, sf_column_name = "geometry_nuts")

color_palette <- viridis::inferno(8, direction = -1)

# Plot using the transformed spatial data
map_cluster_emissions <- ggplot(sample_tree_sf) +
  geom_sf(aes(fill = cluster)) +
  scale_fill_manual(values = color_palette) + 
  labs(title = "Most Frequent Cluster in NUTS3 Region in Germany",
       fill = "Most Frequent Cluster") +
  theme_minimal()

map_cluster_emissions
ggsave("map_cluster_emissions.png", plot = map_cluster_emissions, width = 16, height = 12, dpi = 300)

### Bar plot 
# 
# Arrange the data frame by mean_co2_heating_cluster
# Use the arranged data for plotting
bar_plot <- ggplot(cluster_means, aes(x = reorder(cluster, mean_co2_heating_cluster), y = mean_co2_heating_cluster, fill = mean_co2_heating_cluster)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_c(option = "inferno", direction = -1) +
  labs(x = "Cluster", y = "Mean of CO2 Emissions form Heating", title = "Mean of CO2 Emissions from Heating by Cluster") +
  theme_minimal() +
  theme(legend.position = "none")

# Display the plot
bar_plot
ggsave("bar_plot.png", plot = bar_plot, width = 12, height = 8, dpi = 300)

### 
### Univariate quantile
###
sample_tree_joined$co2.heating.quartile <- cut(sample_tree_joined$co2.heating,
                                               breaks = quantile(sample_tree$co2.heating, probs = 0:8/8),
                                               include.lowest = TRUE,
                                               labels = FALSE)

sample_tree_joined <- sample_tree_joined |> 
  mutate(co2.heating.quartile = as.factor(co2.heating.quartile))

sample_tree_sf <- st_as_sf(sample_tree_joined, sf_column_name = "geometry_nuts")

color_palette <- viridis::inferno(8, direction = -1)

# Plot using the transformed spatial data
map_quantiles_emissions <- ggplot(sample_tree_sf) +
  geom_sf(aes(fill = co2.heating.quartile)) +
  scale_fill_manual(values = color_palette) + 
  labs(title = "Most Frequent Quantile in NUTS3 Region in Germany",
       fill = "Most Frequent Quantile") +
  theme_minimal()

map_quantiles_emissions
ggsave("map_quantiles_emissions.png", plot = map_cluster_emissions, width = 16, height = 12, dpi = 300)