### Dependent variable distribution in each cluster
###
dep_plot_ds <- sample_tree |> 
  select(co2.heating, cluster, mean_co2_heating_cluster) 

color_palette <- viridis::inferno(10, direction = -1)

dep_clust <- ggplot(dep_plot_ds, aes(x = co2.heating)) +
  geom_density(aes(fill = factor(cluster)), alpha = 0.5) +  # Adjust alpha for transparency
  scale_fill_manual(values = color_palette) + 
  facet_wrap(~ cluster, scales = "free_y") +  # Facet by cluster, allow y scales to vary
  theme_minimal() +
  geom_vline(aes(xintercept = mean_co2_heating_cluster), color = "black") +  
  labs(title = "Density of CO2 Heating Values by Cluster",
       x = "CO2 Heating",
       y = "Density",
       fill = "Cluster") +
  theme(strip.text.x = element_text(size = 10),
        legend.position = "none")

dep_clust
ggsave("dep_clust.png", plot = dep_clust, width = 16, height = 12, dpi = 300)

### Socio-economic and demographic variables distributions in each cluster
### 
sample_tree_joined 






