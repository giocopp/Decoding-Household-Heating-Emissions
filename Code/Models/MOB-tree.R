sample_tree <-  readRDS("~/Desktop/Uni/Hertie School/4th Semester/Thesis/Analysis/Data/Derived data/sample_tree.rds")

# Set seed for reproducibility
set.seed(123)
options(scipen = 999)

# Assuming sample_tree is your dataset and you've performed necessary data cleaning steps
dat <- sample_tree |> 
  rename(income_decile = inc.disp.adult.eq.dec) |> 
  dplyr::select(c(over65.dummy, hsize, htype, income_decile, space, building.age, building.type, heating.energy, degurba, co2.heating, owner))

# split data into training and test data
data_split <- initial_split(dat, prop = 0.70)
train_data <- training(data_split)
test_data <- testing(data_split)

# fit glmtree (mob)
mob_tree <- glmtree(co2.heating ~ income_decile | heating.energy + space + owner +
                    building.age + hsize + htype + degurba + over65.dummy + building.type,
                    data = train_data, family = gaussian,
                    prune="BIC", minsize=8000, verbose=TRUE)

plot(mob_tree, 
     gp = gpar(fontsize = 7, fontface = "bold"),
     type = "extended",  # For a simpler tree layout
     drop_terminal = TRUE,
     inner_panel = node_inner(mob_tree, pval = TRUE, id = TRUE),
     lwd = 2, 
     lty = 2)

# Function to calculate metrics and return predictions
evaluate_model <- function(model, data, truth_col) {
  # Use predict() directly without pull(), assuming predictions are returned as a vector or matrix
  predictions <- predict(model, newdata = data)
  
  # If predictions are returned as a matrix (e.g., with class probabilities), extract the relevant column/vector
  # This step might need adjustment based on your specific model output
  if(is.matrix(predictions)) {
    predictions <- predictions[,1]  # Adjust this depending on which column you need
  }
  
  # Create a dataframe for metric calculation, combining actual values and predictions
  results_df <- data.frame(
    truth = data[[truth_col]],  # Extract the actual values using standard subsetting
    estimate = predictions
  )
  
  # Calculate metrics using the results dataframe
  metrics <- metric_set(rmse, rsq)
  model_performance <- metrics(data = results_df, truth = truth, estimate = estimate)
  
  return(list(predictions = predictions, performance = model_performance))
}

# Evaluate model on test data
test_eval <- evaluate_model(mob_tree, test_data, "co2.heating")
print(test_eval$performance)

### Space model
# fit glmtree (mob)
mob_tree_space <- glmtree(co2.heating ~ space | income_decile + heating.energy + owner +
                      building.age + heating.system + hsize + htype + HDD + degurba 
                    + age,
                    data = train_data, family = gaussian,
                    prune="BIC", minsize=8000, verbose=TRUE)

plot(mob_tree_space)

png("mob_tree_space.png", width = 6000, height = 4000, res = 300)  # Adjust width, height, and resolution as needed
par(mar = c(2, 4, 4, 4) + 0.3)  
plot(mob_tree_space, 
     gp = gpar(fontsize = 7, fontface = "bold"),
     type = "extended",  # For a simpler tree layout
     drop_terminal = TRUE,
     inner_panel = node_inner(mob_tree_space, pval = TRUE, id = TRUE),
     lwd = 2, 
     lty = 2)  
dev.off()

### Intercept model
# fit glmtree (mob)
mob_tree_int <- glmtree(co2.heating ~ 1 | income_decile + heating.energy + owner + space +
                            building.age + heating.system + hsize + htype + HDD + degurba 
                          + age,
                          data = train_data, family = gaussian,
                          prune="BIC", minsize=8000, verbose=TRUE)

plot(mob_tree_int, 
     gp = gpar(fontsize = 7, fontface = "bold"),
     type = "extended",  # For a simpler tree layout
     drop_terminal = TRUE,
     inner_panel = node_inner(mob_tree_int, pval = TRUE, id = TRUE),
     lwd = 2, 
     lty = 2)  


## concatenate trained classes and predicted classes from test data to compare means
dat_classified = train_data %>% 
  mutate(tnode = partykit::predict.party(object = mob_tree, newdata = train_data),
         dat = "training") %>% 
  bind_rows(test_data %>% 
              mutate(tnode = partykit::predict.party(object = mob_tree, newdata = test_data),
                     dat = "test"))

## plot node means in training and test data
dat_classified %>%
  ggplot(aes(color=dat,group=interaction(dat,tnode), y=co2.heating)) +
  geom_boxplot(position = position_dodge()) +
  theme_minimal()

