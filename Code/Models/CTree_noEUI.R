library(partykit)
library(tidymodels)
library(tidyverse)
library(ggparty)
library(recipes)
library(caret)
library(grid)

sample_tree <-  readRDS("~/Desktop/Uni/Hertie School/4th Semester/Thesis/Analysis/Data/Derived data/sample_tree.rds")

# Set seed for reproducibility
set.seed(123)
options(scipen = 999)

# Assuming sample_tree is your dataset and you've performed necessary data cleaning steps
dat <- sample_tree |> 
  mutate(income_decile = inc.disp.adult.eq.dec) |> 
  mutate(oil = ifelse(heating.energy == 3, 1, 0)) |>
  na.omit()

trainIndex <- createDataPartition(dat$co2.heating, p = .7, 
                                  list = FALSE, 
                                  times = 1)
train_data <- dat[ trainIndex,]
test_data  <- dat[-trainIndex,]
str(train_data$heating.energy)
# Custom ctree_control settings
ctree_model <- partykit::ctree(co2.heating ~ over65.dummy + hsize + htype + income_decile + space + building.age + building.type + oil + degurba + owner, data = train_data, 
                                        minsplit = 6000,  
                                        minbucket = 9000,  
                                        maxdepth = 3)

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
test_eval <- evaluate_model(ctree_model, test_data, "co2.heating")
print(test_eval$performance)

# Plot the decision tree with improved visuals
png("ctree_model.png", width = 6000, height = 4000, res = 300)  # Adjust width, height, and resolution as needed
par(mar = c(2, 4, 4, 4) + 0.3)  
plot(ctree_model, 
     gp = gpar(fontsize = 10, fontface = "bold"),
     type = "extended",  # For a simpler tree layout
     drop_terminal = TRUE,
     inner_panel = node_inner(ctree_model, pval = TRUE, id = TRUE),
     terminal_panel = node_boxplot(ctree_model, col = "black", fill = "lightgray", width = 0.5,
                  yscale = NULL, ylines = 3, cex = 0.5, id = T, mainlab = NULL, gp = gpar(fontsize = 7.5)),
     lwd = 2, 
     lty = 2)  
dev.off()

### Feature importance
### # You can use varimp() function from partykit to assess feature importance
importance <- varimp(ctree_model)
print(importance)

importance_df <- data.frame(
  Variable = names(importance),
  Importance = importance
)

# Order the data frame based on Importance so the plot is ordered
importance_df <- importance_df[order(importance_df$Importance, decreasing = TRUE),]

# Plot using ggplot2
importance_p <- ggplot(importance_df, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_bar(stat = "identity") +
  xlab("Importance") +
  ylab("Variables") +
  ggtitle("Variable Importance from ctree Model") +
  theme_minimal()

importance_p
ggsave("importance_p_noEUI.png", plot = importance_p, width = 10, height = 8, dpi = 300)

### Predictions for the training data
# Generate node assignments for training and testing data
node_assignments_train <- predict(ctree_model, newdata = train_data, type = "node")
node_assignments_test <- predict(ctree_model, newdata = test_data, type = "node")

# Assuming node_assignments are vectors, if not, adjust as necessary (similar to the prediction adjustment)
if(is.matrix(node_assignments_train)) {
  node_assignments_train <- node_assignments_train[,1]
}
if(is.matrix(node_assignments_test)) {
  node_assignments_test <- node_assignments_test[,1]
}

# Modify the training and testing datasets with node assignments
train_data_mod <- mutate(train_data, tnode = node_assignments_train, dat = "training")
test_data_mod <- mutate(test_data, tnode = node_assignments_test, dat = "test")

# Bind the rows of modified training and testing data
dat_classified <- bind_rows(train_data_mod, test_data_mod)

# Calculate the mean CO2 heating for each node and order nodes by this mean
node_means <- dat_classified %>%
  group_by(tnode) %>%
  summarise(mean_co2_heating = mean(co2.heating, na.rm = TRUE)) %>%
  arrange(mean_co2_heating) %>%
  ungroup()

# Ensure 'tnode' is a factor with levels in the order of mean CO2 heating
dat_classified$tnode_factor <- factor(dat_classified$tnode, levels = node_means$tnode)

# Use the factor with ordered levels for plotting
train_test_plot <- ggplot(dat_classified, aes(x = tnode_factor, y = co2.heating, fill = dat)) +
  geom_boxplot(position = position_dodge(width = 0.8), outlier.alpha = 0.4) + # Set alpha for outliers
  labs(x = "Terminal Node", y = "CO2 Heating") +
  theme_minimal() +
  ggtitle("CO2 Heating by Terminal Node: Training vs. Testing Data (Ordered by Mean CO2 Heating)") +
  scale_fill_manual(values = c("training" = "#0000FFCC", "test" = "#FF0000CC")) + 
  theme(axis.text.x = element_text(vjust = 0.5))

# Display the plot
print(train_test_plot)
ggsave("train_test_plot_EUI.png", plot = train_test_plot, width = 10, height = 8, dpi = 300)

  