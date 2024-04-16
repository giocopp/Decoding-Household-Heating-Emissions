library(partykit)
library(tidymodels)
library(tidyverse)
library(ggparty)
library(recipes)
library(caret)

sample_tree <-  readRDS("~/Desktop/Uni/Hertie School/4th Semester/Thesis/Analysis/Data/Derived data/sample_tree.rds")

# Set seed for reproducibility
set.seed(123)
options(scipen = 999)

# Assuming sample_tree is your dataset and you've performed necessary data cleaning steps
dat <- sample_tree |> 
  select(-c(rid, co2.heating.cap, inc.disp.adult.eq, co2.heating.sqm, co2.heating.cap, co2.intensity, state, inc.disposable, tj.heating)) |>   
  rename(income = inc.disp.adult.eq.dec) |> 
  na.omit()

### Normalization of HDD
normalize_min_max <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# To apply Min-Max normalization on a specific column 'variable'
dat$HDD_norm <- normalize_min_max(dat$HDD)

dat <- dat |> 
  mutate(eui_HDD = eui * HDD_norm) |>
  mutate(eui = round(eui_HDD, 2)) |>
  select(-HDD_norm, -eui_HDD)

data_split <- initial_split(dat, prop = 0.65)
train_data <- training(data_split)
test_data <- testing(data_split)

# Custom ctree_control settingsa
ctree_model_EUI <- partykit::ctree(co2.heating ~ ., data = train_data, 
                               minsplit = 5000,  # Lower than before
                               minbucket = 8000,  # Also lower than before
                               maxdepth = 5)

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
test_eval <- evaluate_model(ctree_model_EUI, test_data, "co2.heating")
print(test_eval$performance)

# Plot the decision tree with improved visuals
# plot(ctree_model_EUI)

# # Plot the decision tree with improved visuals
ggtree <- ggparty(ctree_model_EUI) +
  # Edges: Defines the appearance of the lines connecting the nodes
  geom_edge(color = "gray50", size = 0.5) +
  # Edge Labels: Displays the condition for splitting at each node in dark blue, italic font
  geom_edge_label(color = "darkblue", size = 3, fontface = "italic") +

  # Node Split Variable: Displays the variable name used for splitting at each node, in bold font
  geom_node_splitvar(size = 4, fontface = "bold") +

  # Node Boxplots: Adds a boxplot for the target variable 'co2.heating' in each terminal node,
  # helping to visualize the distribution of target values within each group formed by the tree
  geom_node_plot(gglist = list(
    geom_boxplot(aes(y = co2.heating), outlier.shape = 8))) +

  # General plot aesthetics: Sets labels and applies a minimal theme for a clean appearance
  labs(y = "CO2 Heating", title = "Decision Tree for CO2 Heating") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

print(ggtree)
ggsave("ggtree_EUI.png", plot = ggtree, width = 10, height = 8, dpi = 300)

### Cross Validation
# cv_results <- trainControl(method = "cv", number = 5)
# cv_model <- train(co2.heating ~ ., data = train_data, method = "ctree", trControl = cv_results)
# print(cv_model)

### Feature importance
### # You can use varimp() function from partykit to assess feature importance
importance <- varimp(ctree_model_EUI)
print(importance)

importance_df <- data.frame(
  Variable = names(importance),
  Importance = importance
)

importance_df <- importance_df[order(importance_df$Importance, decreasing = TRUE),]

# Plot using ggplot2
importance_p_EUI <- ggplot(importance_df, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_bar(stat = "identity") +
  xlab("Importance") +
  ylab("Variables") +
  ggtitle("Variable Importance from ctree Model") +
  theme_minimal()

ggsave("importance_p_EUI.png", plot = importance_p_EUI, width = 10, height = 8, dpi = 300)

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
  geom_boxplot(position = position_dodge(width = 0.8)) +
  labs(x = "Terminal Node", y = "CO2 Heating") +
  theme_minimal() +
  ggtitle("CO2 Heating by Terminal Node: Training vs. Testing Data (Ordered by Mean CO2 Heating)") +
  scale_fill_manual(values = c("training" = "blue", "test" = "red")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

# Display the plot
print(train_test_plot)
ggsave("train_test_plot_EUI.png", plot = train_test_plot, width = 10, height = 8, dpi = 300)
