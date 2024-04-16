# Load the population dataset from an RDS file
synthpop <- readRDS("synthpop_D.rds")

# Load the sample dataset from an RDS file
sample_synthpop <- readRDS("sample_synthpop_D.rds")

# Define the subset of interest for both population and sample data
vars_of_interest <- c("age", "hsize", "htype", "owner", "building.type", "building.age",
                      "inc.disposable", "heating", "heating.energy", "space",
                      "exp.heating")

synthpop_selected <- synthpop[, vars_of_interest, drop = FALSE]
sample_synthpop_selected <- sample_synthpop[, vars_of_interest, drop = FALSE]

# Categorical variables for which to perform chi-square tests
categorical_vars <- c("owner", "building.type", "age", "hsize", "htype", "heating", "heating.energy", "space")

# Continuous variables for which to perform the two-sample KS test
continuous_vars <- c("inc.disposable", "exp.heating")

# Function to perform chi-square test
perform_chi_square <- function(population, sample, var) {
  table_pop <- table(population[[var]])
  table_sample <- table(sample[[var]])
  levels_union <- union(levels(factor(population[[var]])), levels(factor(sample[[var]])))
  table_pop <- table(factor(population[[var]], levels = levels_union))
  table_sample <- table(factor(sample[[var]], levels = levels_union))
  total_sample_size <- sum(table_sample)
  population_proportions <- table_pop / sum(table_pop)
  expected_counts <- total_sample_size * population_proportions
  if (any(expected_counts < 5)) {
    chisq_results <- list(p_value = NA)
  } else {
    chisq_results <- chisq.test(table_sample, p = population_proportions, rescale.p = TRUE)
    chisq_results <- list(p_value = chisq_results$p.value)
  }
  return(chisq_results)
}

# Function to perform two-sample KS test
perform_ks_test <- function(population, sample, var) {
  ks_test <- ks.test(x = population[[var]], y = sample[[var]])
  return(list(p_value = ks_test$p.value, statistic = ks_test$statistic))
}

# Apply the chi-square test to each categorical variable
cat_results <- setNames(lapply(categorical_vars, function(var) perform_chi_square(synthpop_selected, sample_synthpop_selected, var)), categorical_vars)

# Apply the KS test to each continuous variable
cont_results <- setNames(lapply(continuous_vars, function(var) perform_ks_test(synthpop_selected, sample_synthpop_selected, var)), continuous_vars)

# Combine results
all_results <- list(categorical = cat_results, continuous = cont_results)

# Output results to a text file
write_results <- function(results, file_name) {
  cat("Categorical Variable Tests:\n", file = file_name)
  for (var_name in names(results$categorical)) {
    cat(sprintf("\nVariable %s - p_value: %s\n", var_name, results$categorical[[var_name]]$p_value), file = file_name, append = TRUE)
  }
  cat("\nContinuous Variable Tests:\n", file = file_name, append = TRUE)
  for (var_name in names(results$continuous)) {
    cat(sprintf("\nVariable %s - p_value: %s, KS Statistic: %s\n", var_name, results$continuous[[var_name]]$p_value, results$continuous[[var_name]]$statistic), file = file_name, append = TRUE)
  }
}

# Specify output file name
output_file_name <- "results_tests.txt"

# Write to file
write_results(all_results, output_file_name)

