# Load the population dataset from an RDS file
synthpop <- readRDS("synthpop_D.rds")

# Load the sample dataset from an RDS file
sample_synthpop <- readRDS("sample_synthpop_D.rds")

# Define the subset of interest for both population and sample data
vars_of_interest <- c("age", "hsize", "htype", "owner", "building.type", "building.age",
                      "inc.disposable", "heating", "heating.energy", "space",
                      "exp.housing", "exp.heating")

synthpop_selected <- synthpop[, vars_of_interest, drop = FALSE]
sample_synthpop_selected <- sample_synthpop[, vars_of_interest, drop = FALSE]

# Categorical variables for which to perform chi-square tests
categorical_vars <- c("owner", "building.type", "age", "hsize", "htype", "heating", "heating.energy", "space")

# Function to perform chi-square test
perform_chi_square <- function(population, sample, var) {
  # Create contingency tables
  table_pop <- table(population[[var]])
  table_sample <- table(sample[[var]])

  # Ensure all levels in the sample are present in the population
  levels_union <- union(levels(factor(population[[var]])), levels(factor(sample[[var]])))
  table_pop <- table(factor(population[[var]], levels = levels_union))
  table_sample <- table(factor(sample[[var]], levels = levels_union))

  # Calculate expected frequencies based on population proportions
  total_sample_size <- sum(table_sample)
  population_proportions <- table_pop / sum(table_pop)
  expected_counts <- total_sample_size * population_proportions

  # Perform the chi-square test if valid
  if (any(expected_counts < 5)) {
    warning(paste("Expected counts are too low for a valid chi-square test for variable:", var))
    return(list(p_value = NA, warning = "Low counts", valid = FALSE))
  } else {
    chisq_results <- chisq.test(table_sample, p = population_proportions, rescale.p = TRUE)
    return(list(p_value = chisq_results$p.value, warning = NULL, valid = TRUE))
  }
}

# Apply the chi-square test to each categorical variable
results <- lapply(categorical_vars, function(var) perform_chi_square(synthpop_selected, sample_synthpop_selected, var))

# Print the results
results

