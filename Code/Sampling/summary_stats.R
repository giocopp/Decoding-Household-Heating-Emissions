### Base R code for the summary statistics of the full population, ran on the PIK HPC Cluster

# Define a vector containing the names of the sampled files
synthpop_data <- readRDS("/p/projects/bymarka/synthpop/giorgio/synthpop_D.rds")

# Convert loaded data to a dataframe if not already
synthpop <- as.data.frame(synthpop_data)

# Selecting specific columns
synthpop_selected <- synthpop[, c("age", "hsize", "htype", "owner", "building.type", "building.age",
                                  "inc.disposable", "heating", "heating.energy", "space",
                                  "exp.housing", "exp.heating")]

continuous_vars <- c("inc.disposable", "exp.heating")

# Summary of the selected data
summary_output <- summary(synthpop_selected[, continuous_vars])

# Write summary to a text file
writeLines(capture.output(summary_output), "summary_output.txt")


