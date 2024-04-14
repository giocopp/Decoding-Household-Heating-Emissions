# Set the seed for reproducibility
set.seed(123)

# Define the vector containing the names of the synthpop files
synthpop_files <- c("synthpop_D.rds")

# Loop over each synthpop file
for (file_name in synthpop_files) {
    # Define the path to the data file on the cluster
    data_file_path <- paste0("/p/projects/bymarka/synthpop/giorgio/", file_name)

    # Read the data
    synthpop <- readRDS(data_file_path)

    # Assuming 'hid' is the household ID variable in your dataset

    # Identify unique household IDs
    unique_hids <- unique(synthpop$hid)

    # Set the overall sample fraction for households
    sample_fraction <- 0.05

    # Calculate the number of households needed for the sample
    num_households_needed <- max(1, round(length(unique_hids) * sample_fraction))

    # Randomly sample household IDs
    set.seed(123)  # Ensure reproducibility
    sampled_hids <- sample(unique_hids, num_households_needed, replace = FALSE)

    # Select all members from sampled households
    sampled_data <- synthpop[synthpop$hid %in% sampled_hids, ]

    # Save the sampled dataset
    output_file_path <- paste0("/p/projects/bymarka/synthpop/giorgio/sample_", file_name)
    saveRDS(sampled_data, output_file_path, compress = TRUE)
}

