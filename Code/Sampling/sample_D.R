# Set the seed for reproducibility
set.seed(123)

# Read the data
synthpop <- readRDS("/p/projects/bymarka/synthpop/giorgio/synthpop_D.rds")

# Assuming 'hid' is the household ID and 'state' is the state variable in your dataset

# Set the overall sample fraction for households
sample_fraction <- 0.05

# Extract unique states
states <- unique(synthpop$state)

# Initialize vector to hold sampled household IDs
sampled_hids <- integer(0)

# Perform stratified sampling by state
for (s in states) {
    # Subset the data for the state
    state_data <- synthpop[synthpop$state == s, ]
    
    # Find unique hids within this state
    state_hids <- unique(state_data$hid)
    
    # Calculate the number of households to sample in this state
    num_households_needed <- max(1, round(length(state_hids) * sample_fraction))
    
    # Sample household IDs
    state_sampled_hids <- sample(state_hids, num_households_needed, replace = FALSE)
    
    # Combine sampled IDs into the main vector
    sampled_hids <- c(sampled_hids, state_sampled_hids)
}

# Select all members from sampled households
sampled_data <- synthpop[synthpop$hid %in% sampled_hids, ]

# Specify file name for output
file_name <- "sampled_data.rds"

# Save the sampled dataset
output_file_path <- paste0("/p/projects/bymarka/synthpop/giorgio/sample_", file_name)
saveRDS(sampled_data, output_file_path, compress = TRUE)

