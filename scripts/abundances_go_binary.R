library(scales)

# Set seed for reproducibility
set.seed(42)

# # Number of samples and species
# n_samples <- 8
# n_species <- 7
# species_labels <- LETTERS[1:n_species]
# sample_names <- c(paste("t0_S", 1:4, sep=""), paste("t2_S", 5:8, sep=""))
# 
# # Initialize the data frame
# df_microbial <- data.frame(matrix(ncol = n_species, nrow = n_samples))
# colnames(df_microbial) <- species_labels
# rownames(df_microbial) <- sample_names
# 
# # Populate the data frame
# for (i in 1:n_species) {
#   species <- species_labels[i]
#   if (species == 'A') {
#     # 'A' is almost absent at t0 and predominant at t2
#     df_microbial[1:4, i] <- sample(0:4, 4, replace = TRUE)
#     df_microbial[5:8, i] <- sample(50:100, 4, replace = TRUE)
#   } else if (species == 'D') {
#     # 'D' is predominant at t0 and almost absent at t2
#     df_microbial[1:4, i] <- sample(50:100, 4, replace = TRUE)
#     df_microbial[5:8, i] <- sample(0:4, 4, replace = TRUE)
#   } else {
#     # Other species have random counts
#     df_microbial[, i] <- sample(0:100, n_samples, replace = TRUE)
#   }
# }
# 
# # Print the data frame
# print(df_microbial)

# Set seed for reproducibility
set.seed(42)

# Number of samples and species
n_samples <- 100
n_species <- 7
species_labels <- LETTERS[1:n_species]
sample_names <- c(paste("t0_S", 1:50, sep=""), paste("t2_S", 51:100, sep=""))

# Initialize the data frame
df_microbial <- data.frame(matrix(ncol = n_species, nrow = n_samples))
colnames(df_microbial) <- species_labels
rownames(df_microbial) <- sample_names

# Populate the data frame
for (i in 1:n_species) {
  species <- species_labels[i]
  if (species == 'A') {
    # 'A' is almost absent at t0 and predominant at t2
    df_microbial[1:50, i] <- sample(0:4, 50, replace = TRUE)
    df_microbial[51:100, i] <- sample(50:100, 50, replace = TRUE)
  } else if (species == 'D') {
    # 'D' is predominant at t0 and almost absent at t2
    df_microbial[1:50, i] <- sample(50:100, 50, replace = TRUE)
    df_microbial[51:100, i] <- sample(0:4, 50, replace = TRUE)
  } else {
    # Other species have random counts
    df_microbial[, i] <- sample(0:100, n_samples, replace = TRUE)
  }
}


View(df_microbial)


# Normalizing the counts per sample to range from 0 to 1
df_microbial_normalized <- as.data.frame(lapply(df_microbial, function(col) {
  col / rowSums(df_microbial)
}))

# Print the normalized data frame
print(df_microbial_normalized)







# Load necessary libraries
library(ggplot2)
library(tidyr)

# Reshape data from wide to long format
df_long <- pivot_longer(df_microbial_normalized, cols = everything(), names_to = "Species", values_to = "Normalized_Count")

# Plot histograms using ggplot2 with facet_wrap
ggplot(df_long, aes(x = Normalized_Count)) +
  geom_histogram(bins = 10, fill = "blue", color = "black") + # Adjust bin width as necessary
  facet_wrap(~Species, scales = "free_y") + # Facets for each species
  theme_minimal() + # Minimal theme
  labs(title = "Histogram of Normalized Counts per Species",
       x = "Normalized Count",
       y = "Frequency")


# Load necessary library
library(ggplot2)

# Data for species A
species_a_data <- df_microbial_normalized$A

# Calculate the 95% confidence interval
ci_lower <- quantile(species_a_data, 0.025)
ci_upper <- quantile(species_a_data, 0.975)

# Plot histogram with highlighted 95% confidence interval
ggplot(data = data.frame(species_a_data), aes(x = species_a_data)) +
  geom_histogram(bins = 10, fill = "gray", color = "black") + # Adjust bin width as necessary
  geom_vline(xintercept = ci_lower, color = "red", linetype = "dashed", size = 1.5,
             label = "Lower 2.5%") +
  geom_vline(xintercept = ci_upper, color = "red", linetype = "dashed", size = 1.5,
             label = "Upper 97.5%") +
  theme_minimal() +
  labs(title = "Histogram of Species A with 95% Confidence Interval",
       x = "Normalized Count",
       y = "Frequency")


# Define a function to calculate binary values based on the 95% confidence interval
binary_transform <- function(data) {
  ci_lower <- quantile(data, 0.025)
  ci_upper <- quantile(data, 0.975)
  # Return 1 for values outside the 95% CI, and 0 for values inside
  return(ifelse(data < ci_lower | data > ci_upper, 1, 0))
}

# Apply the function to each column of the DataFrame
df_microbial_binary <- as.data.frame(lapply(df_microbial_normalized, binary_transform))

# Print the first few rows of the binary DataFrame
head(df_microbial_binary)





# Calculate the sum of counts for each species at each time point
sum_t0 <- colSums(df_microbial_normalized[1:50, ])
sum_t2 <- colSums(df_microbial_normalized[51:100, ])

# Create a data frame for plotting
species_sums <- data.frame(Species = names(sum_t0),
                           Sum_t0 = sum_t0,
                           Sum_t2 = sum_t2)

# Melt the data frame for easier plotting with ggplot2
library(reshape2)
species_sums_melted <- melt(species_sums, id.vars = "Species", variable.name = "TimePoint", value.name = "Sum")

# Plot the sums for each species by time point
library(ggplot2)
ggplot(species_sums_melted, aes(x = Species, y = Sum, fill = TimePoint)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Sum of Species Counts by Time Point",
       x = "Species",
       y = "Sum of Counts") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))








# Load necessary libraries
library(randomForest)
library(ggplot2)

# Assuming df_microbial_normalized is already populated and includes a 'TimePoint' column
# Create the time point vector (label)
time_points <- rep(c('t0', 't2'), each = 50)

# Train a Random Forest model
rf_model <- randomForest(x = df_microbial_normalized, y = as.factor(time_points), ntree = 500, importance = TRUE)

# Extract feature importance for Mean Decrease Accuracy
importance_scores <- importance(rf_model, type = 1)  # Type 1 for mean decrease accuracy
feature_importance <- data.frame(Species = rownames(importance_scores), Importance = importance_scores[, "MeanDecreaseAccuracy"])

# Order the data by importance
feature_importance <- feature_importance[order(-feature_importance$Importance), ]

# Plot the feature importance
ggplot(feature_importance, aes(x = reorder(Species, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Feature Importance of Species (Random Forest)",
       x = "Species",
       y = "Importance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

