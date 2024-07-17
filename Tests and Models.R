

# Find out how many individual entries each event has
# Top Event
cluster_counts <- hotspots_peak %>%
  group_by(event_cluster) %>%
  summarise(event_count = n()) %>%
  arrange(desc(event_count))

cluster_counts



# 31404 is the cluster with max amount of entries

event_31404 <- hotspots %>%
  filter(event_cluster == 31404) 

summary(event_31404)

event_31404$hfi


ggplot(event_31404, aes(x = hfi)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Temperature at Event 31404",
       x = "Temp",
       y = "Frequency") +
  scale_y_continuous(labels = comma) + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))


# report  and 2nd report application of the for presentation (5th aug)


# 5017 there are 60 events in this cluster

event_5017 <- hotspots %>%
  filter(event_cluster == 5017) 

summary(event_5017)

hist(event_5017$ros)

ggplot(event_5017, aes(x = temp)) +
  geom_histogram(binwidth = 0.05, fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of HFI at Event 5017",
       x = "temp",
       y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))



###################### 
 
# Formulating Hypotheses

# Null Hypothesis (H0): Assumes no effect or no difference between groups.

# Alternative Hypothesis (H1): Assumes an effect or a difference.


# Example Hypotheses:


#   Testing Temperature Effect:

# H0: Mean temperature during wildfires is equal to the mean temperature during non-wildfire periods.
# H1: Mean temperature during wildfires is different from the mean temperature during non-wildfire periods.



# Comparing Fire Weather Index (FWI) Across Years:

# H0: Mean FWI is the same across different years.
# H1: Mean FWI differs across different years.

# Selecting Variables
# Choose variables that could logically be influenced by or influence wildfires.

# Temperature (temp)
# Relative Humidity (rh)
# Wind Speed (ws)
# Fire Weather Index (FWI)
# Drought Code (DC)

# Data Manipulation
# Calculating Means: Calculate mean values for the selected variables within specific groups (e.g., wildfire vs. non-wildfire periods).
# Subsetting Data: Create subsets of data based on conditions (e.g., periods with wildfires and without wildfires).




############# # Subsetting data by year
data_2023 <- subset(hotspots_peak, year == 2023)
data_2020 <- subset(hotspots_peak, year == 2020)

# Calculating mean values
mean_fwi_2023 <- mean(data_2023$fwi, na.rm = TRUE)
mean_fwi_2020 <- mean(data_2020$fwi, na.rm = TRUE)
# Repeat for other years and variables as needed

# Performing t-test between two years
t_test_fwi <- t.test(data_2023$fwi, data_2020$fwi)
print(t_test_fwi)

# Repeat the t-test for other pairs of years as needed

# Interpretation:
# p-value < 0.05: Indicates a significant difference in the mean values between the years.
# p-value >= 0.05: Indicates no significant difference.

# Example Hypotheses:
# H0: Mean FWI in 2023 is equal to mean FWI in 2020.
# H1: Mean FWI in 2023 is different from mean FWI in 2020.







######
 
# Subset Data by Clusters:
# Create subsets for each cluster.

# Calculate Mean Values:
# Calculate mean values of your variables of interest (e.g., FWI, temperature) for each cluster.

# Perform t-Tests:
# Compare the mean values between clusters using pairwise t-tests.


# Example: Subsetting for event_5017 and event_31404
cluster_5017 <- subset(hotspots_peak, event_cluster == 5017)
cluster_31404 <- subset(hotspots_peak, event_cluster == 31404)
# Repeat for other clusters as needed

# Example: Calculating mean FWI for cluster 5017 and cluster 31404
mean_fwi_cluster_5017 <- mean(cluster_5017$fwi, na.rm = TRUE)
mean_fwi_cluster_31404 <- mean(cluster_31404$fwi, na.rm = TRUE)
# Repeat for other variables and clusters as needed

# Example: t-test for FWI between cluster 5017 and cluster 31404
t_test_fwi_clusters <- t.test(cluster_5017$fwi, cluster_2$fwi)
print(t_test_fwi_clusters)
# Repeat for other pairs of clusters and variables as needed

# Interpretation:
# p-value < 0.05: Indicates a significant difference in the mean values between the clusters.
# p-value >= 0.05: Indicates no significant difference.

# Example Hypotheses:
# H0: Mean FWI in Cluster 5017 is equal to mean FWI in Cluster 31404.
# H1: Mean FWI in Cluster 5017 is different from mean FWI in Cluster 31404.




# The Welch Two Sample t-test results for comparing the Fire Weather Index (FWI) between Cluster 5017 and Cluster 31404 show:

#   t-value: -14.757
# Degrees of Freedom (df): 69.406
# p-value: < 2.2e-16

# Interpretation:
#   p-value: The extremely small p-value (< 2.2e-16) indicates a highly significant difference in the mean FWIs between the two clusters.
# Confidence Interval (CI): The 95% CI for the difference in means is from -10.319140 to -7.861541. This interval does not include 0, indicating a significant difference.
# Sample Means: The mean FWI for Cluster 5017 is 27.31252, and for Cluster 31404, it is 36.40286.
# Conclusion:
#   There is a statistically significant difference in the mean FWIs between Cluster 5017 and Cluster 31404. Cluster 5017 has a significantly lower mean FWI compared to Cluster 31404. 
# This suggests that the wildfire events in Cluster 5017 had lower FWI values on average compared to those in Cluster 31404.






cluster_summary <- hotspots_peak %>% 
  group_by(event_cluster) %>% 
  summarise(
    mean_fwi = mean(fwi, na.rm = TRUE),
    mean_temp = mean(temp, na.rm = TRUE),
    mean_rh = mean(rh, na.rm = TRUE),
    entries_count = n()
  )




