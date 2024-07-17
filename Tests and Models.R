

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

Mann-Whitney U Test: A non-parametric test if the temperature data does not follow a normal distribution.
Chi-Square Test: For categorical data (e.g., number of wildfire incidents per temperature range).

 
# Formulating Hypotheses

# Null Hypothesis (H0): Assumes no effect or no difference between groups.

# Alternative Hypothesis (H1): Assumes an effect or a difference.


# Example Hypotheses:


# Testing Temperature Effect:

# H0: Mean temperature during wildfires in 2023 is equal to the mean temperature during the 10-year period of wildfires.
# H1: Mean temperature during wildfires in 2023 is different from the mean temperature during the 10-year period of wildfires.

# This test aims to see if mean temperature during wildfires of 2023 is different from the average temperature during wildfires over the 10 year period.
# It can help identify if 2023 was an anomalous year in terms of temperature during wildfires.
# Values to take - mean temperature of 2023, mean temperature of 10 y period


# Comparing Fire Weather Index Across Years:

# H0: Mean FWI is the same across different years.
# H1: Mean FWI differs across different years.



# Variables that could be influenced by or influence wildfires.

# Temperature (temp)
# Relative Humidity (rh)
# Wind Speed (ws)

# Fire Weather Index (FWI)
# Drought Code (DC)
# Head Fire Intencity (HFI)

# Calculate mean values for the selected variables within specific groups.
# Create subsets of data based on conditions.




#############

# Prepare subsets for tests
names(hotspots_peak)
hotspots_peak$month
data_2023 <- subset(hotspots_peak, year == 2023) # year with some historical fires
data_2020 <- subset(hotspots_peak, year == 2020) # year with lower overall fires

event_31404 # the most number of entries in one cluster (September of 2023)
summary(event_31404)

data_sep_all <- subset(hotspots_peak, month == "Sep") # Septembers of all 10 years
summary(data_sep_all)

data_sep_2023 <- subset(data_sep_all, year == 2023) # September in the year 2023
summary(data_sep_2023)

hotspots_peak # the dataset on the whole




# Calculating mean values

mean_fwi_2023 <- mean(data_2023$fwi, na.rm = TRUE)
mean_fwi_2020 <- mean(data_2020$fwi, na.rm = TRUE)







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
 


# Calculating mean FWI for cluster 5017 and cluster 31404
mean_fwi_cluster_5017 <- mean(cluster_5017$fwi, na.rm = TRUE)
mean_fwi_cluster_31404 <- mean(cluster_31404$fwi, na.rm = TRUE)

# t-test for FWI between cluster 5017 and cluster 31404
t_test_fwi_clusters <- t.test(cluster_5017$fwi, cluster_2$fwi)
print(t_test_fwi_clusters)

# p-value < 0.05: Indicates a significant difference in the mean values between the clusters.
# p-value >= 0.05: Indicates no significant difference.

# Hypotheses:
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

# There is a statistically significant difference in the mean FWIs between Cluster 5017 and Cluster 31404. 
# Cluster 5017 has a significantly lower mean FWI compared to Cluster 31404. 
# This suggests that the wildfire events in Cluster 5017 had lower FWI values on average compared to those in Cluster 31404.






cluster_summary <- hotspots_peak %>% 
  group_by(event_cluster) %>% 
  summarise(
    mean_fwi = mean(fwi, na.rm = TRUE),
    mean_temp = mean(temp, na.rm = TRUE),
    mean_rh = mean(rh, na.rm = TRUE),
    entries_count = n()
  )




