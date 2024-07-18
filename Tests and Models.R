# Cluseters to work with ####

# Find out how many individual entries each event has
# Top Event
cluster_counts <- hotspots_peak %>%
  group_by(event_cluster) %>%
  summarise(event_count = n()) %>%
  arrange(desc(event_count))

cluster_counts

cluster_summary <- hotspots_peak %>% 
  group_by(event_cluster) %>% 
  summarise(
    mean_fwi = mean(fwi, na.rm = TRUE),
    mean_temp = mean(temp, na.rm = TRUE),
    mean_rh = mean(rh, na.rm = TRUE),
    
    
    entries_count = n()
  )




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









# One-Sample t-Test:####

# To determine if the mean of a single sample is significantly different
# from a known or hypothesized population mean.

# Sample Mean (mean of a sample)
# Population Mean (the value comparing against)
# Sample Size (number of observations in the sample)
# Sample Standard Deviation
# The sample should be continuous and approximately normally distributed.
# The sample should be randomly selected.

# The histogram should show a bell-shaped curve.
# The Q-Q plot should show points roughly following the straight line.
# Shapiro-Wilk Test: If p-value > 0.05, the data is normally distributed.
# Kolmogorov-Smirnov Test: If p-value > 0.05, the data is normally distributed.

# Output:
# t-value: The test statistic.
# p-value: Probability of observing the data if the null hypothesis is true.
# Confidence Interval: Range where the true mean is likely to fall.
# Sample Mean: Average of the sample data.


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











# Independent Two-Sample t-Test####

# To determine if the means of two independent samples are significantly different.

# Two independent samples.
# Each sample should be continuous and approximately normally distributed.
# The samples should be randomly selected.

# Output:
# t-value: The test statistic.
# p-value: Probability of observing the data if the null hypothesis is true.
# Confidence Interval: Range where the true difference in means is likely to fall.
# Sample Means: Averages of the two sample data sets.

# Hypotheses:

# H0: The means of the two groups are equal.
# H1: The means of the two groups are different.


# Example Hypotheses:
# H0: Mean FWI in 2023 is equal to mean FWI in 2020.
# H1: Mean FWI in 2023 is different from mean FWI in 2020.


# H0: Mean FWI in Cluster 5017 is equal to mean FWI in Cluster 31404.
# H1: Mean FWI in Cluster 5017 is different from mean FWI in Cluster 31404.


# Interpretation:
# p-value < 0.05: Indicates a significant difference in the mean values between the groups.
# p-value >= 0.05: Indicates no significant difference.



# Comparing temperatures in 2023 vs. 2020
t_test_result <- t.test(data_2023$temp, data_2020$temp)
print(t_test_result)


 

# Paired t-Test: ####
# To determine if the mean difference between paired observations is significantly different from zero.


# Paired samples (values before and after event).
# The differences between paired observations should be approximately normally distributed.
# The samples should be randomly selected.

# Histogram of Differences: Should show a bell-shaped curve.
# Q-Q Plot of Differences: Points should roughly follow the straight line.
# Shapiro-Wilk Test on Differences: If p-value > 0.05, the differences are normally distributed.

# Output:   
# t-value: The test statistic.
# p-value: Probability of observing the data if the null hypothesis is true.
# Confidence Interval: Range where the true mean difference is likely to fall.
# Sample Mean of Differences: Average of the differences between paired observations.


# Example Hypotheses:

# H0: The mean difference between paired observations is zero.
# H1: The mean difference between paired observations is not zero.









# Other tests to perform:

# Mann-Whitney U Test: ####
# A non-parametric test if the temperature data does not follow a normal distribution.
# See if there is a significant difference between the distributions of two independent samples.
# Unlike t-tests, which compare means, the Mann-Whitney U Test compares medians.


# Example Hypotheses:

# H0: The distributions (medians) of the two groups are equal.
# H1: The distributions (medians) of the two groups are different.

# p-value < 0.05: Significant difference between the distributions.
# p-value >= 0.05: No significant difference between the distributions.




# Subsetting data for June and July
data_june <- subset(hotspots_peak, month == "Jun" & year == 2023)
data_july <- subset(hotspots_peak, month == "Jul" & year == 2023)

# Visual inspection
hist(data_june$temp, main="June Temperature Distribution", xlab="Temperature", col="blue", breaks=20)
hist(data_july$temp, main="July Temperature Distribution", xlab="Temperature", col="red", breaks=20)
boxplot(data_june$temp, data_july$temp, names=c("June", "July"), main="Temperature Comparison")

# Perform Mann-Whitney U Test
wilcox_test_result <- wilcox.test(data_june$temp, data_july$temp)
print(wilcox_test_result)
# a significant difference between the distributions of temperatures in June and July.
# test shows that the temperature distributions in June and July are significantly different, 
# meaning that one month had systematically higher or lower temperatures than the other. 

# So if visually there is already a difference in means, the test will confirm it by rejection the H0



# ANOVA (Analysis of Variance) Test: ####
# To determine if there are significant differences between the means of three or more independent groups.

# Multiple independent groups.
# Each group should be normally distributed.
# Homogeneity of variances (similar variances across groups).

# Histograms: For each group to check normality.
# Boxplots: To compare medians and variability.
# Levene’s Test: To check for homogeneity of variances.


# Output:
 
# F-value: The test statistic.
# p-value: Probability of observing the data if the null hypothesis is true.
# Group Means: Averages of each group’s data.

# Example Hypotheses:

# H0: All group means are equal.
# H1: At least one group mean is different.


# Subsetting data for June, July, and August
data_june <- subset(hotspots_peak, month == "Jun" & year == 2023)
data_july <- subset(hotspots_peak, month == "Jul" & year == 2023)
data_august <- subset(hotspots_peak, month == "Aug" & year == 2023)

# Combining data into a single data frame with a group identifier
data_combined <- rbind(
  data.frame(temp=data_june$temp, month="June"),
  data.frame(temp=data_july$temp, month="July"),
  data.frame(temp=data_august$temp, month="August")
)

# Visual inspection
hist(data_june$temp, main="June Temperature Distribution", xlab="Temperature", col="blue", breaks=20)
hist(data_july$temp, main="July Temperature Distribution", xlab="Temperature", col="red", breaks=20)
hist(data_august$temp, main="August Temperature Distribution", xlab="Temperature", col="green", breaks=20)
boxplot(temp ~ month, data=data_combined, main="Temperature Comparison")

# Perform ANOVA
anova_result <- aov(temp ~ month, data=data_combined)
summary(anova_result)
# The p-value is extremely small (<0.05), 
# indicating a significant difference between at least one pair of group means.



# Chi-Square Test: ####
# For categorical data (e.g., number of wildfire incidents per temperature range).
# To determine if there is a significant association between two categorical variables.
  
# Two categorical variables.
# Observations should be independent.
# Expected frequency in each category should be at least 5.
 
# Contingency Table: Shows the frequency distribution of categorical variables.
 
# Output:
# Chi-Square Statistic (X^2): Measures the discrepancy between observed and expected frequencies.
# p-value: Probability of observing the data if the null hypothesis is true.
 
# Example Hypotheses:
 
# H0: There is no association between the categorical variables.
# H1: There is an association between the categorical variables.

# p-value < 0.05: Reject H0, significant association between variables.
# p-value >= 0.05: Fail to reject H0, no significant association between variables.



























# Compare temperature ####

# Prepare data for tests

# Variables that could be influenced by or influence wildfires.

# Temperature (temp)
# Relative Humidity (rh)
# Wind Speed (ws)

# Fire Weather Index (FWI)
# Drought Code (DC)
# Head Fire Intencity (HFI)

# Create subsets of data based on conditions.
# Calculate mean values for the selected variables within specific groups.

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
#Temp
mean_temp_sep_all <- mean(data_sep_all$temp, na.rm = TRUE)
mean_temp_sep_all
mean_temp_sep_2023 <- mean(data_sep_2023$temp, na.rm = TRUE)
mean_temp_sep_2023
mean_temp_event_31404 <- mean(event_31404$temp,na.rm = TRUE)
mean_temp_event_31404


# Check for normal distribution

# Temp
# Histograms
hist(data_2023$temp, main="Temperature Distribution in 2023", xlab="Temperature")
hist(data_2020$temp, main="Temperature Distribution in 2020", xlab="Temperature")
hist(event_31404$temp, main="Temperature Distribution in Event 31404", xlab="Temperature")
hist(data_sep_all$temp, main="Temperature Distribution in Septembers (2014-2023)", xlab="Temperature")
hist(data_sep_2023$temp, main="Temperature Distribution in September 2023", xlab="Temperature")

# Q-Q Plots
qqnorm(data_2023$temp); qqline(data_2023$temp, col = "red")
qqnorm(data_2020$temp); qqline(data_2020$temp, col = "red")
qqnorm(event_31404$temp); qqline(event_31404$temp, col = "red")
qqnorm(data_sep_all$temp); qqline(data_sep_all$temp, col = "red")
qqnorm(data_sep_2023$temp); qqline(data_sep_2023$temp, col = "red")

# Shapiro-Wilk Test

set.seed(123)
options(scipen = 999) # numeric format

sample_2023 <- sample(data_2023$temp, 5000)
shapiro.test(sample_2023)
# W = 0.98352, p-value < 0.00000000000000022


sample_2020 <- sample(data_2020$temp, 5000)
shapiro.test(sample_2020)
# W = 0.94733, p-value < 0.00000000000000022


sample_event_31404 <- sample(event_31404$temp, 5000)
shapiro.test(sample_event_31404)
# W = 0.87, p-value < 0.00000000000000022


sample_sep_all <- sample(data_sep_all$temp, 5000)
shapiro.test(sample_sep_all)
# W = 0.97818, p-value < 0.00000000000000022


sample_sep_2023 <- sample(data_sep_2023$temp,5000)
shapiro.test(sample_sep_2023)
# W = 0.96513, p-value < 0.00000000000000022

# All samples are not normally distributed.
# Low W values and extremely small p-values








######################## DRAFT ####



 





wilcox.test(data_2023$temp, data_2020$temp)
kruskal.test(list(data_2023$temp, data_2020$temp, event_31404$temp))


# Calculating mean FWI for cluster 5017 and cluster 31404
mean_fwi_cluster_5017 <- mean(cluster_5017$fwi, na.rm = TRUE)
mean_fwi_cluster_31404 <- mean(cluster_31404$fwi, na.rm = TRUE)

# t-test for FWI between cluster 5017 and cluster 31404
t_test_fwi_clusters <- t.test(cluster_5017$fwi, cluster_2$fwi)
print(t_test_fwi_clusters)







