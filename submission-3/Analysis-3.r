#Analysis-1
# Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, modelsummary)

final.data <- read_rds("data/output/final_ma_data.rds")

#clean the data
ma.data.clean <- final.data %>%
  filter(!is.na(avg_enrollment) & year==2009 & !is.na(partc_score)) #<<


ma.data.clean <- ma.data.clean %>%
  mutate(raw_rating=rowMeans(
    cbind(breastcancer_screen,rectalcancer_screen,cv_cholscreen,diab_cholscreen,
          glaucoma_test,monitoring,flu_vaccine,pn_vaccine,physical_health,
          mental_health,osteo_test,physical_monitor,primaryaccess, nodelays,carequickly,
          overallrating_care,overallrating_plan,
          doctor_communicate,customer_service,osteo_manage,
          diabetes_eye,diabetes_kidney,diabetes_bloodsugar,
          diabetes_chol, bloodpressure,ra_manage,
          copd_test, bladder,falling,appeals_timely,
          appeals_review),
    na.rm=T)) %>%
  select(contractid, planid, fips, avg_enrollment, state, county, raw_rating, partc_score,
         avg_eligibles, avg_enrolled, premium_partc, risk_ab, Star_Rating,
         bid, avg_ffscost, ma_rate)


# Remove all SNPs, 800-series plans, and prescription drug only plans (i.e., plans that do not offer Part C benefits). Provide a box and whisker plot showing the distribution of plan counts by county over time. Do you think that the number of plans is sufficient, too few, or too many?
filtered_data <- final.data %>%
  filter(!(plan_type == "SNP" | grepl("^800", plan_name) | plan_type == "Prescription Drug Only"))

# Group by county and year to calculate plan counts
plan_counts <- filtered_data %>%
  group_by(county, year) %>%
  summarize(plan_count = n_distinct(planid))

# Create a box and whisker plot
problem1 <- ggplot(plan_counts, aes(x = factor(year), y = plan_count)) +
  geom_boxplot() +
  labs(x = "Year", y = "Plan Count", title = "Distribution of Plan Counts by County Over Time") +
  theme_minimal()

problem1
#Problem 2
#Provide bar graphs showing the distribution of star ratings in 2010, 2012, and 2015. How has this distribution changed over time?

# Filter data for the years 2010, 2012, and 2015
filtered_data2 <- final.data %>%
  filter(year %in% c(2010, 2012, 2015))

problem2 <- filtered_data2 %>% 
  ggplot(aes(x = as.factor(Star_Rating), fill = factor(year))) + 
  geom_bar(position = "dodge", color = "black") +
  labs(
    x = "Star Rating",
    y = "Count of Plans",
    title = "Frequency Distribution of Star Ratings"
  ) + 
  theme_bw() +
  scale_fill_manual(values = c("2010" = "#2db82d", "2012" = "#ff7f0e", "2015" = "#1f77b4"))

problem2



#Problem 3
average_payment <- final.data %>%
  group_by(year) %>%
  summarize(avg_benchmark_payment = mean(ma_rate, na.rm = TRUE))

# Create a line plot to visualize the trend of average benchmark payment over time
problem3 <- ggplot(average_payment, aes(x = year, y = avg_benchmark_payment)) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Average Benchmark Payment", title = "Average Benchmark Payment Over Time (2010-2015)") +
  theme_minimal()
problem3
# Problem 4

# Calculate market share data
mkt.share.data <- final.data %>%
  group_by(fips, year) %>%
  summarize(enroll = first(avg_enrolled),
            medicare = first(avg_eligibles),
            bench = mean(ma_rate, na.rm = TRUE)) %>%
  mutate(mkt_share = enroll / medicare)

# Create ggplot object
ma.share <- ggplot(mkt.share.data, aes(x = year, y = mkt_share))

# Add stat_summary layer for summary statistics
problem4 <- ma.share +
  stat_summary(fun = mean, geom = "line") + 
  labs(x = "Year", y = "Market Share", title = "Market Share Over Years") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels if needed



problem4

# Problem 5
# Assuming your data frame is called `plan_data` and contains data for the year 2010

# Filter data for the year 2010
plan_data_2010 <- final.data %>%
  filter(year == 2010)

# Calculate the running variable for each plan
plan_data_2010 <- plan_data_2010 %>%
  mutate(running_variable = Star_Rating - 0.5)

# Round the running variable to the nearest 0.5 to determine the rounded star rating
plan_data_2010 <- plan_data_2010 %>%
  mutate(rounded_star_rating = round(running_variable * 2) / 2)

# Count the number of plans for each rounded star rating
star_rating_counts_2010 <- plan_data_2010 %>%
  group_by(rounded_star_rating) %>%
  summarise(num_plans = n())


ma.data.clean <- final.data %>%
  filter(!is.na(avg_enrollment) & year==2010 & !is.na(partc_score)) 

ma.data.clean <- ma.data.clean %>%
  mutate(raw_rating=rowMeans(
    cbind(breastcancer_screen,rectalcancer_screen,cv_cholscreen,
          glaucoma_test,monitoring,flu_vaccine,pn_vaccine,physical_health,
          mental_health,osteo_test,physical_monitor,primaryaccess
          ,nodelays,carequickly,
          overallrating_care,overallrating_plan,
          doctor_communicate,customer_service,osteo_manage,
          diabetes_eye,diabetes_kidney,diabetes_bloodsugar,
          diabetes_chol,bloodpressure,ra_manage,
          copd_test,bladder,falling,appeals_timely,
          appeals_review),
    na.rm=T)) %>%
  select(contractid, planid, fips, avg_enrollment, state, county, raw_rating, partc_score,
         avg_eligibles, avg_enrolled, premium_partc, risk_ab, Star_Rating,
         bid, avg_ffscost, ma_rate)


ma.rounded <- ma.data.clean %>%
  mutate(rounded_30 = ifelse(raw_rating >= 2.75 & raw_rating < 3.00 & Star_Rating == 3.0, 1, 0),
         rounded_35 = ifelse(raw_rating >= 3.25 & raw_rating < 3.50 & Star_Rating == 3.5, 1, 0),
         rounded_40 = ifelse(raw_rating >= 3.75 & raw_rating < 4.00 & Star_Rating == 4.00, 1, 0), 
         rounded_45 = ifelse(raw_rating >= 4.25 & raw_rating < 4.50 & Star_Rating == 4.50, 1, 0), 
         rounded_50 = ifelse(raw_rating >= 4.75 & raw_rating < 5.00 & Star_Rating == 5.00, 1, 0))

problem5 <- ma.rounded %>%
  summarize(`3-star` = sum(rounded_30),
            `3.5-star` = sum(rounded_35),
            `4-star` = sum(rounded_40),
            `4.5-star` = sum(rounded_45),
            `5-star` = sum(rounded_50))

problem5


# Problem 6
# Using the RD estimator with a bandwidth of 0.125, provide an estimate of the effect of receiving a 3-star versus a 2.5 star rating on enrollments. 
#Repeat the exercise to estimate the effects at 3.5 stars, and summarize your results in a table.

# Install the 'rdrobust' package if not already installed
if (!require("rdrobust")) install.packages("rdrobust")

# Load the 'rdrobust' package
library(rdrobust)

# Estimate the effect of receiving a 3-star versus a 2.5-star rating
ma.rd3 <- ma.data.clean %>%
  filter(Star_Rating==2.5 | Star_Rating==3) %>%
  mutate(score = raw_rating - 2.75,
         treat = (Star_Rating==3.0),
         window1 = (score>=-.125 & score<=.125),
         window2 = (score>=-.125 & score<=.125),
         mkt_share = avg_enrollment/avg_eligibles,
         ln_share = log(mkt_share),
         score_treat=score*treat)

est3 <- rdrobust(y=ma.rd3$mkt_share, x=ma.rd3$score, c=0,
                 h=0.125, p=1, kernel="uniform", vce="hc0",
                 masspoints="off")

summary(est3)



# Extract the coefficient, standard error, z-value, and p-value
coef <- est3$coef
std_err <- est3$se
z_value <- est3$z
p_value <- est3$p

# Create a data frame
problem6.1 <- data.frame(
  Rating = c("3 vs 2.5"),
  Coefficient = coef,
  Std.Error = std_err,
  Z.Value = z_value,
  P.Value = p_value
)

problem6.1

# Estimate the effect of receiving a 3.5-star rating
ma.rd35 <- ma.data.clean %>%
  filter(Star_Rating==3 | Star_Rating==3.5) %>%
  mutate(score = raw_rating - 3.25,
         treat = (score>=0),
         window1 = (score>=-.125 & score<=.125),
         window2 = (score>=-.125 & score<=.125),
         mkt_share = avg_enrollment/avg_eligibles,
         ln_share = log(mkt_share),
         score_treat=score*treat)

est6.2 <- rdrobust(y=ma.rd35$mkt_share, x=ma.rd35$score, c=0,
                  h=0.125, p=1, kernel="uniform", vce="hc0",
                  masspoints="off")

summary(est6.2)

# Extract the coefficient, standard error, z-value, and p-value
coef <- est6.2$coef
std_err <- est6.2$se
z_value <- est6.2$z
p_value <- est6.2$p

# Create a data frame
problem6.2 <- data.frame(
  Rating = c("3 vs 3.5"),
  Coefficient = coef,
  Std.Error = std_err,
  Z.Value = z_value,
  P.Value = p_value
)

# Print the results
print(problem6.2)
problem6.2
# Problem 7


# Estimate the effect of receiving a 3.5-star rating

#Bandwidth .1
ma.rd7.1 <- ma.data.clean %>%
  filter(Star_Rating==3 | Star_Rating==3.5) %>%
  mutate(score = raw_rating - 3.25,
         treat = (score>=0),
         window1 = (score>=-.1 & score<=.1),
         mkt_share = avg_enrollment/avg_eligibles,
         ln_share = log(mkt_share),
         score_treat=score*treat)

est7.1 <- rdrobust(y=ma.rd7.1$mkt_share, x=ma.rd7.1$score, c=0,
                  h=0.1, p=1, kernel="uniform", vce="hc0",
                  masspoints="off")

summary(est7.1)

coef7.1 <- est7.1$coef
std_err7.1 <- est7.1$se
z_value7.1 <- est7.1$z
p_value7.1 <- est7.1$p

problem7.1 <- data.frame(
  Rating = c("3 vs 3.5"),
  Coefficient = coef7.1,
  Std.Error = std_err7.1,
  Z.Value = z_value7.1,
  P.Value = p_value7.1,
  Bandwidth = .1
)

# Print the results
print(problem7.1)

problem7.1

ma.rd7.2 <- ma.data.clean %>%
  filter(Star_Rating==3 | Star_Rating==3.5) %>%
  mutate(score = raw_rating - 3.25,
         treat = (score>=0),
         window2 = (score>=-.12 & score<=.12),
         mkt_share = avg_enrollment/avg_eligibles,
         ln_share = log(mkt_share),
         score_treat=score*treat)

est7.2 <- rdrobust(y=ma.rd7.2$mkt_share, x=ma.rd7.2$score, c=0,
                  h=0.12, p=1, kernel="uniform", vce="hc0",
                  masspoints="off")

summary(est7.2)

coef7.2 <- est7.2$coef
std_err7.2 <- est7.2$se
z_value7.2 <- est7.2$z
p_value7.2 <- est7.2$p

# Create a data frame
problem7.2 <- data.frame(
  Rating = c("3 vs 3.5"),
  Coefficient = coef7.2,
  Std.Error = std_err7.2,
  Z.Value = z_value7.2,
  P.Value = p_value7.2,
  Bandwidth = .12

)
problem7.2

# .13 Bandwidth
ma.rd7.3 <- ma.data.clean %>%
  filter(Star_Rating==3 | Star_Rating==3.5) %>%
  mutate(score = raw_rating - 3.25,
         treat = (score>=0),
         window2 = (score>=-.13 & score<=.13),
         mkt_share = avg_enrollment/avg_eligibles,
         ln_share = log(mkt_share),
         score_treat=score*treat)

est7.3 <- rdrobust(y=ma.rd7.3$mkt_share, x=ma.rd7.3$score, c=0,
                  h=0.13, p=1, kernel="uniform", vce="hc0",
                  masspoints="off")

summary(est7.3)

coef7.3 <- est7.3$coef
std_err7.3 <- est7.3$se
z_value7.3 <- est7.3$z
p_value7.3 <- est7.3$p

# Create a data frame
problem7.3 <- data.frame(
  Rating = c("3 vs 3.5"),
  Coefficient = coef7.3,
  Std.Error = std_err7.3,
  Z.Value = z_value7.3,
  P.Value = p_value7.3,
  Bandwidth = .13
)
problem7.3
# bandwidth .14
ma.rd7.4 <- ma.data.clean %>%
  filter(Star_Rating==3 | Star_Rating==3.5) %>%
  mutate(score = raw_rating - 3.25,
         treat = (score>=0),
         window2 = (score>=-.14 & score<=.14),
         mkt_share = avg_enrollment/avg_eligibles,
         ln_share = log(mkt_share),
         score_treat=score*treat)

est7.4 <- rdrobust(y=ma.rd7.4$mkt_share, x=ma.rd7.4$score, c=0,
                  h=0.14, p=1, kernel="uniform", vce="hc0",
                  masspoints="off")

summary(est7.4)

coef7.4 <- est7.4$coef
std_err7.4 <- est7.4$se
z_value7.4 <- est7.4$z
p_value7.4 <- est7.4$p

# Create a data frame
problem7.4 <- data.frame(
  Rating = c("3 vs 3.5"),
  Coefficient = coef7.4,
  Std.Error = std_err7.4,
  Z.Value = z_value7.4,
  P.Value = p_value7.4,
  Bandwidth = .14
)
problem7.4

# Bandwidth .15
ma.rd7.5 <- ma.data.clean %>%
  filter(Star_Rating==3 | Star_Rating==3.5) %>%
  mutate(score = raw_rating - 3.25,
         treat = (score>=0),
         window2 = (score>=-.15 & score<=.15),
         mkt_share = avg_enrollment/avg_eligibles,
         ln_share = log(mkt_share),
         score_treat=score*treat)

est7.5 <- rdrobust(y=ma.rd7.5$mkt_share, x=ma.rd7.5$score, c=0,
                  h=0.15, p=1, kernel="uniform", vce="hc0",
                  masspoints="off")

summary(est7.5)

coef7.5 <- est7.5$coef
std_err7.5 <- est7.5$se
z_value7.5 <- est7.5$z
p_value7.5 <- est7.5$p

# Create a data frame
problem7.5 <- data.frame(
  Rating = c("3 vs 3.5"),
  Coefficient = coef7.4,
  Std.Error = std_err7.4,
  Z.Value = z_value7.4,
  P.Value = p_value7.4,
  Bandwidth = .15
)

problem7.5

problem7 <- rbind(
  problem7.1,
  problem7.2,
  problem7.3,
  problem7.4,
  problem7.5
)

problem7
# Create a plot
problem.7 <- ggplot(problem7, aes(x = as.factor(Bandwidth), y = Coeff)) +
  geom_bar(stat = "identity", fill = "#e39527")+ 
  labs(title = "Effect of Receiving a 3.5-Star Rating",
       x = "Bandwidth Difference",
       y = "Coefficient") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Problem 8
# Examine (graphically) whether contracts appear to manipulate the running variable. In other words, look at the distribution of the running variable before and after the relevent threshold values. What do you find?

library(ggplot2)



threshold_values <- c(-0.125, 0.125)  # Assuming these are your relevant threshold values

# Subset the data for contracts just below and just above the threshold values
contracts_below <- ma.data.clean %>%
  filter(score >= threshold_values[1] - 0.05 & score <= threshold_values[1] + 0.05)
contracts_above <- ma.data.clean %>%
  filter(score >= threshold_values[2] - 0.05 & score <= threshold_values[2] + 0.05)

# Create density plots or histograms to compare the distribution of the running variable
# before and after the threshold values
plot_below <- ggplot(ma.rounded, aes(x = Star_Rating)) +
  geom_density(fill = "blue", alpha = 0.5) +  # Density plot for contracts below the threshold
  labs(title = "Distribution of Running Variable - Contracts Below Threshold")

plot_above <- ggplot(ma.rounded, aes(x = Star_Rating)) +
  geom_density(fill = "red", alpha = 0.5) +  # Density plot for contracts above the threshold
  labs(title = "Distribution of Running Variable - Contracts Above Threshold")


plot_above

plot_below

# Problem 9
# Similar to question 4, examine whether plans just above the threshold values have different characteristics than contracts just below the threshold values. Use HMO and Part D status as your plan characteristics.

mkt_share_data2 <- ma.data.clean %>%
  filter(plan_type == "HMO", partd == "Yes") %>%
  group_by(fips, year) %>%
  summarize(enroll = first(avg_enrolled),
            medicare = first(avg_eligibles),
            bench = mean(ma_rate, na.rm = TRUE)) %>%
  mutate(mkt_share = enroll / medicare)


ma.share <- ggplot(mkt.share.data, aes(x = year, y = mkt_share))

# Add stat_summary layer for summary statistics
problem9.1 <- ma.share +
  stat_summary(fun = mean, geom = "line") + 
  labs(x = "Year", y = "Market Share", title = "Market Share of HMOs and Part D Only Over Years") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


problem9.1



threshold_values <- c(-0.125, 0.125)  # Assuming these are your relevant threshold values

# Subset the data for contracts just below and just above the threshold values
contracts_below <- mkt_share_data2 %>%
  filter(score >= threshold_values[1] - 0.05 & score <= threshold_values[1] + 0.05)
contracts_above <- ma.da %>%
  filter(score >= threshold_values[2] - 0.05 & score <= threshold_values[2] + 0.05)

# Create density plots or histograms to compare the distribution of the running variable
# before and after the threshold values
plot_below <- ggplot(contracts_below, aes(x = Star_Rating)) +
  geom_density(fill = "blue", alpha = 0.5) +  # Density plot for contracts below the threshold
  labs(title = "Distribution of Running Variable - Contracts Below Threshold")

plot_above <- ggplot(contracts_above, aes(x = Star_Rating)) +
  geom_density(fill = "red", alpha = 0.5) +  # Density plot for contracts above the threshold
  labs(title = "Distribution of Running Variable - Contracts Above Threshold")

# Plot the graphs side by side
plot_grid(plot_below, plot_above, ncol = 2)

plot_above9

plot_below9
   # Rotate x-axis labels if needed
# Problem 10 
# Summarize your findings from 5-9. What is the effect of increasing a star rating on enrollments? Briefly explain your results.
save.image("submission-3/Hwk3_workspace.Rdata")
