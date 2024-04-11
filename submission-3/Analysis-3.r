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

problem2 <-filtered_data2 %>% 
  ggplot(aes(x=as.factor(Star_Rating))) + 
  geom_bar() +
  labs(
    x="Star Rating",
    y="Count of Plans",
    title="Frequency Distribution of Star Ratings"
  ) + theme_bw()

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

#Problem 5
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
ma.rd1_filtered <- ma.data.clean %>%
  mutate(score = raw_rating - 2.5,
         treat = (score >= 0),
         window = (score >= -0.125 & score <= 0.125),  # Adjusted bandwidth
         mkt_share = avg_enrollment / avg_eligibles,
         ln_share = log(mkt_share),
         score_treat = score * treat)
star3 <- lm(mkt_share ~ score + treat + score_treat, data = ma.rd1_filtered %>% filter(score == 0.5))
star35 <- lm(mkt_share ~ score + treat + score_treat, data = ma.rd1_filtered %>% filter(score == 1))

rows <- tribble(~term, ~ m1, ~ m2, ~ m3 , ~ m4,
                'Bandwidth', "0.25", "0.175", "0.175", "0.125")
attr(rows, 'position')  <- 7

modelsummary(list(star3, star35),
          keep=c("score", "treatTRUE", "score_treat"),
          coef_map=c("score"="Raw Score", 
                    "treatTRUE"="Treatment",
                    "score_treat"="Score x Treat"),
          gof_map=c("nobs", "r.squared"),
          add_rows=rows)

est_3_star <- as.numeric(coef(star3[1]))
est_35_star <- as.numeric(coef(star35[1]))
star3
# Create a summary table of results
problem6 <- data.frame(
  Star_Rating = c("3", "3.5"),
  Treatment_Effect = c(est_3_star, est_35_star)
)
# Print the summary table
print(problem6)

# Problem 7

# Define bandwidths
bandwidths <- c(0.1, 0.12, 0.13, 0.14, 0.15)

# Initialize lists to store treatment effect estimates
est_3_star <- vector("numeric", length(bandwidths))
est_35_star <- vector("numeric", length(bandwidths))

# Loop through each bandwidth
for (i in seq_along(bandwidths)) {
  # Filter the data within the specified bandwidth
  ma.rd1_filtered <- ma.rd1 %>%
    mutate(score = raw_rating - 2.25,
           treat = (score >= 0),
           window = (score >= -bandwidths[i] & score <= bandwidths[i]),
           mkt_share = avg_enrollment / avg_eligibles,
           ln_share = log(mkt_share),
           score_treat = score * treat)
  
  # Fit linear models for the treatment effect estimation
  star3 <- lm(mkt_share ~ score + treat + score_treat, data = ma.rd1_filtered %>% filter(score == 0))
  star35 <- lm(mkt_share ~ score + treat + score_treat, data = ma.rd1_filtered %>% filter(score == 0.5))
  
  # Extract the treatment effect estimates
  est_3_star[i] <- as.numeric(star3$coef[3])
  est_35_star[i] <- as.numeric(star35$coef[3])
}

# Create a dataframe for the results
results_df <- data.frame(
  Bandwidth = bandwidths,
  Star_3_Effect = est_3_star,
  Star_3.5_Effect = est_35_star
)

# Plot the results
ggplot(results_df, aes(x = Bandwidth)) +
  geom_line(aes(y = Star_3_Effect, color = "3-Star")) +
  geom_line(aes(y = Star_3.5_Effect, color = "3.5-Star")) +
  labs(x = "Bandwidth", y = "Treatment Effect Estimate", color = "Star Rating") +
  theme_minimal()


# Problem 8

