#Analysis-1

#clean the data
ma.data.clean <- ma.data %>%
  filter(!is.na(avg_enrollment) & year==2009 & !is.na(partc_score)) #<<


ma.data.clean <- ma.data.clean %>%
  mutate(raw_rating=rowMeans(
    cbind(breastcancer_screen,rectalcancer_screen,cv_cholscreen,diabetes_cholscreen,
          glaucoma_test,monitoring,flu_vaccine,pn_vaccine,physical_health,
          mental_health,osteo_test,physical_monitor,primaryaccess,
          hospital_followup,depression_followup,nodelays,carequickly,
          overallrating_care,overallrating_plan,calltime,
          doctor_communicate,customer_service,osteo_manage,
          diabetes_eye,diabetes_kidney,diabetes_bloodsugar,
          diabetes_chol,antidepressant,bloodpressure,ra_manage,
          copd_test,betablocker,bladder,falling,appeals_timely,
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
problem1 <- ggplot(plan_counts, aes(x = year, y = plan_count, fill = county)) +
  geom_boxplot() +
  labs(x = "Year", y = "Plan Count", title = "Distribution of Plan Counts by County over Time") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Problem 2
Provide bar graphs showing the distribution of star ratings in 2010, 2012, and 2015. How has this distribution changed over time?

# Filter data for the years 2010, 2012, and 2015
filtered_data2 <- final.data %>%
  filter(year %in% c(2010, 2012, 2015))

#create one for 2010
 ma.data.clean %>% 
  ggplot(aes(x=as.factor(Star_Rating))) + 
  geom_bar() +
  labs(
    x="Star Rating",
    y="Count of Plans",
    title="Frequency Distribution of Star Ratings"
  ) + theme_bw()


#create one for 2012
 ma.data.clean %>% 
  ggplot(aes(x=as.factor(Star_Rating))) + 
  geom_bar() +
  labs(
    x="Star Rating",
    y="Count of Plans",
    title="Frequency Distribution of Star Ratings"
  ) + theme_bw()

#create one for 2015
 ma.data.clean %>% 
  ggplot(aes(x=as.factor(Star_Rating))) + 
  geom_bar() +
  labs(
    x="Star Rating",
    y="Count of Plans",
    title="Frequency Distribution of Star Ratings"
  ) + theme_bw()

#Problem 3
average_payment <- final.data %>%
  group_by(year) %>%
  summarize(avg_benchmark_payment = mean(benchmark_payment, na.rm = TRUE))

# Create a line plot to visualize the trend of average benchmark payment over time
problem3 <- ggplot(average_payment, aes(x = year, y = avg_benchmark_payment)) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Average Benchmark Payment", title = "Average Benchmark Payment Over Time (2010-2015)") +
  theme_minimal()

# Problem 4

# Calculate the total number of Medicare eligibles for each year
total_eligibles <- final.data %>%
  filter(year %in% 2010:2015) %>%
  group_by(year) %>%
  summarize(total_eligibles = sum(enrollment))



# Group by year and calculate the total enrollment in Medicare Advantage plans for each year
total_enrollment_ma <- final.data %>%
  group_by(year) %>%
  summarize(total_enrollment_ma = sum(enrollment))

# Calculate the average share of Medicare Advantage enrollees relative to all Medicare eligibles for each year
ma_share <- inner_join(total_eligibles, total_enrollment_ma, by = "year") %>%
  mutate(share_ma = total_enrollment_ma / total_eligibles)

# Create a line plot to visualize the trend of the average share of Medicare Advantage over time
problem4 <- ggplot(ma_share, aes(x = year, y = share_ma)) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Average Share of Medicare Advantage", title = "Average Share of Medicare Advantage Over Time (2010-2015)") +
  theme_minimal()


# Problem 5
# Assuming your data frame is called `plan_data` and contains data for the year 2010

# Filter data for the year 2010
plan_data_2010 <- final.data %>%
  filter(year == 2010)

# Calculate the running variable for each plan
plan_data_2010 <- plan_data_2010 %>%
  mutate(running_variable = star_rating - 0.5)

# Round the running variable to the nearest 0.5 to determine the rounded star rating
plan_data_2010 <- plan_data_2010 %>%
  mutate(rounded_star_rating = round(running_variable * 2) / 2)

# Count the number of plans for each rounded star rating
star_rating_counts_2010 <- plan_data_2010 %>%
  group_by(rounded_star_rating) %>%
  summarise(num_plans = n())

# Print the table
print(star_rating_counts_2010)
