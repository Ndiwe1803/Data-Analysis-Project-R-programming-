install.packages("magrittr")
install.packages("GGally")
install.packages("ggrepel")
library(dplyr)
library(ggplot2)
library(magrittr)
library(ggrepel)


# Data Import
hr_data <- read.csv("Downloads/employee_attrition.csv")



# Data Preprocessing
names(hr_data) <- c("Employee_ID", "Record_date", "Birth_date", "Orig_hire_date", "Termination_date",
                    "Age", "Length_of_service", "City_name", "Department_name", "Job_title", "Store_name",
                    "Gender_short", "Gender_full", "Term_reason_desc", "Term_type_desc", "Status_year",
                                    "Status", "Business_unit")


hr_data


#data cleaning

hr_data <- hr_data[, !colnames(hr_data) %in% c("Gender_short","Record_date")]
hr_data




#data  exploration

#check the structure of this data set
str(hr_data)

# check the number of columns and rows
dim(hr_data)

# to generate a summary of the data set
summary(hr_data)





# Question 1: What is the factor that affects employee's layoff

# 1.1 Relationship between Layoff and Age

# Filtering data for layoff and age
layoff_age_data <- hr_data %>% filter(Term_reason_desc == "Layoff") %>%
  select(Age, Term_reason_desc)


# Creating age groups
Age_Group <- cut(layoff_age_data$Age, breaks = c(20, 30, 40, 50, 60, max(layoff_data$Age) + 1),
                 labels = c("20-30", "30-40", "40-50", "50-60", "60-older"), right = FALSE)

# Bar plot of layoff by age group
ggplot(data = layoff_age_data, aes(x = Age_Group)) +
  geom_bar(fill = "firebrick") +
  labs(title = "Relationship between employee's age and layoffs in the company",
       x = "Age", y = "The amount of Layoffs")







# 1.2 Relationship between Layoff and Gender

layoff_gender_data <- hr_data %>%
  filter(Term_reason_desc == "Layoff") %>%
  select(Gender_full, Term_reason_desc)

# Count the number of occurrences for each gender
layoff_gender_counts <- layoff_gender_data %>% count(Gender_full)

# Calculate percentages
layoff_gender_counts$percentage <- layoff_gender_counts$n / sum(layoff_gender_counts$n) * 100

# Pie chart of layoffs by gender
pie_chart <- ggplot(data = layoff_gender_counts, aes(x = "", y = n, fill = Gender_full)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Relationship between employee's gender and layoff",
       fill = "Gender") +
  scale_fill_manual(values = c("blue", "red")) +
  theme_void()

# Add labels with percentages
pie_chart_with_labels <- pie_chart +
  geom_label_repel(aes(label = paste0(round(percentage, 1), "%")),
                   position = position_stack(vjust = 0.5),
                   size = 4)

pie_chart_with_labels




# 1.3 Relationship between employee's department and layoff in the company 

layoff_department_data <- hr_data %>%
  filter(Term_reason_desc == "Layoff") %>%
  select(Department_name, Term_reason_desc)

layoff_department_data <- hr_data %>%
  filter(Term_reason_desc == "Layoff") %>%
  select(Department_name, Term_reason_desc)

# Count the number of occurrences for each department
layoff_department_counts <- layoff_department_data %>% count(Department_name)

# Calculate percentages
layoff_department_counts$percentage <- layoff_department_counts$n / sum(layoff_department_counts$n) * 100

# Pie chart of layoffs by department
layoff_department_data <- hr_data %>%
  filter(Term_reason_desc == "Layoff") %>%
  select(Department_name, Term_reason_desc)

# Count the number of occurrences for each department
layoff_department_counts <- layoff_department_data %>% count(Department_name)

# Calculate percentages
layoff_department_counts$percentage <- layoff_department_counts$n / sum(layoff_department_counts$n) * 100

# Pie chart of layoffs by department
ggplot(data = layoff_department_counts, aes(x = "", y = percentage, fill = Department_name)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Relationship between employee's department and layoff in the company",
       fill = "Department", x = NULL, y = NULL) +
  scale_fill_manual(values = viridis(nrow(layoff_department_counts))) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            size = 3) +
  theme_void()







# 1.4 Relationship between employee's time at the company and layoffs 
layoff_Length_of_service <- hr_data %>% filter(Term_reason_desc == "Layoff") %>%
  select(Length_of_service)

service_counts <- table(layoff_Length_of_service$Length_of_service)

# Convert the counts to a data frame
service_counts_df <- data.frame(service_group = as.numeric(names(service_counts)),
                                count = as.vector(service_counts),
                                stringsAsFactors = FALSE)

# Sort the data frame by Length_of_service
service_counts_df <- service_counts_df[order(service_counts_df$service_group), ]

# Plotting the line graph
ggplot(data = service_counts_df, aes(x = service_group, y = count)) +
  geom_line(color = "steelblue") +
  geom_point(color = "steelblue", size = 3) +
  labs(title = "Relationship between employee's time at the company and layoffs",
       x = "Length of Service", y = "The amount of Layoffs") +
  theme_minimal()


# 1.5 relationship between layoffs and the year of the layoff


# Filtering data for layoff and status year
layoff_Status_year <- hr_data %>%
  filter(Term_reason_desc == "Layoff") %>%
  select(Status_year)

# Count the number of occurrences for each year
layoff_Status_year_counts <- layoff_Status_year %>% count(Status_year)

# Calculate percentages
layoff_Status_year_counts$percentage <- layoff_Status_year_counts$n / sum(layoff_Status_year_counts$n) * 100

# Convert Status_year to a factor
layoff_Status_year_counts$Status_year <- factor(layoff_Status_year_counts$Status_year)

# Pie chart of layoffs by year
ggplot(data = layoff_Status_year_counts, aes(x = "", y = percentage, fill = Status_year)) +
  geom_bar(stat = "identity", width = 1) +
  labs(title = "Relationship between layoffs and the year of the layoff",
       fill = "Year", x = NULL, y = NULL) +
  scale_fill_manual(values = viridis(nrow(layoff_Status_year_counts))) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            position = position_stack(vjust = 0.5),
            size = 3) +
  theme_void()





#Question 2: What is the factor that affects employee's retirement


# 2.1 Relationship between employee's time at the company and retirement in the company

Retirement_Length_of_service <- hr_data %>% 
  filter(Term_reason_desc == "Retirement") %>% 
  select(Length_of_service)

Retirement_Length_of_service <- table(Retirement_Length_of_service$Length_of_service)

# Convert the counts to a data frame
Retirement_Length_of_service_df <- data.frame(service_group = as.numeric(names(Retirement_Length_of_service)),
                                              count = as.vector(Retirement_Length_of_service),
                                              stringsAsFactors = FALSE)

# Sort the data frame by service_group
Retirement_Length_of_service_df <- Retirement_Length_of_service_df[order(Retirement_Length_of_service_df$service_group), ]

# Plotting the line graph
ggplot(data = Retirement_Length_of_service_df, aes(x = service_group, y = count)) +
  geom_line(color = "steelblue") +
  geom_point(color = "steelblue", size = 3) +
  labs(title = "Relationship between employee's time at the company and retirement",
       x = "Time in the company", y = "The amount of Retirements") +
  theme_minimal()



# 2.2 Relationship between employee's age and retirement in the company

#filtering age and retirement
Retirement_age <- hr_data %>% 
  filter(Term_reason_desc == "Retirement") %>% 
  select(Age)

Retirement_age_counts <- table(Retirement_age$Age)

# Convert the counts to a data frame
Retirement_age_df <- data.frame(Age = as.factor(names(Retirement_age_counts)),
                                count = as.vector(Retirement_age_counts),
                                stringsAsFactors = FALSE)

# Plotting the pie chart
ggplot(data = Retirement_age_df, aes(x = "", y = count, fill = Age)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = " Relationship between employee's age and retirement",
       fill = "Age") +
  scale_fill_discrete() +
  theme_void()


#Question 3: What is the factor that affects employee's resignation

# 3.1 Relationship between employee's department and resignations in the company

# filtering resignation and department names
Resignation_Department_name <- hr_data %>% 
  filter(Term_reason_desc == "Resignaton") %>% 
  select(Department_name)

# Plotting the pie chart
ggplot(data = Resignation_Department_name, aes(x = "", fill = Department_name)) +
  geom_bar(width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Relationship between employee's department and resignations in the company",
       fill = "Department") +
  scale_fill_discrete() +
  theme_void()

# 3.2 Relationship between employee's employee's gender and resignation

# filtering resignation and gender
Resignation_Gender <- hr_data %>% 
  filter(Term_reason_desc == "Resignaton") %>% 
  select(Gender_full, Term_reason_desc)


# Count the number of occurrences for each gender
Resignation_Gender_counts <- Resignation_Gender %>% count(Gender_full)

# Calculate percentages
Resignation_Gender_counts$percentage <- Resignation_Gender_counts$n / sum(Resignation_Gender_counts$n) * 100

# Pie chart of layoffs by gender
pie_chart <- ggplot(data = Resignation_Gender_counts, aes(x = "", y = n, fill = Gender_full)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Relationship between employee's gender and resignation",
       fill = "Gender") +
  scale_fill_manual(values = c("limegreen", "coral")) +
  theme_void()

# Add labels with percentages
pie_chart_with_labels <- pie_chart +
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            position = position_stack(vjust = 0.5),
            size = 4)

pie_chart_with_labels
 

# 3.3 Relationship between employee's employee's age and resignation

#filtering age and resignation
resignation_age <- hr_data %>% 
  filter(Term_reason_desc == "Resignaton") %>% 
  select(Age)



# Bar plot of layoff by age group
ggplot(data = resignation_age, aes(x = Age)) +
  geom_bar(fill = "dodgerblue") +
  labs(title = "Relationship between employee's age and resignation",
       x = "Age", y = "The amount of resignations")

# 3.4 Relationship between employee's employee's time at the company and Resignation


Resignation_Length_of_service <- hr_data %>% 
  filter(Term_reason_desc == "Resignaton") %>% 
  select(Length_of_service)

Resignation_Length_of_service <- table(Resignation_Length_of_service$Length_of_service)

# Convert the counts to a data frame
Resignation_Length_of_service_df <- data.frame(service_group = as.numeric(names(Resignation_Length_of_service)),
                                              count = as.vector(Resignation_Length_of_service),
                                              stringsAsFactors = FALSE)

# Sort the data frame by service_group
Resignation_Length_of_service_df <- Resignation_Length_of_service_df[order(Resignation_Length_of_service_df$service_group), ]

# Plotting the line graph
ggplot(data = Resignation_Length_of_service_df, aes(x = service_group, y = count)) +
  geom_line(color = "steelblue") +
  geom_point(color = "steelblue", size = 3) +
  labs(title = "Relationship between employee's time at the company and resignation in the company",
       x = "Time in the company", y = "The amount of resignations") +
  theme_minimal()


# Question 4: What is the factor that affect employee's lenght of time at the company


# 4.1 Relationship between an employee's time at the company and their department

# Calculate average time spent in each department
average_time_by_department <- hr_data %>%
  group_by(Department_name) %>%
  summarize(average_time = mean(Length_of_service))

# Sort the data by average time in ascending order
average_time_by_department <- average_time_by_department[order(average_time_by_department$average_time), ]

# Plotting the bar plot
ggplot(data = average_time_by_department, aes(x = Department_name, y = average_time)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "Relationship between an employee's time at the company and their department",
       x = "Department",
       y = "Average Time (in years)")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



# 4.2 Relationship between an employee's time at the company and their business unit in the company

# Calculate average time spent in each business unit
average_time_by_Business_unit <- hr_data %>%
  group_by(Business_unit) %>%
  summarize(average_time = mean(Length_of_service))

# Sort the data by average time in ascending order
average_time_by_Business_unit <- average_time_by_Business_unit[order(average_time_by_Business_unit$average_time), ]

# Plotting the bar plot
ggplot(data = average_time_by_Business_unit, aes(x = Business_unit, y = average_time)) +
  geom_bar(stat = "identity", fill = "grey") +
  labs(title = "Relationship between an employee's time at the company and their business unit in the company",
           x = "Business unit",
           y = "Average Time (in years)") 

# 4.3 Relationship between an employee's time at the company and the age in which they were hired

hr_data <- hr_data %>%
  mutate(Birth_date = as.Date(Birth_date, format = "%m/%d/%Y"),
         Orig_hire_date = as.Date(Orig_hire_date, format = "%m/%d/%Y"))

# Calculate age at hiring in years
hr_data <- hr_data %>%
  mutate(age_at_hiring = as.numeric(difftime(Orig_hire_date, Birth_date, units = "days")) / 365)

# Define age groups
age_groups <- cut(hr_data$age_at_hiring, breaks = c(0, 20, 30, 40, 50, Inf),
                  labels = c("0-20", "21-30", "31-40", "41-50", "51+"), right = FALSE)

# The average time spent by each age group
average_time_by_age <- hr_data %>%
  group_by(age_group = age_groups) %>%
  summarize(average_time = mean(Length_of_service))

# Plotting the bar plot
ggplot(data = average_time_by_age, aes(x = age_group, y = average_time)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Relationship between an employee's time at the company and the age in which they were hired",
       x = "Age Group at Hiring",
       y = "Average Time Spent (in years)")


# Question 5: Does a canditate's age affect company hiring policy


# 5.1 Relationship between age at hiring and the department 

average_age_by_department <- hr_data %>%
  group_by(Department_name) %>%
  summarize(average_age = mean(age_at_hiring))

# Sort the data by average age in ascending order
average_age_by_department <- average_age_by_department[order(average_age_by_department$average_age), ]

# plotting the bar plot
ggplot(data = average_age_by_department, aes(x = Department_name, y = average_age)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  labs(title = "Relationship between age at hiring and the department",
       x = "Department name",
       y = "Average age")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



# 5.2 Relationship between age at hiring and business unit
average_age_by_Business_unit <- hr_data %>%
  group_by(Business_unit) %>%
  summarize(average_age = mean(age_at_hiring))


# Sort the data by average time in ascending order
average_age_by_Business_unit <- average_age_by_Business_unit[order(average_time_by_Business_unit$average_time), ]

# plotting the bar plot
ggplot(data = average_age_by_Business_unit, aes(x = Business_unit, y = average_age)) +
  geom_bar(stat = "identity", fill = c("dodgerblue", "firebrick")) +
  labs(title = "Relationship between age at hiring and business unit",
       x = "Business unit",
       y = "Average age") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


