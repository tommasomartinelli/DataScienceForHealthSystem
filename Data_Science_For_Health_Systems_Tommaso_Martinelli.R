###Libraries
library(kableExtra)
library(car)
library(dplyr)
library(ggplot2)
library(vioplot)
library(sf)
library(gridExtra)

###Dataset uploading

hiv_data <- read.csv("C:/Users/Tommaso/Desktop/uni/data science/ProgettoData/HIV1.csv", header = TRUE, sep = ",")
head(hiv_data)
names(hiv_data)
summary(hiv_data)

unique_val_period <- unique(hiv_data$Period.type)
num_unique_val_period <- length(unique_val_period)

cat("Number of unique values of Period type:", num_unique_val_period)

unique_val_location <- unique(hiv_data$Location.type)
num_unique_val_location <- length(unique_val_location)

cat("Number of unique values of Location type:", num_unique_val_location)


###Data preprocessing: Selecting and renaming columns

columns_to_select <- c("ParentLocation", "Location", "Period", "Dim1", "FactValueNumeric", "FactValueNumericLow", "FactValueNumericHigh")
modified_data <- hiv_data %>%
  select(all_of(columns_to_select))
names(modified_data) <- c("geographic_area", "state", "year", "sex", "value",  "lower_confidence_interval",  "upper_confidence_interval")
head(modified_data)
summary(modified_data)

#Manage sex feature
total_infections_both <- modified_data %>%
  filter(sex == "Both sexes") %>%
  summarize(total_infections = mean(value, na.rm = TRUE))

total_infections_male <- modified_data %>%
  filter(sex == "Male") %>%
  summarize(total_infections = mean(value, na.rm = TRUE))

total_infections_female <- modified_data %>%
  filter(sex == "Female") %>%
  summarize(total_infections = mean(value, na.rm = TRUE))

total_infections_both
total_infections_female
total_infections_male

mean_infections_male <- total_infections_male$total_infections
mean_infections_female <- total_infections_female$total_infections

average_infections_both <- (mean_infections_male + mean_infections_female) / 2

cat("Average male infections:", mean_infections_male, "\n")
cat("Average female infections:", mean_infections_female, "\n")
cat("Average 'Both sexes' infetions:", total_infections_both$total_infections, "\n")
cat("Mean between male and female infections:", average_infections_both, "\n")

#Eliminate "Both sexes"
filtered_data <- modified_data %>% 
  filter(sex %in% c("Male", "Female"))
head(filtered_data)

#Checking Nan values
na_counts <- colSums(is.na(modified_data))
print(na_counts)
total_samples <- nrow(filtered_data)
print(total_samples)
filtered_data_clean <- filtered_data %>%
  filter(!is.na(value))


###Exploratory analysis

#Check the distribution of values
hist(filtered_data_clean$value, breaks = 20, main = "Value distribution", xlab = "Value", cex.main = 1.15, cex.lab = 1.15)

##Explore "sex" feature
boxplot(value ~ sex, data = filtered_data_clean, main = "Differences in new infections between males and females",
        xlab = "Sex", ylab = "Value", col = "green")

ggplot(filtered_data_clean, aes(x = sex, y = value, fill = sex)) +
  geom_boxplot() +
  labs(title = "Differences in new infections between males and females",
       x = "Sex",
       y = "Value") +
  theme_minimal() +
  theme(text = element_text(size = 14),  
        plot.title = element_text(size = 14, hjust = 0.5))  


vioplot(filtered_data$value ~ filtered_data$sex, 
        names = c("Male", "Female"), 
        main = "Differences in new infections between males and females", 
        xlab = "Sex", ylab = "Value", col = "green")

ggplot(filtered_data_clean, aes(x = value, fill = sex)) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribution of new infection values",
       x = "Value",
       y = "Density") +
  theme_minimal()+
  theme(text = element_text(size = 14))

stripchart(filtered_data_clean$value ~ filtered_data_clean$sex, 
           method = "jitter", pch = 16, vertical = TRUE, 
           main = "Distribution of new infection values", 
           xlab = "Value", ylab = "Sex", col = "lightgreen")

ggplot(filtered_data_clean, aes(x = value, y = sex, color = sex)) +
  geom_point(alpha = 0.5, position = position_jitter(width = 0.2)) +
  labs(title = "Distribution of new infection values",
       x = "Value",
       y = "Sex") +
  theme_minimal() +
  theme(text = element_text(size = 14), 
        plot.title = element_text(size = 14, hjust = 0.5)) 
  

##Analysis of variations over the years
count_per_year <- filtered_data_clean %>%
  group_by(year) %>%
  summarize(count = n())

print(n=33, count_per_year)

average_infections_per_year <- filtered_data_clean %>%
  group_by(year) %>%
  summarize(average_infections = mean(value, na.rm = TRUE))

average_infections_per_year$year <- as.factor(average_infections_per_year$year)

ggplot(average_infections_per_year, aes(x = year, y = average_infections)) +
  geom_bar(stat = "identity", fill = "#1f77b7") +
  labs(title = "Mean of New Infections by Year",
       x = "Year",
       y = "Mean of new infection") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = "none")+
  theme(text = element_text(size = 14), 
        plot.title = element_text(size = 14, hjust = 0.5)) 

ggplot(filtered_data_clean, aes(x = value, fill = year)) +
  geom_histogram(binwidth = 0.1, position = "identity", alpha = 0.5) +
  facet_wrap(~year, ncol = 3) +
  labs(title = "Distribution of New Infections per Year",
       x = "Value",
       y = "Frequency") +
  theme_minimal()
#sex differences over the years
average_infections_per_year1 <- filtered_data_clean %>%
  group_by(year, sex, .groups = "drop") %>%
  summarize(average_infections = mean(value, na.rm = TRUE))

ggplot(average_infections_per_year1, aes(x = year, y = average_infections, color = sex)) +
  geom_line() +
  labs(title = "Differences in Mean New Infections between Males and Females",
       x = "Year",
       y = "Mean of new infection",
       color = "Sex") +
  theme_minimal() +
  theme(text = element_text(size = 14), 
        plot.title = element_text(size = 14, hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


##Geographical areas analysis
#Average infections per area
average_infections_by_area <- filtered_data %>%
  group_by(geographic_area) %>%
  summarize(average_infections = mean(value, na.rm = TRUE)) %>%
  arrange(average_infections)

ggplot(average_infections_by_area, aes(x = reorder(geographic_area, average_infections), y = average_infections, fill = geographic_area)) +
  geom_bar(stat = "identity") +
  labs(title = "Average of New Infections by Geographical Area",
       x = "Geographical Area",
       y = "Average of new infections") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set2") +
  guides(fill = FALSE)+
  theme(text = element_text(size = 14), 
        plot.title = element_text(size = 14, hjust = 0.5)) 

#Average number of new infections per year
average_infections_by_area_year <- filtered_data %>%
  group_by(geographic_area, year) %>%
  summarize(average_normalized_infections = mean(value, na.rm = TRUE)) 

ggplot(average_normalized_infections_by_area_year, aes(x = year, y = average_normalized_infections, color = geographic_area)) +
  geom_line() +
  labs(title = "Average Infections by Geographical Area and Year",
       x = "Year",
       y = "Average Infections",
       color = "Geographical Area") +
  theme_minimal() +
  theme(legend.position = "right")

##Analysing individual areas
#Define functions to be reused for each area


analyze_geographic_area <- function(data, area_name) {
  area_data <- data %>%
    filter(geographic_area == area_name)
  
  area_data_clean <- area_data %>%
    filter(!is.na(value))
  
  average_infections_by_year <- area_data_clean %>%
    group_by(year) %>%
    summarize(average_infections = mean(value, na.rm = TRUE))
  
  avg_bar_plot <- ggplot(average_infections_by_year, aes(x = year, y = average_infections)) +
    geom_bar(stat = "identity", fill = "#1f77b7") +
    labs(title = paste("Average Infections Over the Years in", area_name),
         x = "Year",
         y = "Average Infections") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    theme(text = element_text(size = 14), 
          plot.title = element_text(size = 14, hjust = 0.5)) 
  
  average_infections_by_sex <- area_data_clean %>%
    group_by(year, sex, .groups = "drop") %>%
    summarize(average_infections = mean(value, na.rm = TRUE))
  
  line_plot <- ggplot(average_infections_by_sex, aes(x = year, y = average_infections, color = sex)) +
    geom_line() +
    labs(title = paste("Average Infections by State in", area_name),
         x = "Year",
         y = "Average Infections",
         color = "Sex") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    theme(text = element_text(size = 14),
          plot.title = element_text(size = 14, hjust = 0.5)) 
  
  average_infections_by_state <- area_data_clean %>%
    group_by(state) %>%
    summarize(average_infections = mean(value, na.rm = TRUE))
  
  if (area_name == "Africa") {
    average_infections_by_state <- average_infections_by_state %>%
      filter(average_infections >= 1.2)
  }
  
  bar_plot <- ggplot(average_infections_by_state, aes(x = reorder(state, -average_infections), y = average_infections, fill = state)) +
    geom_bar(stat = "identity") +
    labs(title = paste("Average Infections by State in", area_name),
         x = "State",
         y = "Average Infections",
         fill = "State") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "none")+
    theme(text = element_text(size = 14),
          plot.title = element_text(size = 14, hjust = 0.5))
  
  return(list(line_plot = line_plot, bar_plot = bar_plot, avg_bar_plot = avg_bar_plot))
}

#Heatmap
create_heatmap <- function(data, area_name) {
  heatmap_data <- data %>%
    filter(geographic_area == area_name) %>%
    select(state, year, value)
  
  heatmap_data <- heatmap_data %>%
    arrange(state, year)
  
  total_infections_heatmap <- heatmap_data %>%
    group_by(state, year) %>%
    summarize(total_infections = mean(value, na.rm = TRUE))
  
  heatmap_plot <- ggplot(total_infections_heatmap, aes(x = year, y = state, fill = total_infections)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "red") +
    labs(title = paste("Heatmap of New Infections for", area_name),
         x = "Year",
         y = "State",
         fill = "New Infections") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(text = element_text(size = 14)) +
    scale_x_continuous(breaks = c(1990, seq(1995, max(total_infections_heatmap$year), by = 5)))
  
  return(heatmap_plot)
}


#African Analysis
plots_africa <- analyze_geographic_area(filtered_data, "Africa")
heatmap_africa <- create_heatmap(filtered_data, "Africa")
print(plots_africa$avg_bar_plot)
print(plots_africa$line_plot)
print(plots_africa$bar_plot)
print(heatmap_africa)

#Americas Analysis
plots_america <- analyze_geographic_area(filtered_data, "Americas")
heatmap_america <- create_heatmap(filtered_data, "Americas")
print(plots_america$avg_bar_plot)
print(plots_america$line_plot)
print(plots_america$bar_plot)
print(heatmap_america)

#Europe Analysis
plots_europe <- analyze_geographic_area(filtered_data, "Europe")
heatmap_europe <- create_heatmap(filtered_data, "Europe")
print(plots_europe$avg_bar_plot)
print(plots_europe$line_plot)
print(plots_europe$bar_plot)
print(heatmap_europe)

#Western Pacific Analysis
plots_westpacific <- analyze_geographic_area(filtered_data, "Western Pacific")
heatmap_westpacific <- create_heatmap(filtered_data, "Western Pacific")
print(plots_westpacific$avg_bar_plot)
print(plots_westpacific$line_plot)
print(plots_westpacific$bar_plot)
print(heatmap_westpacific)

#South-east Asia Analysis
plots_southeastasia <- analyze_geographic_area(filtered_data, "South-East Asia")
heatmap_southeastasia <- create_heatmap(filtered_data, "South-East Asia")
print(plots_southeastasia$avg_bar_plot)
print(plots_southeastasia$line_plot)
print(plots_southeastasia$bar_plot)
print(heatmap_southeastasia)

#Eastern Mediterranean Analysis
plots_easternmediterranean <- analyze_geographic_area(filtered_data, "Eastern Mediterranean")
heatmap_easternmediterranean <- create_heatmap(filtered_data, "Eastern Mediterranean")
print(plots_easternmediterranean$avg_bar_plot)
print(plots_easternmediterranean$line_plot)
print(plots_easternmediterranean$bar_plot)
print(heatmap_easternmediterranean)

combined_plot1 <- grid.arrange(
  plots_europe$avg_bar_plot,
  plots_westpacific$avg_bar_plot,
  plots_southeastasia$avg_bar_plot,
  plots_easternmediterranean$avg_bar_plot,
  nrow = 2 
)

combined_plot2 <- grid.arrange(
  plots_europe$line_plot,
  plots_westpacific$line_plot,
  plots_southeastasia$line_plot,
  plots_easternmediterranean$line_plot,
  nrow = 2 
)

print(combined_plot1)
print(combined_plot2)

###Statistical test
##Statistical tests on the sex variable
ci_male <- filtered_data_clean %>%
  filter(sex == "Male") %>%
  summarize(lower_ci = mean(lower_confidence_interval), upper_ci = mean(upper_confidence_interval))

ci_female <- filtered_data_clean %>%
  filter(sex == "Female") %>%
  summarize(lower_ci = mean(lower_confidence_interval), upper_ci = mean(upper_confidence_interval))

if (ci_male$upper_ci < ci_female$lower_ci || ci_female$upper_ci < ci_male$lower_ci) {
  print("The confidence intervals do not overlap")
} else {
  print("The confidence intervals overlap")
}

mean_male <- mean(filtered_data_clean$value[filtered_data_clean$sex == "Male"])
mean_female <- mean(filtered_data_clean$value[filtered_data_clean$sex == "Female"])

cat("Average of new cases per male:", mean_male, "\n")
cat("Average of new cases per female:", mean_female, "\n")

sd_male <- sd(filtered_data_clean$value[filtered_data_clean$sex == "Male"])
sd_female <- sd(filtered_data_clean$value[filtered_data_clean$sex == "Female"])

cat("Standard deviation of new cases for males:", sd_male, "\n")
cat("Standard deviation of new cases for females:", sd_female, "\n")

median_male <- median(filtered_data_clean$value[filtered_data_clean$sex == "Male"])
median_female <- median(filtered_data_clean$value[filtered_data_clean$sex == "Female"])

cat("Median of new cases per males:", median_male, "\n")
cat("Median of new cases per females:", median_female, "\n")

percentage_difference <- ((mean_female - mean_male) / mean_male) * 100

cat("Percentage difference in average new cases between males and females:", percentage_difference, "%\n")

#Verifying normality
qqnorm(filtered_data_clean$value[filtered_data_clean$sex == "Male"])
qqline(filtered_data_clean$value[filtered_data_clean$sex == "Male"], col = "red")
qqnorm(filtered_data_clean$value[filtered_data_clean$sex == "Female"])
qqline(filtered_data_clean$value[filtered_data_clean$sex == "Female"], col = "red")

shapiro_test_male <- shapiro.test(filtered_data_clean$value[filtered_data_clean$sex == "Male"])
shapiro_test_female <- shapiro.test(filtered_data_clean$value[filtered_data_clean$sex == "Female"])

print("verifying normality- Male:")
print(shapiro_test_male)
print("verifying normality - Female:")
print(shapiro_test_female)

#Non-parametric test application

mannwhitney_test <- wilcox.test(value ~ sex, data = filtered_data_clean, paired = FALSE)
print("Mann-Whitney U test results:")
print(mannwhitney_test)


##statistical tests on the geographical area variable
ggplot(filtered_data_clean, aes(x = geographic_area, y = value, fill = geographic_area)) +
  geom_boxplot() +
  labs(title = "Distribution of new cases by geographical area",
       x = "Geographic Area",
       y = "New cases") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(text = element_text(size = 14))

#Function for qqplots
create_qq_plot <- function(data, area_name) {
  qqplot_data <- data %>%
    filter(geographic_area == area_name) %>%
    select(value)
  
  ggplot(qqplot_data, aes(sample = value)) +
    geom_qq() +
    geom_qq_line(color = "red") +
    labs(title = paste("QQ Plot per l'area geografica:", area_name),
         x = "Quantili teorici",
         y = "Quantili osservati") +
    theme_minimal()
}

areas <- unique(filtered_data_clean$geographic_area)
qq_plots <- lapply(areas, function(area) create_qq_plot(filtered_data_clean, area))

for (i in seq_along(areas)) {
  print(qq_plots[[i]])
}

#check normality and homoschedasticity to apply ANOVA
shapiro_test_results <- filtered_data_clean %>%
  group_by(geographic_area) %>%
  summarize(p_value = shapiro.test(value)$p.value)

shapiro_test_results

bartlett_test_result <- bartlett.test(value ~ geographic_area, data = filtered_data_clean)
bartlett_test_result

#cannot apply ANOVA
#Kruskal Wallis test

kruskal_wallis_result <- kruskal.test(value ~ geographic_area, data = filtered_data_clean)


print(kruskal_wallis_result)

#Post-hoc analysis
# Perform the Mann-Whitney U test for each pair of areas

pairwise_test_bonferroni <- pairwise.wilcox.test(filtered_data_clean$value, filtered_data_clean$geographic_area, p.adjust.method = "bonferroni")

print(pairwise_test_bonferroni)

pairwise_test_bh <- pairwise.wilcox.test(filtered_data_clean$value, filtered_data_clean$geographic_area, p.adjust.method = "BH")

print(pairwise_test_bh)


