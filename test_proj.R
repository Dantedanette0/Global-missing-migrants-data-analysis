library(tidyverse)
library(maps)
library(scales)
library(ggthemes)
library(stringr)

# Load the dataset
d <- read.csv("Global Missing Migrants Dataset.csv")

# Function to group years into 3-year intervals for better analysis granularity
group_years <- function(year) {
  if (year %in% 2014:2016) "2014-2016"
  else if (year %in% 2017:2019) "2017-2019"
  else if (year %in% 2020:2023) "2020-2023"
  else as.character(year)
}

# Apply the year grouping function to the dataset
d$Year_Group <- sapply(d$Incident.year, group_years)

# Summarize incidents by Year_Group, counting various demographics
incidents_by_year_gender <- d %>%
  group_by(Year_Group) %>%
  summarise(
    Male_Survived = sum(Number.of.Survivors * (Number.of.Males > 0), na.rm = TRUE),
    Female_Survived = sum(Number.of.Survivors * (Number.of.Females > 0), na.rm = TRUE),
    Children_Survived = sum(Number.of.Survivors * (Number.of.Children > 0), na.rm = TRUE),
    Male_Dead = sum(Number.of.Dead * (Number.of.Males > 0), na.rm = TRUE),
    Female_Dead = sum(Number.of.Dead * (Number.of.Females > 0), na.rm = TRUE),
    Children_Dead = sum(Number.of.Dead * (Number.of.Children > 0), na.rm = TRUE),
    Male_Missing = sum(Minimum.Estimated.Number.of.Missing * (Number.of.Males > 0), na.rm = TRUE),
    Female_Missing = sum(Minimum.Estimated.Number.of.Missing * (Number.of.Females > 0), na.rm = TRUE),
    Children_Missing = sum(Minimum.Estimated.Number.of.Missing * (Number.of.Children > 0), na.rm = TRUE)
  ) %>%
  pivot_longer(cols = -Year_Group, names_to = "Category", values_to = "Incidents") # Reshape data for easier plotting

# Separate 'Category' into more descriptive 'Gender' and 'Status' columns
incidents_by_year_gender <- incidents_by_year_gender %>%
  separate(Category, into = c("Gender", "Status"), sep = "_")


# Custom function to group years into 3-year intervals
group_years <- function(year) {
  if (year %in% 2014:2016) {
    return("2014-2016")
  } else if (year %in% 2017:2019) {
    return("2017-2019")
  } else if (year %in% 2020:2023) {
    return("2020-2023")
  } else {
    return(as.character(year))
  }
}

# Apply the year grouping function to categorize data into year groups
d$Year_Group <- sapply(d$Incident.year, group_years)

# Calculate total counts for dead, missing, and survivors for pie chart visualization
total_dead <- sum(d$Number.of.Dead, na.rm = TRUE)
total_missing <- sum(d$Minimum.Estimated.Number.of.Missing, na.rm = TRUE)
total_survivors <- sum(d$Number.of.Survivors, na.rm = TRUE)

# Create a dataframe for the pie chart data with calculated totals
pie_data <- data.frame(
  Category = c("Dead", "Missing", "Survived"),
  Count = c(total_dead, total_missing, total_survivors)
)

# Calculate the percentage for each category to display in the pie chart
pie_data$Percentage <- (pie_data$Count / sum(pie_data$Count)) * 100

# Count total incidents per country, useful for geographic visualizations
incidents_total <- d %>%
  separate_rows(Country.of.Origin, sep = "[,;/]") %>%  # Handle multiple countries per row
  group_by(Country.of.Origin) %>%
  summarise(Total_Incidents = n(), .groups = 'drop')  # Count incidents by country

# Prepare detailed incident data by country, excluding NAs for specific counts
incidents_details <- d %>%
  filter(!is.na(Number.of.Dead) | !is.na(Minimum.Estimated.Number.of.Missing) | !is.na(Number.of.Survivors)) %>%
  separate_rows(Country.of.Origin, sep = "[,;/]") %>%
  group_by(Country.of.Origin) %>%
  summarise(
    Deaths = sum(Number.of.Dead, na.rm = TRUE),
    Survived = sum(Number.of.Survivors, na.rm = TRUE),
    Missing = sum(Minimum.Estimated.Number.of.Missing, na.rm = TRUE),
    .groups = 'drop'
  )

# Merge total incident data with detailed counts for mapping
mapped_data <- world_map %>%
  left_join(incidents_total, by = c("region" = "Country.of.Origin")) %>%
  left_join(incidents_details, by = c("region" = "Country.of.Origin"))






############################################################plots





# Visualization 1: Geographic Map of Total Incidents
map_total_incidents <- ggplot(mapped_data, aes(x = long, y = lat, group = group, fill = Total_Incidents)) +
  geom_polygon(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "grey50", name = "Total Incidents") +
  labs(title = "Total Number of Incidents by Country", x = "", y = "") +
  theme_minimal() +
  theme(legend.key.size = unit(0.8, "cm"),
        legend.position = "bottom", axis.text.x = element_blank(), 
        axis.text.y = element_blank())

# Visualization 2: Geographic Map of Deaths
map_deaths <- ggplot(mapped_data, aes(x = long, y = lat, group = group, fill = Deaths)) +
  geom_polygon(color = "white") +
  scale_fill_gradient(low = "red", high = "darkred", na.value = "grey50", name = "Deaths") +
  labs(title = "Number of Deaths by Country", x = "", y = "") +
  theme_minimal() +
  theme(legend.key.size = unit(0.8, "cm"), 
        legend.position = "bottom",
        axis.text.x = element_blank(),
        axis.text.y = element_blank())

# Visualization 3: Geographic Map of Missing
map_missing <- ggplot(mapped_data, aes(x = long, y = lat, group = group, fill = Missing)) +
  geom_polygon(color = "white") +
  scale_fill_gradient(low = "yellow", high = "orange", na.value = "grey50", name = "Missing") +
  labs(title = "Number of Missing by Country", x = "", y = "") +
  theme_minimal() +
  theme(legend.key.size = unit(0.8, "cm"), 
        legend.position = "bottom",
        axis.text.x = element_blank(), 
        axis.text.y = element_blank())

# Visualization 4: Geographic Map of Survived
map_survived <- ggplot(mapped_data, aes(x = long, y = lat, group = group, fill = Survived)) +
  geom_polygon(color = "white") +
  scale_fill_gradient(low = "green", high = "darkgreen", na.value = "grey50", name = "Survived") +
  labs(title = "Number of Survived by Country", x = "", y = "") +
  theme_minimal() +
  theme(legend.key.size = unit(0.8, "cm"), 
        legend.position = "bottom", 
        axis.text.x = element_blank(),
        axis.text.y = element_blank())

# Visualization 5: Incidents Summarized by 3-Year Intervals
incidents_summary_plot <- ggplot(incidents_by_year_gender, aes(x = Year_Group, y = Incidents, fill = Status)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~Gender, scales = "fixed") +
  theme_economist_white() +
  scale_fill_brewer(palette = "Set1") +
  labs(x = "Year Group", 
       y = "Number of Incidents",
       title = "Incidents Summarized by 3-Year Intervals")

# Visualization 6: Distribution of Causes of Death
cause_of_death_plot <- ggplot(cause_of_death_counts, aes(x = "", y = Count, fill = Cause.of.Death)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  scale_fill_manual(values = colors) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_stack(vjust = 0.6), size = 4, fontface = "bold", hjust = 0.5) +
  labs(fill = "Cause of Death: ", title = "Distribution of Causes of Death") +
  theme_test() +
  theme(axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(), 
        legend.position = "bottom", 
        legend.text = element_text(size = 8))


# printing plots
print(map_total_incidents)
print(map_deaths)
print(map_missing)
print(map_survived)
print(incidents_summary_plot)
print(cause_of_death_plot)

