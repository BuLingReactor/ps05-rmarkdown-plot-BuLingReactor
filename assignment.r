dataset <- dataset <- read.delim("gapminder.csv", header = TRUE)
    
cat("There are", nrow(dataset), "rows and", ncol(dataset), "columns.\n")

length(unique(dataset$iso3))
length(unique(dataset$iso2))
length(unique(dataset$name))


compare <- function(col1, col2) {
    result <- data.frame(find = unique(dataset[, col1]),
                         count = integer(length(unique(dataset[, col1]))))
    for (i in 1:nrow(result)) {
        result[i, "count"] = 
            length(unique(subset(dataset, dataset[, col1] == result[i, "find"],
                                 select = col2))[, 1])
    }
    print(result)
    result
}

compare1 <- compare("iso2", "name")
print(unique(compare1$count))
print(unique(dataset[dataset$iso2 == compare1[compare1$count != 1, ]$find, ]$name))

compare2 <- compare("name", "iso3")
print(unique(compare2$count))
print(unique(dataset[dataset$name == compare2[compare2$count != 1, ]$find, ]$iso3))

max(dataset$time, na.rm = TRUE)
min(dataset$time, na.rm = TRUE)

library(dplyr)

missing_co2 <- dataset %>%
    group_by(time) %>%
    summarise(missing = sum(is.na(co2))) %>%
    arrange(desc(missing))

missing_co2_PC <- dataset %>%
    group_by(time) %>%
    summarise(missing = sum(is.na(co2_PC))) %>%
    arrange(desc(missing))

print(missing_co2)
print(missing_co2_PC)

library(dplyr)
library(ggplot2)

subset_data <- subset(dataset,
                      name %in% c("United States of America", "China", "India",
                                  "France", "Japan", "Indonesia"))

ggplot(subset_data, aes(x = time, y = co2, group = name, color = name)) +
    geom_line(size = 1.2) +
    labs(x = "Year", y = "CO2 Emissions (kt)") +
    scale_x_continuous(limits = c(1960, 2019))

ggplot(subset_data, aes(x = time, y = co2_PC, group = name, color = name)) +
    geom_line(size = 1.2) +
    labs(x = "Year", y = "CO2 emissions (metric tons per capita)") +
    scale_x_continuous(limits = c(1960, 2019))


library(tidyr)

# Group the data by year and region, and summarize the mean CO2 emissions per capita
avg_co2_pc_by_year_region <- dataset %>% 
    group_by(time, region) %>% 
    summarize(mean_co2_pc = mean(co2_PC, na.rm = TRUE)) 

# Reshape the data into a wide format with years as rows and continents as columns
wide_avg_co2_pc_by_year_region <- spread(avg_co2_pc_by_year_region, key = region, value = mean_co2_pc)

# Print the results
print(wide_avg_co2_pc_by_year_region, n = 100)

data_1960 <- subset(dataset, time == 1960)
data_1960 <- data_1960[data_1960$region != "", ]

ggplot(data_1960, aes(x = GDP_PC, y = lifeExpectancy)) +
    geom_point(aes(size = totalPopulation, color = region)) +
    scale_size_continuous(range = c(1, 10)) +
    labs(x = "GDP per capita", y = "Life expectancy") +
    theme_minimal() +
    geom_text(aes(label = name), size = 1.5) + 
    ggtitle("GDP per capita vs. Life expectancy by Country in 1960")

data_2019 <- subset(dataset, time == 2019)
data_2019 <- data_2019[data_2019$region != "", ]

ggplot(data_2019, aes(x = GDP_PC, y = lifeExpectancy)) +
    geom_point(aes(size = totalPopulation, color = region)) +
    scale_size_continuous(range = c(1, 10)) +
    labs(x = "GDP per capita", y = "Life expectancy") +
    theme_minimal() +
    geom_text(aes(label = name), size = 1.5) + 
    ggtitle("GDP per capita vs. Life expectancy by Country in 2019")


my_data <- subset(dataset, select = c("region", "time", "lifeExpectancy"))

avg_life_expectancy <- aggregate(lifeExpectancy ~ region + time,
                                 data = my_data, FUN = mean)

regions <- unique(avg_life_expectancy$region)
avg_life_expectancy <- 
    data.frame(region = regions, avg_life_1960 =
        avg_life_expectancy[avg_life_expectancy$time == 1960, ]$lifeExpectancy,
        avg_life_2019 = 
        avg_life_expectancy[avg_life_expectancy$time == 2019, ]$lifeExpectancy)

print(avg_life_expectancy)

average_LE_growth_raw <- dataset[dataset$region != "", ] %>%
    group_by(region, time) %>%
    summarize(avg_life_expectancy = mean(lifeExpectancy, na.rm = TRUE)) %>%
    ungroup()

average_LE_growth_raw <- average_LE_growth_raw[complete.cases(average_LE_growth_raw),]

average_LE_growth <- data.frame(time = unique(average_LE_growth_raw$time))

for (i in unique(average_LE_growth_raw$region)) {
    tmp <- average_LE_growth_raw %>%
        filter(region == i) %>%
        mutate(prev = lag(avg_life_expectancy), growth = avg_life_expectancy - prev) %>%
        select(time, growth) %>%
        rename(!!paste("growth", i) := growth)
    print(tmp)
    average_LE_growth <- merge(average_LE_growth, tmp, by = "time")
}

print(average_LE_growth)

gdp_pc_data <- dataset %>%
    select(iso2, GDP_PC, time) %>%
    group_by(iso2) %>%
    filter(time %in% c(1960, 2019), iso2 != "") %>%
    spread(key = time, value = GDP_PC) %>%
    filter(!is.na(`1960`) | !is.na(`2019`))

ggplot(melt(bucket), aes(x = `iso2`, fill = `1960`)) +
    geom_bar(stat="identity") +
    xlab("Miles per Gallon") +
    ylab("Frequency") +
    ggtitle("Histogram of Miles per Gallon") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 6))
ggplot(data = gdp_pc_data, aes(x = `iso2`, y = `2019`)) +
    geom_bar(stat="identity") +
    xlab("Miles per Gallon") +
    ylab("Frequency") +
    ggtitle("Histogram of Miles per Gallon") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 6))

       