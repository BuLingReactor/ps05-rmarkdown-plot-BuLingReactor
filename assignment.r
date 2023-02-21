dataset <- read.delim("gapminder.csv", header = TRUE)
    
cat("There are", nrow(dataset), "rows and", ncol(dataset), "columns.\n")
head(dataset)

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

cat("Year max: ", max(dataset$time, na.rm = TRUE))
cat("Year min: ", min(dataset$time, na.rm = TRUE))

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

avg_co2_pc_by_year_region <- dataset %>% 
    group_by(time, region) %>% 
    filter(name != "") %>% 
    summarize(mean_co2_pc = mean(co2_PC, na.rm = TRUE)) 
wide_avg_co2_pc_by_year_region <- 
    spread(avg_co2_pc_by_year_region, key = region, value = mean_co2_pc) %>%
    select(-V1)
print(wide_avg_co2_pc_by_year_region)

# wide_avg_co2_pc_by_year_region <- wide_avg_co2_pc_by_year_region %>%
#    filter(time %in% c(1960, 2016))
# wide_avg_co2_pc_by_year_region <-
#     as.data.frame(t(wide_avg_co2_pc_by_year_region))[-1, ]
# colnames(wide_avg_co2_pc_by_year_region) = c("1960", "2016")
# print(wide_avg_co2_pc_by_year_region)

# require(reshape2)
# wide_avg_co2_pc_by_year_region <- melt(wide_avg_co2_pc_by_year_region, id = "time")

avg_co2_pc_by_year_region <- avg_co2_pc_by_year_region %>%
    filter(time %in% c(1960, 2016)) %>%
    ungroup()
# avg_co2_pc_by_year_region <- melt(avg_co2_pc_by_year_region, id = "time")
avg_co2_pc_by_year_region$time <- as.character(avg_co2_pc_by_year_region$time)

ggplot(avg_co2_pc_by_year_region,
    aes(x = region, y = mean_co2_pc, fill = time)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(x = "Continent", y = "Average CO2 emissions per capita")

emitters <- dataset %>%
    group_by(region) %>%
    filter(time == 2016 & region != "") %>%
    filter(!is.na(co2_PC)) %>%
    select("name", "co2_PC")
    
largest_emitters <- emitters %>% arrange(desc(co2_PC)) %>% slice(1:3)
print(largest_emitters)
smallest_emitters <- emitters %>% arrange(co2_PC) %>% slice(1:3)
print(smallest_emitters)

# PART 4

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
        rename(!!paste("growth", i)  := growth)
#    print(tmp)
    average_LE_growth <- merge(average_LE_growth, tmp, by = "time")
}

print(average_LE_growth)

gdp_pc_data <- dataset %>%
    select(name, GDP_PC, time) %>%
    filter(time %in% c(1960, 2019), name != "") %>%
    group_by(name) %>%
    filter(!any(is.na(`GDP_PC`)), 1960 %in% time & 2019 %in% time) %>%
    ungroup()

 #   select(iso2, GDP_PC, time) %>%
 #   group_by(iso2)#%>%
 #    %>%
 #   spread(key = time, value = GDP_PC) %>%
 #   filter(!is.na(`1960`) | !is.na(`2019`))

gdp_pc_data$time = as.character(gdp_pc_data$time)
gdp_pc_data[gdp_pc_data$time == 2019, ]$GDP_PC = gdp_pc_data[gdp_pc_data$time == 2019, ]$GDP_PC -
    gdp_pc_data[gdp_pc_data$time == 1960, ]$GDP_PC

ggplot(data = gdp_pc_data, aes(x = `name`, y = `GDP_PC`, fill = `time`)) +
    geom_bar(stat="identity") +
    geom_col(position = position_stack(reverse = TRUE)) + 
    xlab("Countries by name") +
    ylab("GDP per captia") +
    ggtitle("Histogram of GDP per capita by countries, 1960 and 2019") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 5))

rank_1960 <- dataset %>% 
    filter(time == 1960, name != "", lifeExpectancy != "") %>% 
    mutate(rank = rank(-lifeExpectancy)) %>%
    select("iso2", rank)

cat("The rank of LE of USA in the world in 1960 is", 
    rank_1960[rank_1960$iso2 == "US", ]$rank, "\n")

rank_2019 <- dataset %>% 
    filter(time == 2019, iso3 != "", lifeExpectancy != "") %>% 
    mutate(rank = rank(-lifeExpectancy)) %>%
    select("iso2", rank)

cat("The rank of LE of USA in the world in 2019 is", 
    rank_2019[rank_2019$iso2 == "US", ]$rank, "\n")

cat("The relative rank of LE of USA in the world in 1960 is", 
    rank_1960[rank_1960$iso2 == "US", ]$rank / nrow(rank_1960), "\n")

cat("The relative rank of LE of USA in the world in 2019 is", 
    rank_2019[rank_2019$iso2 == "US", ]$rank / nrow(rank_2019), "\n")
       