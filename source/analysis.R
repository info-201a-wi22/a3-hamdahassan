library(areaplot)
library(stringr)
library(dplyr)
library(ggplot2)
library("plotly")
library("dplyr")
library("tidyr")
library("stringr")
library("leaflet") 
library("sf")
library(maps) 

incarceration_trends <- read.csv(
  "https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv",
  stringsAsFactors = FALSE
)


# This is the data I chose to create and use for my five values. It excludes
# a lot of columns that were unnecessary to me and it leaves out N/A values.
inc_data <- incarceration_trends %>%
  group_by(year) %>%
  select(state, fips, total_pop_15to64, female_pop_15to64, male_pop_15to64, black_pop_15to64, region,
         county = county_name, total_prison_pop, female_prison_pop, male_prison_pop, 
         black_prison_pop, white_prison_pop) %>%
  drop_na()

# This will later be used for the charts.
sum_inc_data <- inc_data %>%
  group_by(year) %>%
  summarise(total_pop = sum(total_pop_15to64), female_total = sum(female_pop_15to64),
            male_total = sum(male_pop_15to64), black_total = sum(black_pop_15to64), 
            total_prison = sum(total_prison_pop), female_prison = sum(female_prison_pop),
            male_prison = sum(male_prison_pop), black_prison = sum(black_prison_pop), 
            white_prison = sum(white_prison_pop)) 



# This code here created a summary of the average of Black prison population from 
# each year, I used this code to compute the highest average 
# and the lowest average and the years each occurred.
summary_all_years <- inc_data %>%
  select(year, black_prison_pop) %>%
  summarise(mean_each_year = mean(black_prison_pop))


# These are the highest and lowest averages from all years starting from 1990
# ending in 2016
highest_mean <- max(summary_all_years$mean_each_year) 
highest_mean
mean_1990 <- summary_all_years$mean_each_year[1]
lowest_mean <- min(summary_all_years$mean_each_year)
lowest_mean


# I used this code to figure out the average Black prison Population
# from all counties on the most recent year which is the year 2016.
summary_table2016 <- inc_data %>%
  select(year, black_prison_pop) %>%
  filter(year == "2016") %>%
  summarise(average_value = mean(black_prison_pop))

mean_pop_2016 <- summary_table2016$average_value

# This code produces the change between the  averages of staring year and the end
# year which are 1990 and 2016. 
change_in_years <- inc_data %>%
  select(year, black_prison_pop) %>%
  filter(year == "1990") %>%
  summarise(start_year = mean(black_prison_pop)) 

change <- summary_table2016$average_value - change_in_years$start_year
  

# Trends over time Chart: Black population in the system from 1990-2016
# chart that shows the changes in the population over the years, male, female, 
# and Black population

z <- plot_ly(data = sum_inc_data, 
        x = ~year, 
        y = ~black_total,
        color = ~year,
        type = "scatter") %>%
  layout(title = "Black Population In The System (1990-2016):",
         xaxis = list(title = "Years"),
         yaxis = list(title = "Total Black Population")
    
  )
z
# Variable Comparison Chart
# This chart will compare the regions and see which region had the highest Black 
# prisoners and which had the lowest.

#ggplot(data = inc_data) +
 # geom_point(mapping = aes(x = black_prison, y = white_prison))
x <- ggplot(data = inc_data) +
  geom_col(mapping = aes(x = region, y = black_prison_pop, fill = region), position = "dodge") +
  ggtitle("Black Prison Pop. From Each Region:") +
  labs(y= "Black Prison Pop", x = "Region")




# MAP:
blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),        
    axis.text = element_blank(),        
    axis.ticks = element_blank(),       
    axis.title = element_blank(),       
    plot.background = element_blank(),  
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.border = element_blank()      
  )

# Map of California state with regions, showing which regions had more 
# Black prisoners.
map_table <- incarceration_trends %>% 
  select(state, year, county_name, fips, Population = black_prison_pop)


ca_map <- map_data("county") %>% 
  unite(polyname, region, subregion, sep = ",")

join <- left_join(ca_map, county.fips)

joining_ca <- left_join(map_table, join) %>% 
  filter(state == "CA" & year == "2010")


final_ca <- ggplot(data= joining_ca) +
  geom_polygon(mapping = aes(x= long, y=lat, group = group, fill = Population)) + 
  blank_theme + 
  coord_map() +
  scale_fill_continuous(limits = c(0, max(joining_ca$Population)), high = "blue", low = "pink", 
                        na.value = "green") +
  ggtitle("Black Prison Population In California From 2010:")
