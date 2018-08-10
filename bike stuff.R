library(readxl)
library(lubridate)

input_file <- "data/Hawthorne Tilikum Steel daily bike counts 073118.xlsx"
bridge_name <- "Hawthorne"

# define a funtion that load bike counts data
load_data <- function(input_file, bridge_name) {
  bikecounts <- read_excel(input_file,
                           sheet = bridge_name,
                           skip = 1)
  bikecounts$name <- bridge_name
  bikecounts
}

Tilikum <- load_data(input_file, "Tilikum")
Hawthorne <- load_data(input_file, "Hawthorne")

# use the column names of Tilikum for Hawthorne
names(Hawthorne) <- names(Tilikum)

Steel <- load_data(input_file, "Steel")
names(Steel) <- c("date", "lower", "westbound", "eastbound", "total", "name")

# combine all three data frame for all three bridges
bikecounts <- bind_rows(Hawthorne, 
                        Tilikum, 
                        Steel %>% select(-lower)) # exclude the `lower` col in Steel data frame

# average daily bike counts by bridge
bikecounts %>% 
  group_by(name) %>% 
  summarize(avg_daily_counts=mean(total, na.rm=TRUE))

# average monthly bike counts by bridge
bikecounts %>% 
  # first create ym column as a unique month identifier
  group_by(name, ym=floor_date(date, "month")) %>%
  summarize(total_monthly_counts=sum(total), counts=n()) %>% 
  # then average by month over years for each bridge
  group_by(name, month(ym)) %>% 
  summarize(avg_monthly_counts=mean(total_monthly_counts))

library(tidyverse)
library(tidyr)

# bike is new object created. bikecounts was datasource.
bike <- bikecounts %>%
  gather(westbound, eastbound, key = "direction", value = "counts")

library(lubridate)

#need to refer to object to save and view the result
bike <- bike %>% 
  mutate(dow=wday(date, label=TRUE))


library(gapminder)
library(tidyverse)

bike <- bike
  filter(bike, counts<100)

  bike <- bike
  filter(bike, name =="Hawthorne")
  
bike <- bike
filter(bike, name=="Hawthorne" , counts<100)

bike %>%
  filter(name == "Hawthorne") %>%
  select(direction, counts)

newbike <- bike %>%
  mutate(weirdnumber = total * counts)

bikecounts
  filter(is.na(westbound) | is.na(eastbound)) 
  
# download data from a link with dplyr
  dat <- read_csv("https://raw.githubusercontent.com/cities/datascience2017/master/data/NHTS2009_dd.csv")
  
somedata <- dat %>%
  group_by(HOUSEID)


avgtrip <- somedata %>%
  group_by(HOUSEID) %>%
  summarize(avgtrip = mean(TRPMILES))
  
HHDATA <- somedata %>%
  filter(HHSIZE > 3) %>%
  select(TRVLMIN, TRPMILES, HOUSEID, HH_RACE, HHFAMINC, TRPTRANS) %>%
  group_by(HOUSEID) %>%
  print(n = Inf)
 
#Recode race 
HHDATArace <- HHDATA %>% 
  mutate(hh_race_str=recode (HH_RACE, 
                                 "01"="White",
                                 "02"="African American, Black",
                                 "03"="Asian Only",
                                 "04"="American Indian, Alaskan Native",
                                 "05"="Native Hawaiian, other Pacific",
                                 "06"="Multiracial",
                                 "07"="Hispanic/Mexican",
                                 "97"="Other specify",
                             .default = as.character(NA)))

 #Recode trip type
HHTriptype <- HHDATArace %>% mutate(driving=ifelse(TRPTRANS %in% c("01", "02", "03", "04", "05", "06", "07"), 1, 0),
              driving=ifelse(TRPTRANS %in% c("-1", "-7", "-8", "-9"), NA, driving) # retain missing values as NA
)
  

ggplot(data = bike) + 
  geom_point(mapping = aes(x = date, y = counts))  
 
ggplot(data = bike) + 
  geom_point(mapping = aes(x = date, y = counts, color = name)) 
  
ggplot(data = bike) + 
  geom_point(mapping = aes(x = date, y = counts, size = name))
  
ggplot(data = bike) + 
  geom_point(mapping = aes(x = date, y = counts), color = "blue")
  

ggplot(data = bike) + 
  geom_point(mapping = aes(x = date, y = counts), color = "red")

ggplot(data = bike) + 
  geom_point(mapping = aes(x = date, y = counts))

ggplot(data = bike) + 
  geom_smooth(mapping = aes(x = date, y = counts, linetype = name))


ggplot(data = bike) + 
  stat_summary(
    mapping = aes(x = name, y = counts),
    fun.ymin = min,
    fun.ymax = max,
    fun.y = median
  )

ggplot(data = bike) + 
  geom_bar(mapping = aes(x = name, fill = dow))
ggplot(data = bike) + 
  geom_bar(mapping = aes(x = name, fill = dow))

ggplot(data = bike) + 
  geom_bar(mapping = aes(x = name, fill = dow), position = "fill")

ggplot(data = bike) + 
  geom_bar(mapping = aes(x = name, fill = dow), position = "dodge")

ggplot(data = bike) + 
  geom_point(mapping = aes(x = dow, y = counts), position = "jitter")


ggplot(data = bike, mapping = aes(x = dow, y = counts)) + 
  geom_boxplot() +
  coord_flip()

timebike <- bike %>% 
  mutate(theyear=year(date))

ggplot(data = timebike, mapping = aes(x = theyear, y = counts)) + 
  geom_boxplot() +
  coord_flip()


ggplot(data = timebike) + 
  geom_point(mapping = aes(x = theyear, y = counts, color = name)) 

(trip_nested <- HHTriptype %>% 
    group_by(HOUSEID) %>% 
    nest())

le_vs_yr <- function(df) {
  lm(lifeExp ~ I(year - 1950), data = df)
}

(trip_nested <- HHTriptype %>% 
    group_by(HH_RACE) %>% 
    nest())





