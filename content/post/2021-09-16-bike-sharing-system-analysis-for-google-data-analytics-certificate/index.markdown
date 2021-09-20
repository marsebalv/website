---
title: Bike sharing system analysis for Google Data Analytics Certificate
author: Mariano S. Alvarez
date: '2021-09-16'
slug: bike-sharing-system-analysis-for-google-data-analytics-certificate
categories: []
tags: []
subtitle: ''
summary: ''
authors: []
lastmod: '2021-09-16T16:41:00+02:00'
featured: no
image:
  caption: ''
  focal_point: ''
  preview_only: no
projects: []
---

This is the project I worked on after completing the *Google Data Analytics Certificate* first 7 courses, as part of the final course. I chose to analyze this data set as I love biking and was curious about the patterns which may be hidden in the data of a bike-sharing system. The data can be obtained [here](https://divvy-tripdata.s3.amazonaws.com/index.html), from which I used the monthly data from August 2020 to July 2021. Let's start by setting the context for the project, its task, the ultimate goal and the motivation behind the assignment.

Cyclistic is presented as a bike-share program that features more than 5,800 bicycles and 600 docking stations in Chicago. The offered pricing plans are: single-ride passes, full-day passes and annual memberships. Customers who purchase single-ride or full-day passes are referred to as *casual riders*, and those who purchase annual memberships are *members*.

**Task:** Understand how casual riders and annual members use Cyclistic bikes differently.

**Goal:** Convert casual riders into annual members.

**Motivation:** Annual members are much more profitable than casual riders.

## Data cleaning

We first need to clean the data to make it reliable. We will use the file form July 2021 as example and proceed similarly with the other months. We load the libraries first,


```r
rm(list=ls())

#----------------------------------------------
# We start by loading the libraries to be used

# Data cleaning and managing
library("dplyr")
library("skimr") # to clean data
library("data.table")

# Data plotting
library("RColorBrewer")
library("ggplot2")
library("gridExtra")
library("grid")              # Needed to manipulate textgrobs in grid.arrange
#----------------------------------------------
```

### July 2021

We start by exploring the first of the 12 files (one for each month) that we will be analyzing, to detect problems or inconsistencies. As it is the first data file we are opening, we can check the summary of attributes.


```r
path = './'
file = '202107-divvy-tripdata.csv'

data = read.csv(paste0(path,file),stringsAsFactors = FALSE)

# Summary of attributes
str(data)
```

```
## 'data.frame':	822410 obs. of  13 variables:
##  $ ride_id           : chr  "0A1B623926EF4E16" "B2D5583A5A5E76EE" "6F264597DDBF427A" "379B58EAB20E8AA5" ...
##  $ rideable_type     : chr  "docked_bike" "classic_bike" "classic_bike" "classic_bike" ...
##  $ started_at        : chr  "2021-07-02 14:44:36" "2021-07-07 16:57:42" "2021-07-25 11:30:55" "2021-07-08 22:08:30" ...
##  $ ended_at          : chr  "2021-07-02 15:19:58" "2021-07-07 17:16:09" "2021-07-25 11:48:45" "2021-07-08 22:23:32" ...
##  $ start_station_name: chr  "Michigan Ave & Washington St" "California Ave & Cortez St" "Wabash Ave & 16th St" "California Ave & Cortez St" ...
##  $ start_station_id  : chr  "13001" "17660" "SL-012" "17660" ...
##  $ end_station_name  : chr  "Halsted St & North Branch St" "Wood St & Hubbard St" "Rush St & Hubbard St" "Carpenter St & Huron St" ...
##  $ end_station_id    : chr  "KA1504000117" "13432" "KA1503000044" "13196" ...
##  $ start_lat         : num  41.9 41.9 41.9 41.9 41.9 ...
##  $ start_lng         : num  -87.6 -87.7 -87.6 -87.7 -87.7 ...
##  $ end_lat           : num  41.9 41.9 41.9 41.9 41.9 ...
##  $ end_lng           : num  -87.6 -87.7 -87.6 -87.7 -87.7 ...
##  $ member_casual     : chr  "casual" "casual" "member" "member" ...
```

Now we have a list of the attributes and their type, so we can proceed to check and clean the data. We have character and numeric variables.  

* *ride_id* is the identification of the ride, we will not be using it as we do not have any other data source to match with users.
* *rideable_type* shows one of three options: classic bike, docked bike or electric bike. This should be checked.
* *started_at* and *ended_at* are character variables which include date and time of start and end of the ride respectively. We should change the format to a date-time format.
* *start_station_name*, *start_station_id*, *end_station_name*, *end_station_id* are character variables to identify the stations between which the ride was made.
* *start_lat*, *start_lng*, *end_lat*, *end_lng* are numeric variables showing the latitude and longitude of the locations between which the ride was made.
* *member_casual* is a character variable showing if the ride was made by a casual rider (single-day or full-day pass) or a member (annual membership)

To be able to operate with the time of start and end of the variable, we change the format to **POSIXct**, which is more efficient when working with data frames.


```r
data$started_at = as.POSIXct(data$started_at,format="%Y-%m-%d %H:%M:%S",tz="America/Chicago")
data$ended_at = as.POSIXct(data$ended_at,format="%Y-%m-%d %H:%M:%S",tz="America/Chicago")
```

And we can create a variable with the duration of the ride in minutes


```r
data$ride_duration = as.numeric(difftime(data$ended_at,data$started_at, units = "mins"))
```

We now check the data before starting to clean. We can use the `skimr` package


```r
skim_without_charts(data)
```


Table: Table 1: Data summary

|                         |       |
|:------------------------|:------|
|Name                     |data   |
|Number of rows           |822410 |
|Number of columns        |14     |
|_______________________  |       |
|Column type frequency:   |       |
|character                |7      |
|numeric                  |5      |
|POSIXct                  |2      |
|________________________ |       |
|Group variables          |None   |


**Variable type: character**

|skim_variable      | n_missing| complete_rate| min| max| empty| n_unique| whitespace|
|:------------------|---------:|-------------:|---:|---:|-----:|--------:|----------:|
|ride_id            |         0|             1|  16|  16|     0|   822410|          0|
|rideable_type      |         0|             1|  11|  13|     0|        3|          0|
|start_station_name |         0|             1|   0|  53| 87263|      718|          0|
|start_station_id   |         0|             1|   0|  36| 87262|      711|          0|
|end_station_name   |         0|             1|   0|  53| 93158|      715|          0|
|end_station_id     |         0|             1|   0|  36| 93158|      708|          0|
|member_casual      |         0|             1|   6|   6|     0|        2|          0|


**Variable type: numeric**

|skim_variable | n_missing| complete_rate|   mean|     sd|     p0|    p25|    p50|    p75|     p100|
|:-------------|---------:|-------------:|------:|------:|------:|------:|------:|------:|--------:|
|start_lat     |         0|             1|  41.90|   0.04|  41.65|  41.88|  41.90|  41.93|    42.07|
|start_lng     |         0|             1| -87.65|   0.03| -87.84| -87.66| -87.64| -87.63|   -87.52|
|end_lat       |       731|             1|  41.90|   0.04|  41.63|  41.88|  41.90|  41.93|    42.15|
|end_lng       |       731|             1| -87.65|   0.03| -87.85| -87.66| -87.64| -87.63|   -87.49|
|ride_duration |         0|             1|  24.21| 214.19|  -0.20|   7.58|  13.35|  23.73| 49107.15|


**Variable type: POSIXct**

|skim_variable | n_missing| complete_rate|min                 |max                 |median              | n_unique|
|:-------------|---------:|-------------:|:-------------------|:-------------------|:-------------------|--------:|
|started_at    |         0|             1|2021-07-01 00:00:22 |2021-07-31 23:59:58 |2021-07-17 13:58:37 |   659640|
|ended_at      |         0|             1|2021-07-01 00:04:51 |2021-08-12 17:45:41 |2021-07-17 14:28:04 |   658663|

1. Character variables: the variables *rideable_type* and *member_casual* have 3 and 2 unique values as expected. Even though the complete rate is 1, there can be missing data as these are characters (that is, a " " value), which is shown in the empty column for the station names/ids.
2. Numeric variables: there are 731 values missing in the end coordinates of the rides.
3. POSIXct variables: there is no missing data.

#### Begin cleaning

In the summary we found that there are no missing *start_lat* and *start_lng* but there are some empty *start_station_name*. The names of the stations could be completed if there is only one pair of *(start_lat;start_lng)* for each station. This is not the case. The observations with empty *start_station_name* have *start_lat* and *start_lng* truncated with two decimal places, for example


```r
head(data[(data$start_station_name == ""),c("start_station_name","start_lat","start_lng")])
```

```
##      start_station_name start_lat start_lng
## 1181                        41.96    -87.69
## 1615                        41.89    -87.63
## 1617                        41.91    -87.64
## 1618                        41.92    -87.67
## 1621                        41.90    -87.69
## 2339                        41.89    -87.61
```

While the observations with complete *start_station_name* have *start_lat* and *start_lng* truncated with more decimal places, for example


```r
head(data[(data$start_station_name != ""),c("start_station_name","start_lat","start_lng")])
```

```
##             start_station_name start_lat start_lng
## 1 Michigan Ave & Washington St  41.88398 -87.62468
## 2   California Ave & Cortez St  41.90036 -87.69670
## 3         Wabash Ave & 16th St  41.86038 -87.62581
## 4   California Ave & Cortez St  41.90036 -87.69670
## 5   California Ave & Cortez St  41.90035 -87.69668
## 6   California Ave & Cortez St  41.90033 -87.69674
```

Therefore, if we seek to assign a station name to a *(start_lat;start_lng)*, the assignment might not be unique. We can verify this using the information we have. We can truncate coordinates to two decimal places and then verify if there is a unique station assigned to each pair of coordinates.


```r
# Subset the attributes
stations = data[(data$start_station_name != ""),c("start_station_name","start_lat","start_lng")]
# Truncate coordinates to two decimal places
stations$start_lat = trunc(stations$start_lat*100)/100
stations$start_lng = trunc(stations$start_lng*100)/100
# Order by latitude and longitude
stations = stations[with(stations, order(start_lat,start_lng)),]
```

Using the *stations* data frame, we first remove repeated observations (a match in all three attributes) and then use dplyr to keep only those rows with repeated pair of coordinates.


```r
# Remove repeated entries (considering complete row)
repeated_stations = unique(stations)
# Use dplyr to find repeated pairs of coordinates
repeated_stations <- repeated_stations %>%
  group_by(start_lat,start_lng) %>%
  filter(n()>1) %>%
  data.frame()
# Show first results
head(repeated_stations, 10)
```

```
##                                  start_station_name start_lat start_lng
## 1                           Michigan Ave & 114th St     41.68    -87.62
## 2                       S Michigan Ave & E 118th St     41.68    -87.62
## 3                          Vincennes Ave & 104th St     41.70    -87.65
## 4                        S Aberdeen St & W 106th St     41.70    -87.65
## 5                            Indiana Ave & 103rd St     41.70    -87.61
## 6                             Vernon Ave & 107th St     41.70    -87.61
## 7                          Chicago State University     41.71    -87.60
## 8                           Maryland Ave & 104th St     41.71    -87.60
## 9                           Greenwood Ave & 97th St     41.71    -87.59
## 10 Woodlawn & 103rd - Olive Harvey Vaccination Site     41.71    -87.59
```

This shows that using two decimal places, it is not possible to fill the missing station names data using the coordinates, as there is more than one possibility at least for some coordinates. Therefore, we can keep the observations with missing station name data if it is acceptable and will not be used in the analysis or remove it. I choose to remove it due to lack of feedback for this project. Let's take a look at the percentage of the observations has missing data on start_station_name or end_station_name


```r
# Make a copy of the data frame to clean
data_clean = data
# Which percentage of the observations has missing data on start_station_name or end_station_name?
no_station_name = 100*sum(((data_clean$start_station_name == "")| (data_clean$end_station_name == "")))/length(data_clean$start_station_name)
print(paste0("The amount of observations with missing data in station name is of ",
             as.character(sum(((data_clean$start_station_name == "")| (data_clean$end_station_name == "")))),
             " over a total of ",as.character(length(data_clean$start_station_name)),
             ", which is a ",as.character(round(no_station_name,digits=2)),"%"))
```

```
## [1] "The amount of observations with missing data in station name is of 130089 over a total of 822410, which is a 15.82%"
```

We remove those cells


```r
data_clean = data_clean[!((data_clean$start_station_name == "")| (data_clean$end_station_name == "")),]
```

And now we can compare the amount of user types and bike types before and after the cleaning. We use the `data.table` package


```r
# Before cleaning
data = as.data.table(data)
total_obs = length(data$ride_id)

users_relative_dt = data[, relative := length(ride_id)/total_obs, by= .(member_casual)]
users_relative_dt = unique(users_relative_dt[,.(member_casual,relative)])
users_relative_dt = users_relative_dt[order(member_casual),]
users_relative_dt$data_status = "all"

# After cleaning
data_clean = as.data.table(data_clean)
total_obs_clean = length(data_clean$ride_id)

users_relative_dt_clean = data_clean[, relative := length(ride_id)/total_obs, by= .(member_casual)]
users_relative_dt_clean = unique(users_relative_dt_clean[,.(member_casual,relative)])
users_relative_dt_clean = users_relative_dt_clean[order(member_casual),]
users_relative_dt_clean$data_status = "cleaned"

# Bind in row dimension
df_barplot_user = rbind(users_relative_dt,users_relative_dt_clean)
```

And plot


```r
ggplot(df_barplot_user, aes(fill=member_casual, y=relative, x=data_status)) + 
  theme_classic()+
  geom_bar(position="dodge", stat="identity")+
  scale_fill_brewer(palette="Set2")+
  scale_y_continuous(limits=c(0,0.85),expand = c(0,0))+
  labs(y = "Proportion", x = "Data status")+
  ggtitle("Proportion of user types")+
  theme(axis.text = element_text(size=16),axis.title = element_text(size=16),title = element_text(size=16))+
  theme(legend.title = element_blank(), legend.text = element_text(size=16))
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-13-1.png" width="672" />

We see that the proportion of user types remains similar after removing rows with missing data. Now that the data is cleaned, we can do a final check using again `skimr`


```r
skim_without_charts(data_clean)
```


Table: Table 2: Data summary

|                         |           |
|:------------------------|:----------|
|Name                     |data_clean |
|Number of rows           |692321     |
|Number of columns        |15         |
|Key                      |NULL       |
|_______________________  |           |
|Column type frequency:   |           |
|character                |7          |
|numeric                  |6          |
|POSIXct                  |2          |
|________________________ |           |
|Group variables          |None       |


**Variable type: character**

|skim_variable      | n_missing| complete_rate| min| max| empty| n_unique| whitespace|
|:------------------|---------:|-------------:|---:|---:|-----:|--------:|----------:|
|ride_id            |         0|             1|  16|  16|     0|   692321|          0|
|rideable_type      |         0|             1|  11|  13|     0|        3|          0|
|start_station_name |         0|             1|  10|  53|     0|      711|          0|
|start_station_id   |         0|             1|   3|  36|     0|      704|          0|
|end_station_name   |         0|             1|  10|  53|     0|      709|          0|
|end_station_id     |         0|             1|   3|  36|     0|      702|          0|
|member_casual      |         0|             1|   6|   6|     0|        2|          0|


**Variable type: numeric**

|skim_variable | n_missing| complete_rate|   mean|     sd|     p0|    p25|    p50|    p75|     p100|
|:-------------|---------:|-------------:|------:|------:|------:|------:|------:|------:|--------:|
|start_lat     |         0|             1|  41.90|   0.04|  41.65|  41.88|  41.90|  41.93|    42.06|
|start_lng     |         0|             1| -87.64|   0.02| -87.78| -87.65| -87.64| -87.63|   -87.53|
|end_lat       |         0|             1|  41.90|   0.04|  41.65|  41.88|  41.90|  41.93|    42.07|
|end_lng       |         0|             1| -87.64|   0.02| -87.78| -87.66| -87.64| -87.63|   -87.53|
|ride_duration |         0|             1|  24.19| 230.05|  -0.20|   7.72|  13.45|  23.98| 49107.15|
|relative      |         0|             1|   0.42|   0.03|   0.39|   0.39|   0.45|   0.45|     0.45|


**Variable type: POSIXct**

|skim_variable | n_missing| complete_rate|min                 |max                 |median              | n_unique|
|:-------------|---------:|-------------:|:-------------------|:-------------------|:-------------------|--------:|
|started_at    |         0|             1|2021-07-01 00:00:30 |2021-07-31 23:59:57 |2021-07-17 15:20:47 |   571788|
|ended_at      |         0|             1|2021-07-01 00:07:05 |2021-08-12 17:45:41 |2021-07-17 15:51:09 |   570291|

In the table we can see that there is no missing or empty data now, and data is reliable. What does stand out is the 100-percentile for the ride duration (this is in minutes), but the most extreme long rides will be separated afterwards. We finish by saving the data.


```r
data_clean_0721 = data_clean

rm("data_clean","data")

# Uncomment to save
# saveRDS(data_clean_0721,file="data_clean_0721.RDS")
```

We repeat this analysis for each of the 12 months to find inconsitencies with the data. In 2021 no additional issues have been found in the data. However, for 2020:

* Some observations had negative values for ride duration, which is not possible and were removed.
* In some months, the attributes *start_station_id* and *end_station_id* are numeric instead of character, as for the previous months. This was changed for consistency.
* There are missing values on the start_station_id and end_station_id, and those are associated mostly to the station “W Oakdale Ave & N Broadway”, when the rest of the attributes are complete. This does not seem to be a problem for the analysis, so no action was taken.
* The attribute *rideable_type* has two categories in November 2020 and in the previous months (the category classic_bike does not exist).

After carefully processing each month's data, we can work with the clean data and start the analysis. We load the cleaned data file. Remember Data correspond to the period **August 2020 to July 2021**.


```r
rm(list=ls())

#----------------------------------------------
# We start by loading the libraries to be used

# Data cleaning and managing
library("dplyr")
library("skimr") # to clean data
library("tidyverse")
library("lubridate")
library("data.table")

# Data plotting
library("RColorBrewer")
library("ggplot2")
library("gridExtra")
library("grid")              # Needed to manipulate textgrobs in grid.arrange
#----------------------------------------------

# Let's give the final touches to the data
path = './'
file = 'cyclistic_data_clean_0820_to_0721.RDS'
data = read_rds(paste0(path,file))
```

We create four new attributes of day of the week, month and year and hour of start of the ride.


```r
data$day_of_week = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", 
                  "Friday", "Saturday")[as.POSIXlt(data$started_at)$wday + 1]

data$month = month(data$started_at)
data$year = year(data$started_at)
data$hour_start = hour(data$started_at)

data = as.data.table(data)
```

Let's start by removing the very long duration data. We can save it on a different data frame in case specific analysis is necessary. We can choose 2 hours (120 min) as a limit. This is the 1.48% of the data


```r
percent_long_rides = sum(data$ride_duration>=120)/length(data$ride_duration)*100
print(percent_long_rides)
```

```
## [1] 1.478131
```

```r
# We can separate the long rides if they are requested to be included in the analysis.
# data_long_rides = data[data$ride_duration>=120,]
```

For this analysis, we remove the observations with negative ride length (minutes) and keep only those rides of with length lower than 2 hours. Only 1.48% of the observations show rides with larger length and a very minor amount shows rides of length of several hours (up to a day), so these are removed not to skew the analysis.


```r
data = data[data$ride_duration<120 & data$ride_duration>0,]
```

We first look at the number of rides for each month according to the user type (*member* are those users with an annual membership and *casual* those with single or full-day passes).


```r
total_rides_user = data[, total := length(ride_id), by= .(month,year,member_casual)]
total_rides_user = unique(total_rides_user[,.(month,year,member_casual,total)])
total_rides_user = total_rides_user[, date := as.Date(as.character(ISOdate(year,month,1)))]

total_rides_user_last = total_rides_user[total_rides_user$date == max(total_rides_user$date),]
total_rides_user_last = total_rides_user_last[order(total),]

# Adjust plot size in output
options(repr.plot.width = 10, repr.plot.height = 6)

# Plot
ggplot(total_rides_user, aes(x=date, y=total, color=member_casual)) + 
  theme_classic()+                             # sets white background and left/bottom axis lines
  geom_line(size=1.0)+                         # plot lines
  geom_point(aes(shape = member_casual), size = 3)+                                # add points
  scale_color_brewer(palette="Set2")+          # set colors
  scale_x_date(limits = c(min(total_rides_user$date),max(total_rides_user$date)),   # adjust x axis scale limits
               expand = c(0,3),                                                     # reduce extra space
               breaks="1 month",                                                    # set breaks for x axis
               date_labels='%m-%y')+                                                # set format for x labels
  theme(axis.text.x = element_text(angle = 45, vjust =1, hjust=1))+                 # sets orientation of x labels
  scale_y_continuous(limits=c(0,max(total_rides_user$total+15000)),                 # adjust y axis scale limits
                     expand = c(0,0),                                               # reduce extra space
                     labels = scales::comma,                                        # set format for y labels
                     sec.axis = dup_axis(                                           # adds name for each line
                       breaks = total_rides_user_last$total,
                       labels = total_rides_user_last$member_casual,
                       name = NULL
                     ))+
  labs(title = 'Total rides',                                                       # change titles/captions
       subtitle = 'Per month, separated by user type',
       caption = 'Data from https://divvy-tripdata.s3.amazonaws.com/index.html',
       y = 'Number of rides',
       x = 'Month'
       )+
  theme(axis.line.y.right = element_blank(),    # remove secondary axis line
        axis.ticks.y.right = element_blank(),   # remove secondary axis ticks
        axis.text = element_text(size=14),      # adjust size of fonts
        axis.title = element_text(size=14),
        title = element_text(size=12),
        )+
  guides(color = 'none', shape = 'none')                        # remove unnecessary legend
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-20-1.png" width="672" />

There is a larger number of rides by members than by casual riders, for each month except July of 2021. The number of total rides decreases considerably in winter months, when there is the largest proportion of members using the system.

We also analyzed the amount of rides by day of the week according to user type.


```r
# Total rides by weekday and user type

data$day_of_week = factor(data$day_of_week, levels= c("Sunday", "Monday","Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

total_rides_user_weekday = data[, total := length(ride_id), by= .(day_of_week,member_casual)]
total_rides_user_weekday = unique(total_rides_user_weekday[,.(day_of_week,member_casual,total)])
total_rides_user_weekday = total_rides_user_weekday[order(total_rides_user_weekday$day_of_week),]
total_rides_user_weekday_last = total_rides_user_weekday[day_of_week == "Saturday",] 

# Adjust plot size in output
options(repr.plot.width = 10, repr.plot.height = 6)

# Plot
ggplot(total_rides_user_weekday, aes(x=as.numeric(day_of_week), y=total, color=member_casual)) + 
  theme_classic()+                             # sets white background and left/bottom axis lines
  geom_line(size=1.0)+                         # plot lines
  geom_point(aes(shape = member_casual), size = 3)+                                # add points
  scale_color_brewer(palette="Set2")+          # set colors
  theme(axis.text.x = element_text(angle = 45, vjust =1, hjust=1))+                 # sets orientation of x labels
  scale_x_continuous(limits = c(1,7.1),
                     expand = c(0,0),
                     breaks = seq(1,7),
                     labels = as.character(unique(total_rides_user_weekday$day_of_week)))+
  scale_y_continuous(limits = c(0,max(total_rides_user_weekday$total+20000)),                 # adjust y axis scale limits
                     expand = c(0,0),                                               # reduce extra space
                     labels = scales::comma,                                        # set format for y labels
                     sec.axis = dup_axis(                                           # adds name for each line
                       breaks = total_rides_user_weekday_last$total,
                       labels = total_rides_user_weekday_last$member_casual,
                       name = NULL
                     ))+
  labs(title = 'Total rides',                                                       # change titles/captions
       subtitle = 'Per day of week, separated by user type',
       caption = 'Data from https://divvy-tripdata.s3.amazonaws.com/index.html',
       y = 'Number of rides',
       x = 'Day of week'
  )+
  theme(axis.line.y.right = element_blank(),    # remove secondary axis line
        axis.ticks.y.right = element_blank(),   # remove secondary axis ticks
        axis.text = element_text(size=14),      # adjust size of fonts
        axis.title = element_text(size=14),
        title = element_text(size=12),
  )+
guides(color = 'none', shape = 'none')                        # remove unnecessary legend
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-21-1.png" width="672" />


The amount of rides by members show a more steady behavior along the week, with fewer rides on Sundays and Mondays. Casual riders, on the other hand, almost double in weekends and are more steady in the week days.

For the ride duration according to the day of the week and user type, we found the following.


```r
data$day_of_week = factor(data$day_of_week, levels= c("Sunday", "Monday","Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

mean_duration_user_weekday = data[, ave_duration := mean(ride_duration), by= .(day_of_week,member_casual)]
mean_duration_user_weekday = unique(mean_duration_user_weekday[,.(day_of_week,member_casual,ave_duration)])
mean_duration_user_weekday = mean_duration_user_weekday[order(mean_duration_user_weekday$day_of_week),]
mean_duration_user_weekday_last = mean_duration_user_weekday[day_of_week == "Saturday",] 

# Adjust plot size in output
options(repr.plot.width = 10, repr.plot.height = 6)

# Plot
ggplot(mean_duration_user_weekday, aes(x=as.numeric(day_of_week), y=ave_duration, color=member_casual)) + 
  theme_classic()+                             # sets white background and left/bottom axis lines
  geom_line(size=1.0)+                         # plot lines
  geom_point(aes(shape = member_casual), size = 3)+                                # add points
  scale_color_brewer(palette="Set2")+          # set colors
  theme(axis.text.x = element_text(angle = 45, vjust =1, hjust=1))+                 # sets orientation of x labels
  scale_x_continuous(limits = c(1,7.1),
                     expand = c(0,0),
                     breaks = seq(1,7),
                     labels = as.character(unique(mean_duration_user_weekday$day_of_week)))+
  scale_y_continuous(limits = c(0,max(mean_duration_user_weekday$ave_duration*1.05)),                 # adjust y axis scale limits
                     expand = c(0,0),                                               # reduce extra space
                     breaks = seq(0,30,5),
                     labels = scales::comma,                                        # set format for y labels
                     sec.axis = dup_axis(                                           # adds name for each line
                       breaks = mean_duration_user_weekday_last$ave_duration,
                       labels = mean_duration_user_weekday_last$member_casual,
                       name = NULL
                     ))+
  labs(title = 'Average length of ride',                                                       # change titles/captions
       subtitle = 'Per day of week, separated by user type',
       caption = 'Data from https://divvy-tripdata.s3.amazonaws.com/index.html',
       y = 'Length (minutes)',
       x = 'Day of week'
  )+
  theme(axis.line.y.right = element_blank(),    # remove secondary axis line
        axis.ticks.y.right = element_blank(),   # remove secondary axis ticks
        axis.text = element_text(size=14),      # adjust size of fonts
        axis.title = element_text(size=14),
        title = element_text(size=12),
  )+
guides(color = 'none', shape = 'none')                        # remove unnecessary legend
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-22-1.png" width="672" />

We see that members, on average, make shorter rides than casual riders (around 15 minutes vs. around 25 minutes), for every day of the week.

If we now look at the distribution of ride lengths according to user type:


```r
# Adjust plot size in output
options(repr.plot.width = 10, repr.plot.height = 6)

# Plot
ggplot() +
  theme_bw()+   
  geom_histogram(data[data$member_casual == "member",],
                 mapping = aes(x = ride_duration, y = ..density..),fill= "#d95f02") +
  geom_label( aes(x=60, y=0.025, label="member"), fill="#d95f02",color="white",fontface="bold") +
  geom_histogram(data[data$member_casual == "casual",],
                 mapping = aes(x = ride_duration, y = -..density..), fill= "#1b9e77")+
  geom_label( aes(x=60, y=-0.025, label="casual"), fill="#1b9e77",color="white",fontface="bold")+
  scale_y_continuous(limits=c(-0.06,0.06),                 # adjust y axis scale limits
                     expand = c(0,0),
                     breaks = c(-0.04,-0.02,0,0.02,0.04))+
  labs(title = 'Length of ride frequency',                                                       # change titles/captions
       subtitle = 'Separated by user type',
       caption = 'Data from https://divvy-tripdata.s3.amazonaws.com/index.html',
       y = 'Density',
       x = 'Ride duration (minutes)'
  )+
  geom_hline(yintercept=0,size=0.25)+
  theme(axis.line.y.right = element_blank(),    # remove secondary axis line
        axis.ticks.y.right = element_blank(),   # remove secondary axis ticks
        axis.text = element_text(size=14),      # adjust size of fonts
        axis.title = element_text(size=14),
        title = element_text(size=12),
  )+
  coord_flip(expand=FALSE)+scale_x_reverse()
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-23-1.png" width="672" />

we find that shorter rides are more frequent for members than for casual riders, and that casual riders are the ones who still make trips longer than 60 minutes.

We can also study the hourly pattern of the users for weekdays and weekends. Let's start on the weekdays analysis.


```r
# Adjust plot size in output
options(repr.plot.width = 10, repr.plot.height = 6)

# Plot
ggplot() +
  theme_bw()+   
  geom_histogram(data[data$member_casual == "member" & (day_of_week != "Saturday" & day_of_week != "Sunday"),],
                 mapping = aes(x = hour_start, y = ..density..),fill= "#d95f02",
                 bins = 24) +
  geom_label( aes(x=3, y=0.075, label="member"), fill="#d95f02",color="white",fontface="bold") +
  geom_histogram(data[data$member_casual == "casual" & (day_of_week != "Saturday" & day_of_week != "Sunday"),],
                 mapping = aes(x = hour_start, y = -..density..), fill= "#1b9e77",
                 bins = 24)+
  geom_label( aes(x=3, y=-0.075, label="casual"), fill="#1b9e77",color="white",fontface="bold")+
  scale_y_continuous(limits=c(-0.13,0.13),                 # adjust y axis scale limits
                     expand = c(0,0),
                     breaks = seq(-0.1,0.1,0.05))+
  labs(title = 'Start hour frequency',                                                       # change titles/captions
       subtitle = 'Separated by user type, weekdays',
       caption = 'Data from https://divvy-tripdata.s3.amazonaws.com/index.html',
       y = 'Density',
       x = 'Hour of start'
  )+
  geom_hline(yintercept=0,size=0.25)+
  theme(axis.line.y.right = element_blank(),    # remove secondary axis line
        axis.ticks.y.right = element_blank(),   # remove secondary axis ticks
        axis.text = element_text(size=14),      # adjust size of fonts
        axis.title = element_text(size=14),
        title = element_text(size=12),
  )+
  coord_flip(expand=FALSE)+scale_x_reverse()
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-24-1.png" width="672" />

We see that members tend to use more the bikes between 6 and 8 a.m. than casual riders, the formers probably using the bike to get to work. During the afternoon, between 4 and 6 p.m., both user types show higher frequency of use

And for the weekends


```r
# Adjust plot size in output
options(repr.plot.width = 10, repr.plot.height = 6)

# Plot
ggplot() +
  theme_bw()+   
  geom_histogram(data[data$member_casual == "member" & (day_of_week == "Saturday" | day_of_week == "Sunday"),],
                 mapping = aes(x = hour_start, y = ..density..),fill= "#d95f02",
                 bins = 24) +
  geom_label( aes(x=3, y=0.075, label="member"), fill="#d95f02",color="white",fontface="bold") +
  geom_histogram(data[data$member_casual == "casual" & (day_of_week == "Saturday" | day_of_week == "Sunday"),],
                 mapping = aes(x = hour_start, y = -..density..), fill= "#1b9e77",
                 bins = 24)+
  geom_label( aes(x=3, y=-0.075, label="casual"), fill="#1b9e77",color="white",fontface="bold")+
  scale_y_continuous(limits=c(-0.13,0.13),                 # adjust y axis scale limits
                     expand = c(0,0),
                     breaks = seq(-0.1,0.1,0.05))+
  labs(title = 'Start hour frequency',                                                       # change titles/captions
       subtitle = 'Separated by user type, weekends',
       caption = 'Data from https://divvy-tripdata.s3.amazonaws.com/index.html',
       y = 'Density',
       x = 'Hour of start'
  )+
  geom_hline(yintercept=0,size=0.25)+
  theme(axis.line.y.right = element_blank(),    # remove secondary axis line
        axis.ticks.y.right = element_blank(),   # remove secondary axis ticks
        axis.text = element_text(size=14),      # adjust size of fonts
        axis.title = element_text(size=14),
        title = element_text(size=12),
  )+
  coord_flip(expand=FALSE)+scale_x_reverse()
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-25-1.png" width="672" />

For the weekends, however, the hourly pattern of use of the system is quite similar.

Let's find out if the most popular stations to start and end the ride are the same for both user types. The 10 most popular for members are:


```r
# Adjust plot size in output
options(repr.plot.width = 15, repr.plot.height = 10)

#Members

input_start = data[member_casual == "member",]$start_station_name
input_end = data[member_casual == "member",]$end_station_name

# Start
popular_start = as.data.table(table(input_start))
# First, we reorder as factors
popular_start = mutate(popular_start, input_start = fct_reorder(input_start, N))
# Now we order the data table to subsample for the plot
popular_start = popular_start[order(N,decreasing = TRUE),]

g1 <- ggplot(popular_start[1:10], aes(x=input_start, y=N)) +
  geom_segment( aes(x=input_start, xend=input_start, y=0, yend=N), color="#4d9221") +
  geom_point( color="#a1d76a", size=4) +
  theme_light() +
  coord_flip() +
  theme(axis.title.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_text(size=14),      # adjust size of fonts
        axis.title = element_text(size=14),
        title = element_text(size=14))+
  labs(title = 'Most popular stations to start',                       # change titles/captions
       subtitle = 'Members',
       caption = 'Data from https://divvy-tripdata.s3.amazonaws.com/index.html',
       y = 'Number of rides',
  )

# End
popular_end = as.data.table(table(input_end))
# First, we reorder as factors
popular_end = mutate(popular_end, input_end = fct_reorder(input_end, N))
# Now we order the data table to subsample for the plot
popular_end = popular_end[order(N,decreasing = TRUE),]

g2 <- ggplot(popular_end[1:10], aes(x=input_end, y=N)) +
  geom_segment( aes(x=input_end, xend=input_end, y=0, yend=N), color="#c51b7d") +
  geom_point( color="#e9a3c9", size=4) +
  theme_light() +
  coord_flip() +
  theme(axis.title.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_text(size=14),      # adjust size of fonts
        axis.title = element_text(size=14),
        title = element_text(size=14))+
  labs(title = 'Most popular stations to end',                       # change titles/captions
       subtitle = 'Members',
       caption = 'Data from https://divvy-tripdata.s3.amazonaws.com/index.html',
       y = 'Number of rides',
  )

grid.arrange(g1,g2,nrow=2)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-26-1.png" width="672" />

And for casual riders:


```r
# Adjust plot size in output
options(repr.plot.width = 15, repr.plot.height = 10)

# Casual

input_start = data[member_casual == "casual",]$start_station_name
input_end = data[member_casual == "casual",]$end_station_name

# Start
popular_start = as.data.table(table(input_start))
# First, we reorder as factors
popular_start = mutate(popular_start, input_start = fct_reorder(input_start, N))
# Now we order the data table to subsample for the plot
popular_start = popular_start[order(N,decreasing = TRUE),]

g3 <- ggplot(popular_start[1:10], aes(x=input_start, y=N)) +
  geom_segment( aes(x=input_start, xend=input_start, y=0, yend=N), color="#4d9221") +
  geom_point( color="#a1d76a", size=4) +
  theme_light() +
  coord_flip() +
  theme(axis.title.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_text(size=14),      # adjust size of fonts
        axis.title = element_text(size=14),
        title = element_text(size=14))+
  labs(title = 'Most popular stations to start',                       # change titles/captions
       subtitle = 'Casual',
       caption = 'Data from https://divvy-tripdata.s3.amazonaws.com/index.html',
       y = 'Number of rides',
  )

# End
popular_end = as.data.table(table(input_end))
# First, we reorder as factors
popular_end = mutate(popular_end, input_end = fct_reorder(input_end, N))
# Now we order the data table to subsample for the plot
popular_end = popular_end[order(N,decreasing = TRUE),]

g4 <- ggplot(popular_end[1:10], aes(x=input_end, y=N)) +
  geom_segment( aes(x=input_end, xend=input_end, y=0, yend=N), color="#c51b7d") +
  geom_point( color="#e9a3c9", size=4) +
  theme_light() +
  coord_flip() +
  theme(axis.title.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_text(size=14),      # adjust size of fonts
        axis.title = element_text(size=14),
        title = element_text(size=14))+
  labs(title = 'Most popular stations to end',                       # change titles/captions
       subtitle = 'Casual',
       caption = 'Data from https://divvy-tripdata.s3.amazonaws.com/index.html',
       y = 'Number of rides',
  )

grid.arrange(g3,g4,nrow=2)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-27-1.png" width="672" />

The top 3 most popular stations to start the ride for members are: Clark St & Elm St, Wells St & Concord Ln and Kingsbury St & Kinzie St. For casual members those are different, with Streeter Dr & Grand Ave the most popular by far, and then Lake Shore Dr & Monroe and Millennium Park.

We can end with some summary data for each type of user:

## Members


```r
skim_without_charts(data[data$member_casual == "member",.(ride_duration)])
```


Table: Table 3: Data summary

|                         |                             |
|:------------------------|:----------------------------|
|Name                     |data[data$member_casual =... |
|Number of rows           |2329771                      |
|Number of columns        |1                            |
|Key                      |NULL                         |
|_______________________  |                             |
|Column type frequency:   |                             |
|numeric                  |1                            |
|________________________ |                             |
|Group variables          |None                         |


**Variable type: numeric**

|skim_variable | n_missing| complete_rate| mean|    sd|   p0|  p25|   p50|   p75|   p100|
|:-------------|---------:|-------------:|----:|-----:|----:|----:|-----:|-----:|------:|
|ride_duration |         0|             1| 13.8| 11.27| 0.02| 6.13| 10.53| 18.07| 119.98|

## Casual riders


```r
skim_without_charts(data[data$member_casual == "casual",.(ride_duration)])
```


Table: Table 4: Data summary

|                         |                             |
|:------------------------|:----------------------------|
|Name                     |data[data$member_casual =... |
|Number of rows           |1768627                      |
|Number of columns        |1                            |
|Key                      |NULL                         |
|_______________________  |                             |
|Column type frequency:   |                             |
|numeric                  |1                            |
|________________________ |                             |
|Group variables          |None                         |


**Variable type: numeric**

|skim_variable | n_missing| complete_rate|  mean|    sd|   p0|   p25|   p50|  p75|   p100|
|:-------------|---------:|-------------:|-----:|-----:|----:|-----:|-----:|----:|------:|
|ride_duration |         0|             1| 25.47| 22.58| 0.02| 10.22| 17.95| 32.1| 119.98|

# Conclusions and recommendations

* **Casual riders reduce considerably in winter** -> The best time to start a marketing campaign to convert casual riders may be spring.
* **Casual riders are more active on weekends** -> Greater chance to encounter casual riders on weekends if a personal campaign is launched.
* **Casual riders might not be using a bike to go to work** -> Room to understand the cause and inform alternatives (e.g. electric bikes to commute)
* **Casual riders use Streeter Dr & Grand Ave the most** -> Greater chance to encounter casual riders there for personal campaigns.
