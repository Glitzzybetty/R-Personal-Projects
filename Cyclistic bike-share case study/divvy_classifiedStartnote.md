# Cyclistic\_Exercise\_Full\_Year\_Analysis

This analysis is for case study 1 from the Google Data Analytics
Certificate (Cyclistic). It’s originally based on the case study
“‘Sophisticated, Clear, and Polished’: Divvy and Data Visualization”
written by Kevin Hartman (found here:
<https://artscience.blog/home/divvy-dataviz-case-study>). We will be
using the Divvy dataset for the case study. The purpose of this script
is to consolidate downloaded Divvy data into a single dataframe and then
conduct simple analysis to help answer the key question: “In what ways
do members and casual riders use Divvy bikes differently?”

    knitr::opts_chunk$set(echo = TRUE)
    library(tidyverse)  #helps wrangle data
    library(lubridate)  #helps wrangle date attributes
    library(ggplot2)  #helps visualize data
    library(dplyr)
    #getwd() #displays your working directory
    #setwd("C:/Users/Oikudayisi/Desktop/My textbook and Projects/R assignment/Client 2 Assignments/divvy dataset project") #sets your working directory to simplify calls to data ... make sure to use your OWN username instead of mine ;)

\#===================== \# STEP 1: COLLECT DATA \#=====================
\# Upload Divvy datasets (csv files) here

    q1_2019 <- read_csv("Divvy_Trips_2019_Q1.csv")
    q2_2019 <- read_csv("Divvy_Trips_2019_Q2.csv")
    q3_2019 <- read_csv("Divvy_Trips_2019_Q3.csv")
    q4_2019 <- read_csv("Divvy_Trips_2019_Q4.csv")
    q1_2020 <- read_csv("Divvy_Trips_2020_Q1.csv")

\#==================================================== \# STEP 2:
WRANGLE DATA AND COMBINE INTO A SINGLE FILE
\#==================================================== \# Compare column
names each of the files \# While the names don’t have to be in the same
order, they DO need to match perfectly before we can use a command to
join them into one file

    colnames(q3_2019)

    ##  [1] "trip_id"           "start_time"       
    ##  [3] "end_time"          "bikeid"           
    ##  [5] "tripduration"      "from_station_id"  
    ##  [7] "from_station_name" "to_station_id"    
    ##  [9] "to_station_name"   "usertype"         
    ## [11] "gender"            "birthyear"

    colnames(q4_2019)

    ##  [1] "trip_id"           "start_time"       
    ##  [3] "end_time"          "bikeid"           
    ##  [5] "tripduration"      "from_station_id"  
    ##  [7] "from_station_name" "to_station_id"    
    ##  [9] "to_station_name"   "usertype"         
    ## [11] "gender"            "birthyear"

    colnames(q2_2019)

    ##  [1] "01 - Rental Details Rental ID"                   
    ##  [2] "01 - Rental Details Local Start Time"            
    ##  [3] "01 - Rental Details Local End Time"              
    ##  [4] "01 - Rental Details Bike ID"                     
    ##  [5] "01 - Rental Details Duration In Seconds Uncapped"
    ##  [6] "03 - Rental Start Station ID"                    
    ##  [7] "03 - Rental Start Station Name"                  
    ##  [8] "02 - Rental End Station ID"                      
    ##  [9] "02 - Rental End Station Name"                    
    ## [10] "User Type"                                       
    ## [11] "Member Gender"                                   
    ## [12] "05 - Member Details Member Birthday Year"

    colnames(q1_2020)

    ##  [1] "ride_id"            "rideable_type"     
    ##  [3] "started_at"         "ended_at"          
    ##  [5] "start_station_name" "start_station_id"  
    ##  [7] "end_station_name"   "end_station_id"    
    ##  [9] "start_lat"          "start_lng"         
    ## [11] "end_lat"            "end_lng"           
    ## [13] "member_casual"

## Rename columns to make them consisent with q1\_2020 (as this will be the supposed going-forward table design for Divvy)

    #run once or error shows up
    q4_2019 <- q4_2019 %>% rename(
                       ride_id = trip_id
                       ,rideable_type = bikeid 
                       ,started_at = start_time  
                       ,ended_at = end_time  
                       ,start_station_name = from_station_name 
                       ,start_station_id = from_station_id 
                       ,end_station_name = to_station_name 
                       ,end_station_id = to_station_id 
                       ,member_casual = usertype)

    q3_2019 <- q3_2019 %>%
                       rename(ride_id = trip_id
                       ,rideable_type = bikeid 
                       ,started_at = start_time  
                       ,ended_at = end_time  
                       ,start_station_name = from_station_name 
                       ,start_station_id = from_station_id 
                       ,end_station_name = to_station_name 
                       ,end_station_id = to_station_id 
                       ,member_casual = usertype)

    q2_2019 <- q2_2019 %>%
                       rename(ride_id = "01 - Rental Details Rental ID"
                       ,rideable_type = "01 - Rental Details Bike ID" 
                       ,started_at = "01 - Rental Details Local Start Time"  
                       ,ended_at = "01 - Rental Details Local End Time"  
                       ,start_station_name = "03 - Rental Start Station Name" 
                       ,start_station_id = "03 - Rental Start Station ID"
                       ,end_station_name = "02 - Rental End Station Name" 
                       ,end_station_id = "02 - Rental End Station ID"
                       ,member_casual = "User Type")

    q1_2019 <- q1_2019 %>%
                       rename(ride_id = trip_id
                       ,rideable_type = bikeid 
                       ,started_at = start_time  
                       ,ended_at = end_time  
                       ,start_station_name = from_station_name 
                       ,start_station_id = from_station_id 
                       ,end_station_name = to_station_name 
                       ,end_station_id = to_station_id 
                       ,member_casual = usertype)

## Inspect the dataframes and look for inconguencies

## Convert ride\_id and rideable\_type to character so that they can stack correctly

    q4_2019 <-  mutate(q4_2019, ride_id = as.character(ride_id)
                       ,rideable_type = as.character(rideable_type)) 
    q3_2019 <-  mutate(q3_2019, ride_id = as.character(ride_id)
                       ,rideable_type = as.character(rideable_type)) 
    q2_2019 <-  mutate(q2_2019, ride_id = as.character(ride_id)
                       ,rideable_type = as.character(rideable_type)) 

    q1_2019 <-  mutate(q1_2019, ride_id = as.character(ride_id)
                       ,rideable_type = as.character(rideable_type)) 

## Stack individual quarter’s data frames into one big data frame

    all_trips <- bind_rows(q1_2019, q2_2019, q3_2019, q4_2019, q1_2020)

# Remove lat, long, birthyear, and gender fields as this data was dropped beginning in 2020

    all_trips <- all_trips %>%  
      select(-c(start_lat, start_lng, end_lat, end_lng, birthyear, gender, "01 - Rental Details Duration In Seconds Uncapped", "05 - Member Details Member Birthday Year", "Member Gender", "tripduration"))

\#====================================================== \# STEP 3:
CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
\#====================================================== \# Inspect the
new table that has been created

    colnames(all_trips)  #List of column names

    ## [1] "ride_id"            "started_at"        
    ## [3] "ended_at"           "rideable_type"     
    ## [5] "start_station_id"   "start_station_name"
    ## [7] "end_station_id"     "end_station_name"  
    ## [9] "member_casual"

    nrow(all_trips)  #How many rows are in data frame?

    ## [1] 4244891

    dim(all_trips)  #Dimensions of the data frame?

    ## [1] 4244891       9

    head(all_trips)  #See the first 6 rows of data frame.  Also tail(qs_raw)

    ## # A tibble: 6 x 9
    ##   ride_id  started_at          ended_at           
    ##   <chr>    <dttm>              <dttm>             
    ## 1 21742443 2019-01-01 00:04:37 2019-01-01 00:11:07
    ## 2 21742444 2019-01-01 00:08:13 2019-01-01 00:15:34
    ## 3 21742445 2019-01-01 00:13:23 2019-01-01 00:27:12
    ## 4 21742446 2019-01-01 00:13:45 2019-01-01 00:43:28
    ## 5 21742447 2019-01-01 00:14:52 2019-01-01 00:20:56
    ## 6 21742448 2019-01-01 00:15:33 2019-01-01 00:19:09
    ## # ... with 6 more variables: rideable_type <chr>,
    ## #   start_station_id <dbl>, start_station_name <chr>,
    ## #   end_station_id <dbl>, end_station_name <chr>,
    ## #   member_casual <chr>

    str(all_trips)  #See list of columns and data types (numeric, character, etc)

    ## tibble [4,244,891 x 9] (S3: tbl_df/tbl/data.frame)
    ##  $ ride_id           : chr [1:4244891] "21742443" "21742444" "21742445" "21742446" ...
    ##  $ started_at        : POSIXct[1:4244891], format: "2019-01-01 00:04:37" ...
    ##  $ ended_at          : POSIXct[1:4244891], format: "2019-01-01 00:11:07" ...
    ##  $ rideable_type     : chr [1:4244891] "2167" "4386" "1524" "252" ...
    ##  $ start_station_id  : num [1:4244891] 199 44 15 123 173 98 98 211 150 268 ...
    ##  $ start_station_name: chr [1:4244891] "Wabash Ave & Grand Ave" "State St & Randolph St" "Racine Ave & 18th St" "California Ave & Milwaukee Ave" ...
    ##  $ end_station_id    : num [1:4244891] 84 624 644 176 35 49 49 142 148 141 ...
    ##  $ end_station_name  : chr [1:4244891] "Milwaukee Ave & Grand Ave" "Dearborn St & Van Buren St (*)" "Western Ave & Fillmore St (*)" "Clark St & Elm St" ...
    ##  $ member_casual     : chr [1:4244891] "Subscriber" "Subscriber" "Subscriber" "Subscriber" ...

    summary(all_trips)  #Statistical summary of data. Mainly for numerics

    ##    ride_id            started_at                 
    ##  Length:4244891     Min.   :2019-01-01 00:04:37  
    ##  Class :character   1st Qu.:2019-06-05 11:55:20  
    ##  Mode  :character   Median :2019-08-05 11:56:21  
    ##                     Mean   :2019-08-09 22:35:19  
    ##                     3rd Qu.:2019-10-06 08:15:10  
    ##                     Max.   :2020-03-31 23:51:34  
    ##                                                  
    ##     ended_at                   rideable_type     
    ##  Min.   :2019-01-01 00:11:07   Length:4244891    
    ##  1st Qu.:2019-06-05 12:16:37   Class :character  
    ##  Median :2019-08-05 12:28:06   Mode  :character  
    ##  Mean   :2019-08-09 22:59:17                     
    ##  3rd Qu.:2019-10-06 08:48:37                     
    ##  Max.   :2020-05-19 20:10:34                     
    ##                                                  
    ##  start_station_id start_station_name end_station_id 
    ##  Min.   :  1.0    Length:4244891     Min.   :  1.0  
    ##  1st Qu.: 77.0    Class :character   1st Qu.: 77.0  
    ##  Median :174.0    Mode  :character   Median :174.0  
    ##  Mean   :202.5                       Mean   :203.3  
    ##  3rd Qu.:289.0                       3rd Qu.:291.0  
    ##  Max.   :675.0                       Max.   :675.0  
    ##                                      NA's   :1      
    ##  end_station_name   member_casual     
    ##  Length:4244891     Length:4244891    
    ##  Class :character   Class :character  
    ##  Mode  :character   Mode  :character  
    ##                                       
    ##                                       
    ##                                       
    ## 

## There are a few problems we will need to fix:

## (1) In the “member\_casual” column, there are two names for members (“member” and “Subscriber”) and two names for casual riders (“Customer” and “casual”). We will need to consolidate that from four to two labels.

## (2) The data can only be aggregated at the ride-level, which is too granular. We will want to add some additional columns of data – such as day, month, year – that provide additional opportunities to aggregate the data.

## (3) We will want to add a calculated field for length of ride since the 2020Q1 data did not have the “tripduration” column. We will add “ride\_length” to the entire dataframe for consistency.

## (4) There are some rides where tripduration shows up as negative, including several hundred rides where Divvy took bikes out of circulation for Quality Control reasons. We will want to delete these rides.

## In the “member\_casual” column, replace “Subscriber” with “member” and “Customer” with “casual”

## Before 2020, Divvy used different labels for these two types of riders … we will want to make our dataframe consistent with their current nomenclature

## N.B.: “Level” is a special property of a column that is retained even if a subset does not contain any values from a specific level

## Begin by seeing how many observations fall under each usertype

    table(all_trips$member_casual)

    ## 
    ##     casual   Customer     member Subscriber 
    ##      48480     880637     378407    2937367

## Reassign to the desired values (we will go with the current 2020 labels)

    all_trips <-  all_trips %>% 
      mutate(member_casual = recode(member_casual
                                    ,"Subscriber" = "member"
                                    ,"Customer" = "casual"))

    # Check to make sure the proper number of observations were reassigned
    table(all_trips$member_casual)

    ## 
    ##  casual  member 
    ##  929117 3315774

## Add columns that list the date, month, day, and year of each ride

## This will allow us to aggregate ride data for each month, day, or year … before completing these operations we could only aggregate at the ride level

## <https://www.statmethods.net/input/dates.html> more on date formats in R found at that link

    all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
    all_trips$month <- format(as.Date(all_trips$date), "%m")
    all_trips$day <- format(as.Date(all_trips$date), "%d")
    all_trips$year <- format(as.Date(all_trips$date), "%Y")
    all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

## Add a “ride\_length” calculation to all\_trips (in seconds)

## <https://stat.ethz.ch/R-manual/R-devel/library/base/html/difftime.html>

    all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

## Inspect the structure of the columns to confirm made changes

    str(all_trips)

    ## tibble [4,244,891 x 15] (S3: tbl_df/tbl/data.frame)
    ##  $ ride_id           : chr [1:4244891] "21742443" "21742444" "21742445" "21742446" ...
    ##  $ started_at        : POSIXct[1:4244891], format: "2019-01-01 00:04:37" ...
    ##  $ ended_at          : POSIXct[1:4244891], format: "2019-01-01 00:11:07" ...
    ##  $ rideable_type     : chr [1:4244891] "2167" "4386" "1524" "252" ...
    ##  $ start_station_id  : num [1:4244891] 199 44 15 123 173 98 98 211 150 268 ...
    ##  $ start_station_name: chr [1:4244891] "Wabash Ave & Grand Ave" "State St & Randolph St" "Racine Ave & 18th St" "California Ave & Milwaukee Ave" ...
    ##  $ end_station_id    : num [1:4244891] 84 624 644 176 35 49 49 142 148 141 ...
    ##  $ end_station_name  : chr [1:4244891] "Milwaukee Ave & Grand Ave" "Dearborn St & Van Buren St (*)" "Western Ave & Fillmore St (*)" "Clark St & Elm St" ...
    ##  $ member_casual     : chr [1:4244891] "member" "member" "member" "member" ...
    ##  $ date              : Date[1:4244891], format: "2019-01-01" ...
    ##  $ month             : chr [1:4244891] "01" "01" "01" "01" ...
    ##  $ day               : chr [1:4244891] "01" "01" "01" "01" ...
    ##  $ year              : chr [1:4244891] "2019" "2019" "2019" "2019" ...
    ##  $ day_of_week       : chr [1:4244891] "Tuesday" "Tuesday" "Tuesday" "Tuesday" ...
    ##  $ ride_length       : 'difftime' num [1:4244891] 390 441 829 1783 ...
    ##   ..- attr(*, "units")= chr "secs"

## Convert “ride\_length” from Factor to numeric so we can run calculations on the data

    is.factor(all_trips$ride_length)

    ## [1] FALSE

    all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
    is.numeric(all_trips$ride_length)

    ## [1] TRUE

## Remove “bad” data

## The dataframe includes a few hundred entries when bikes were taken out of docks and checked for quality by Divvy or ride\_length was negative

## We will create a new version of the dataframe (v2) since data is being removed

## <https://www.datasciencemadesimple.com/delete-or-drop-rows-in-r-with-conditions-2/>

    all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]

\##===================================== \## STEP 4: CONDUCT DESCRIPTIVE
ANALYSIS \##===================================== \## Descriptive
analysis on ride\_length (all figures in seconds)

    mean(all_trips_v2$ride_length) #straight average (total ride length / rides)

    ## [1] 1439.315

    median(all_trips_v2$ride_length) #midpoint number in the ascending array of ride lengths

    ## [1] 691

    max(all_trips_v2$ride_length) #longest ride

    ## [1] 10632022

    min(all_trips_v2$ride_length) #shortest ride

    ## [1] 1

## You can condense the four lines above to one line using summary() on the specific attribute

    summary(all_trips_v2$ride_length)

    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ##        1      402      691     1439     1250 10632022

## Compare members and casual users

    aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)

    ##   all_trips_v2$member_casual all_trips_v2$ride_length
    ## 1                     casual                3556.8495
    ## 2                     member                 848.3654

    aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)

    ##   all_trips_v2$member_casual all_trips_v2$ride_length
    ## 1                     casual                     1542
    ## 2                     member                      579

    aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)

    ##   all_trips_v2$member_casual all_trips_v2$ride_length
    ## 1                     casual                 10632022
    ## 2                     member                  9056634

    aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

    ##   all_trips_v2$member_casual all_trips_v2$ride_length
    ## 1                     casual                        2
    ## 2                     member                        1

## See the average ride time by each day for members vs casual users

    aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

    ##    all_trips_v2$member_casual all_trips_v2$day_of_week
    ## 1                      casual                   Friday
    ## 2                      member                   Friday
    ## 3                      casual                   Monday
    ## 4                      member                   Monday
    ## 5                      casual                 Saturday
    ## 6                      member                 Saturday
    ## 7                      casual                   Sunday
    ## 8                      member                   Sunday
    ## 9                      casual                 Thursday
    ## 10                     member                 Thursday
    ## 11                     casual                  Tuesday
    ## 12                     member                  Tuesday
    ## 13                     casual                Wednesday
    ## 14                     member                Wednesday
    ##    all_trips_v2$ride_length
    ## 1                 3768.9980
    ## 2                  825.5616
    ## 3                 3359.6077
    ## 4                  845.8799
    ## 5                 3339.9183
    ## 6                  973.5914
    ## 7                 3559.2956
    ## 8                  927.2716
    ## 9                 3803.1191
    ## 10                 811.8656
    ## 11                3562.1547
    ## 12                 829.8321
    ## 13                3702.9008
    ## 14                 813.8020

## Notice that the days of the week are out of order. Let’s fix that.

    all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

## Now, let’s run the average ride time by each day for members vs casual users

    aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

    ##    all_trips_v2$member_casual all_trips_v2$day_of_week
    ## 1                      casual                   Sunday
    ## 2                      member                   Sunday
    ## 3                      casual                   Monday
    ## 4                      member                   Monday
    ## 5                      casual                  Tuesday
    ## 6                      member                  Tuesday
    ## 7                      casual                Wednesday
    ## 8                      member                Wednesday
    ## 9                      casual                 Thursday
    ## 10                     member                 Thursday
    ## 11                     casual                   Friday
    ## 12                     member                   Friday
    ## 13                     casual                 Saturday
    ## 14                     member                 Saturday
    ##    all_trips_v2$ride_length
    ## 1                 3559.2956
    ## 2                  927.2716
    ## 3                 3359.6077
    ## 4                  845.8799
    ## 5                 3562.1547
    ## 6                  829.8321
    ## 7                 3702.9008
    ## 8                  813.8020
    ## 9                 3803.1191
    ## 10                 811.8656
    ## 11                3768.9980
    ## 12                 825.5616
    ## 13                3339.9183
    ## 14                 973.5914

## analyze ridership data by type and weekday

    all_trips_v2 %>% 
      mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
      group_by(member_casual, weekday) %>%  #groups by usertype and weekday
      summarise(number_of_rides = n()                           #calculates the number of rides and average duration 
                ,average_duration = mean(ride_length)) %>%      # calculates the average duration
      arrange(member_casual, weekday)                               # sorts

    ## `summarise()` has grouped output by 'member_casual'. You
    ## can override using the `.groups` argument.

    ## # A tibble: 14 x 4
    ## # Groups:   member_casual [2]
    ##    member_casual weekday number_of_rides average_duration
    ##    <chr>         <ord>             <int>            <dbl>
    ##  1 casual        Sun              185059            3559.
    ##  2 casual        Mon              105188            3360.
    ##  3 casual        Tue               93238            3562.
    ##  4 casual        Wed               94946            3703.
    ##  5 casual        Thu              105599            3803.
    ##  6 casual        Fri              125779            3769.
    ##  7 casual        Sat              215536            3340.
    ##  8 member        Sun              292198             927.
    ##  9 member        Mon              520703             846.
    ## 10 member        Tue              566722             830.
    ## 11 member        Wed              558254             814.
    ## 12 member        Thu              548160             812.
    ## 13 member        Fri              512462             826.
    ## 14 member        Sat              317267             974.

## Let’s visualize the number of rides by rider type

    all_trips_v2 %>% 
      mutate(weekday = wday(started_at, label = TRUE)) %>% 
      group_by(member_casual, weekday) %>% 
      summarise(number_of_rides = n()
                ,average_duration = mean(ride_length)) %>% 
      arrange(member_casual, weekday)  %>% 
      ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
      geom_col(position = "dodge")

    ## `summarise()` has grouped output by 'member_casual'. You
    ## can override using the `.groups` argument.

![](divvy_classified_files/figure-markdown_strict/unnamed-chunk-29-1.png)
\## Let’s create a visualization for average duration

    all_trips_v2 %>% 
      mutate(weekday = wday(started_at, label = TRUE)) %>% 
      group_by(member_casual, weekday) %>% 
      summarise(number_of_rides = n()
                ,average_duration = mean(ride_length)) %>% 
      arrange(member_casual, weekday)  %>% 
      ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
      geom_col(position = "dodge")

    ## `summarise()` has grouped output by 'member_casual'. You
    ## can override using the `.groups` argument.

![](divvy_classified_files/figure-markdown_strict/unnamed-chunk-30-1.png)
\##================================================= \## STEP 5: EXPORT
SUMMARY FILE FOR FURTHER ANALYSIS
\##================================================= \## Create a csv
file that we will visualize in Excel, Tableau, or my presentation
software \## N.B.: This file location is for a Mac. If you are working
on a PC, change the file location accordingly (most likely
“C:\_USERNAME...”) to export the data. You can read more here:
<https://datatofish.com/export-dataframe-to-csv-in-r/>

    counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
    write.csv(counts, file = '/avg_ride_length.csv')

\#You’re done! Congratulations!
