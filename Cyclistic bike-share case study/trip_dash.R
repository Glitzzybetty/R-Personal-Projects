---
title: "DIVVY Business Dashboard"
output: 
  flexdashboard::flex_dashboard:
    orientation: rows
    vertical_layout: fill
    theme: flatly
    highlight: zenburn
    logo: divvy.png

---
# <font size = "4">Overview </font>

<font size = "4.5">

This dashboard communicates the divvy cyclist business patronage from 2019 to 2020 Q1:

- **The Metadata Info** pane shows the data structure, before and after cleaning the data set, it shows visualization of missing values, data distribution and major statistics to look at before deciding on how to further manipulate/ analyse the data set. A clean data function was written to handle the data cleaning and other feature engineering.

- **The Weekly Explr** pane is a weekly view of activities in the business. Finding shows that:

    * Cyclist members (subscribed) are more active than Casual members. 
    * Casual members Average Trip duration is higher than subscribed member.
    * The gender participation shows that Men are active than Women but their average trip duration are similar on some days.

- **The Monthly Explr** pane is a monthly view of activities in the business. Finding shows that:

    * Cyclist members (subscribed) are more active than Casual members, but their trends looks similar on a monthly view. 
    * Also Casual members Average Trip duration is higher but with a little gap, except for January and February.
    * The gender participation shows that Men are active than Women but their average trip duration are similar on some days, Also, Women activities were missing or not on record from April to June, Questions needs to be directed to the operation team.

- **The Quarterly Explr** pane is a Quarterly view of activities in the business. Finding shows that:
        
    * Cyclist members (subscribed) are more active than Casual members, by a large trendon quarter view. 
    * But Casual members Average Trip duration is higher than all subscribed members, this confirms the weekly activity report.
    * The gender participation shows that Men are active than Women but their average trip duration are very close on some days. Also, Women activities were missing or not on record in Q2, Questions needs to be directed to the operation team.

- **Activity Forecast** pane shows the average trip duration and daily patronage forecast of business operation up to 12 months and 365 days respectively.
         
    * The average trip duration forecast from the MAE implies that the predictions of the model are off by about 199.75 trip duration per day. While the RMSE indicates that the standard deviation of the prediction errors (residuals) is about 265.38 trip duration day. 

    * The daily patronage forecast from the MAE implies that the predictions of the model are off by about 2219.75 avg daily rides per day. While the RMSE indicates that the standard deviation of the prediction errors (residuals) is about 2796.20 avg daily rides per day. 
        
    * If we are planning resources (like the number of bikes available), business might need to consider these errors to ensure they don't under- or over-estimate demand.
     
</font>

```{r setup, include=FALSE}
knitr::opts_chunk$set(warning = FALSE)
# Load necessary libraries for meta data inspection
library(flexdashboard)
library(tidyverse) #contains library(dplyr)
#library(skimr)
#library(naniar)
library(DT)
library(DataExplorer)
library(htmltools)
library(htmlwidgets)
library(prophet)
library(dygraphs)
#library(janitor)
#library(inspectdf)
```

```{r message=FALSE, warning=FALSE}
#import important libraries for data cleaning and exploration
#library(tidyverse)  #helps wrangle data
library(lubridate)  #helps wrangle date attributes
library(ggplot2)  #helps visualize data
library(plotly)
library(DescTools)
```

```{r include=FALSE}
#read the data
all_trips <- read_csv("all_trips.csv")
```

Metadata Info 
===================================================================================================

```{r skim, eval=FALSE, include=FALSE}
skim(all_trips)
```


```{r glimpse, eval=FALSE, include=FALSE}
  # Display Metadataset in an interactive table
datatable(glimpse(all_trips), options = list(pageLength = 10, autoWidth = TRUE))
```

```{r Hmisc, eval=FALSE, warning=FALSE, include=FALSE}
library(Hmisc)
describe(all_trips)
```

```{r psych describe, eval=FALSE, warning=FALSE, include=FALSE}
# Display the full dataset in an interactive table This table also gives full descriptive statistics of the Dataset
datatable(describe(all_trips), options = list(pageLength = 10, autoWidth = TRUE))
```

```{r jantor summary, eval=FALSE, warning=FALSE, include=FALSE}
# Summarize data using janitor
summary_table <- all_trips %>%
  janitor::adorn_totals("row") %>%
  janitor::adorn_totals("col") %>%
  janitor::adorn_pct_formatting(digits = 1) %>%
  janitor::adorn_ns()

print(summary_table)
```

```{r plotmissing data, eval=FALSE, include=FALSE}
# Plot missing data using DataExplorer
plot_missing(all_trips)
```

```{r inspect, eval=FALSE, include=FALSE}
# Inspect data frame using inspectdf
print(inspect_cat(all_trips))
print(inspect_num(all_trips))
print(inspect_na(all_trips))
print(inspect_imb(all_trips))

```

```{r}
#Data Cleaning function for modularity
# Single function to clean the dataset
clean_all_dtrips <- function(data) {
  # Remove incoherent data
  data <- data %>%
    select(-c("...1", tripduration, start_lat, start_lng, end_lat, end_lng,
              "01 - Rental Details Duration In Seconds Uncapped", 
              "05 - Member Details Member Birthday Year", "Member Gender"))
  
  # create new trip duration
  data <- data %>%
    mutate(tripduration = as.numeric(abs(started_at - ended_at)))
  
  # Replace NA values in birthyear with the mean
  mean_birthyear <- round(mean(data$birthyear, na.rm = TRUE))
  data <- data %>%
    mutate(birthyear = replace_na(birthyear, mean_birthyear))
  
  # Replace NA values in gender with the mode
  mode_gender <- Mode(data$gender, na.rm = TRUE)
  data <- data %>%
    mutate(gender = replace_na(gender, mode_gender))
  
  # Recode and rename member_casual column
  data <- data %>%
    mutate(member_casual = recode(member_casual,
                                  "Subscriber" = "Cyclistic members",
                                  "member" = "Cyclistic member",
                                  "Customer" = "casual")) %>%
    rename(member_category = member_casual)
  
  # Extract date components
  data <- data %>%
    mutate(date = as.Date(started_at),
           month = format(as.Date(date), "%b"),
           day = format(as.Date(date), "%d"),
           year = format(as.Date(date), "%Y"),
           day_of_week = format(as.Date(date), "%A"),
           year_quarter = quarters(date))
  
  #To add End of Month Column
  data <- data %>%
  mutate(end_of_month = ceiling_date(started_at, "month") - days(1))
  
  #To remove cases where trip duration was 0 due to technical issues
  data <- data %>%
  filter(!(start_station_name == "HQ QR" | tripduration < 0))

  #Reorder day of the week
  data <- data %>%
  mutate(day_of_week = factor(day_of_week, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"), ordered = TRUE))
  
  #Reorder day of the month
  data <- data %>%
  mutate(month = factor(month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), ordered = TRUE))
  
  
  return(data)
}

# cleaning the dataset renamed to all_dtrips
all_dtrips <- clean_all_dtrips(all_trips)
```

Row
-----------------------------------------------------------------------

### Raw Data Metadata Info

```{r echo=FALSE, warning=FALSE}
# Create metadata report using DataExplorer

# Generate the report and save it to an HTML file if it doesn't exist
if (!file.exists("data_report.html")) {
  create_report(all_trips, output_file = "data_report.html", config = configure_report(plot_str_args = list(type = "diagonal", fontSize = 15, width = 900, margin = list(left = 350, right = 250)),), report_title = "Metadata Info for Raw")
}
#width="100%" height="600px" frameborder="0" scrolling="auto" data-external="1"
```

<!-- Embed the HTML report using an iframe -->
<iframe src="data_report.html" width="100%" height="100%" frameborder="0" scrolling="auto" data-external=""></iframe>


### Cleaned Data Metadata Info

```{r echo=FALSE, warning=FALSE}
# Create metadata report using DataExplorer

# Generate the report and save it to an HTML file if it doesn't exist
if (!file.exists("cdata_report.html")) {
  create_report(all_dtrips, output_file = "cdata_report.html",config = configure_report(plot_str_args = list(type = "diagonal", fontSize = 15, width = 900, margin = list(left = 350, right = 250)),), report_title = "Metadata Info for Cleaned")
}
#tags$iframe(src = "data_report.html", width = '100%', height = '600px', frameborder = 0, scrolling = 'auto') now works becos of dygraph library
```

<!-- Embed the HTML report using an iframe -->
<iframe src="cdata_report.html" width="100%" height="100%" frameborder="0" scrolling="auto" data-external=""></iframe>


Weekly Explr
===================================================================================================

Row
-------------------------------------------------------------------------
### Avg Trip Duration
```{r}
valueBox((format(round(mean(all_dtrips$tripduration)), big.mark = ","))) 
```

### Avg Age Group of Riders
```{r}
valueBox((format(round(mean(2020 - all_dtrips$birthyear)), big.mark = ","))) 
```

### Most Popular Destination
```{r}
valueBox((format(Mode(all_dtrips$start_station_name), big.mark = ","))) 
```

### Most Popular Gender
```{r}
valueBox((format(Mode(all_dtrips$gender), big.mark = ","))) 
```

Row
-------------------------------------------------------------------------
### Membership Participation Trend (Weekly)

```{r}
## Let's visualize Membership Participation Trend (Weekly)
weeklyTrend <- all_dtrips %>% 
  group_by(member_category, day_of_week) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(tripduration)) %>% 
  arrange(member_category, day_of_week)  
  
  plot_ly(weeklyTrend, x= ~day_of_week, y=~number_of_rides, color = ~member_category) %>% layout(title = "",
                      xaxis = list(title = " "),
                      yaxis = list(title = "Number of Trips (N)")) %>%
    add_lines()
```

### Gender Participation Trend (Weekly)

```{r}
## Let's visualize Gender Participation Trend (Weekly)
genderTrend <- all_dtrips %>% 
  group_by(gender, day_of_week) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(tripduration)) %>% 
  arrange(gender, day_of_week)  
  
plot_ly(genderTrend, x= ~day_of_week, y=~number_of_rides, color = ~gender) %>% layout(title = "",
                      xaxis = list(title = " "),
                      yaxis = list(title = "Number of Trips (N)")) %>%
    add_lines()
```

Row
-------------------------------------------------------------------------

### Members Avg Trip Duration Trend (Weekly)

```{r}
## Let's visualize the Members Avg Trip Duration Trend (Weekly)
plot_ly(weeklyTrend, x= ~day_of_week, y=~average_duration, color = ~member_category) %>% layout(title = "",
                      xaxis = list(title = " "),
                      yaxis = list(title = "Average Trip Duration")) %>%
    add_lines()
```

### Gender Avg Trip Duration Trend (Weekly)
```{r}
## Let's visualize Gender Avg Trip Duration Trend (Weekly)
plot_ly(genderTrend, x= ~day_of_week, y=~average_duration, color = ~gender) %>% layout(title = "",
                      xaxis = list(title = " "),
                      yaxis = list(title = "Average Trip Duration")) %>%
    add_lines()
```


Monthly Explr
===================================================================================================

Row
-------------------------------------------------------------------------
### Total Trips
```{r}
valueBox((format(n_distinct(all_dtrips$ride_id), big.mark = ",")))
```

### Number of Bicycles
```{r}
valueBox((format(n_distinct(all_dtrips$rideable_type), big.mark = ","))) 
```

### Most Popular Weekday
```{r}
valueBox((format(Mode(all_dtrips$day_of_week), big.mark = ","))) 
```

### Most Popular Month
```{r}
valueBox((format(Mode(all_dtrips$month), big.mark = ","))) 
```

Row
-------------------------------------------------------------------------
### Members Participation Trend (Monthly)
```{r}
## Let's visualize Membership Participation Trend (Weekly)
monthTrend <- all_dtrips %>% 
  group_by(member_category, month) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(tripduration)) %>% 
  arrange(member_category, month)

plot_ly(monthTrend, x= ~month, y=~number_of_rides, type = 'scatter',  mode = 'markers', color = ~member_category, size = 100, opacity = 20.5) %>% layout(title = "",
                      xaxis = list(title = " "),
                      yaxis = list(title = "Number of Trips (N)")) #%>% add_lines()
```


### Gender Participation Trend (Month)
```{r}
## Let's visualize Gender Participation Trend (Weekly)
monthGTrend <- all_dtrips %>% 
  group_by(gender, month) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(tripduration)) %>% 
  arrange(gender, month)  
  
plot_ly(monthGTrend, x= ~month, y=~number_of_rides, type = 'scatter',  mode = 'markers', color = ~gender, size = 100, opacity = 20.5) %>% layout(title = "",
                      xaxis = list(title = " "),
                      yaxis = list(title = "Number of Trips (N)")) #%>% add_lines()
```

Row
-------------------------------------------------------------------------

### Members Avg Trip Duration Trend (Weekly)

```{r}
## Let's visualize the Members Avg Trip Duration Trend (Weekly)
plot_ly(monthTrend, x= ~month, y=~average_duration, type = 'scatter',  mode = 'markers', color = ~member_category, size = 100, opacity = 20.5) %>% layout(title = "",
                      xaxis = list(title = " "),
                      yaxis = list(title = "Average Trip Duration")) #%>%add_lines()
```

### Gender Avg Trip Duration Trend (Weekly)
```{r}
## Let's visualize Gender Avg Trip Duration Trend (Weekly)
plot_ly(monthGTrend, x= ~month, y=~average_duration, type = 'scatter',  mode = 'markers', color = ~gender, size = 100, opacity = 20.5) %>% layout(title = "",
                      xaxis = list(title = " "),
                      yaxis = list(title = "Average Trip Duration")) #%>% add_lines()
```

Quarterly Explr
===================================================================================================

Row
-------------------------------------------------------------------------
### Avg Number of Daily Rides
```{r}
date_range <- range(all_dtrips$date)
num_days <- as.numeric(difftime(date_range[2], date_range[1], units = "days")) + 1

valueBox((format(round(n_distinct(all_dtrips$ride_id)/num_days), big.mark = ","))) 

```

### Number of Stations
```{r}
valueBox((format(n_distinct(all_dtrips$end_station_name), big.mark = ","))) 
```

### Most Busy Quarter
```{r}
valueBox((format(Mode(all_dtrips$year_quarter), big.mark = ","))) 
```

### Most Active Membership Status
```{r}
valueBox((format(Mode(all_dtrips$member_category), big.mark = ","))) 
```

Row
-------------------------------------------------------------------------
### Members Participation (Quarterly)
```{r}
## Let's visualize Membership Participation Trend (Weekly)
quarterTrend <- all_dtrips %>% 
  group_by(member_category, year_quarter) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(tripduration)) %>% 
  arrange(member_category, year_quarter)

plot_ly(quarterTrend, x= ~year_quarter, y=~number_of_rides, type = 'bar',   color = ~member_category, size = 100, opacity = 20.5) %>% layout(title = "",
                      xaxis = list(title = " "),
                      yaxis = list(title = "Number of Trips (N)")) #%>% add_lines()
```


### Gender Participation (Quarterly)
```{r}
## Let's visualize Gender Participation Trend (Weekly)
quarterGTrend <- all_dtrips %>% 
  group_by(gender, year_quarter) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(tripduration)) %>% 
  arrange(gender, year_quarter)  
  
plot_ly(quarterGTrend, x= ~year_quarter, y=~number_of_rides, type = 'bar', color = ~gender, size = 100, opacity = 20.5) %>% layout(title = "",
                      xaxis = list(title = " "),
                      yaxis = list(title = "Number of Trips (N)")) #%>% add_lines()
```

Row
-------------------------------------------------------------------------

### Members Avg Trip Duration (Quarterly)

```{r}
## Let's visualize the Members Avg Trip Duration Trend (Weekly)
plot_ly(quarterTrend, x= ~year_quarter, y=~average_duration, type = 'bar', color = ~member_category, size = 100, opacity = 20.5) %>% layout(title = "",
                      xaxis = list(title = " "),
                      yaxis = list(title = "Average Trip Duration")) #%>%add_lines()
```

### Gender Avg Trip Duration (Quarterly)
```{r}
## Let's visualize Gender Avg Trip Duration Trend (Weekly)
plot_ly(quarterGTrend, x= ~year_quarter, y=~average_duration, type = 'bar', color = ~gender, size = 100, opacity = 20.5) %>% layout(title = "",
                      xaxis = list(title = " "),
                      yaxis = list(title = "Average Trip Duration")) #%>% add_lines()
```

Activity Forecast
===================================================================================================

Row
-------------------------------------------------------------------------

### Trip Duration Forecast
```{r}

df2 <- all_dtrips %>% group_by(end_of_month) %>% summarise(y = mean(tripduration)) %>% rename(ds = end_of_month)
  
# Fit the model
model <- prophet(df2)

# Create a dataframe with future dates
future <- make_future_dataframe(model, periods = 12, freq = "month")  # Predicting for the next 12 months

# Make predictions
forecast <- predict(model, future)

# Calculate performance metrics on the training data
actuals <- df2$y
predictions <- forecast$yhat[1:nrow(df2)]

mae <- mean(abs(actuals - predictions))
rmse <- sqrt(mean((actuals - predictions)^2))

# Create the dygraphs plot
dyplot <- dyplot.prophet(model, forecast)

# Create an HTML widget with custom annotations
annotate_text <- sprintf("MAE: %.2f\nRMSE: %.2f", mae, rmse)
annotation <- tags$div(style = "position: absolute; top: 10px; right: 10px; background-color: white; padding: 5px; border: 1px solid black;", annotate_text)

# Combine the dygraph and the annotation
combined_widget <- htmlwidgets::prependContent(dyplot, annotation)

# Print the combined widget
combined_widget
```

Row
-------------------------------------------------------------------------
### Daily Rides Forecast
```{r}
# Aggregate the data by day
daily_data <- all_dtrips %>% 
  group_by(ds = date) %>% 
  summarise(y = n())

# Fit the Prophet model
model <- prophet(daily_data)

# Create a dataframe with future dates
future <- make_future_dataframe(model, periods = 365)  # Predicting for the next 365 days

# Make predictions
forecast <- predict(model, future)

# Calculate and print performance metrics (optional)
actuals <- daily_data$y
predictions <- forecast$yhat[1:nrow(daily_data)]

mae <- mean(abs(actuals - predictions))
rmse <- sqrt(mean((actuals - predictions)^2))


# Plot the forecast
dygraph_plot <- dyplot.prophet(model, forecast)

# Create an HTML widget with custom annotations
annotate_text <- sprintf("MAE: %.2f\nRMSE: %.2f", mae, rmse)
annotation <- tags$div(style = "position: absolute; top: 10px; right: 10px; background-color: white; padding: 5px; border: 1px solid black;", annotate_text)

# Combine the dygraph and the annotation
combined_widgetP <- htmlwidgets::prependContent(dygraph_plot, annotation)

# Print the combined widget
combined_widgetP
```
