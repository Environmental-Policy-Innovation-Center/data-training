
##########################################
#### Interactive Visualizations in R #####
##########################################

#################################
## Created by Gabriel Watson ####
## On: 2.14.2023             ####
## Last Updated: 2.14.2023   ####
#################################

## For more information see here: https://docs.google.com/document/d/120he0fviHVCUAE3Fx4vBIP5fCb5jzC7X0CWcTZVpDB8/edit# 
## Github: https://github.com/Environmental-Policy-Innovation-Center/data-training 

### Sections ### 
##     Libraries 
##     Data Import  
##     Data Cleaning 
##     Static Chart 
##     Interactive Chart 

### Libraries ### 
# Note we need to install, and then import the packages ## 
install.packages("tidyverse","htmlwidgets","lubridate","plotly","ggplot2","htmltools")
library(tidyverse)
library(htmlwidgets)
library(lubridate)
library(plotly)
library(ggplot2)
library(htmltools)

## Data Import ##
## TO DO: Download your local weather station data, and add it to the 'data' folder.
APG_weather <- read.csv("data/3232516.csv")

## A little data cleaning ##
## Change Date to a Date format ##
## Only include full years ## 
APG_weather_cleaned <- APG_weather %>% 
                       mutate(DATE = ymd(DATE))%>%
                       filter(year(DATE) < year(ymd("2023-01-01")))


## Let's make a chart showing averages highs (temperature and precipitation)! ## 
APG_weather_max_year <- APG_weather_cleaned %>%
                    select(DATE,PRCP,TMAX)%>%
                    ## TO DO: Change grouping to Monthly ## 
                    ## Hint (You'll need to use 'Month' function from lubridate: https://www.rdocumentation.org/packages/lubridate/versions/1.9.1/topics/month)
                    group_by(year(DATE))%>%
                    summarize_all(mean, na.rm = TRUE)

###########################
#### End Data Cleaning ####
###########################


#####################
### Static Chart  ### 
#####################

## TO DO 'Uncomment' Sections to reveal how each line effects the chart (don't forget to add the '+' !) ## 
APG_temp_high_avg_year <- ggplot(APG_weather_max_year, aes(x = DATE, y = TMAX))+
                          geom_point(size = 3, shape = 21, color = "grey", fill = "#57749b")+
                          geom_hline(yintercept = mean(APG_weather_max_year$TMAX, na.rm = TRUE), color = "grey")+
                          ylab("Average Yearly High")+
                          xlab("Date")+
                          theme_classic()+
                          ggtitle("Avearge Daily Highs for Aberdeen Proving Ground: 1919 to 2023")

## View the plot ## 
plot(APG_temp_high_avg_year)

###########################
#### End Static Chart  ####
###########################
                    

##########################
### Interactive Chart  ### 
##########################

## We'll be turning our static ggplot into an interactive plotly and then bundling with HTML Widgets ## 
## Note: the '::' to force package. 
## Note: command 'partial_bundle' - this reduces size by a 1/3rd ##  
htmlwidgets::saveWidget(plotly::partial_bundle(plotly::ggplotly(APG_temp_high_avg_year)), "results/APG_temp_high_avg_year_v1.html")






