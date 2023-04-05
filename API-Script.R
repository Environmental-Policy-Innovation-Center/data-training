
##########################################
#### EJScreen API and TidyCensus in R ####
##########################################

#################################
## Created by Gabriel Watson ####
## On: 4.5.2023              ####
## Last Updated: 4.5.2023    ####
#################################

## For more information see here: https://docs.google.com/document/d/120he0fviHVCUAE3Fx4vBIP5fCb5jzC7X0CWcTZVpDB8/edit# 
## Github: https://github.com/Environmental-Policy-Innovation-Center/data-training 
## Powerpoint: https://docs.google.com/presentation/d/1OqBHsGBlieIJtD51U8Uderg_l4SrJk_aMUwb0f_cBj0/edit#slide=id.g22a46c24ab3_0_53

### Sections ### 
##     Libraries 
##     Tidycensus
##     EJscreen
##     Joining data 
##     Results

### Libraries ### 
# Note we need to install, and then import the packages ## 
install.packages("tidyverse","tidycensus","jsonlite","httr")
library(tidyverse)
library(tidycensus)
library(jsonlite)
library(httr)
library(ggplot2)
library(leaflet)
library(htmltools)
library(aws.s3)
## Tidycensus ## 
# Loading Key
census_api_key("75334cf462de3743fb3dfd412a15b15bb517781f")

# Getting vars
AcsVars <- load_variables(2020, "acs5", cache = TRUE)


## We just need Black Alone and census IDs 
MD_Black <- get_acs(geography = "tract", 
                   state = "MD",
                   variables = c(BlackAlone = "B01001B_001", TotalPop = "B01003_001"), 
                   year = 2020, 
                   geometry = TRUE, output = "wide")

## Filter to Baltimore City and removing unneeded variables 
Balt_Black <- MD_Black %>%
              mutate(CountyCode = substr(GEOID,1,5))%>%
              filter(CountyCode == "24510")%>%
              filter(TotalPopE != 0)%>%
              mutate(Per_Black = BlackAloneE/TotalPopE)%>%
              select(GEOID,BlackAloneE,TotalPopE,Per_Black)

## Check 
sum(Balt_Black$BlackAloneE)
# https://www.census.gov/quickfacts/baltimorecitymaryland

## Creating a function for pulling in EJ Screen Traffic Data 
## We could alter this function to pull in another variable by adding 'var' to the function inputs OR pull multiple variables
EJScreenTraffic <- function(GEOID)
{
print(GEOID)
## EJ SCREEN ## 
Response <- GET("https://ejscreen.epa.gov/mapper/ejscreenRESTbroker.aspx", 
                query = list(areaid = GEOID, areatype = "tract", f = "pjson"))
## Parsing JSON
ParsedJSON <- fromJSON(content(Response,as="text"))

## Getting the variable we need - State Traffic Percentile by VCensus tract
Balt_Traffic_Per <- as.numeric(ParsedJSON$S_E_TRAFFIC_PER)
}

## Calling function and adding to dataset
Balt_Black_Traffic <- Balt_Black %>%
                      rowwise()%>%
                      mutate(State_Traffic_Percentile = EJScreenTraffic(GEOID))%>%
                      mutate(Traffic_Black_Score = ((Per_Black *100) + State_Traffic_Percentile)/2)

## Quick look at the data ## 
ggplot(Balt_Black_Traffic, aes(x=State_Traffic_Percentile, y=Per_Black))+
      geom_point()+
      theme_minimal()

binpal <- colorBin("OrRd", Balt_Black_Traffic$Traffic_Black_Score, 9, pretty = FALSE)


## Lets look spatially ##
Baltimore_Traffic_Black <- leaflet(options = leafletOptions(minZoom = 6)) %>%
                          addProviderTiles(providers$CartoDB.VoyagerLabels)%>%
                          addPolygons(data = Balt_Black_Traffic, 
                          fillOpacity = .5, 
                          weight = .25, 
                          color = "grey", 
                          fillColor = ~binpal(Traffic_Black_Score), 
                          label = ~htmlEscape(paste0("Proritization Score: ",round(Traffic_Black_Score,2),"; % Black: ",round(Per_Black*100,2), "; Traffic Percentile: ",round(State_Traffic_Percentile,2))))

## Saving to HTML widget
htmlwidgets::saveWidget(Baltimore_Traffic_Black, "results/Baltimore_Traffic_Black_v1.html")


Sys.setenv("AWS_ACCESS_KEY_ID" = "YOUR KEY",
           "AWS_SECRET_ACCESS_KEY" = "YOUR SECRET KEY",
           "AWS_DEFAULT_REGION" = "us-east-1")

## Putting in AWS Bucket 
put_object(
  file = "results/Baltimore_Traffic_Black_v1.html",
  object = "training/Baltimore_Traffic_Black_v1.html",
  bucket = "epic-general"
)



                      













