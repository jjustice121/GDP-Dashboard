# GDP-Dashboard
# This R program retrieves and processes data from the BEA API
#BEA API Pull Script for GDP Dashboard
#Author: Jessie Justice
#Date: 05/13/2024

#Import Libraries
library(tidyverse)
library(httr)
library(jsonlite)
library(haven)

#Data Ingestion/API Call

#write function to submit GET request
BEA_Data_Series_Retrieve <- function(key,method = "GetData",dsname,tbname,lc,FIPS,year,format = "json"){
  
  Root_Path = "https://apps.bea.gov/api/data?"
  
  unified_path = paste0(Root_Path,"&UserID=", key, "&method=",method,"&datasetname=",dsname,"&TableName=",tbname,"&LineCode=",lc,"&GeoFIPS=",FIPS,"&Year=",year,"&ResultFormat=",format)
  
  print(unified_path)
  
  res = GET(unified_path)
  
  data = fromJSON(rawToChar(res$content))
  
  print(data$BEAAPI$Results[1:5])
  
  return(data$BEAAPI$Results$Data)
  
}

#test function
test <- BEA_Data_Series_Retrieve(key = '3E705AC2-2AB5-4896-9ED6-681E45D96DB5',dsname = "Regional",tbname = "CAINC4",lc = "30", FIPS = "STATE",year = "ALL")

#pull data of interest for dashboard

#GDP for NAICS Industries, all-industry total
SAGDP <- BEA_Data_Series_Retrieve(key = '3E705AC2-2AB5-4896-9ED6-681E45D96DB5',dsname = "Regional",tbname = "SQGDP9",lc = "1", FIPS = "STATE",year = "ALL")

#GDP for NAICS Industries for each industry




#Employment for NAICS Industries


#Data Pre-Processing

#break out column with year and quarter in individual columns
SAGDP <- SAGDP %>% mutate(Year = substr(TimePeriod,1,4), Qtr = substr(TimePeriod,6,6))

#aggregate to get yearly values
SAGDPANN <- SAGDP %>% group_by(GeoName,Year) %>% summarise(AnnualDataValue = sum(as.numeric(DataValue))) %>% ungroup()
SAGDP <- SAGDP %>% inner_join(SAGDPANN, by = c("GeoName","Year"))
