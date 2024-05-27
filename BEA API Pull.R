#BEA API Pull Script for GDP Dashboard
#Author: Jessie Justice
#Date: 05/13/2024

#Import Libraries
library(plyr)
library(tidyverse)
library(httr)
library(jsonlite)
library(haven)
library(crosswalkr)

#Data Ingestion/API Call

#write function to submit GET request for data
BEA_Data_Series_Retrieve <- function(key,method = "GetData",dsname,tbname,lc,FIPS,year,format = "json",output_path){
  
  Root_Path = "https://apps.bea.gov/api/data?"
  
  unified_path = paste0(Root_Path,"&UserID=", key, "&method=",method,"&datasetname=",dsname,"&TableName=",tbname,"&LineCode=",lc,"&GeoFIPS=",FIPS,"&Year=",year,"&ResultFormat=",format)
  
  print(unified_path)
  
  res = GET(unified_path)
  
  data = fromJSON(rawToChar(res$content))
  
  print(data$BEAAPI$Results[1:5])
  
  write.csv(data$BEAAPI$Results$Data,paste0(output_path,"/",tbname,"_",lc,".csv"))
  
  return(data$BEAAPI$Results$Data)
  
}

#write function to get lc names/values
BEA__Data_Parameter_Retrieve <- function(key,method = "GetParameterValuesFiltered",dsname,tbname,targetparam,FIPS,format = "json",output_path){
  
  Root_Path = "https://apps.bea.gov/api/data?"
  
  unified_path = paste0(Root_Path,"&UserID=", key, "&method=",method,"&datasetname=",dsname,"&TargetParameter=",targetparam,"&TableName=",tbname,"&GeoFIPS=",FIPS,"&ResultFormat=",format)
  
  print(unified_path)
  
  res = GET(unified_path)
  
  data = fromJSON(rawToChar(res$content))
  
  print(data$BEAAPI$Results[1:5])
  
  #write.csv(data,paste0(output_path,"/",tbname,"_",targetparam,".csv"))
  
  return(data$BEAAPI$Results$Data)
  
}

#note: function won't return list of lcs and descriptions but will print them in console

#pull data of interest for dashboard

#GDP for NAICS Industries, all-industry total
#SAGDP <- BEA_Data_Series_Retrieve(key = '3E705AC2-2AB5-4896-9ED6-681E45D96DB5',dsname = "Regional",tbname = "SAGDP9",lc = "1", FIPS = "STATE",year = "ALL", output_path = 'C:/Users/djset/Desktop/Independent Projects/Tableau/Macroeconomic (GDP) Dashboard/Data Out')

#get GDP by NAICS Industries by iterating over relevant line-codes in table

#submit request for lc names/values
LCs <- BEA__Data_Parameter_Retrieve(key = '3E705AC2-2AB5-4896-9ED6-681E45D96DB5',dsname = "Regional",targetparam = "LineCode",tbname = "SQGDP9", FIPS = "STATE", output_path = 'C:/Users/djset/Desktop/Independent Projects/Tableau/Macroeconomic (GDP) Dashboard/Data Out')

#linecode list
lclist <- list(1:3,6,10:13,25,34:36,45,51,56,60,64,65,69,70,76,79,82:86)
lclist <- as.character(unlist(lclist))


#iterate over list of linecodes using API Call function, export files for each lc to ouput path
for(i in lclist){ 

    BEA_Data_Series_Retrieve(key = '3E705AC2-2AB5-4896-9ED6-681E45D96DB5',dsname = "Regional",tbname = "SQGDP9", lc = i, FIPS = "STATE",year = "ALL", output_path = 'C:/Users/djset/Desktop/Independent Projects/Tableau/Macroeconomic (GDP) Dashboard/Data Out')

}

#read and concatenate files at output path

setwd('C:/Users/djset/Desktop/Independent Projects/Tableau/Macroeconomic (GDP) Dashboard/Data Out')
allStateGDPbyIND <- ldply(list.files(), read.csv, header=TRUE)

#row crosswalk for linecode/industry name

#add column for linecode
allStateGDPbyIND['lc'] <- substr(allStateGDPbyIND$Code,8,9)

#crosswalk linecode with industry name and NAICS codes

#import crosswalk file
cw <- read.csv("C:/Users/djset/Desktop/Independent Projects/Tableau/Macroeconomic (GDP) Dashboard/Data In/BEAAPI_Industry_LineCode_Crosswalk - Sheet1.csv")

#clean crosswalk file, convert linecodes to strings
cw$lc <- as.character(cw$lc)

#add column for industry
allStateGDPbyIND$Industry <- encodefrom(allStateGDPbyIND, var = lc, cw_file = cw, raw = lc, clean = Industry_Name, label = Industry_Name)

#add column for NAICS codes
allStateGDPbyIND$NAICS <- encodefrom(allStateGDPbyIND, var = lc, cw_file = cw, raw = lc, clean = NAICS_Codes, label = NAICS_Codes)

#break out column with year and quarter in individual columns
allStateGDPbyIND <- allStateGDPbyIND %>% mutate(Year = substr(TimePeriod,1,4), Qtr = substr(TimePeriod,6,6))

#aggregate to get yearly values
allStateGDPbyIND_ANN <- allStateGDPbyIND %>% group_by(GeoName,Year) %>% summarise(AnnualDataValue = sum(DataValue)) %>% ungroup()
allStateGDPbyIND <- allStateGDPbyIND %>% inner_join(allStateGDPbyIND_ANN, by = c("GeoName","Year"))

#aggregate to get yearly values by industry
allStateGDPbyIND_ANN_2 <- allStateGDPbyIND %>% group_by(GeoName,Year,Industry) %>% summarise(AnnualDataValueIndustry = sum(DataValue)) %>% ungroup()
allStateGDPbyIND <- allStateGDPbyIND %>% inner_join(allStateGDPbyIND_ANN_2, by = c("GeoName","Year","Industry"))

#write final dataframe to csv

#drop irrelevant columns
allStateGDPbyIND <- allStateGDPbyIND %>% select(-c(X.1,X,NoteRef))

#export file
write.csv(allStateGDPbyIND,'C:/Users/djset/Desktop/Independent Projects/Tableau/Macroeconomic (GDP) Dashboard/Data Out/allStateRGDPbyIndustry.csv')
