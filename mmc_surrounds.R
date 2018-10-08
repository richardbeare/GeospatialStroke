## Choropleth for vicinity of MMC. Predicted stroke incidence by post code

## Illustrate the combination of geospatial geometries and census age data.
## coding up a simple incidence model from the NEMESIS paper:
##
##
## @article{thrift2000stroke,
##  title={Stroke Incidence on the East Coast of Australia The North East Melbourne Stroke Incidence Study (NEMESIS)},
##  author={Thrift, Amanda G and Dewey, Helen M and Macdonell, Richard AL and McNeil, John J and Donnan, Geoffrey A},
##  journal={Stroke},
##  volume={31},
##  number={9},
##  pages={2087--2092},
##  year={2000},
##  publisher={Am Heart Assoc}
##}
##
##
##
## Combined Male/Female incidence per 100 000.
##
## Age     Incidnce
## 0-14     0
## 15-24    5
## 25-34    30
## 35-44    44
## 45-54    111
## 55-64    299
## 65-74    747
## 75-84    1928
## 85+      3976

## 2016 census data
# https://datapacks.censusdata.abs.gov.au/datapacks/
# Dempgraphic data by postcode is available, with age information in 
# "ABSData/2016 Census GCP Postal Areas for VIC/2016Census_G01_VIC_POA.csv"

## Boundary data available from the same site
## ---- LibrariesAndCensusData ----

library(tidyverse)
library(sf)
library(units)
postcodeboundariesAUS <- sf::st_read("ABSData/Boundaries/POA_2016_AUST.shp")

basicDemographicsVIC <- readr::read_csv("ABSData/2016 Census GCP Postal Areas for VIC/2016Census_G01_VIC_POA.csv")

## ---- MonashMedicalCentre ----
## Location of hopsital providing acute stroke services
## address: 246 Clayton Rd, Clayton VIC, 3168
MMCLocation <- st_sfc(st_point(x=c(lon=145.1217375, lat=-37.9205463)), crs=st_crs(postcodeboundariesAUS))

## ---- StrokeIncidence ----
computeStrokeCount <- function(demographics)
{
  ## Add a stroke estimate column to demographics
  demographics <- mutate(demographics, 
                         Age_0_24_yr_P = Age_0_4_yr_P + Age_5_14_yr_P + 
                           Age_15_19_yr_P + Age_20_24_yr_P)
  ageframe <- select(demographics, 
                     Age_0_24_yr_P, Age_25_34_yr_P, 
                     Age_35_44_yr_P, Age_45_54_yr_P, 
                     Age_55_64_yr_P, Age_65_74_yr_P,
                     Age_75_84_yr_P, Age_85ov_P)
  incidence <- c(5, 30, 44, 111, 299, 747, 1928, 3976)/100000

  strokeNum <- function(therow)
  {
    return( sum(incidence * therow))
  }
  
  demographics <- mutate(demographics, stroke_count_estimate=apply(ageframe, MARGIN=1, strokeNum))
  return(demographics)
}


basicDemographicsVIC <- computeStrokeCount(basicDemographicsVIC)

## ---- JoinCensusAndBoundaries ----
## Join the demographics and shape tables, retaining victoria only
## use postcode boundaries as the reference data frame so that coordinate
## reference system is retained.
basicDemographicsVIC <- right_join(postcodeboundariesAUS, basicDemographicsVIC, 
                                  by=c("POA_CODE" = "POA_CODE_2016"))

## ---- SpatialComputations ----
## Add some geospatial measures to the data frame
## Reproduce the existing area column as a demo.
basicDemographicsVIC <- mutate(basicDemographicsVIC, 
                               PostcodeArea=units::set_units(st_area(geometry), km^2))

## Distance to MMC
basicDemographicsVIC <- mutate(basicDemographicsVIC, 
                               DistanceToMMC=units::set_units(st_distance(geometry,MMCLocation)[,1], km))

## ---- FilteringPostcodes ----
## Make a small dataset for MMC surrounds.
basicDemographicsMMC <- filter(basicDemographicsVIC, DistanceToMMC < set_units(20, km))

## Display with tmap

## ---- InteractiveDisplay ----
library(tmap)
tmap_mode("view")

tm_shape(basicDemographicsMMC, name="Annual stroke counts") + 
  tm_polygons("stroke_count_estimate", id="POA_NAME", popup.vars=c("Cases"="stroke_count_estimate"), alpha=0.6) + 
  tm_shape(MMCLocation) + tm_symbols(col='red') + 
  tm_basemap("OpenStreetMap")

