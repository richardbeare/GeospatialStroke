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

## ---- RPackageCheck ----
ip <- installed.packages () [, 1] # names of installed packages
requiredpackages <- c("tidyverse", "sf", "here", 
                      "units",  "tmaptools", "tmap", "knitr")
if (!all(requiredpackages %in% ip)) {
  msg <- paste("This script requires the following packages: ", paste(requiredpackages, collapse=", "))
  message(msg)
  message("Attempting to install them")
  options(repos=c(CRAN="https://cloud.r-project.org"))
  missingCRAN <- setdiff(requiredpackages, ip)
  if (length(missingCRAN) > 0) {
    message(paste("Missing packages are", missingCRAN))
    install.packages(missingCRAN)
  }
}
## ---- Libraries ----
library(tidyverse)
library(sf)
library(units)
library(tmaptools)

## ---- CensusData ----
postcodeboundariesAUS <- sf::read_sf(
  here::here("ABSData", 
            "Boundaries", 
            "POA_2016_AUST.shp"))

basicDemographicsVIC <- readr::read_csv(
  here::here("ABSData", 
            "2016 Census GCP Postal Areas for VIC", 
            "2016Census_G01_VIC_POA.csv"))

## ---- MonashMedicalCentre ----
## Location of hopsital providing acute stroke services
## address: 246 Clayton Rd, Clayton VIC, 3168
MMCLocation <- tmaptools::geocode_OSM("Monash Medical Centre, Clayton, Victoria, Australia", as.sf=TRUE)
MMCLocation

## ---- JoinCensusAndBoundaries ----
## Join the demographics and shape tables, retaining victoria only
## use postcode boundaries as the reference data frame so that coordinate
## reference system is retained.
basicDemographicsVIC <- right_join(postcodeboundariesAUS, basicDemographicsVIC, 
                                  by=c("POA_CODE" = "POA_CODE_2016"))

## ---- StrokeIncidence ----

basicDemographicsVIC <- mutate(basicDemographicsVIC, 
                               Age_0_24_yr_P = Age_0_4_yr_P + Age_5_14_yr_P + 
                                 Age_15_19_yr_P + Age_20_24_yr_P)
basicDemographicsVIC <- mutate(basicDemographicsVIC, stroke_count_estimate = ( 
  Age_0_24_yr_P  * 5 +  
    Age_25_34_yr_P * 30 +   
    Age_35_44_yr_P * 44 +  
    Age_45_54_yr_P * 111 +  
    Age_55_64_yr_P * 299 +  
    Age_65_74_yr_P * 747 + 
    Age_75_84_yr_P * 1928 +  
    Age_85ov_P     * 3976) / 100000)

## ---- SpatialComputations ----
## Add some geospatial measures to the data frame
## Reproduce the existing area column as a demo.
basicDemographicsVIC <- mutate(basicDemographicsVIC, 
                               PostcodeArea=units::set_units(st_area(geometry), km^2))

## Distance to MMC
basicDemographicsVIC <- sf::st_transform( basicDemographicsVIC, crs = sf::st_crs( MMCLocation ) )
basicDemographicsVIC <- mutate(basicDemographicsVIC, 
                               DistanceToMMC=units::set_units(st_distance(geometry,MMCLocation)[,1], km))

## comments re auto-great-circle, straight line assumption, 
## distance to polygon or polygon-centroid ...
plot(basicDemographicsVIC["DistanceToMMC"])

## ---- FilteringPostcodes ----
## Make a small dataset for MMC surrounds.
basicDemographicsMMC <- filter(basicDemographicsVIC, DistanceToMMC < set_units(20, km))

## ---- PostcodesTable ----
## tables to paste into latex
tt <- knitr::kable(select(head(basicDemographicsMMC), POA_NAME, Tot_P_P, stroke_count_estimate, DistanceToMMC), format="latex")
writeLines(tt, "mmcdemograhics")
## ---- InteractiveDisplay ----
library(tmap)
tmap_mode("view")

MMCLocation <- mutate(MMCLocation, ID="Monash Medical Centre")
basicDemographicsMMC <- mutate(basicDemographicsMMC, Over65 = Age_65_74_yr_P + Age_75_84_yr_P + Age_85ov_P)
tm_shape(basicDemographicsMMC, name="Annual stroke counts") + 
  tm_polygons("stroke_count_estimate", id="POA_NAME", popup.vars=c("Cases"="stroke_count_estimate"), alpha=0.6) + 
  tm_shape(MMCLocation) + tm_markers() + 
  tm_basemap("OpenStreetMap")


## ---- Dummy ----

## Display with Mapdeck
#library(mapdeck)
#set_token(read.dcf("~/Documents/.googleAPI", fields = "MAPBOX"))

#mapdeck(
#  location = c(145.2, -37.9)
#  , zoom = 9
#  ) %>%
#  add_polygon(
#    data = basicDemographicsMMC
#    , fill_colour = "stroke_count_estimate"
#    , elevation = "stroke_count_estimate"
#    , tooltip = "stroke_count_estimate"
#  )








