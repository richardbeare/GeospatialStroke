The R and python analyses differed in 2 primary ways:

1.  The R routing used a weighted network, so cars were preferentially
    routed along the weighted version, yet resultant distances
    calculated from the unweighted version. The python routing used a
    non-weighted graph, with the network instead reduced to only those
    ways usable by cars.
2.  The R analyses sampled actual postcode addresses with the `PSMA`
    package, while the python analyses simply sampled random points
    within each postcode.

The effects of these 2 differences are compared here (in R code). This
code is merely a stripped-down version of code from the main document.

``` r
library(tidyverse)
library(sf)
library(units)
library(tmaptools)
#> Warning in fun(libname, pkgname): rgeos: versions of GEOS runtime 3.7.1-CAPI-1.11.1
#> and GEOS at installation 3.7.0-CAPI-1.11.0differ
```

# Data pre-processing

Load and clean Census Data

``` r
postcodeboundariesAUS <- 
    file.path(here::here(), "ABSData", "Boundaries/POA_2016_AUST.shp") %>%
    sf::read_sf ()

basicDemographicsVIC <- file.path(here::here(), "ABSData",
                                  "2016 Census GCP Postal Areas for VIC",
                                  "2016Census_G01_VIC_POA.csv") %>%
    readr::read_csv()
#> Parsed with column specification:
#> cols(
#>   .default = col_double(),
#>   POA_CODE_2016 = col_character()
#> )
#> See spec(...) for full column specifications.
basicDemographicsVIC <- select(basicDemographicsVIC, POA_CODE_2016, starts_with("Age_"), -starts_with("Age_psns_"))
```

JoinCensusAndBoundaries

``` r
basicDemographicsVIC <- right_join(postcodeboundariesAUS,
                                   basicDemographicsVIC, 
                                   by=c("POA_CODE" = "POA_CODE_2016"))
```

Geocode and transform
RehabNetwork

``` r
rehab_addresses <- c(DandenongHospital = "Dandenong Hospital, Dandenong VIC 3175, Australia",
                     CaseyHospital = "62-70 Kangan Dr, Berwick VIC 3806, Australia",
                     KingstonHospital = "The Kingston Centre, Heatherton VIC 3202, Australia")
RehabLocations <- tmaptools::geocode_OSM(rehab_addresses, as.sf=TRUE)
RehabLocations <- sf::st_transform(RehabLocations,
                                   sf::st_crs(basicDemographicsVIC))
```

Postcodes surrounding rehab locations

``` r
dist_to_loc <- function (geometry, location){
    units::set_units(st_distance(geometry, location)[,1], km)
}
dist_range <- units::set_units(10, km)

basicDemographicsVIC_old <- basicDemographicsVIC
basicDemographicsVIC <- mutate(basicDemographicsVIC,
       DirectDistanceToDandenong = dist_to_loc(geometry,RehabLocations["DandenongHospital", ]),
       DirectDistanceToCasey     = dist_to_loc(geometry,RehabLocations["CaseyHospital", ]),
       DirectDistanceToKingston  = dist_to_loc(geometry,RehabLocations["KingstonHospital", ]),
       DirectDistanceToNearest   = pmin(DirectDistanceToDandenong,
                                        DirectDistanceToCasey,
                                        DirectDistanceToKingston)
)
basicDemographicsRehab <- filter(basicDemographicsVIC,
                                 DirectDistanceToNearest < dist_range) %>%
        mutate(Postcode = as.numeric(POA_CODE16)) %>%
        select(-starts_with("POA_"))
```

## Data sampling

The major difference between the R and python code is the sampling
method. The R code sampled actual random addresses from postcodes,
whereas the python code - simply because of the unavailability of the
amazing `PSMA` R package - could not do this, and so sample random
points from within the postcode polygons. These two approaches are
replicated here in R code, the first referred to as `randomaddresses`,
the second as `randomPoints`.

``` r
addressesPerPostcode <- 1000
```

Random addresses:

``` r
library(PSMA)
samplePCode <- function(pcode, number) {
  d <- fetch_postcodes(pcode)
  return(d[, .SD[sample(.N, min(number, .N))], by=.(POSTCODE)])
}

randomaddresses <- map(basicDemographicsRehab$Postcode,
                       samplePCode,
                       number=addressesPerPostcode) %>%
            bind_rows() %>%
            sf::st_as_sf(coords = c("LONGITUDE", "LATITUDE"),
                         crs=st_crs(basicDemographicsRehab),
                         agr = "constant")
```

Random points:

``` r
randomPoints <- apply (basicDemographicsRehab, 1, function (i) {
                           x <- st_sample (i$geometry,
                                           size = addressesPerPostcode)
                           st_sf (POSTCODE = i$Postcode,
                                  geometry = x)
                         })
randomPoints <- do.call (rbind, randomPoints)
st_crs (randomPoints) <- 4326
```

Code to examine the distributions. The two are not shown here, to avoid
junking up the repo with unnecessary files, but there really is a
striking difference - the postcodes are much more concentrated where
people actually live, and so much greater overall spatial heterogeneity,
while the random points have relatively many more points in less
populated areas.

``` r
library (mapdeck)
set_token(Sys.getenv("MAPBOX_TOKEN"))
mapdeck(location = c(145.2, -38), zoom = 14) %>%
    add_scatterplot (randomaddresses, radius = 2)
mapdeck(location = c(145.2, -38), zoom = 14) %>%
    add_scatterplot (randomPoints, radius = 2)
```

## Street Network

``` r
bounding_polygon <- sf::st_transform(basicDemographicsRehab,
                                     sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
bounding_polygon <- bounding_polygon [, 1:2]
```

``` r
library(dodgr)
system.time (
dandenong_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
)
saveRDS (dandenong_streets, file = "../dandenong-streets.Rds")
```

``` r
dandenong_streets <- readRDS ("../dandenong-streets.Rds")
library (dodgr)
net <- weight_streetnet (dandenong_streets, wt_profile = "motorcar")
net <- net [which (net$d_weighted < .Machine$double.xmax), ]
```

An unweighted network analogous to that used in the python analyses can
then be created simply by

``` r
net_unwt <- net
net_unwt$d_weighted <- net_unwt$d
```

A final bit of pre-processing to speed up the following code:

``` r
nodes <- dodgr_vertices (net) # same for both net and net_unwt
```

That suffices to now examine the differences in estimated cases per
centre.

## CasesPerCentre

``` r
cases_per_centre <- function (randomxy, net, nodes, RehabLocations)
{
    fromCoords <- st_coordinates (st_transform (randomxy, crs = 4326))
    fromIDX <- match_pts_to_graph (nodes, fromCoords, connected = TRUE)
    from <- nodes$id [fromIDX]
    toCoords <- st_coordinates (st_transform (RehabLocations, crs = 4326))
    to <- nodes$id [match_pts_to_graph (nodes, toCoords, connected = TRUE)]
    d <- dodgr_dists (net, from = from, to = to)

    DestNames <- c(rownames(RehabLocations), "Disconnected")
    DestNumber <- as.numeric (apply(d, MARGIN=1, which.min))
    DestNumber [is.na (DestNumber)] <- 4 # the disconnected points
    BestDestination <- DestNames[DestNumber]
    postcodes <- data.frame (POSTCODE = randomxy$POSTCODE,
                             DestNumber = DestNumber,
                             Destination = BestDestination,
                             stringsAsFactors = FALSE) %>%
        group_by (POSTCODE, DestNumber, Destination) %>%
        summarise (n = length (DestNumber))

    postcodes %>%
        filter (Destination != "Disconnected") %>%
        group_by (Destination) %>%
        summarise (total = sum (n)) %>%
        mutate (percent = 100 * total / sum (total))
}
```

Then run that function for the four possible combinations of
differences:

``` r
library (knitr) # just for neat table output
kable (cases_per_centre (randomaddresses, net, nodes, RehabLocations))
```

| Destination       | total |  percent |
| :---------------- | ----: | -------: |
| CaseyHospital     | 10799 | 19.41428 |
| DandenongHospital | 16355 | 29.40278 |
| KingstonHospital  | 28470 | 51.18294 |

``` r
kable (cases_per_centre (randomaddresses, net_unwt, nodes, RehabLocations))
```

| Destination       | total |  percent |
| :---------------- | ----: | -------: |
| CaseyHospital     | 11200 | 20.13519 |
| DandenongHospital | 15816 | 28.43377 |
| KingstonHospital  | 28608 | 51.43104 |

``` r
kable (cases_per_centre (randomPoints, net, nodes, RehabLocations))
```

| Destination       | total |  percent |
| :---------------- | ----: | -------: |
| CaseyHospital     | 10695 | 19.03465 |
| DandenongHospital | 16127 | 28.70237 |
| KingstonHospital  | 29365 | 52.26298 |

``` r
kable (cases_per_centre (randomPoints, net_unwt, nodes, RehabLocations))
```

| Destination       | total |  percent |
| :---------------- | ----: | -------: |
| CaseyHospital     | 11111 | 19.77504 |
| DandenongHospital | 15715 | 27.96910 |
| KingstonHospital  | 29361 | 52.25586 |

And that only makes a very small difference, in spite of the huge
apparent difference in distributions of random points, and still does
not reproduce the values generated in the python code.
