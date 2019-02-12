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

The `addressesPerPostcode` value below is modified by the estimated
stroke rate per postcode calculated in the python code.

``` r
addressesPerPostcode <- 1000
```

The python code has fewer postcodes than the R code, with numbers
determined manually here by comparing the corresponding maps. The
reduced version equivalent to the python code is:

``` r
#mapview::mapview (basicDemographicsRehab_py)
removes <- c (40, 56, 57, 53, 43, 10, 7, 8, 29, 11, 1, 3)
index <- seq (nrow (basicDemographicsRehab))
basicDemographicsRehab_py <- basicDemographicsRehab [!index %in% removes, ]
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
randomaddresses_py <- map(basicDemographicsRehab_py$Postcode,
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
randomPoints_py <- apply (basicDemographicsRehab_py, 1, function (i) {
                           x <- st_sample (i$geometry,
                                           size = addressesPerPostcode)
                           st_sf (POSTCODE = i$Postcode,
                                  geometry = x)
                         })
randomPoints_py <- do.call (rbind, randomPoints_py)
st_crs (randomPoints_py) <- 4326
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

## direct sample of street network points within postcode boundary

Following the python code, simply sample a fixed number of random points
from the street network within the entire postcode boundary, as well as
simply from within the boundary itself.

``` r
npts <- 10000
pts_in_net <- as.matrix (nodes [sample (nrow (nodes), size = npts),
                         c ("x", "y")]) %>%
    as.data.frame () %>%
    st_as_sf (coords = c (1, 2)) %>%
    st_sf (crs = st_crs (basicDemographicsRehab))

assign_postcodes <- function (pts, basicDemographicsRehab)
{
    pts_in_postcodes <- st_contains (basicDemographicsRehab, pts)
    postcodes <- rep (NA, length (pts))
    for (i in seq (pts_in_postcodes))
        postcodes [pts_in_postcodes [[i]] ] <- basicDemographicsRehab$Postcode [i]
    st_sf (POSTCODE = postcodes,
           geometry = pts$geometry)
}

pts_in_net <- assign_postcodes (pts_in_net, basicDemographicsRehab)
#> although coordinates are longitude/latitude, st_contains assumes that they are planar

# Then points randomly sample from within the bounding polygon of all postcodes
bp <- st_union (basicDemographicsRehab)
pts_in_poly <- st_sf (geometry = st_sample (bp, size = npts))
#> although coordinates are longitude/latitude, st_intersects assumes that they are planar
pts_in_poly <- assign_postcodes (pts_in_poly, basicDemographicsRehab)
#> although coordinates are longitude/latitude, st_contains assumes that they are planar
```

That suffices to now examine the differences in estimated cases per
centre.

## CasesPerCentre

``` r
cases_per_centre <- function (randomxy, net, nodes, RehabLocations, stroke_rate)
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
    index <- match (postcodes$POSTCODE, stroke_rate$POSTCODE)
    postcodes$load <- stroke_rate$strokes [index]

    postcodes %>%
        filter (Destination != "Disconnected") %>%
        group_by (Destination) %>%
        summarise (total = sum (load)) %>%
        mutate (percent = 100 * total / sum (total))
}
```

Then run that function for the eight possible combinations of
differences, first loading the stroke rate estimates from the python
code to use to load the final postcode-based
estimates.

``` r
stroke_rate <- read.csv ("../../python/notebooks/data/postcode_strokes.csv",
                         stringsAsFactors = FALSE)
stroke_rate$POSTCODE <- substr (stroke_rate$POA_CODE, 4, 7)
library (knitr) # just for neat table output
kable (cases_per_centre (randomaddresses, net, nodes, RehabLocations, stroke_rate))
```

| Destination       |     total |  percent |
| :---------------- | --------: | -------: |
| CaseyHospital     |  705.3581 | 13.82937 |
| DandenongHospital | 2001.2102 | 39.23607 |
| KingstonHospital  | 2393.8670 | 46.93456 |

``` r
kable (cases_per_centre (randomaddresses, net_unwt, nodes, RehabLocations, stroke_rate))
```

| Destination       |     total |  percent |
| :---------------- | --------: | -------: |
| CaseyHospital     |  705.3581 | 15.00688 |
| DandenongHospital | 1748.7257 | 37.20509 |
| KingstonHospital  | 2246.1491 | 47.78804 |

``` r
kable (cases_per_centre (randomPoints, net, nodes, RehabLocations, stroke_rate))
```

| Destination       |     total |  percent |
| :---------------- | --------: | -------: |
| CaseyHospital     |  809.1363 | 15.54715 |
| DandenongHospital | 2001.2102 | 38.45227 |
| KingstonHospital  | 2394.0547 | 46.00058 |

``` r
kable (cases_per_centre (randomPoints, net_unwt, nodes, RehabLocations, stroke_rate))
```

| Destination       |     total |  percent |
| :---------------- | --------: | -------: |
| CaseyHospital     |  809.1363 | 16.28608 |
| DandenongHospital | 1850.5974 | 37.24834 |
| KingstonHospital  | 2308.5340 | 46.46557 |

``` r

# The `_py` addresses from the reduced set of postcodes
kable (cases_per_centre (randomaddresses_py, net, nodes, RehabLocations, stroke_rate))
```

| Destination       |     total |  percent |
| :---------------- | --------: | -------: |
| CaseyHospital     |  479.1516 | 13.00412 |
| DandenongHospital | 1354.5159 | 36.76142 |
| KingstonHospital  | 1850.9452 | 50.23446 |

``` r
kable (cases_per_centre (randomaddresses_py, net_unwt, nodes, RehabLocations, stroke_rate))
```

| Destination       |     total |  percent |
| :---------------- | --------: | -------: |
| CaseyHospital     |  479.1516 | 14.80624 |
| DandenongHospital | 1121.3991 | 34.65229 |
| KingstonHospital  | 1635.5964 | 50.54147 |

``` r
kable (cases_per_centre (randomPoints_py, net, nodes, RehabLocations, stroke_rate))
```

| Destination       |     total |  percent |
| :---------------- | --------: | -------: |
| CaseyHospital     |  479.1516 | 12.76910 |
| DandenongHospital | 1354.5159 | 36.09702 |
| KingstonHospital  | 1918.7638 | 51.13388 |

``` r
kable (cases_per_centre (randomPoints_py, net_unwt, nodes, RehabLocations, stroke_rate))
```

| Destination       |     total |  percent |
| :---------------- | --------: | -------: |
| CaseyHospital     |  479.1516 | 13.23190 |
| DandenongHospital | 1223.2709 | 33.78094 |
| KingstonHospital  | 1918.7638 | 52.98716 |

``` r

# And finally the "trulyRandomAddresses" simply sample from within the enclosing
# polygon of all postcodes
kable (cases_per_centre (pts_in_net, net, nodes, RehabLocations, stroke_rate))
```

| Destination       |     total |  percent |
| :---------------- | --------: | -------: |
| CaseyHospital     |  809.1363 | 15.60523 |
| DandenongHospital | 1981.8425 | 38.22237 |
| KingstonHospital  | 2394.0547 | 46.17241 |

``` r
kable (cases_per_centre (pts_in_net, net_unwt, nodes, RehabLocations, stroke_rate))
```

| Destination       |     total |  percent |
| :---------------- | --------: | -------: |
| CaseyHospital     |  809.1363 | 17.08275 |
| DandenongHospital | 1748.7257 | 36.91968 |
| KingstonHospital  | 2178.7059 | 45.99757 |

``` r
kable (cases_per_centre (pts_in_poly, net, nodes, RehabLocations, stroke_rate))
```

| Destination       |     total |  percent |
| :---------------- | --------: | -------: |
| CaseyHospital     |  809.1363 | 15.82109 |
| DandenongHospital | 1911.0995 | 37.36783 |
| KingstonHospital  | 2394.0547 | 46.81108 |

``` r
kable (cases_per_centre (pts_in_poly, net_unwt, nodes, RehabLocations, stroke_rate))
```

| Destination       |     total |  percent |
| :---------------- | --------: | -------: |
| CaseyHospital     |  809.1363 | 16.86134 |
| DandenongHospital | 1748.7257 | 36.44116 |
| KingstonHospital  | 2240.9030 | 46.69749 |

And that only makes a very small difference, in spite of the huge
apparent difference in distributions of random points, and still does
not reproduce the values generated in the python code.
