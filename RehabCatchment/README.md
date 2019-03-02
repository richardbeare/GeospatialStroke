## 0\. Package loading

``` r
library(tidyverse)
library(sf)
library(units)
library(tmaptools)
#> Warning in fun(libname, pkgname): rgeos: versions of GEOS runtime 3.7.1-CAPI-1.11.1
#> and GEOS at installation 3.7.0-CAPI-1.11.0differ
library (mapview)
```

## 1\. Loading census and boundary data

Load postcode boundaries and demographic data from the 2016 census.

``` r
postcodeboundariesAUS <- 
    file.path(here::here(), "ABSData", "Boundaries", "POA_2016_AUST.shp") %>%
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
```

Clean up the demographics table so that it only contains columns of
interest, which in this case are the postcodes and age related columns.
The columns about education status are being removed for clarity.

``` r
basicDemographicsVIC <- select(basicDemographicsVIC, POA_CODE_2016,
                               starts_with("Age_"),
                               -starts_with("Age_psns_"))
```

## 2\. Geocoding hospital locations

Geocoding transforms a text address into a latitude/longitude
coordinate. In this example we are using the OpenStreetMap Nominatim
service, that can be queried without an API
key.

``` r
rehab_addresses <- c(DandenongHospital = "Dandenong Hospital, Dandenong VIC 3175, Australia",
                     CaseyHospital = "62-70 Kangan Dr, Berwick VIC 3806, Australia",
                     KingstonHospital = "The Kingston Centre, Heatherton VIC 3202, Australia")
RehabLocations <- tmaptools::geocode_OSM(rehab_addresses, as.sf=TRUE)
```

These `RehabLocations` then need to be transformed to the same
coordinate reference system as the `basicDemographicsVIC`.

``` r
RehabLocations <- sf::st_transform(RehabLocations,
                                   sf::st_crs(postcodeboundariesAUS))
```

These locations can then be viewed with `mapview` in one line:

``` r
mapview (RehabLocations)
```

![](map1.png)

## 3\. Combine demographics and spatial data

Join the demographics and shape tables of postcode boundaries, retaining
Victoria only. Use postcode boundaries as the reference data frame so
that coordinate reference system is retained. The `right_join` uses
postcodes in the right hand argument (basicDemographicsVIC) to determine
which rows to keep in the output.

``` r
basicDemographicsVIC <- right_join(postcodeboundariesAUS,
                                   basicDemographicsVIC, 
                                   by=c("POA_CODE" = "POA_CODE_2016"))
```

## 4\. Compute distance to each service centre from each postcode

There are 698 postcodes which we now want to reduce to only those within
a zone around the rehab locations. In this example we use a 10km
straight-line distance as a simple approach to producing a set of
postcodes of interest. Distances are calculated to centroids of each
postcode polygon. (Running this code produces a warning that
`st_centroid` does not give correct results for longitude/latitude data,
but results are nevertheless good enough for our purposes here.)

``` r
dist_to_loc <- function (geometry, location){
    units::set_units(st_distance(st_centroid (geometry), location)[,1], km)
}
dist_range <- units::set_units(10, km)

basicDemographicsVIC <- mutate(basicDemographicsVIC,
       DirectDistanceToDandenong = dist_to_loc(geometry,RehabLocations["DandenongHospital", ]),
       DirectDistanceToCasey     = dist_to_loc(geometry,RehabLocations["CaseyHospital", ]),
       DirectDistanceToKingston  = dist_to_loc(geometry,RehabLocations["KingstonHospital", ]),
       DirectDistanceToNearest   = pmin(DirectDistanceToDandenong,
                                        DirectDistanceToCasey,
                                        DirectDistanceToKingston)
    )
#> Warning in st_centroid.sfc(geometry): st_centroid does not give correct
#> centroids for longitude/latitude data

#> Warning in st_centroid.sfc(geometry): st_centroid does not give correct
#> centroids for longitude/latitude data

#> Warning in st_centroid.sfc(geometry): st_centroid does not give correct
#> centroids for longitude/latitude data

basicDemographicsRehab <- filter(basicDemographicsVIC,
                                 DirectDistanceToNearest < dist_range) %>%
        mutate(Postcode = as.numeric(POA_CODE16)) %>%
        select(-starts_with("POA_"))
```

That reduces the data down to 45 nearby postcodes, with the last 2 lines
converting all prior postcode columns (of which there were several all
beginning with “POA”) to a single numeric column named “Postcode”.

``` r
mapview (basicDemographicsRehab)
```

![](map3.png)

## 5\. Randomly sample addresses in postcodes

Case loads for rehabilitation centres will be estimated based on a set
of random addresses. The addresses are generated by sampling a geocoded
database, PSMA, to produce a specified number of unique addresses per
postcode. The number of addresses selected will depend on the subsequent
processing steps, with numbers being reduced if queries to web services
are involved.

``` r
addressesPerPostcode <- 1000
```

We define a function, `samplePCode`, to sample a single postcode and
apply it to every postcode using the `map` function. Sampling syntax is
due to the use of data.table inside PSMA. The last `st_as_sf()` command
converts the points labelled “LONGITUDE” and “LATITUDE” into `sf::POINT`
objects. The results are combined in a single table.

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
head(randomaddresses)
#> Simple feature collection with 6 features and 13 fields
#> Attribute-geometry relationship: 13 constant, 0 aggregate, 0 identity
#> geometry type:  POINT
#> dimension:      XY
#> bbox:           xmin: 145.048 ymin: -37.88979 xmax: 145.0847 ymax: -37.86604
#> epsg (SRID):    4283
#> proj4string:    +proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs
#>   POSTCODE ADDRESS_DETAIL_INTRNL_ID STREET_LOCALITY_INTRNL_ID
#> 1     3145                 13046925                    571696
#> 2     3145                 12384629                    494635
#> 3     3145                  9600427                    588362
#> 4     3145                 11946322                    531034
#> 5     3145                 12926757                    590471
#> 6     3145                 10666879                    493880
#>   BUILDING_NAME LOT_NUMBER FLAT_NUMBER NUMBER_FIRST STREET_NAME
#> 1          <NA>       <NA>         412            5      DUDLEY
#> 2          <NA>       <NA>          NA           21      BRUNEL
#> 3          <NA>       <NA>           3           17      REPTON
#> 4          <NA>       <NA>           3          410  WATTLETREE
#> 5          <NA>       <NA>         220         1341   DANDENONG
#> 6          <NA>       <NA>          NA            9   WOODLANDS
#>   STREET_TYPE_CODE lat_int  lat_rem lon_int lon_rem
#> 1           STREET     -37 -8803919     145  479747
#> 2           STREET     -37 -8691318     145  521673
#> 3             ROAD     -37 -8810039     145  520930
#> 4             ROAD     -37 -8660377     145  560749
#> 5             ROAD     -37 -8875785     145  803590
#> 6            GROVE     -37 -8897857     145  847355
#>                     geometry
#> 1  POINT (145.048 -37.88039)
#> 2 POINT (145.0522 -37.86913)
#> 3   POINT (145.0521 -37.881)
#> 4 POINT (145.0561 -37.86604)
#> 5 POINT (145.0804 -37.88758)
#> 6 POINT (145.0847 -37.88979)
```

## 6\. Display sample addresses and postcodes

Note that there are 44000 random addresses. Plotting this many points
can be quite slow using `mapview`, so if you want to view the results,
you might need to be patient. (Much faster plotting can be achieved with
an API key via `mapdeck`.)

``` r
mapview(randomaddresses, cex = 2, color = "blue")
```

![](map2.png)

These postcode polygons as shown above can be viewed with `mapview` like
this:

``` r
mapview (basicDemographicsRehab)
```

## 7\. Create a street network database

Road distance and travel time from each address to each hospital can be
computed using a database of the street network within the bounding
polygon defined by `basicDemographicsRehab`. Street network data can be
obtained from OpenStreetMap using the `dodgr` package, which calls the
`osmdata` package to do the downloading.

Use of a carefully selected polygon of interest will, in many cases,
dramatically reduce the download volume compared to a simple rectangular
bounding box. Here we use the polygon defined by our nearby postcodes,
which first need to be re-projected onto the CRS of OpenStreetMap data.
`st_union` merges all of the polygons to form the single enclosing
polygon, and the final command extracts the coordinates in a form
required for the dodgr query.

``` r
bounding_polygon <- sf::st_transform(basicDemographicsRehab,
                                     sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
bounding_polygon <- bounding_polygon [, 1:2]
```

We can now download the street network enclosed within the polygon. Note
that this is still a rather large network - over 40MB of data
representing over 60,000 street sections - that might take a minute or
two to process. It is therefore easier to save the result to disc for
quicker re-use.

``` r
library(dodgr)
system.time (
dandenong_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
)
saveRDS (dandenong_streets, file = file.path(here::here(),"RehabCatchment", "dandenong-streets.Rds")
```

This generates a file that is 41 MB in size. The network can then be
re-loaded
with

``` r
dandenong_streets <- readRDS (file.path(here::here(),"RehabCatchment", "dandenong-streets.Rds"))
```

revealing that the number of distinct street lines in this street
network is

``` r
format (nrow (dandenong_streets), big.mark = ",")
#> [1] "62,624"
```

## 8\. Estimation of travel time

Travel time is estimated using distance along the stret network. The
`dodgr` package needs to decompose the `sf`-formatted street network,
which consists of long, connected road segments, into individual edges.
This is done with the `weight_streetnet()` function, which modifies the
distance of each edge to reflect typical travel conditions for a
nominated mode of transport.

``` r
library (dodgr)
net <- weight_streetnet (dandenong_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#> [1] "626,391"
```

This command has decomposed the 62,624 streets into 626,391 disinct
segments. The resultant network has a `d_weighted` column which
preferentially weights the distances for the nominated mode of tranport.
Those parts of the network which are unsuitable for vehicular transport
have values of `.Machine$double.xmax =` `r .Machine$double.xmax`.
Because we want to align our random points to the *routable* component
of the network, these need to be removed.

``` r
net <- net [which (net$d_weighted < .Machine$double.xmax), ]
nrow (net)
#> [1] 437145
```

This reduces the number of edges in the network to 437,145. We can now
use the `net` object to calculate the distances, along with simple
numeric coordinates of our routing points, projected on to the same CRS
as OpenStreetMap (OSM), which is
4326:

``` r
fromCoords <- st_coordinates (st_transform (randomaddresses, crs = 4326))
toCoords <- st_coordinates (st_transform (RehabLocations, crs = 4326))
```

Although not necessary, distance calculation is quicker if we map these
`from` and `to` points precisely on to the network itself. OSM assigns
unique identifiers to every single object, and so our routing
coordinates can be converted to OSM identifiers of the nearest street
nodes. The nodes themselves are obtained with the `dodgr_vertices()`
function.

``` r
nodes <- dodgr_vertices (net)
fromIDX <- match_pts_to_graph (nodes, fromCoords, connected = TRUE)
from <- unique (nodes$id [fromIDX])
to <- nodes$id [match_pts_to_graph (nodes, toCoords, connected = TRUE)]

## Store each addresses 
randomaddresses <- mutate(randomaddresses, NodeIDX=fromIDX, GraphNodeID=nodes$id[fromIDX])
```

The matrices of `from` and `to` coordinates have now been converted to
simple vectors of OSM identifiers. Calculating the pair-wise distances
between all of those coordinates is as simple as,

``` r
d <- dodgr_dists (net, from = from, to = to)
```

And that takes only around 1.0 seconds to calculate distances between (3
rehab centres times 20,000 random addresses = ) 60,000 pairs of points.
Travel times may then be presumed directly proportional to those
distances.

## 9\. Address-based catchment basins

First assign each point to its nearest hospital according to the street
network distances returned from `dodgr_dists`. Note that points on the
outer periphery of the network may not necessarily be connected to the
main part of the network, as we’ll see below. The following code assigns
each source address to the nearest destination.

``` r
DestNames <- c(rownames(RehabLocations), "Disconnected")
DestNumber <- as.numeric (apply(d, MARGIN=1, which.min))
DestNumber [is.na (DestNumber)] <- 4 # the disconnected points
BestDestination <- DestNames[DestNumber]
table (BestDestination)
#> BestDestination
#>     CaseyHospital DandenongHospital      Disconnected  KingstonHospital 
#>              3921              9332                26             11579
```

And there are 26 points that are not connected. The allocation of
points, including these disconnected ones, can be inspected on a map
with the following code, start by setting up a `data.frame` of
`fromCoords`.

``` r
fromCoords <- nodes [match (from, nodes$id), ]
fromCoords$DestNumber <- DestNumber
fromCoords$Destination <- BestDestination
```

The results can be viewed with `mapview`, first requiring these points
to be converted to `sf` form, where `coords = 2:3` simply specifies the
longitude and latitude columns, and the `select` command filters the
data down to just the geometrical points and the `DestNumber`, so the
latter will be automatically used to colour the `mapview` points.

``` r
fromCoords_sf <- st_as_sf (fromCoords, coords = 2:3, crs = 4326) %>%
    select (c (DestNumber, geometry))
mapview (fromCoords_sf)
```

![](map4.png)

This map (in its interactive form) clearly reveals that the 26
destinations that are disconnected from the street network all lie in
the periphery, and can be simply discarded.

## 10\. Polygon catchment basins

As a final step, we’ll convert those clusters of points into enclosing
polygons, using a Voronoi tesselation. `sf::st_voronoi` doesn’t return
the polygons in the same order as the original points, requiring a
manual re-sorting in order to use this to match voronoi polygons to
points for each catchment.

``` r
g <- st_multipoint(as.matrix(fromCoords[,c("x", "y")]))
v <- st_voronoi(x=g) # results in geometry collection objects
v <- st_collection_extract(v) # converts to polygons
fromCoords_sf <- st_as_sf(fromCoords, coords=c("x", "y"))
vorder <- unlist(st_intersects(fromCoords_sf, v))
v <- v[vorder] # polygons in same order as points
v <- st_sf (DestNumber = fromCoords$DestNumber,
            Destination = fromCoords$Destination,
            geometry = v,
            crs = 4326)
```

We then combine the Voronoi polygons associated with each rehabilitation
centre to produce larger polgons defining the each catchment region.

``` r
bounding_polygon <- sf::st_transform(basicDemographicsRehab,
                                     sf::st_crs(4326)) %>%
  sf::st_union () 
v <- lapply (1:3, function (i) {
                 v [v$DestNumber == i, ] %>%
                     st_intersection (bounding_polygon) %>%
                     st_union() })
v <- st_sf (DestNumber = 1:3,
            Destination = DestNames [1:3],
            geometry = do.call (c, v))
```

Then plot with `mapview`, with easy addition of rehab centres:

``` r
mapview (v) %>%
    addFeatures (data = RehabLocations)
```

![](map5.png)

## 11\. Estimate caseload per centre

Finally, we use a per postcode breakdown of proportion of addresses
going to each centre, so that we can compute the number of cases going
to each centre.

In step 8 above we recorded the node id of each address. We now join the
destination to the random address information based on the node id,
allowing us to produce per postcode summaries, and thus per
rehabilitation centre
estimates.

``` r
randomaddresses <- left_join(randomaddresses, fromCoords, by=c("GraphNodeID"="id"))
postcodes <- st_set_geometry(randomaddresses, NULL) %>% group_by(POSTCODE, Destination) %>% summarise(n=length(DestNumber))
head (postcodes)
```

| POSTCODE | Destination       |    n |
| -------: | :---------------- | ---: |
|     3145 | KingstonHospital  | 1000 |
|     3147 | Disconnected      |    9 |
|     3147 | KingstonHospital  |  991 |
|     3148 | KingstonHospital  | 1000 |
|     3149 | DandenongHospital |   63 |
|     3149 | Disconnected      |    4 |

This table provides the breakdown for each postcode of cases going to
each rehab centre. We simply need to allocate all of these to each
centre with the following code, which converts the final estimated total
cases to each centre into relative proportions.

``` r
postcodes %>%
    filter (Destination != "Disconnected") %>%
    group_by (Destination) %>%
    summarise (total = sum (n)) %>%
    mutate (percent = 100 * total / sum (total))
```

| Destination       | total | percent |
| :---------------- | ----: | ------: |
| CaseyHospital     |  5998 |   13.64 |
| DandenongHospital | 14244 |   32.40 |
| KingstonHospital  | 23719 |   53.95 |

Those results reflect random samples from each postcode, and so do not
reflect possible demograhic differences in stroke rates between
postcodes. That can be derived using the following table of stroke
incidence per 100,000:

| Age   | Incidence |
| ----- | --------- |
| 0-14  | 0         |
| 15-24 | 5         |
| 25-34 | 30        |
| 35-44 | 44        |
| 45-54 | 111       |
| 55-64 | 299       |
| 65-74 | 747       |
| 75-84 | 1928      |
| 85+   | 3976      |

We have the demographic profile of each postcode in
`basicDemographicsRehab`, for which we now need to regroup some of the
columns (0-4 + 5-14, and 15-19 + 20-24). This then gives the total
population for that postcode for each demographic group, from which we
can work out the expected stroke incidence. The following code also
removes previous demographic columns (the `select` line).

``` r
basicDemographicsRehab <- basicDemographicsRehab %>%
        select(-starts_with("POA_"))
```

``` r
s <- 1 / 100000 # rate per 100,000
basicDemographicsRehab <- basicDemographicsRehab %>%
    mutate (stroke_cases = s * ((Age_15_19_yr_P + Age_20_24_yr_P) * 5 +
            Age_25_34_yr_P * 30 +
            Age_35_44_yr_P * 44 +
            Age_45_54_yr_P * 111 +
            Age_55_64_yr_P * 299 +
            Age_65_74_yr_P * 747 +
            Age_75_84_yr_P * 1928 +
            Age_85ov_P * 3976)) %>%
    select (-c (contains ("_yr_"), contains ("85ov")))
```

The per postcode estimate of stroke cases is then joined to our
simulation
data.

``` r
basicDemographicsRehab <- rename (basicDemographicsRehab, POSTCODE = Postcode)
postcodes <- left_join (postcodes, basicDemographicsRehab, by = "POSTCODE") %>%
    select (POSTCODE, DestNumber, Destination, stroke_cases)
postcodes
```

| POSTCODE |    n | AREA\_SQKM | Destination       | stroke\_cases |
| -------: | ---: | ---------: | :---------------- | ------------: |
|     3145 | 1000 |       8.98 | KingstonHospital  |         66.22 |
|     3147 |    9 |       5.38 | Disconnected      |         48.22 |
|     3147 |  991 |       5.38 | KingstonHospital  |         48.22 |
|     3148 | 1000 |       3.00 | KingstonHospital  |         21.98 |
|     3149 |   63 |      15.18 | DandenongHospital |        130.96 |
|     3149 |    4 |      15.18 | Disconnected      |        130.96 |
|     3149 |  933 |      15.18 | KingstonHospital  |        130.96 |
|     3156 |    1 |      53.51 | CaseyHospital     |        103.44 |
|     3156 |  974 |      53.51 | DandenongHospital |        103.44 |
|     3156 |   25 |      53.51 | Disconnected      |        103.44 |

The number of random addresses with valid destinations is then included
in our postcodes data set.

``` r
postcodesamples <- filter(postcodes, Destination != "Disconnected") %>% 
  group_by(POSTCODE) %>% 
  summarise(totalsamples=sum(n))
postcodes <- left_join(postcodes, postcodesamples, by="POSTCODE")
```

Finally the proportion of cases from a postcode attending a
rehabilitation center can be computed by dividing the number of random
addresses attending a center by the total number of random addresses
(usually 1000). The number of cases from a postcode attending a center
is therefore the estimated stroke case count for the postcode multiplied
by that proportion. The total loading can be computed by adding the
contributions from all postcodes.

``` r
postcodes %>%
    filter (Destination != "Disconnected") %>%
    group_by (Destination) %>%
    summarise (total = sum (stroke_cases * n/totalsamples)) %>%
    mutate (percent = 100 * total / sum (total))
```

| Destination       | total | percent |
| :---------------- | ----: | ------: |
| CaseyHospital     |   287 |   10.60 |
| DandenongHospital |   882 |   32.54 |
| KingstonHospital  |  1541 |   56.86 |
