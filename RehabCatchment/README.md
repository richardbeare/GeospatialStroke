## Note on visualisations

Results generated in the following code are visualised in three distinct
ways:

1.  Using the `tmap` package which requires no “API key”, but may be
    quite slow to visualise, or may even fail (depending on machine
    capabilities);
2.  Using the `mapview` package which also requires no “API key”, and
    may often be somewhat more responsive than `tmap`; and
3.  Using the `mapdeck` package which requires an API to first be
    obtained by registering on `https://mapbox.com`, but which is must
    faster and more responsive than either `tmap` or `mapview`, and
    generally provides much better visualisations.

## —- Pre-load Libraries

``` r
library(tidyverse)
library(sf)
library(units)
library(tmaptools)
#> Warning in fun(libname, pkgname): rgeos: versions of GEOS runtime 3.7.1-CAPI-1.11.1
#> and GEOS at installation 3.7.0-CAPI-1.11.0differ
if (requireNamespace ("tmap")) # only load if installed
    library (tmap)
if (requireNamespace ("mapview"))
    library (mapview)
if (requireNamespace ("mapdeck"))
    library (mapdeck)
```

A mapbox token is required for the `mapdeck` package, which must be set
with the `set_token()` function, either by

``` r
set_token("<paste_my_token_here>")
```

or by making a file `~/.Renviron` containing the line

``` bash
MAPBOX_TOKEN="<paste_my_token_here>"
```

and then running in R:

## —- Load Census Data

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
```

Clean up the demographics to only those columns that we’re interested
in. Presume just for illustrative purposes here that those are only the
basic “Age” classes. There are also columns about the ages of persons
attending educational institutions which need to be
removed.

``` r
basicDemographicsVIC <- select(basicDemographicsVIC, POA_CODE_2016, starts_with("Age_"), -starts_with("Age_psns_"))
```

## —- JoinCensusAndBoundaries —-

Join the demographics and shape tables, retaining victoria only use
postcode boundaries as the reference data frame so that coordinate
reference system is retained.

``` r
basicDemographicsVIC <- right_join(postcodeboundariesAUS,
                                   basicDemographicsVIC, 
                                   by=c("POA_CODE" = "POA_CODE_2016"))
```

## —- GeocodeRehabNetwork —-

To be clean
up

``` r
rehab_addresses <- c(DandenongHospital = "Dandenong Hospital, Dandenong VIC 3175, Australia",
                     CaseyHospital = "62-70 Kangan Dr, Berwick VIC 3806, Australia",
                     KingstonHospital = "The Kingston Centre, Heatherton VIC 3202, Australia")
RehabLocations <- tmaptools::geocode_OSM(rehab_addresses, as.sf=TRUE)
```

transform rehab locations to the same reference system

``` r
RehabLocations <- sf::st_transform(RehabLocations,
                                   sf::st_crs(basicDemographicsVIC))
```

## Check geocoding

The following code demonstrates the three ways described above to
produce interactive map output of the locations of rehab centres, first
with `tmap`:

``` r
library(tmap)
tmap_mode("view")
tm <- tm_basemap("OpenStreetMap") +
    tm_shape(RehabLocations) +
    tm_markers()
```

with `mapview`:

``` r
mapview (RehabLocations)
```

and then with `mapdeck`:

``` r
mapdeck(location = c(145.2, -38),
        zoom = 12) %>%
    add_pointcloud (RehabLocations,
        layer_id = "rehab-locations")
```

![](map1.png)

## —- Postcodes surrounding rehab locations

There are 699 postcodes which we now want to reduce to only those within
a specified distance of the rehab locations, chosen here as 10km. Note
that we just use straight line distances here, because we only need to
roughly determine which postcodes surround our rehab centres. The
subsequent calculations will then use more accurate distances along
street networks.

``` r
dist_to_loc <- function (geometry, location){
    units::set_units(st_distance(geometry, location)[,1], km)
}
dist_range <- units::set_units(10, km)

#basicDemographicsVIC <- basicDemographicsVIC_old
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

That reduces the data down to 47 nearby postcodes, with the last 2 lines
converting all prior postcode columns (of which there were several all
beginning with “POA”) to a single numeric column named “Postcode”.

## —- SamplePostCodes —-

Select random addresses using a geocoded database

``` r
if (!"PSMA" %in% installed.packages() [, 1])
    devtools::install_github("HughParsonage/PSMA")
```

We then specify how many random addresses we want to sample per
postcode. These addresses will provide the points ultimately used to
determine case loads for the three rehab centres.

``` r
addressesPerPostcode <- 1000
```

A special function so we can sample the postcodes as we go. Sampling
syntax is due to the use of data.table inside PSMA. The last
`st_as_sf()` command converts the points labelled “LONGITUDE” and
“LATITUDE” into `sf::POINT` objects. (This function takes a few
seconds because of the `fetch_postcodes` call.)

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
#> bbox:           xmin: 145.0302 ymin: -37.86577 xmax: 145.0368 ymax: -37.85169
#> epsg (SRID):    4283
#> proj4string:    +proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs
#>   POSTCODE ADDRESS_DETAIL_INTRNL_ID STREET_LOCALITY_INTRNL_ID
#> 1     3144                 11140190                    435530
#> 2     3144                  9964454                    523275
#> 3     3144                 11156456                    461746
#> 4     3144                 12233794                    440855
#> 5     3144                 11599434                    427134
#> 6     3144                  9959649                    434378
#>   BUILDING_NAME LOT_NUMBER FLAT_NUMBER NUMBER_FIRST STREET_NAME
#> 1          <NA>       <NA>          NA           39      THANET
#> 2          <NA>       <NA>           8           64    STANHOPE
#> 3          <NA>       <NA>           3            8        PARK
#> 4          <NA>       <NA>          NA           21   CLAREMONT
#> 5          <NA>       <NA>          NA           10    PARKSIDE
#> 6          <NA>       <NA>          NA            1       PLANT
#>   STREET_TYPE_CODE lat_int  lat_rem lon_int lon_rem
#> 1           STREET     -37 -8618086     145  363917
#> 2           STREET     -37 -8585539     145  319289
#> 3           STREET     -37 -8562923     145  368138
#> 4           AVENUE     -37 -8657707     145  302313
#> 5           STREET     -37 -8598360     145  342249
#> 6           STREET     -37 -8516892     145  324336
#>                     geometry
#> 1 POINT (145.0364 -37.86181)
#> 2 POINT (145.0319 -37.85855)
#> 3 POINT (145.0368 -37.85629)
#> 4 POINT (145.0302 -37.86577)
#> 5 POINT (145.0342 -37.85984)
#> 6 POINT (145.0324 -37.85169)
```

## —- PlotSampleLocations —-

With `tmap`:

``` r
tmap_mode("view")
tm_shape(randomaddresses) +
    tm_dots(clustering=FALSE) + 
    tm_basemap("OpenStreetMap")
```

with `mapview`:

``` r
mapview(randomaddresses, cex = 2, color = "blue")
```

or with `mapdeck`

``` r
mapdeck(location = c(145.2, -38),
        zoom = 14) %>%
    add_pointcloud (randomaddresses,
                    radius = 2,
                    layer_id = "randomaddresses")
```

![](map2.png)

In contrast, the postcode polygons can be viewed with `tmap` like this:

``` r
tmap_mode("view")
tm_shape(basicDemographicsRehab) +
    tm_polygons("Postcode") + 
    tm_basemap("OpenStreetMap")
```

with `mapview` like this:

``` r
mapview (basicDemographicsRehab)
```

or with `mapdeck` like this:

``` r
mapdeck(location = c(145.2, -38),
        zoom = 12) %>%
    add_polygon (basicDemographicsRehab,
                 stroke_colour = "black",
                 stroke_width = 100,
                 #fill_colour = "Postcode",
                 fill_colour = "#22cccc",
                 fill_opacity = 250)
```

![](map3.png)

## —- AddressesToRehab —-

Compute the road distance and travel time from each address to each
hospital. This first requires a local copy of the street network within
the bounding polygon defined by `basicDemographicsRehab`. This is
easiest done with the `dodgr` package, which directly calls the
`osmdata` package to do the downloading.

### Street Network

The basic way to download the street network is within a defined,
implicitly rectangular, bounding box, but in this case that extends from
Mornington to St Kilda, and out to the Dandenongs, and even Koo Wee
Rup\! It is much better to extract the street network only within the
polygon defining our nearby postcode areas, which first needs to be
re-projected onto the CRS of OpenStreetMap data, which is epsg4326.
`st_union` merges all of the polygons to form the single enclosing
polygon, and the final command simply extracts the longitudinal and
latitudinal coordinates of that polygon (rather than leaving them in
`sf` format).

``` r
bounding_polygon <- sf::st_transform(basicDemographicsRehab,
                                     sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
bounding_polygon <- bounding_polygon [, 1:2]
```

We can now download the street network enclosed within that polygon.
Note that this is still a rather large network - over 40MB of data
representing over 60,000 street sections - that might take a minute or
two to process. It is therefore easier to save the result to disc for
quicker re-usage.

``` r
library(dodgr)
system.time (
dandenong_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
)
saveRDS (dandenong_streets, file = "dandenong-streets.Rds")
```

This generates a file that is 41 MB in size. The network can then be
re-loaded with

``` r
dandenong_streets <- readRDS ("dandenong-streets.Rds")
```

revealing that the number of distinct street lines in this street
network is

``` r
format (nrow (dandenong_streets), big.mark = ",")
#> [1] "62,624"
```

### Distances to Hospitals

The `dodgr` package needs to de-compose the `sf`-formatted street
network, which consists of long, connected road segments, into
individual edges. This is done with the `weight_streetnet()` function,
which modifies the distance of each edge to reflect typical travel
conditions for a nominated mode of transport.

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
```

The matrices of `from` and `to` coordinates have now been converted to
simple vectors of OSM identifiers. Calculating the pair-wise distances
between all of those coordinates is as simple as,

``` r
d <- dodgr_dists (net, from = from, to = to)
```

And that takes only around 0.8 seconds to calculate distances between (3
rehab centres times 20,000 random addresses = ) 60,000 pairs of points.

## —- CatchmentBasins —-

First assign each point to its nearest hospital according to the street
network distances returned from `dodgr_dists`. Note that points on the
outer periphery of the network may not necessarily be connected to the
main part of the network, as we’ll see below.

``` r
DestNames <- c(rownames(RehabLocations), "Disconnected")
# assign each source address to the nearest destination
DestNumber <- as.numeric (apply(d, MARGIN=1, which.min))
DestNumber [is.na (DestNumber)] <- 4 # the disconnected points
BestDestination <- DestNames[DestNumber]
table (BestDestination)
#> BestDestination
#>     CaseyHospital DandenongHospital      Disconnected  KingstonHospital 
#>              7420             11054               126             13633
```

And there are 126 points that are not connected. The allocation of
points, including these disconnected ones, can be inspected on a map
with the following code, start by setting up a `data.frame` of
`fromCoords`.

``` r
fromCoords <- nodes [match (from, nodes$id), ]
fromCoords$DestNumber <- DestNumber
fromCoords$Destination <- BestDestination
```

The results can be viewed with the usual 3 approaches, with both `tmap`
and `mapview` first requiring these points to be converted to `sf` form.
This in turn requires first extracting the coordinates as a simple
numeric matrix.

``` r
fromCoords_xy <- select (fromCoords, c (x, y)) %>%
    as.matrix ()
fromCoords_sf <- sapply (seq (nrow (fromCoords_xy)), function (i)
        st_sfc (st_point (fromCoords_xy [i, ])))
fromCoords_sf <- st_sfc (fromCoords_sf, crs = 4326)
fromCoords_sf <- st_sf ("DestNumber" = fromCoords$DestNumber,
                        geometry = fromCoords_sf)
```

Then the plotting via `tmap`:

``` r
tmap_mode("view")
tm_shape(fromCoords_sf) +
    tm_dots(col = "DestNumber") +
    tm_basemap("OpenStreetMap")
```

with `mapview`:

``` r
mapview (fromCoords_sf)
```

or with `mapdeck`:

``` r
mapdeck(location = c(145.2, -38),
        zoom = 10) %>%
    add_pointcloud (data = fromCoords, lon = "x", lat = "y",
                    fill_colour = "DestNumber",
                    radius = 5,
                    palette = "plasma")
```

![](map4.png)

This map (in its interactive form) clearly reveals that the 126
destinations that are disconnected from the street network all lie in
the periphery, and can be simply discarded.

## —- CatchmentBasin Polygons —-

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

We then just need to filter those Voronoi polygons associated with each
catchment, and extract the surrounding polygons. (This can take quite
some time.)

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

Then plot with `tmap` (with which it is not trivial to overlay points
marking rehab centres):

``` r
tmap_mode("view")
tm_shape(v) +
    tm_polygons(col = "DestNumber", alpha = 0.5, palette = topo.colors(3)) +
    tm_basemap("OpenStreetMap")
```

with `mapview` (with easy addition of rehab centres):

``` r
mapview (v) %>%
    addFeatures (data = RehabLocations)
```

or with `mapdeck` (likewise):

``` r
mapdeck(location = c(145.2, -38),
        zoom = 10) %>%
    add_polygon (data = v,
                    fill_colour = "DestNumber",
                    fill_opacity = 150,
                    palette = "plasma") %>%
    add_pointcloud (data = RehabLocations,
                    radius = 5)
```

![](map5.png)

## —- CasesPerCentre —-

Finally, we need a per postcode breakdown of proportion of addresses
going to each centre, so that we can compute the number of cases going
to each centre. The above procedure occasionally mapped multiple
addresses onto the same network points. As we were only interested in
the enclosing polygons, these repeated points were removed. In the
present case, however, these repeats need to be counted, so we need to
go back to where we were before, which is the `randomaddresses`.

``` r
dim (randomaddresses); dim (fromCoords)
#> [1] 56000    14
#> [1] 32233     7
length (from); length (DestNumber)
#> [1] 32233
#> [1] 32233
```

We need to repeat the calculation of `DestNumber` using the full set of
`fromCoords`, including those repeatedly matched onto same network
points.

``` r
fromCoords <- st_coordinates (st_transform (randomaddresses, crs = 4326))
fromIDX <- match_pts_to_graph (nodes, fromCoords, connected = TRUE)
from <- nodes$id [fromIDX]
to <- nodes$id [match_pts_to_graph (nodes, toCoords, connected = TRUE)]
d <- dodgr_dists (net, from = from, to = to)
DestNames <- c(rownames(RehabLocations), "Disconnected")
DestNumber <- as.numeric (apply(d, MARGIN=1, which.min))
DestNumber [is.na (DestNumber)] <- 4 # the disconnected points
BestDestination <- DestNames[DestNumber]
```

The following lines then group the above data by both postcode and
destination hospital.

``` r
postcodes <- data.frame (POSTCODE = randomaddresses$POSTCODE,
                         DestNumber = DestNumber,
                         Destination = BestDestination,
                         stringsAsFactors = FALSE) %>%
    group_by (POSTCODE, DestNumber, Destination) %>%
    summarise (n = length (DestNumber))
postcodes
```

| POSTCODE | DestNumber | Destination      |    n |
| -------: | ---------: | :--------------- | ---: |
|     3144 |          3 | KingstonHospital |  971 |
|     3144 |          4 | Disconnected     |   29 |
|     3145 |          3 | KingstonHospital | 1000 |
|     3146 |          3 | KingstonHospital |  906 |
|     3146 |          4 | Disconnected     |   94 |
|     3147 |          3 | KingstonHospital |  995 |

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

| Destination       | total |  percent |
| :---------------- | ----: | -------: |
| CaseyHospital     | 10817 | 19.44769 |
| DandenongHospital | 16325 | 29.35043 |
| KingstonHospital  | 28479 | 51.20188 |
