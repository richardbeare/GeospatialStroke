# Morgue section of the paper

The "Morgue" section of a paper is used as a place to gather (see [wiki article on this idea](https://en.wikipedia.org/wiki/Morgue_file)).


## Examples

Need the right clinical connections for these. Will discuss with Thanh.


The examples will provide a framework for introducing the concepts and packages. We’ll avoid a detailed discussion of theory.


Monash Health - Hospital details:

Monash Medical Centre (acute) : 246 Clayton Rd, Clayton VIC, 3168

Addresses that OSM finds OK
Dandenong Hospital (acute and rehab) : Dandenong Hospital, Dandenong VIC 3175, Australia
Casey Hospital (acute and rehab) : 62-70 Kangan Dr, Berwick VIC 3806
Kingston Hospital (rehab) : The Kingston Centre, Heatherton VIC 3202, Australia


### Visualisation of geospatial data

* Choropleths
* points on maps
* interactivity. 
* Maps as data.
* Complex visualisation for training (map deck). 
* Travel time isochrones.


[This link might be of interest for isochrones](https://stories.thinkingmachin.es/airport-iso/)
David Cooley - sketching out the visualisation part as you see it.


### A worked example

### Geocoding. What is it, why is it.

Suppose a study has collected patient addresses. Summarising the addresses via a map is useful and may be accomplished in a number of ways. Individual points on the map, coloured by various characteristics, is useful if there is a relatively small number of addresses. This requires geocoding of addresses - i.e. translating the street address to a latitude/longitude. Larger numbers of addresses may be more usefully examined by regional summaries, such as postcodes, or by heat maps. Heatmaps also need geocoding.


Careful to avoid too much visualisation info here.


Reverse geocoding - application to simulation studies.
[Also note the PSMA package by Hugh Parsonage](https://github.com/HughParsonage/PSMA) - it needs a good README, but [this issue](https://github.com/HughParsonage/PSMA/issues/2) outlines its use. PSMA core dataset needs to be discussed in the sources of data section.
Geocoding - I use dismo::geocode for a very flexible interface to the Google API, but many others exist now so list the good ones .... Dodgr is king for calculating distances on networks, and integrates with network transport data from OSM. The best geocoding tool for Australia is PSMA, as it is super super fast due to no internet/API situation. 


## Choropleth

The choropleth is a classical geospatial visualization technique in which regions are coloured by a statistic or classification (see figure …). They are widely used to display political and demographic data (see figure …). This example demonstrates choropleths to display predicted stroke cases in the vicinity of out health network. Three hospitals are used to display services for acute stroke: 1) Monash medical centre, Clayton; 2) Dandenong; and 3) Casey. There are five steps to create a choropleth:

1. Retrieve census data :
   1. Using Australian 2016 census, obtain:
      1. Demographics
      2. Shape files for post code boundaries (note: explain what a shape file is)
1. Generate relevant post codes 
   1. E.g., bounding box around monash medical center
1. Use incidence data to estimate strokes per post code 
   1. (Have this set up from previous study)
   2. (note: incidence of what? Stroke data? Where is this from?)
   3. (note: what sort of model is used to estimate incidence? This can get complex)
1. Tidy data so each variable forms a column
   1. Discuss the idea of nested columns with simple features?
1. Display with ggmap
   1. (Note: before displaying with ggmap, the data needs to be in tidy form, so that the columns/variables can be mapped to graphical aesthetics.)
   2. Note: What columns/variables do we have?
1. Alternative display with osmplotr (maps as data idea) - colour roads by stat.
   1. Note: how does this change the data structure requirements? 


#### Retrieving census data

Illustrate loading and using census data, both spatial and demographic.


#### Generate relevant post code

Base on post codes in vicinity of a hospital. 


#### Estimate stroke incidence around post code

Point out that we could colour by hospital attendance data. 


#### Tidy data


#### Visualise

* Using ggmap
* Using osmplotr
* Using leaflet
* Using mapdeck
* Key point: demonstrate that once the data structure is complete, the visualisation is not as much work


#### Discuss

* Highlight that same procedure can be used with real hospital data.

### Extending choropleths (Geocoding)

Choropleths are useful for summarising large scale data, but what if we want more detail. For example, display of location of new cases may be useful for planning outpatient rehab services. Rehab services are provided by Dandenong, Casey and Kingston. We now modify the previous example to include individual information, which we generate randomly. 

1. Generate random addresses within each postcode (using database).
2. Generate coordinates from each address (database and google)
3. Display points 
4. Colour by something stroke related (random), shape by sex.


Note:

* Are these plots choropleths or are they points on a map?
* Is this more of a modelling / simulation type situation?

## Distances

Note:
* There should be a section on "Calculating Distances", which describes the process, and some of the features that need to be accounted for.
* Each of these "atomic" pieces of geospatial data analysis can then be combined in illustrative examples


## Routing and travel times

The APIs from Google etc, and what they allow. Tools for querying them.


Ad hoc routing and travel time calculations: dijkstra and a* algorithms, weighted by distance (graph edge length) or by free-flow travel time (a function of edge length and inferred/imputed speed limit) or by congested travel time (when congestion/slowdown information is available, either from traffic simulation or real-time data).


* [Dodgr package by Mark Padgham](https://github.com/ATFutures/dodgr) could help with calculation of distances using graphs

## Sources of curated data

This should cover specifics on how to load curated data, the nature of the data, how shapes are represented. Some ideas on data types:


* Australian census
   * Shapefiles
   * Demographics
* Eechidna package
* Tidycensus package (USA)
* ABS geographic data structures  [ASGS package by Hugh Parsonage](https://github.com/HughParsonage/ASGS)
* OpenStreetMap (easily loaded, analyzed, visualized with Python/OSMnx!)
* [osmdata package](https://github.com/ropensci/osmdata)
   * Application: [Rwalkable package](https://github.com/ropenscilabs/rwalkable) (finds points of interest near a location)

1-In this article, we review and illustrate the use of these tools to show the transportation of patients in our hospital network (Monash Health) as the patients transit from acute to rehabilitation hospitals.


2-movement of doctors in hospital-responding to code stroke. Can mapbox and mapdeck help?


3-movement of family visiting patients in hospital-can mapbox and mapdeck help?


### Computation on geospatial data

Note:
* This section states that geospatial info needs to be pre-processed
* It needs to explain why this pre-processing needs to happen


Geospatial information typically needs to be transformed/preprocessed/combined multiple sources before being used in statistical analysis. May involve assigning samples to regions, sampling within regions, smoothing, interpolation (krigging etc), gridding, tesselation, combining data from different scales. Geocoding. Travel times, road distances.


**Mark P and Michael Sumner to throw some ideas in here. What are the key computations, representations needed. Where are they in R-package land?**


Assigning samples / Aggregation - point-in-polygon tests as engine to aggregate values-at-points into larger regions, requires known-coordinates for point and polygon data (ability to transform), and efficiencies for tree-search, i.e. we don’t test if a point is in a polygon if it falls outside the polygon’s bounding box. The simple features package sf is the state of the art, relying on compiled code to calculate p-in-p and bbox filtering. For given situations (an acceptable overall precision, or unchanged regions that are used for more summaries at later times) it can be easier and more efficient to use a rasterized version of polygons and so fasterize/raster is is the right tool.  Our tools sometimes 1) identify polygon 2) index values 3) acculate summaries all in one, but sf has included improvements on being able to keep these as separate steps, and if the rasterize version can be used it’s easy to manage in data frames because the relationship is trivial. 


Sampling within regions - sp::spsample established sophisticated sampling techniques for within regions, along lines, with various rules. The spatial package had some early work in this, and spatstat also is very sophisticated but doesn’t include knowledge of map projections or ability to transform coordinates, and has less capacity for complex sets of regions as sf does. 


Smoothing, interpolation, kriging are closely related in intent but use quite different methods. The gstat package is the strongest here, but has not yet been updated to sf-era. Raster simplifies some of the capabilities here, spatstat includes some tools, and there are many new packages with some support. The simplest kind of gridding is to count points within raster cells, or to aggregate point values within raster cells - and raster is still the best at this, and can be scaled up on this task extremely well with dplyr. 


Tesselation involves breaking up regions into smaller, simpler pieces and keeping them grouped as per their source region. This might be used to compartmentalize a region into subregions, to speed up overlay algorithms or to provide greater structure for regions within larger regions, perhaps for visualization. RTriangle (available via sfdct) is the only real contender here for speed and flexibility, and has a non-commercial-use license so inaccessible to some users. 


# Thoughts / ideas to come back to


Geoff Boeing

* Do we want to scope this more narrowly? Some subset of all medical researchers? Certain types/uses of geospatial information?
