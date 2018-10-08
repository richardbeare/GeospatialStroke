# METADATA FOR DIGITAL BOUNDARY FILES

Australian Statistical Geography Standard (ASGS) Volume 3 - Non-ABS Structures (cat no. 1270.0.55.003)

**Data Currency**: 12 July 2016
**Presentation Format**: Digital boundaries
**Custodian**: Australian Bureau of Statistics (ABS)

## DESCRIPTION
**Abstract**:
The Australian Statistical Geography Standard (ASGS)  brings together in one framework all of the regions which the ABS and many others organisations use to collect, release and analyse geographically classified statistics. The ASGS ensures that these statistics are comparable and geospatially integrated and provides users with an coherent set of standard regions so that they can access, visualise, analyse and understand statistics.  The 2016 ASGS will be used for the 2016 Census of Population and Housing and progressively introduced into other ABS data collections. The ABS encourages the use of the ASGS by other organisations to improve the comparability and usefulness of statistics generally, and in analysis and visualisation of statistical and other data.

This product, **Australian Statistical Geography Standard (ASGS) Volume 3 - Non-ABS Structures** (cat no. 1270.0.55.003), is the third in a series of five volumes that describe the structures that make up the ASGS. Its purpose is to outline the conceptual basis for the design of the Non-ABS Structures. This product contains several elements including the manual, region names and codes and the digital boundaries current for the ASGS Edition 2016 (date of effect 1 July 2016).

The digital boundaries for Volume 3 of the ASGS are the region types for supported non-ABS structures. These region types are: 
* Local Government Area (LGA)
* Postal Area (POA)
* State Suburb (SSC)
* Commonwealth Electoral Division (CED)
* State Electoral Division (SED)
* Natural Resource Management Region (NRMR)
* Australian Drainage Division (ADD)

**File Nomenclature**:
File names have the format `[type]_[YYYY]_[COVERAGE]` where: 
`[type]` represents the type of boundaries in each file
* LGA = Local Government Areas
* POA = Postal Areas
* SSC = State Suburbs
* CED = Commonwealth Electoral Divisions
* SEC = State Electoral Divisions
* NRMR = Natural Resource Management Region
* ADD = Australian Drainage Division

`[YYYY]` represents the Australian Statistical Geography Standard (ASGS) Edition by year. `2016` is the current edition.
`[COVERAGE]` indicates the geographic area covered by the data as defined in the ASGS manual. The value will be `AUS` for the non-ABS structures.

**State and Territory Codes and Names**
Within the files, the States and Territories are identified by unique one digit codes.
| Code | State/Territory | 
|------|-----------------|
| 1 | New South Wales  |
| 2 | Victoria |
| 3 | Queensland |
| 4 | South Australia |
| 5 | Western Australia | 
| 6 | Tasmania |
| 7 | Northern Territory |
| 8 | Australian Capital Territory |
| 9 | Other Territories |

**Australia**
The code for Australia is shown as `036` where it appears as the parent geography of data released on a State and Territory level, or where coverage is for the whole of Australia.

This allows alignment with both the UN Statistical Division's *"Standard country or area codes for statistical use (M49)"* ( https://unstats.un.org/unsd/methodology/m49/ ) and ISO 3166-1 *"Codes for the representation of names of countries and their subdivisions"* alpha-3 codes ( https://www.iso.org/iso-3166-country-codes.html )

### File Attributes:
For each file type the field name, data type, and length is shown.

__Note__ - While metadata for each spatial unit in the ASGS is shown, any given file will only contain the referenced spatial unit, and the parent spatial units above it in the ASGS hierarchy.

**MapInfo (.mid/.mif & TAB) and Geopackage (.gpkg)**
| Count | Field               | Data Type | Length | Name                            |
|-------|---------------------|-----------|--------|---------------------------------|
|  1    | LGA_CODE_2016       | Character | 8      | 2016 Local Government Area Code |
|  2    | LGA_NAME_2016       | Character | 50     | 2016 Local Government Area Name |
|  3    | POA_CODE_2016       | Character | 7      | 2016 Postal Area Code           |
|  4    | POA_CODE_2016       | Character | 40     | 2016 Postal Area Name           |
|  5    | SSC_CODE_2016       | Character | 8      | 2016 State Suburb Code          |
|  6    | SSC_NAME_2016       | Character | 45     | 2016 State Suburb Name          |
|  7    | CONF_VALUE          | Character | 12     | SSC Confidence Value            |
|  8    | CED_CODE_2016       | Character | 6      | 2016 Commonwealth Electoral Division Code |
|  9    | CED_NAME_2016       | Character | 40     | 2016 Commonwealth Electoral Division Name |
| 10    | SED_CODE_2016       | Character | 8      | 2016 State Electoral Division Code |
| 11    | SED_NAME_2016       | Character | 50     | 2016 State Electoral Division Name |
| 12    | NRMR_CODE_2016      | Character | 7      | 2016 Natural Resource Management Region Code |
| 13    | NRMR_NAME_2016      | Character | 40     | 2016 Natural Resource Management Region Name |
| 14    | ADD_CODE_2016       | Character | 3      | 2016 State/Territory Code |
| 15    | ADD_NAME_2016       | Character | 40     | 2016 State/Territory Name |
| 16    | AREA_ALBERS_SQKM    | Float     | -      | Area (Albers) in sq/km    |

**ESRI Shape Files (.shp)**
| Count | Field      | Data Type | Length | Name                            |
|-------|------------|-----------|--------|---------------------------------|
|  1    | LGA_CODE   | Character | 8      | 2016 Local Government Area Code |
|  2    | LGA_NAME   | Character | 50     | 2016 Local Government Area Name |
|  3    | POA_CODE   | Character | 7      | 2016 Postal Area Code           |
|  4    | POA_CODE   | Character | 40     | 2016 Postal Area Name           |
|  5    | SSC_CODE   | Character | 8      | 2016 State Suburb Code          |
|  6    | SSC_NAME   | Character | 45     | 2016 State Suburb Name          |
|  7    | CONF_VALUE | Character | 12     | SSC Confidence Value            |
|  8    | CED_CODE   | Character | 6      | 2016 Commonwealth Electoral Division Code |
|  9    | CED_NAME   | Character | 40     | 2016 Commonwealth Electoral Division Name |
| 10    | SED_CODE   | Character | 8      | 2016 State Electoral Division Code |
| 11    | SED_NAME   | Character | 50     | 2016 State Electoral Division Name |
| 12    | NRMR_CODE  | Character | 7      | 2016 Natural Resource Management Region Code |
| 13    | NRMR_NAME  | Character | 40     | 2016 Natural Resource Management Region Name |
| 14    | ADD_CODE_  | Character | 3      | 2016 State/Territory Code |
| 15    | ADD_NAME_  | Character | 40     | 2016 State/Territory Name |
| 16    | AREA_SQKM  | Float     | -      | Area (Albers) in sq/km    |

**Note**: CONF_VALUE field provides an indicator of how accurately the SSC represents the suburb/locality based on the percentage of common population. The values that are applied to each SSC are:
* 94% and above common population - very good
* 88 to less than 94% common population - good
* 75 to less than 88% common population - acceptable
* 50 to less than 75% common population - poor
* Less than 50% common population - very poor

### XML METADATA FILE
The compressed download files include geospatial metadata data for each region type in Extensible Markup Language (XML) format. The geospatial metadata conforms to International Organisation for Standardization (ISO) geospatial metadata standard, `ISO 19115:2003`, and the associated XML implementation schema specified by `ISO 19139:2012`.

*DATA CURRENCY*
**Date of Effect**: 12 July 2016

*DATASET STATUS*
**Progress**: Completed dataset
**Maintenance and Update Frequency**:
No further updates for these boundaries planned. There will be a progressive release of the other regions that make up the ASGS until late 2018 (ASGS Volumes 4 and 5). The ASGS will be revised in 2021.

*ACCESS*
**Stored Data Format**:
Digital as separate files for each level of the Main Structure and GCCSA of the ASGS 2016.

**Available Format**:
The digital boundary files are in MapInfo TAB format (.TAB), MapInfo Interchange Format (.MID .MIF), Geopackage (.gpkg) and ESRI Shapefile (.shp) format. 

**Spatial Representation Type**: Vector

**Access Constraints**:
Copyright Commonwealth of Australia administered by the ABS.  Unless otherwise noted, content is licensed under a Creative Commons Attribution 2.5 Australia licence.

**Datum**: Geocentric Datum of Australia 1994 (GDA94)

**Projection**: Geographical (i.e. Latitudes and Longitudes)

**Geographic Extent**: Geographic Australia.

The Australian Statistical Geography Standard (ASGS) uses the Geographic definition of Australia, as set out in section 2B of the Acts Interpretation Act 1901, which currently defines Australia or the Commonwealth as meaning:

*"...the Commonwealth of Australia and, when used in a geographical sense, includes Norfolk Island, the Territory of Christmas Island and the Territory of Cocos (Keeling) Islands, but does not include any other external Territory."*

Included in this definition of Geographic Australia are the:
* States of New South Wales, Victoria, Queensland, South Australia, Western Australia and Tasmania
* Northern Territory
* Australian Capital Territory (ACT)
* Territory of Cocos (Keeling) Islands
* Territory of Christmas Island
* Jervis Bay Territory
* Territory of Norfolk Island

**Extent - Geographic Bounding Box**:

* North Bounding Latitude: -8
* South Bounding Latitude: -45
* West Bounding Latitude: 96
* East Bounding Latitude: 169

*DATA QUALITY*
**Lineage**:
Mesh Block boundaries were created using various sources including the PSMA digital datasets and ABS boundaries, zoning information from state planning agencies and imagery.

**Positional Accuracy**:
Positional accuracy is an assessment of the closeness of the location of the spatial objects in relation to their true positions on the earth's surface. The positional accuracy includes both a horizontal accuracy assessment and a vertical accuracy assessment. Positional accuracy for ABS boundaries is dependent on the accuracy of the features they have been aligned to. ABS boundaries are aligned to a number of layers supplied by the PSMA with an accuracy of `+/-50 mm`. PSMA layers and their positional accuracy are as follows: 
> ***Transport and Topography***
> `+/- 2 meters` in urban areas and `+/- 10 meters` in rural and remote areas
> ***CadLite***
> `+/- 2 meters` in urban areas and `+/- 10 meters` in rural and remote areas
> ***Administrative Boundaries***
> Derived from the cadastre data from each Australian State and Territory jurisdiction. 
> ***Greenspace and Hydrology*** 
> 90% of well-defined features are within `1mm` (at plot scale) of their true position, eg `1:500` equates to `+/- 0.5metre` and `1:25,000` equates to `+/- 25 metres`. 
> 
> Relative spatial accuracy of these themes reflects that of the jurisdictional source data. The accuracy is `+/- 2 metres` in urban areas and `+/- 10 metres` in rural and remote areas.

No "shift" of data as a means of "cartographic enhancement" to facilitate presentation has been employed for any real world feature. 

**Attribute Accuracy**:
All codes and labels for all levels within the ASGS non-ABS Structures are fully validated as accurate at time of release.

**Logical Consistency**:
Regions are closed polygons. Attribute records without spatial objects have been included in the data for administrative purposes.

**Completeness**:
All levels of the non-ABS structures supported within the 2016 ASGS are represented.

*CONTACT INFORMATION*
**Contact Organisation**: Australian Bureau of Statistics
For further information email <client.services@abs.gov.au> or contact the National Information and Referral Service (NIRS) on `1300 135 070`.