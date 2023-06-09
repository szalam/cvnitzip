## Description
The repo contains code to preprocess nitrate data and analyze/visualize at zipcode level. Large or sensitive dataset is not available in this repo but was imported from local directories.


```
├───data 										<stores raw data, processed data, shapefiles, and visuals>
│   │
│   ├───processed 								<processed data exported by codes in this repo or prepared separately elsewhere>
│   │   │
│   │   └───shapefile							<shapefiles created>
│   │
│   ├───raw									<raw data used in the analysis. Not all data uploaded in the repo>
│   │
│   ├───shapefile								<raw shapefiles>
│   │
│   └───visual									<visuals exported>
│
└───src
    │
    └───data
            get_nitrate_at_all_zipcode.R	<get mean nitrate at all zipcodes in CA>
            get_nitrate_at_select_zipcodes.R	<get mean nitrate at user defined zipcodes in CA>
            get_subset_nitrate_data.R		<from large nitrate dataset csv subset only necessary for analysis>
            get_zipcodes_cv.R			<Create a shapefile containing zipcodes in CA>
```
