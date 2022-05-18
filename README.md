# FreshwaterBio  
## Study to model freshwater biodiversity using projected climate conditions and asses the benefits of management interventions   
### R Scripts and the datasets they produce


1. Tidying_CT_fish.R - Cleans and formats fish survey data collected by the CT DEEP

    + This script uses the raw fish database provided by the CT DEEP and produces three final files *ct_method.RData*, *ct_event.RData*, and *ct_fish.RData*, which are saved here: C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata/
    + ct_event.RData:
        +  UID - unique ID starting with MA
        +  state - the state of the sampling event
        +  date - date of sampling event
        +  waterbody - whether the event occurred in a lotic or lentic environment (determined by searchng for 'lake' or 'pond' in the location name)
        +  latitude - latitude of sampling event
        +  longitude - longitude of sampling event
        +  project - the group that went sampling
        +  source - the person and agency who provided the data
    + ct_method.RData:
        +  UID - unique ID starting with MA
        +  gear - the equipment used, for example, backpack efish, seine, etc
        +  goal - targeted vs total pick up
        +  target - if a targeted survey, the spp they were targeting
        +  reach_length_m - length of the stream surveyed, in m
        +  avg_reach_width_m - average width of the stream surveyed, in m. This may have been calculated differently in different surveys, so is not totally reliable, but serves as a decent representation of the stream for high level comparisons. 
        +  efish_duration_s - If the gear is a efish method, this is the duration of the time spend electrofishing, in seconds
        +  efish_runs - If the gear is a efish method, this is the total number of runs the survey team completed during the survey. Contrast with run_num in the fish data set.
    + ct_fish.RData:
        +  UID - unique ID starting with MA
        +  scientific_name - organism scientific name
        +  lenth_mm - the length of each fish, if measured during the survey, in mm
        +  run_num - the numbered run that the fish was observed. NOT how many runs total that survey event included, but the individual run where this fish was measured.
        +  count - the total number of that species of fish that was observed during the survey. This will be repetitive if there were multiple fish of the same spp caught and measured, because it is the total number of spp per survey. I did it this was so that we could either groupby UID and scientific name and sum the number of rows, OR select for the UID and the scientific name and use unique() and both would give the same count.  


2. Tidying_Ma_Fish.R - Cleans and formats fish survey data collected by MassWildlife

    + This script uses the raw fish Access database provided by Mass Wildlife and produces four final files *ma_method.RData*, *ma_event.RData*, *ma_species*, and *ma_fish.RData*, which are saved here: C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata/
    + ma_event.RData:
        +  these fields are identical to the ct_method.RData file desscibed above.
    +  ma_fish.RData: 
        +  weight_g - the weight of each fish, if weighed during the survey, in grams
        +  the remaining fields are identical to the ct_fish.RData file desscibed above.
    +  ma_method.RData:
        +  daylight - whether the survey occured at night or during the day
        +  the reaming variables are the same at ct_method.RData, but there is no 'target' column, this is because it was not easy to figure out what the target was during targeted surveys because even during the targeted surveys they still often recorded all the species that they captured.
        +  efish_runs - it is important to look at the 'gear' type when interpreting this column.  a large value is probably associated with something other than efish (like multiple nets or minnow traps).  I lumped all runs and passes into this column (probably should not have done that), so the gear type is important here.
    +  ma_species.RData - except for renaming variables, no editing was done to this file and it is essentially the same table in the orginal access database.
        +  common_name - species common name
        +  scientific_name - species scientific name
        +  origin - native vs not native (for MA, this will not necessarily be the same in other new england states)
        +  temperature_preference - need to clarify with Jason S what each value means (warm, cold water...)
        +  tolerance - tolerant, intolerant, need to clarify what values mean
        +  eco_function - functional group in stream
        +  stream_preference - this column is not really filled in
        
3. Tidying_NH_Fish.R - Cleans and formats fish survey data collected by the NH DES and NH DFG

    +  This script uses multiple raw files, one provided by the NH Dept of Environmental Services and a few provided by the NH Dept of Fish and Game. The same four files are produced: *nh_method*, *nh_event*, *nh_fish*, and *nh_species*.  This files contain data collected from both of these two agencies.  The final files are saved here: C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata/.  The DFG data was a little messy because it is a comibination of data collected from different agencies and projects, so it is worth reviewing the code to see the dataframe editing.
    +  nh_event: 
        + these fields are identical to the ct_method.RData file desscibed above.
    +  nh_fish.RData: 
        + These fields are the same as the ma_method.RData file described above.
    +  nh_method.RData
        +  This fields are the same as the ma_method.Rdata file described above, but we also retained in the two comment columns from the raw data for reference. The 'target' field was determined by a list of the projects/descriptiosn provided from Matt from DFG, that described the sampling mission, and by consulting with Matt. 
        +  There is no 'daylight' field.
    +  nh_species.RData:
        +  This file has only common and scientific names in a look up table. 
        
4. Tidying_RI_FIsh.R - Cleans and formats fish survey data collected by the RI DEM

    +  This script uses data from the RI Dept of Env Management (DEM) and produces three files: *ri_event*, *ri_method*, and *ri_fish*.  The three files have the same fields as those produced in the previous scripts.  The final files are saved here: C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata/.
    
5. Tidying_VT_FIsh.R - Cleans and formats fish survey data collected by the VT DEC and VT DFW

    +  This R script uses data from two sources, the VT Dept of Environmental Conservation and the VT Dept of Fish and Wildlife and produces seven files *dfw_event*, *dfw_method*, and *dfw_fish*, which uses the data from the DFW, and *dec_event*, *dec_method*, *dec_fish*, and *dec_species*, which uses data from the DEC.  NOTE: unlike New Hampshire, where I combined the data collected by the two different agencies into a single file, I kept data from these two agencies separate only because I recieved the DFW data much later and had already produced and used the DEC data in future scripts. The final files are saved here: C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata/.   
    +  all dataframe produced has the same meanings as in previous scriptes
    
6. tidying_fish_all.R - combines the already cleaned fish survey data sets to create a set of master fish survey data files.

    +  This R script using the cleaned event, method, and fish datasets that were described in the previous sections, for example, ma_event, ma_method, and ma_fish.  It joins each of the state by state dataasts and does additional cleaning to make them compatible.  
    +  It saves five files: *all_fish_event.RData*, *all_fish_count.RData*, *all_fish_size.RData*, *all_fish_presence.RData*, and *all_fish_method.RData* to the following folder: C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata/
    +  It also produces two spatial files projected in the same projection as the NHD data: *all_fish_count.shp* and *all_fish_event.shp*, which are saved here: C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/sppdata/.  
    +  We also create two look up tables that are saved to this github repo:
        +  fishspp_count.csv - fish common name, scientific name, and total count across all surveys
        +  fishgenus_count.csv - fish genus and total count across all surveys

7. tidying_mussel.R

     + currently working on this data file - fill in when finished

### Rmarkdown Files

1. fishdataexploration.Rmd

    +  This file has code that explores the data, but does not write any file analysis files.  It invetigates the different method types, fish data that was collected. It also does some spaital analysis by reading in the differnet NHD layers and then joining across state lines - however, these files are not going to be used because we ended up just downloaded the NHDPlus data for our region of interest (Francis did this part). Maybe figures are written to the github tmpfigures folder.
    +  Fish distribution plots are made on line 489 using the all_fish_count.shp and saved in tmpfigures/fish_dist_plots/, a file in the github folder, but has not be pushed to the remote repo.
    
2. fishanalysis.Rmd

     + currently working on this data file - fill in when finished




