# crop_model_analysis #
These are R files for the analysis and visualization of aquacrop Senegal crop model.

### File descriptions and usage ###
***millet_factors.R*** : Start with this file. It takes the yield summary results generated in the python aquacrop output (millet_FINAL_xxx.csv) and merges it with the soil and weather data to create a master file output with yield and many potential soil and weather factors. Output: millet_factors_fullrun.csv is used as a starting point in other routines below.

***soil_summary.R*** : This file encapsulates useful soil routines and is a supporting script to millet_factors.R. It has a method for combining the first 4 layers (60cm) of soil into aggregated texture components. It uses those texture components to assign a USDA texture class to the soil. Finally it has a a method get_soil() that wraps the other methods to return the complete soil datatable to other routines that might need it.  Relies on inputfile:  AEZ_samples_with_soil3.csv

***weather_summary.R***: This file encapsulates reading weather data and creating monthly aggregated statistics. It is used by millet_factors.R to make the master list of yield influencing factors. The collect_weather_inputs() function relies on per zone csv input files named 'XXXX_weather_from_python.csv' to create a datatable of month_weather_summary used by other routines. 


