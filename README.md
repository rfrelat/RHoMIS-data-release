# RHoMIS dataset and analysis of large scale farm household characteristics

Script and data material related to :  
**Against disposable data: a large-scale, standardised database on smallholder farming practices and development outcomes** by Léo Gorman, James Hammond, Romain Frelat, Mark Caulfield,  Dan Milner, ... and Mark van Wijk.



The original RHoMIS datasets are stored in two csv files in the Data folder (full_survey_data.csv and indicator_data.csv). The data was transformed to [farmhousehold data format](https://github.com/rfrelat/farmhousehold) and stored in the file HHDB_RHOMIS_02122024.Rds.   


The scripts and analysis are organised as follow:  

- 1_Get_RHoMIS.R : transform the RHoMIS data into farmhousehold data format (the file HHDB_RHOMIS_02122024.Rds is created)  
- 2_Example_HDDS.R : perform the analysis of household-level drivers of dietary diversity according to farming system (Figures 2, 3 and 9; Table 2)  
- 3_Example_Gender.R : run the gender analysis over agricultural production and income (Figures 4, 5 and 6).






[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)



