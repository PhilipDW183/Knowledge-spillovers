# Dissertation
This github respository contains code that uses patent data to explore the effects of knowledge spillovers over distance. The following descitpion details the way in which the workbooks have been used and in which order, to achieve this aim.

1)<b> Getting_citation.ipynb.</b></br>
This workbook firstly extract all patents that are registered within the UK and merges them with citations through the citation database. 
  
2)<b> Exploring_citation_data.ipynb.</b></br>
This workbook builds on the first by linking this with location and IPC codes for the cited patents.

3)<b> Control_data.ipynb.</b></br>
This workbook uses both the EPO and PCT datasets to create a set of potential patents for the control data. This does so by removing duplicates from the EPO and PCT datasets, and merges them together to form a large dataframe that includes patents from both the EPO and PCT datasets.

4)<b> Creating EPO controls, Creating PCT controls.ipynb</b></br>
This workbook uses the data from both 'Exploring citation data.ipynb' and 'Control data.ipynb' to create dataframes that contain cited, citing and control patents, along with their details for both teh EPO and PCT datasets. 

5)<b> Removing self controls and splitting controls.ipynb</b></br>
This workbook then uses the output from Creating EPO and PCT control workbooks and removes any self-citations from the process based on firm and application id matches.

6)<b> merging_location.R</b></br>
This workbook primarily gets the centroids for the regions from different shapefiles, inline with the regional specification used by the OECD REGPAT database for both the EPO and the PCT. Once these centroids are extracted and linked to regions, they are outputted to seperate shapefiles for use in the regional merging.ipynb workbook.

7)<b> Regional merging.ipynb</b></br>
This workbook utilises the shapefile outputs from merging_location.R, and merges them all into a single shapefile that can be used to link regions to their centroids. In doing so it cleans up many of the regional names so that they match with the OECD specifications. The result is outputed as a single shapefile.

8)<b> Binomial test.ipynb</b></br>
This workbook has three main functionalities. Firstly, it performs a binomial t-test on the data once the self-citation are removed in order to check the matching probability from both the treatment and control dataframes. Once this is performed then the results can be merged with the full shapefile created in regional merging.ipynb. This does so, so that the individual locations may be attached to specific regions and hence distance can be calculated between cited /citing and control pairs. Thus, in this workbook distance is also calculated, with the results being outputted as a single file.

9)<b> Final data exploration.ipynb</b></br>
This workbook performs the final data exploration. Here, another binomial t-test is performed to check for evidence of localisation once self-citations have been removed, and that individuals have been assigned to a geographic region. Once this is performed, several regression methods are used, including: linear probability models, Logistic regression and Probit regression. The output from these is then used as the final results in the completed work. Data is then used to create graphics that inform the results in the final output, such as visualisations of the distance decay and patent shares in different years. Finally, the data is outputted to files that can be merged in R to create visualisation of the regions from which the orignal patents and citing patents come from.

10)<b> Plotting citations.R</b></br>
This workbook uses the data from Final data exploration, merges the results with spatial dataframes and produces the visualisations of the regions from which original, cited and citing patents come from.

All files have been commented so that the process can be followed. 

Data was extracted from the following sources:</br>
OECD REGPAT database</br>
OECD REGPAT citations database </br>
GADM</br>
US census</br>
Eurostat</br>
....
