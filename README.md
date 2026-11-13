# GLKN_AnnualReports
This is code was developed to compile and render Great Lakes Network discrete water quality annual reports.

This repository contains the following: 
<ul>
<li>WaterQuality_AnnualReport.Rmd: The park specific annual report RMarkdown</li>
<li>WaterQuality_AnnualReport_Network.Rmd: The network specific annual report RMarkdown</li>
<li>WaterQuality_AnnualReport_compile.R: The compile document to import, wrangle, and create functions for both types of annual reports.</li>
<li>WaterQuality_AnnualReport_render.R: The document used to create the folder structure and render all reports for both types of annual reports.</li>

## Steps for rendering annual reports: 

1. Clone the `main` branch to your computer
2. Create a `data` folder in your working directory
3. Create a `reports` folder in your working directory
3. Run the wqp_retrival.R document in the `GLKNViz` repository to get an updated version of the Water Quality Portal dataset
4. In WaterQuality_AnnualReport_render.R, update `current_year` and `discharge_date` 
5. Run all lines of WaterQuality_AnnualReport_render.R
6. All network reports will be in the `reports` folder under the `current_year` you set 