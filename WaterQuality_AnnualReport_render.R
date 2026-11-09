# This is the document that renders the park and network specific reports for:
# WaterQuality_AnnualReport.Rmd
# WaterQuality_AnnualReport_Network.Rmd
# WaterQuality_AnnualReport_compile.r
#
# Run this document to produce reports for all parks. Please make sure to update
# current_year to the year for the reports.

# libraries
library(rmarkdown)

# parks
parks <- c("Isle Royale National Park" = "ISRO",
           "Pictured Rocks National Lakeshore" = "PIRO",
           "St. Croix National Scenic Riverway" = "SACN",
           "Sleeping Bear Dunes National Lakeshore" = "SLBE",
           "Voyageurs National Park" = "VOYA")

# UPDATE YEAR ----
current_year <- 2025
discharge_date <- "2025-12-01"

# Creating files ----
## creating year file
if(!file.exists(file.path("./reports",
                          current_year))) {
  dir.create(file.path("./reports",
                       current_year))
  message("Folder created: ",
          file.path("./reports",
                    current_year))
} else{
  message("Folder already exists: ",
          normalizePath(file.path("./reports",
                                  current_year)))
}

## creating network file
if(!file.exists(file.path("./reports",
                          current_year,
                          "network reports"))) {
  dir.create(file.path("./reports",
                       current_year,
                       "network reports"))
  message("Folder created: ",
          file.path("./reports",
                    current_year,
                    "network reports"))
} else{
  message("Folder already exists: ",
          normalizePath(file.path("./reports",
                                  current_year,
                                  "network reports")))
}

# Rendering park specific documents (WaterQuality_AnnualReport.Rmd) ----
for(full_name in names(parks)){
  
  # getting short name
  short_name <- parks[[full_name]]
  
  message(paste0("Rendering report for ",
                 short_name))
  
  # render the correct Rmd file
  render(input = "WaterQuality_AnnualReport.Rmd",
         # Name output
         output_file = paste0("WaterQuality_AnnualReport_",
                              short_name,
                              current_year,
                              ".html"),
         # output director
         output_dir = file.path("./reports",
                                current_year),
         # setting params
         params = list(park = short_name,
                       park_full = full_name,
                       current_year = current_year,
                       discharge_date = discharge_date),
         # start each render with clean environment
         envir = new.env(),
         quiet = TRUE)

cat(sprintf(paste0(short_name, " report written to: %s\n"), 
            normalizePath(file.path("./reports",
                                    current_year))))

}

# Rendering park specific documents for the NETWORK (WaterQuality_AnnualReport_Network.Rmd) ----
for(full_name in names(parks)){
  
  # getting short name
  short_name <- parks[[full_name]]
  
  message(paste0("Rendering report for ",
                 short_name))
  
  # render the correct Rmd file
  render(input = "WaterQuality_AnnualReport_Network.Rmd",
         # Name output
         output_file = paste0("WaterQuality_AnnualReport_Network_",
                              short_name,
                              current_year,
                              ".html"),
         # output director
         output_dir = file.path("./reports",
                                current_year,
                                "network reports"),
         # setting params
         params = list(park = short_name,
                       park_full = full_name,
                       current_year = current_year,
                       discharge_date = discharge_date),
         # start each render with clean environment
         envir = new.env(),
         quiet = TRUE)
  
  cat(sprintf(paste0(short_name, " report written to: %s\n"), 
              normalizePath(file.path("./reports",
                                      current_year,
                                      "network reports"))))
  
}
