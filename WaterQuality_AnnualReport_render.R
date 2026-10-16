# This is the document that renders the park specific report for:
# WaterQuality_AnnualReport.Rmd
# WaterQuality_AnnualReport_compile.r

# libraries
library(rmarkdown)

# parks
parks <- c("Isle Royale National Park" = "ISRO",
           "Pictured Rocks National Lakeshore" = "PIRO",
           "St. Croix National Scenic Riverway" = "SACN",
           "Sleeping Bear Dunes National Lakeshore" = "SLBE",
           "Voyageurs National Park" = "VOYA")

current_year <- 2024
# format(Sys.Date(), "%Y")

# creating year file
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
                              format(Sys.Date(), "%Y"),
                              ".html"),
         # output director
         output_dir = file.path("./reports",
                                current_year),
         # setting params
         params = list(park = short_name,
                       park_full = full_name,
                       current_year = current_year,
                       discharge_date = "2024-12-01"),
         # start each render with clean environment
         envir = new.env(),
         quiet = TRUE)

cat(sprintf(paste0(short_name, "report written to: %s\n"), 
            normalizePath(file.path("./reports",
                                    current_year))))

}
