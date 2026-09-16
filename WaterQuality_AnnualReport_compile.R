# This is the file that runs all the data wrangling for the
# WaterQuality_AnnualReport.Rmd. If the Rmd does not knit, this is
# where the error will come from.

# Note: Run the wqp_retrival.R document in the GLKN Visualizer project to
# get data for this report. 

# Libraries ----
library(readr) # tidyverse data import
library(dplyr) # data wrangling
library(ggplot2) # plotting
# library(knitr) # for kable
# library(kableExtra) # custom kable features
library(leaflet) # for mapping
library(gt) # for table
library(tidyr) # for pivoting
library(forcats) # releveling factors

# Start of Source Code ----
## Loading data ----

## water quality data (from wqp_retrival.r in Data Visualizer)
wqp_data1 <- read_csv(paste0("./data/wqp_glkn_",
                             format(Sys.Date(), "%Y"),
                             ".csv"))
# or 
# wqp_data1 <- read_csv(paste0("./data/wqp_glkn_2026.csv"))

# keeping all site data from active sites
wqp_data2 <- wqp_data1 |> 
  filter(any(year == 2024),
         .by = MonitoringLocationName)
  # filter(any(year == format(Sys.Date(), "%Y")),
         # .by = MonitoringLocationIdentifier)

## Data wrangling ----

## Sites to keep
keep_sites <- c("Beaver Lake", "Feldtmann Lake", "Lake Ahmik", "Lake Desor",
                "Lake George", "Lake Harvey", "Lake Richie", "Sargent Lake",
                "Siskiwit Lake", "Chapel Lake", "Grand Sable Lake", "Legion Lake",
                "Miners Lake", "Trapper's Lake", "CCC", "Earl", "Hwy 70", 
                "Leonards", "Nam Tr.", "Nevers", "Norway Pt.", "Osceola",
                "Bass Lake", "Florence Lake", "Loon Lake", "Manitou Lake",
                "North Bar", "Shell Lake", "Brown Lake", "Cruiser Lake", 
                "Ek Lake", "Little Trout Lake", "Locator Lake", "Mukooda Lake",
                "Peary Lake", "Ryan Lake", "Shoepack Lake")

wqp_data <- wqp_data2 |> 
  # filtering lakes 
  dplyr::filter(MonitoringLocationName %in% keep_sites) |> 
  # renaming columns for ease
  dplyr::rename(start_date = ActivityStartDate,
                end_date = ActivityEndDate,
                depth = ActivityDepthHeightMeasure.MeasureValue,
                depth_unit = ActivityDepthHeightMeasure.MeasureUnitCode,
                value = ResultMeasureValue,
                value_unit = ResultMeasure.MeasureUnitCode,
                lat = LatitudeMeasure,
                lon = LongitudeMeasure) |>
  # adding month and period (for historic vs current)
  dplyr::mutate(month_name = lubridate::month(end_date,
                                              label = TRUE,
                                              abbr = FALSE),
                period = case_when(year >= 2024 ~ "Current",
                                   year < 2024 ~ "Historic",
                                   TRUE ~ NA),
                period = fct_relevel(period,
                                     c("Historic",
                                       "Current")))
                # period = case_when(year == format(Sys.Date(), "%Y") ~ "Current",
                #                    year < format(Sys.Date(), "%Y") ~ "Historic"))

## park types 
park_type <- c("ISRO" = "lake",
               "PIRO" = "lake",
               "SACN" = "river",
               "SLBE" = "lake",
               "VOYA" = "lake")

# Parameters based on park type:
river_param <- c("Phosphorus", "Nitrate + Nitrite", 
                 "Chlorophyll a, free of pheophytin",
                 "Total suspended solids", "Calcium",
                 "Chloride") 

lake_param <- c("Temperature, water", "Phosphorus", "Water level", "Calcium",
                "Dissolved oxygen (DO)", "Chlorophyll a, free of pheophytin",
                "Depth, Secchi disk depth", "Organic carbon")

## Data prep functions ----
### map data 
prep_map <- function(df){
  df |> 
    # selecting lat/lon
    select(MonitoringLocationName,
           lat,
           lon) |>
    # getting distinct values 
    distinct()
}

### boxplot data 
prep_boxplot <- function(df, park_code, type){
  
  # setting type 
  type <- type[[park_code]]
  
  # filtering based on type and releveling 
  parameters <- if(identical(type, "river")){
    df |> 
      dplyr::filter(CharacteristicName %in% river_param,
                    !is.na(value)) |> 
      dplyr::mutate(CharacteristicName = forcats::fct_relevel(CharacteristicName,
                                                              c("Phosphorus",  
                                                                "Nitrate + Nitrite",
                                                                "Calcium",
                                                                "Chlorophyll a, free of pheophytin",
                                                                "Total suspended solids", 
                                                                "Chloride")))
  } else{
    df |> 
      dplyr::filter(CharacteristicName %in% lake_param,
                    !is.na(value)) |> 
      dplyr::mutate(CharacteristicName = forcats::fct_relevel(CharacteristicName,
                                                              c("Temperature, water", 
                                                                "Phosphorus", 
                                                                "Water level", 
                                                                "Calcium",
                                                                "Dissolved oxygen (DO)", 
                                                                "Chlorophyll a, free of pheophytin",
                                                                "Depth, Secchi disk depth", 
                                                                "Organic carbon")))
  }
}

### table data 
prep_table <- function(df, park_code, type){
  # setting type 
  type <- type[[park_code]]
  
  # filtering based on type
  parameters <- if(identical(type, "river")){
    df |> 
      dplyr::filter(CharacteristicName %in% river_param)|> 
      filter(!is.na(value)) |> 
      summarise(med = median(value, na.rm = TRUE),
                stan_dev = sd(value, na.rm = TRUE),
                min_val = min(value, na.rm = TRUE),
                max_val = max(value, na.rm = TRUE),
                .by = c("AxisName",
                        "period")) |> 
      pivot_wider(names_from = period,
                  values_from = c(med, 
                                  stan_dev,
                                  min_val,
                                  max_val)) |> 
      dplyr::mutate(across(where(is.numeric), \(x) round(x, 2))) |> 
      dplyr::mutate(AxisName = forcats::fct_relevel(AxisName,
                                                    c("Phosphorus",  
                                                      "Nitrate + Nitrite",
                                                      "Calcium",
                                                      "Chlorophyll a, free of pheophytin",
                                                      "Total suspended solids", 
                                                      "Chloride"))) |> 
      arrange(AxisName)
  } else{
    df |> 
      dplyr::filter(CharacteristicName %in% lake_param)|> 
      filter(!is.na(value)) |> 
      summarise(med = median(value, na.rm = TRUE),
                stan_dev = sd(value, na.rm = TRUE),
                min_val = min(value, na.rm = TRUE),
                max_val = max(value, na.rm = TRUE),
                .by = c("AxisName",
                        "period")) |>   
      pivot_wider(names_from = period,
                  values_from = c(med, 
                                  stan_dev,
                                  min_val,
                                  max_val)) |> 
      dplyr::mutate(across(where(is.numeric), \(x) round(x, 2))) |> 
      dplyr::mutate(AxisName = forcats::fct_relevel(AxisName,
                                                    c("Water Temperature (deg C)", 
                                                      "Dissolved Oxygen (mg/L)",
                                                      "Phosphorus (ug/L)", 
                                                      "Chlorophyll a (mg/m3)", 
                                                      "Water level (m)", 
                                                      "Secchi Disk Depth (m)",
                                                      "Calcium (mg/L)",
                                                      "Dissolved Organic Carbon (mg/L)"))) |> 
      arrange(AxisName)
  }
}

### depth profiles 
prep_profiles <- function(df, park_code, type){
  # setting type 
  type <- type[[park_code]]
  
  # filtering based on type
  parameters <- if(identical(type, "lake")){
    
    # current years data
    current_data <- df |> 
      # filtering depth data and current year
      dplyr::filter(CharacteristicName %in% c("Dissolved oxygen (DO)",
                                              "Temperature, water"),
                    year == 2024) |> 
      # year == format(Sys.Date(), "%Y"))|> 
      filter(!is.na(value)) |> 
      # adding month names 
      mutate(month_long = format(as.Date(end_date), "%b")) |> 
      # arranging for plot
      arrange(AxisName,
              month,
              depth) |> 
      mutate(month_long = factor(month_long,
                                 levels = unique(month_long)))
    
      # getting unique months 
      valid_months <- unique(current_data$month)
      
    # all data 
    all_data <- df |> 
      # filtering valid months 
      filter(month %in% valid_months,
             CharacteristicName %in% c("Dissolved oxygen (DO)",
                                       "Temperature, water")) |> 
      # round depths
      mutate(depth = round(depth, 0)) |> 
      filter(!is.na(value)) |> 
      # adding month names
      mutate(month_long = format(as.Date(end_date), "%b")) |>
      # getting min and max
      summarise(min_value = min(value, na.rm = TRUE),
                max_value = max(value, na.rm = TRUE),
                .by = c("depth",
                        "month",
                        "AxisName",
                        "month_long")) |> 
      # arranging for plot 
      arrange(AxisName,
              month,
              depth) |> 
      mutate(month_long = factor(month_long,
                                 levels = unique(month_long)))
    
    return(list(current = current_data,
                historic = all_data))
    
  } else{
    # river data doesn't have depth data 
    return(NULL)
  }
}

## Plot functions ----

### map 
plot_map <- function(df){
  leaflet(df) |> 
    addProviderTiles("Esri.WorldStreetMap") |> 
    addCircleMarkers(lng = ~lon,
                     lat = ~lat,
                     radius = 5,
                     color = "blue",
                     popup = ~MonitoringLocationName)
}

### box plot 
plot_boxplot <- function(df){
  ggplot() + 
    geom_boxplot(data = df,
                 aes(x = period,
                     y = value,
                     fill = period)) + 
    facet_wrap(~AxisName,
               scales = "free",
               ncol = 4) +
    labs(fill = "Period",
         x = "Period",
         y = "Value") +
    scale_fill_manual(values = c("#457B9D",
                                 "#6B8E23")) +
    theme_bw() + 
    theme(text = element_text(size = 16))
}

### table 
create_table <- function(df){
  df |> 
    gt() |> 
    tab_spanner(label = html("<strong>Historic</strong>"),
                columns = c(min_val_Historic,
                            med_Historic,
                            max_val_Historic,
                            stan_dev_Historic)) |> 
    tab_spanner(label = html("<strong>Current</strong>"),
                columns = c(min_val_Current,
                            med_Current,
                            max_val_Current,
                            stan_dev_Current)) |> 
    cols_label(AxisName = html("<strong>Parameter</strong>"),
               min_val_Historic = html("<strong>Minimum</strong>"),
               med_Historic = html("<strong>Median</strong>"),
               max_val_Historic = html("<strong>Maximum</strong>"),
               stan_dev_Historic = html("<strong>Standard \nDeviation</strong>"),
               min_val_Current = html("<strong>Minimum</strong>"),
               med_Current = html("<strong>Median</strong>"),
               max_val_Current = html("<strong>Maximum</strong>"),
               stan_dev_Current = html("<strong>Standard \nDeviation</strong>")) 
    
}

### depth profile
plot_depthprofile <- function(current, historic){

  # if river park
  if (is.null(current) || is.null(historic)) {
    return(invisible(NULL))   # do nothing
  }
  
  # if lake park
  ggplot() +   
    # historic ribbons
    geom_ribbon(data = historic,
                aes(xmin = min_value,
                    xmax = max_value,
                    y = depth,
                    fill = AxisName),
                size = 10,
                # fill = "grey70",
                alpha = 0.3) +
    geom_path(data = current,
              aes(x = value,
                  y = depth,
                  color = AxisName),
              linewidth = 1.5) +
    geom_point(data = current,
               aes(x = value,
                   y = depth,
                   color = AxisName),
               size = 3) +
    facet_wrap(~month_long) +
    labs(x = "Value",
         y = "Depth (m)",
         fill = "Parameter",
         color = "Parameter") +
    scale_fill_manual(values = c("#457B9D",
                                 "#795548")) +
    scale_color_manual(values = c("#457B9D",
                                  "#795548")) +
    theme_bw() + 
    theme(text = element_text(size = 16))
}



