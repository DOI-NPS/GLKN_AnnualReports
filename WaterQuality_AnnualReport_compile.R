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
library(tidyr) # for pivoting, extract
library(forcats) # releveling factors
library(dataRetrieval) # discharge data 
library(sf) # spatial
library(lubridate)

# Start of Source Code ----
## Loading data ----

## water quality data (from wqp_retrival.r in Data Visualizer)
wqp_data1 <- read_csv(paste0("./data/wqp_glkn_",
                             format(Sys.Date(), "%Y"),
                             ".csv"))
# or 
# wqp_data1 <- read_csv("./data/wqp_glkn_2026.csv")

# keeping all site data from active sites
wqp_data2 <- wqp_data1 |> 
  filter(any(year == 2024),
         .by = MonitoringLocationName)
  # filter(any(year == format(Sys.Date(), "%Y")),
         # .by = MonitoringLocationIdentifier)

## discharge data from USGS
discharge_sites <- c("USGS-05331833",
                     "USGS-05333500",
                     "USGS-05340500")

discharge1 <- read_waterdata_daily(monitoring_location_id = discharge_sites,
                                   parameter_code = "00060",
                                   properties = c("monitoring_location_id",
                                                  "time",
                                                  "value",
                                                  "unit_of_measure"),
                                   time = c("2007-01-01",
                                            "2024-12-31"),
                                            # format(Sys.Date(), "%Y-%m-%d")),
                                   statistic_id = "00003") # mean

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

## Water Quality Data 
wqp_data3 <- wqp_data2 |> 
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
  # cleaning up columns 
  select(MonitoringLocationIdentifier,
         MonitoringLocationName,
         end_date,
         year,
         month,
         depth,
         depth_unit,
         ResultDetectionConditionText,
         DetectionQuantitationLimitTypeName,
         CharacteristicName,
         value,
         value_unit,
         Park,
         MonitoringLocationTypeName,
         lat,
         lon,
         LowerPoint,
         UpperPoint,
         LowerDescription,
         UpperDescription,
         Reference,
         AxisName)

## Discharge Data 
discharge2 <- discharge1 |>
  # extracting lat/lon
  dplyr::mutate(lon = sf::st_coordinates(geometry)[,1],
                lat = sf::st_coordinates(geometry)[,2])|> 
      sf::st_drop_geometry() |> 
  # updating column to match wqp_data
  dplyr::mutate(CharacteristicName = "Discharge",
                AxisName = "Discharge (ft^3/s)",
                year = lubridate::year(time),
                month = as.character(lubridate::month(time))) |>
  dplyr::rename(end_date = time,
                value_unit = unit_of_measure)
  

# matching streamflow sites to wqp sites 
site_lookup <- data.frame(monitoring_location_id = c("USGS-05331833", 
                                                     "USGS-05331833", 
                                                     "USGS-05331833",
                                                     "USGS-05333500", 
                                                     "USGS-05333500", 
                                                     "USGS-05333500",
                                                     "USGS-05340500", 
                                                     "USGS-05340500"),
                          MonitoringLocationIdentifier = c("11NPSWRD_WQX-SACN_NAKA_84.6", 
                                                           "11NPSWRD_WQX-SACN_NAKA_41.3", 
                                                           "11NPSWRD_WQX-SACN_NAKA_4.8",
                                                           "11NPSWRD_WQX-SACN_STCR_138.9", 
                                                           "11NPSWRD_WQX-SACN_STCR_104.0", 
                                                           "11NPSWRD_WQX-SACN_STCR_89.7",
                                                           "11NPSWRD_WQX-SACN_STCR_63.8", 
                                                           "11NPSWRD_WQX-SACN_STCR_43.7"))

# merging dataframes to "fill" in the dataset 
discharge <- discharge2 |> 
  left_join(site_lookup,
            by = "monitoring_location_id",
            relationship = "many-to-many")

## Merging Data
wqp_data4 <- bind_rows(wqp_data3,
                       discharge)

wqp_data <- wqp_data4 |> 
  fill(MonitoringLocationName,
       Park,
       MonitoringLocationTypeName,
       .by = "MonitoringLocationIdentifier") |> 
  # adding month and period (for historic vs current)
  dplyr::mutate(month_name = lubridate::month(end_date,
                                              label = TRUE,
                                              abbr = FALSE),
                period = case_when(year >= 2024 ~ "Current",
                                   year < 2024 ~ "Historic",
                                   TRUE ~ NA),
                period = fct_relevel(period,
                                     c("Historic",
                                       "Current")),
                site_type = case_when(CharacteristicName == "Discharge" ~ "Stream Guage",
                                      TRUE ~ "Monitoring"))
  # period = case_when(year == format(Sys.Date(), "%Y") ~ "Current",
  #                    year < format(Sys.Date(), "%Y") ~ "Historic"))

## park types 
park_type <- c("ISRO" = "lake",
               "PIRO" = "lake",
               "SACN" = "river",
               "SLBE" = "lake",
               "VOYA" = "lake")

# Parameters based on park type:
river_param <- c("Discharge (ft^3/s)", "Phosphorus (ug/L)", "Total suspended solids (mg/L)",
                 "Calcium (mg/L)", "Water Temperature (deg C)", 
                 "Chlorophyll a (mg/m3)", "Nitrate (ug/L)", "Chloride (mg/L)")

lake_param <- c("Water Temperature (deg C)", "Dissolved Oxygen (mg/L)",
                "Phosphorus (ug/L)", "Chlorophyll a (mg/m3)",
                "Dissolved Organic Carbon (mg/L)", "Secchi Disk Depth (m)",
                "Calcium (mg/L)", "Water level (m)")

# Date labels 
date_labels <- c(paste0("2006 - ", as.numeric(format(Sys.Date(), "%Y")) - 1),
                 format(Sys.Date(), "%Y"))

## Data prep functions ----
### map data 
prep_map <- function(df){
  df |> 
    # assigning USGS names 
    mutate(MonitoringLocationName = case_when(!is.na(monitoring_location_id) ~ monitoring_location_id,
                                              TRUE ~ MonitoringLocationName)) |> 
    # selecting lat/lon
    select(MonitoringLocationName,
           site_type,
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
  if(identical(type, "river")){
    df |> 
      dplyr::filter(AxisName %in% river_param,
                    !is.na(value))  |> 
      dplyr::mutate(AxisName = forcats::fct_relevel(AxisName,
                                                    c("Discharge (ft^3/s)",
                                                      "Total suspended solids (mg/L)",
                                                      "Phosphorus (ug/L)", 
                                                      "Calcium (mg/L)", 
                                                      "Water Temperature (deg C)", 
                                                      "Nitrate (ug/L)", 
                                                      "Chlorophyll a (mg/m3)", 
                                                      "Chloride (mg/L)")))
  } else{
    df |> 
      dplyr::filter(AxisName %in% lake_param,
                    !is.na(value)) |> 
      dplyr::mutate(AxisName = forcats::fct_relevel(AxisName,
                                                    c("Water Temperature (deg C)", 
                                                      "Phosphorus (ug/L)", 
                                                      "Dissolved Organic Carbon (mg/L)", 
                                                      "Calcium (mg/L)",
                                                      "Dissolved Oxygen (mg/L)", 
                                                      "Chlorophyll a (mg/m3)",
                                                      "Secchi Disk Depth (m)", 
                                                      "Water level (m)")))
  }
}

## timeseries data 
prep_timeseries <- function(df, park_code, type){
  
  # setting type 
  type <- type[[park_code]]
  
  # filtering based on type and releveling 
  if(identical(type, "river")){
    data <- df |> 
      dplyr::filter(AxisName %in% river_param,
                    !is.na(value))  |> 
      dplyr::mutate(AxisName = forcats::fct_relevel(AxisName,
                                                    c("Discharge (ft^3/s)",
                                                      "Total suspended solids (mg/L)",
                                                      "Phosphorus (ug/L)", 
                                                      "Calcium (mg/L)", 
                                                      "Water Temperature (deg C)", 
                                                      "Nitrate (ug/L)", 
                                                      "Chlorophyll a (mg/m3)", 
                                                      "Chloride (mg/L)")))  |> 
      # summarising data 
      dplyr::summarise(value = case_when(n() == 1 ~ value[1], # needed with duplicate values 
                                         n() == 2 ~ mean(value, na.rm = TRUE),
                                         n() >= 3 ~ median(value, na.rm = TRUE)),
                       .by = c(Park,
                               MonitoringLocationName,
                               month,
                               month_name,
                               year,
                               AxisName,
                               lat,
                               lon,
                               value_unit,
                               period))
    
    data_current <- data |> 
      dplyr::filter(period == "Current")
    
    data_historic <- data |> 
      dplyr::filter(period == "Historic",
                    !is.na(value)) |> 
      dplyr::summarise(hist_min = min(value, na.rm = TRUE),
                       hist_max = max(value, na.rm = TRUE),
                       hist_2 = quantile(value, probs = 0.025, na.rm = TRUE),
                       hist_95 = quantile(value, probs = 0.95, na.rm = TRUE),
                       hist_25 = quantile(value, probs = 0.25, na.rm = TRUE),
                       hist_75 = quantile(value, probs = 0.75, na.rm = TRUE),
                       hist_med = median(value, na.rm = TRUE),
                       .by = c(MonitoringLocationName,
                               AxisName,
                               month,
                               month_name))
    
    return(list(current = data_current,
                historic = data_historic))
      
  } else{
    data <- df |> 
      dplyr::filter(AxisName %in% lake_param,
                    !is.na(value)) |> 
      dplyr::mutate(AxisName = forcats::fct_relevel(AxisName,
                                                    c("Water Temperature (deg C)", 
                                                      "Phosphorus (ug/L)", 
                                                      "Dissolved Organic Carbon (mg/L)", 
                                                      "Calcium (mg/L)",
                                                      "Dissolved Oxygen (mg/L)", 
                                                      "Chlorophyll a (mg/m3)",
                                                      "Secchi Disk Depth (m)", 
                                                      "Water level (m)")))  |> 
      # summarising data 
      dplyr::summarise(value = case_when(n() == 1 ~ value[1], # needed with duplicate values 
                                         n() == 2 ~ mean(value, na.rm = TRUE),
                                         n() >= 3 ~ median(value, na.rm = TRUE)),
                       .by = c(Park,
                               MonitoringLocationName,
                               month,
                               month_name,
                               year,
                               AxisName,
                               lat,
                               lon,
                               value_unit,
                               period))
    
    data_current <- data |> 
      dplyr::filter(period == "Current")
    
    data_historic <- data |> 
      dplyr::filter(period == "Historic",
                    !is.na(value)) |> 
      dplyr::summarise(hist_min = min(value, na.rm = TRUE),
                       hist_max = max(value, na.rm = TRUE),
                       hist_2 = quantile(value, probs = 0.025, na.rm = TRUE),
                       hist_95 = quantile(value, probs = 0.95, na.rm = TRUE),
                       hist_25 = quantile(value, probs = 0.25, na.rm = TRUE),
                       hist_75 = quantile(value, probs = 0.75, na.rm = TRUE),
                       hist_med = median(value, na.rm = TRUE),
                       .by = c(MonitoringLocationName,
                               AxisName,
                               month,
                               month_name))
    
    return(list(current = data_current,
                historic = data_historic))
  }
}

### table data 
prep_table <- function(df, park_code, type){
  # setting type 
  type <- type[[park_code]]
  
  # filtering based on type
  if(identical(type, "river")){
    df |> 
      dplyr::filter(AxisName %in% river_param)|> 
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
      dplyr::mutate(across(where(is.numeric), \(x) round(x, 1))) |> 
      dplyr::mutate(AxisName = forcats::fct_relevel(AxisName,
                                                    c("Discharge (ft^3/s)",
                                                      "Water Temperature (deg C)",
                                                      "Phosphorus (ug/L)",
                                                      "Chlorophyll a (mg/m3)",
                                                      "Total suspended solids (mg/L)",
                                                      "Nitrate (ug/L)", 
                                                      "Calcium (mg/L)", 
                                                      "Chloride (mg/L)"))) |> 
      arrange(AxisName)
  } else{
    df |> 
      dplyr::filter(AxisName %in% lake_param)|> 
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
      dplyr::mutate(across(where(is.numeric), \(x) round(x, 1))) |> 
      dplyr::mutate(AxisName = forcats::fct_relevel(AxisName,
                                                    c("Water Temperature (deg C)", 
                                                      "Dissolved Oxygen (mg/L)",
                                                      "Phosphorus (ug/L)", 
                                                      "Chlorophyll a (mg/m3)", 
                                                      "Dissolved Organic Carbon (mg/L)", 
                                                      "Secchi Disk Depth (m)",
                                                      "Calcium (mg/L)",
                                                      "Water level (m)"))) |> 
      arrange(AxisName)
  }
}

### depth profiles 
prep_profiles <- function(df, park_code, type){
  # setting type 
  type <- type[[park_code]]
  
  # filtering based on type
  if(identical(type, "lake")){
    
    # current years data
    current_data <- df |> 
      # filtering depth data and current year
      dplyr::filter(AxisName %in% c("Dissolved Oxygen (mg/L)",
                                    "Water Temperature (deg C)"),
                    year == 2024) |> 
      # year == format(Sys.Date(), "%Y"))|> 
      filter(!is.na(value)) |> 
      # adding month names 
      mutate(month_long = format(as.Date(end_date), "%B")) |> 
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
             AxisName %in% c("Dissolved Oxygen (mg/L)",
                             "Water Temperature (deg C)")) |> 
      # round depths
      mutate(depth = round(depth, 0)) |> 
      filter(!is.na(value)) |> 
      # adding month names
      mutate(month_long = format(as.Date(end_date), "%B")) |>
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
plot_map <- function(df, park_code, type){
  
  # setting type 
  type <- type[[park_code]]
  
  if(identical(type, "lake")){
  
  leaflet(df) |> 
    addProviderTiles("Esri.WorldStreetMap") |> 
    addCircleMarkers(lng = ~lon,
                     lat = ~lat,
                     radius = 5,
                     color = "blue",
                     popup = ~MonitoringLocationName)
  } else{
      
    # color palette
    col_pal <- colorFactor(palette = c("blue",
                                       "darkgreen"),
                           domain = df$site_type)
    
    leaflet(df) |> 
      addProviderTiles("Esri.WorldStreetMap") |> 
      addCircleMarkers(lng = ~lon,
                       lat = ~lat,
                       radius = 5,
                       color = ~col_pal(site_type),
                       popup = ~MonitoringLocationName)
    
    }
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
         y = "Value",
         alt = "A boxplot showing current year data compared to all data collected at the site.") +
    scale_fill_manual(labels = date_labels,
                      values = c("#0982C8",
                                 "#6B8E15")) +
    scale_x_discrete(labels = date_labels) +
    theme_bw() + 
    theme(text = element_text(size = 16))
}

### timeseries 
plot_timeseries <- function(current, historic){
  
  ggplot() + 
    geom_ribbon(data = historic,
                aes(ymin = hist_min,
                    ymax = hist_max,
                    x = as.numeric(month),
                    fill = "Historic Range"),
                alpha = 0.3) +
    geom_ribbon(data = historic,
                aes(ymin = hist_2,
                    ymax = hist_95,
                    x = as.numeric(month),
                    fill = "Hist. 95% Range"),
                alpha = 0.3) +
    geom_ribbon(data = historic,
                aes(ymin = hist_25,
                    ymax = hist_75,
                    x = as.numeric(month),
                    fill = "Hist. 50% Range"),
                alpha = 0.3) +
    geom_line(data = historic,
              aes(x = as.numeric(month),
                  y = hist_med,
                  color = "Hist. Median"),
              size = 1) +
    geom_point(data = current,
               aes(x = as.numeric(month),
                   y = value,
                   color = date_labels[2])) +
    scale_fill_manual(name = "",
                      values = c("Historic Range" = "#8FD3FA",
                                 "Hist. 95% Range" = "#63C1F8",
                                 "Hist. 50% Range" = "#27A9F5")) +
    scale_color_manual(name = "",
                       values = c("Hist. Median" = "#0982C8",
                                  "2026" = "black")) +
    labs(x = "Month",
         y = "Value",
         alt = "A timeseries figure showing monthly surface measurements in the upper 2m of the water column for the site and each parameter.") + 
    scale_x_continuous(breaks = 1:12,
                       labels = month.abb) +
    facet_wrap(~AxisName,
               scales = "free",
               ncol = 4) +
    theme_bw() + 
    theme(text = element_text(size = 16))
}

### table 
create_table <- function(df){
  df |> 
    gt() |> 
    tab_spanner(label = html(paste0("<strong>2006 - ",
                                    as.numeric(format(Sys.Date(), "%Y")) - 1,
                                    "</strong>")),
                columns = c(min_val_Historic,
                            med_Historic,
                            max_val_Historic,
                            stan_dev_Historic)) |> 
    tab_spanner(label = html(paste0("<strong>",
                                    format(Sys.Date(), "%Y"),
                                    "</strong>")),
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
               stan_dev_Current = html("<strong>Standard \nDeviation</strong>")) |> 
    cols_align_decimal(columns = c("min_val_Historic", 
                                   "med_Historic",
                                   "max_val_Historic", 
                                   "stan_dev_Historic",
                                   "min_val_Current",
                                   "med_Current",
                                   "max_val_Current",
                                   "stan_dev_Current"))
    
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
         color = "Parameter",
         alt = "Depth profile plots for dissolved oxygen (mg/L) and water temperature in different months for the site. Historic minimum and maximum ranges are plotted as bands.") +
    scale_fill_manual(values = c("#0982C8",
                                 "#C8AF60")) +
    scale_color_manual(values = c("#0982C8",
                                  "#C8AF60")) +
    theme_bw() + 
    theme(text = element_text(size = 16))
}



