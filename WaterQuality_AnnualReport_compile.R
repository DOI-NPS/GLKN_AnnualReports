# This is the file that runs all the data wrangling for the
# WaterQuality_AnnualReport.Rmd and the WaterQuality_AnnualReport_Network.Rmd. 
# This file wrangles all the water quality portal data and USGS stream gauge 
# data. This file also builds the functions that filter and plot each section of
# the annual report. In most functions, a dataset, park code, park type, and 
# scope need to be assigned. The dataset, park code, and type will all auto 
# populate when running the WaterQuality_AnnualReport_render.R. To switch between 
# annual reports for parks and annual reports for the Network, change which scope 
# is commented out at the top of the WaterQuality_AnnualReport.Rmd.
# 
# If the Rmd does not knit, this is where the error will come from.

# Note: Run the wqp_retrival.R document in the GLKN Visualizer project to
# get data for this report. 

# Libraries ----
library(readr) # tidyverse data import
library(dplyr) # data wrangling
library(ggplot2) # plotting
library(leaflet) # for mapping
library(gt) # for table
library(tidyr) # for pivoting, extract
library(forcats) # releveling factors
library(dataRetrieval) # discharge data 
library(sf) # spatial
library(lubridate)

# Start of Source Code ----

# When troubleshooting make sure to run the setup chunk in the .Rmd file.

## params from markdown ----
current_year <- as.integer(params$current_year)
discharge_date <- as.character(params$discharge_date)

## Loading data ----

## water quality data (from wqp_retrival.r in Data Visualizer): Make sure year is correct
wqp_path <- sprintf(paste0("./data/wqp_glkn_",
                           format(Sys.Date(), "%Y"),
                           ".csv"))
# or 
# wqp_path <- sprintf(paste0("./data/wqp_glkn_2026.csv")) # Make sure to update year

# Stop if file doesn't exist
stopifnot(file.exists(wqp_path))

# load data 
wqp_data1 <- readr::read_csv(wqp_path)

# keeping all site data from active sites
wqp_data2 <- wqp_data1 |> 
  filter(any(year == current_year, na.rm = TRUE),
         .by = MonitoringLocationName) |> 
  filter(year <= current_year)

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
                                            discharge_date),
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
         MonitoringLocationName_Long,
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
  tidyr::fill(MonitoringLocationName,
              MonitoringLocationName_Long,
              Park,
              MonitoringLocationTypeName,
              .by = "MonitoringLocationIdentifier") |> 
  # adding month and period (for historic vs current)
  dplyr::mutate(month_name = lubridate::month(end_date,
                                              label = TRUE,
                                              abbr = FALSE),
                period = case_when(year >= current_year ~ "Current",
                                   year < current_year ~ "Historic",
                                   TRUE ~ NA),
                period = fct_relevel(period,
                                     c("Historic",
                                       "Current")),
                site_type = case_when(CharacteristicName == "Discharge" ~ "Stream Guage",
                                      TRUE ~ "Monitoring"))

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
date_labels <- c(paste0("2006 - ", as.numeric(current_year) - 1),
                 as.character(current_year))

## Data prep functions ----

### map data 
prep_map <- function(df){
  # preps data for map
  # ARGS:
    # df: park filtered data
  # RETURNS: data for plotting map
  
  df |> 
    # assigning USGS names 
    dplyr::mutate(MonitoringLocationName_Long = dplyr::case_when(!is.na(monitoring_location_id) ~ 
                                                                   monitoring_location_id,
                                              TRUE ~ MonitoringLocationName_Long)) |> 
    # selecting lat/lon
    dplyr::select(MonitoringLocationName_Long,
                  site_type,
                  lat,
                  lon) |>
    # getting distinct values 
    dplyr::distinct()
}

### boxplot data 
prep_boxplot <- function(df, scope = c("filtered", "full")){
  # preps data for boxplots
  # ARGS:
    # df: park filtered data
    # park_code: park code of data
    # type: type of park (river or lake)
    # scope: scope for park or network annual report (filtered or full)
  # RETURNS: A dataset for boxplot with either a limited or full list of 
  # parameters to plot.
  
  # selecting scope
  scope <- match.arg(scope)
  
  if(scope == "filtered"){
    
    # setting params based on type and releveling 
    if(identical(type, "river")){
      # param <- river_param
      param_order <- c("Discharge (ft^3/s)",
                       "Total suspended solids (mg/L)",
                       "Phosphorus (ug/L)",
                       "Calcium (mg/L)",
                       "Water Temperature (deg C)",
                       "Nitrate (ug/L)",
                       "Chlorophyll a (mg/m3)",
                       "Chloride (mg/L)")
    } else { # type == lake
      # param <- lake_param
      param_order <- c("Water Temperature (deg C)",
                       "Phosphorus (ug/L)",
                       "Dissolved Organic Carbon (mg/L)",
                       "Calcium (mg/L)",
                       "Dissolved Oxygen (mg/L)",
                       "Chlorophyll a (mg/m3)",
                       "Secchi Disk Depth (m)",
                       "Water level (m)")
    }
      df |> 
        # filtering river parameters and removing NA values
        dplyr::filter(AxisName %in% selected_params,
                      !is.na(value))  |> 
        # releveling parameters
        dplyr::mutate(AxisName = forcats::fct_relevel(AxisName,
                                                      param_order))
      
  } else { # scope == "full"
    df |> 
      # removing NA values
      dplyr::filter(!is.na(value))
  }
}

## timeseries data 
prep_timeseries <- function(df, scope = c("filtered", "full")){
  # preps data for timeseries
  # ARGS:
  # df: park filtered data
  # park_code: park code of data
  # type: type of park (river or lake)
  # scope: scope for park or network annual report (filtered or full)
  # RETURNS: A dataset for timeseries plots with either a limited or full list of 
  # parameters to plot.
  
  # selecting scope
  scope <- match.arg(scope)
  
  if(scope == "filtered"){
    
    # setting params based on type and releveling 
    if(identical(type, "river")){
      # param <- river_param
      param_order <- c("Discharge (ft^3/s)",
                       "Total suspended solids (mg/L)",
                       "Phosphorus (ug/L)", 
                       "Calcium (mg/L)", 
                       "Water Temperature (deg C)", 
                       "Nitrate (ug/L)", 
                       "Chlorophyll a (mg/m3)", 
                       "Chloride (mg/L)")
    } else { # type == lake 
      # param <- lake_param
      param_order <- c("Water Temperature (deg C)", 
                       "Phosphorus (ug/L)", 
                       "Dissolved Organic Carbon (mg/L)", 
                       "Calcium (mg/L)",
                       "Dissolved Oxygen (mg/L)", 
                       "Chlorophyll a (mg/m3)",
                       "Secchi Disk Depth (m)", 
                       "Water level (m)")
    }
      
      data <- df |> 
        # filtering based on params (river or lake) and removing NA values
        dplyr::filter(AxisName %in% selected_params,
                      !is.na(value))  |> 
        # releving params
        dplyr::mutate(AxisName = forcats::fct_relevel(AxisName,
                                                      param_order))  |> 
        # summarising data 
        dplyr::summarise(value = case_when(n() == 1 ~ value[1], # needed with duplicate values 
                                           n() == 2 ~ mean(value, na.rm = TRUE),
                                           n() >= 3 ~ median(value, na.rm = TRUE)),
                         .by = c(Park,
                                 MonitoringLocationName_Long,
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
                         .by = c(MonitoringLocationName_Long,
                                 AxisName,
                                 month,
                                 month_name))
      
      return(list(current = data_current,
                  historic = data_historic))
      
  } else { # scope == full 
    
    data <- df |> 
      # removing NA values
      dplyr::filter(!is.na(value))  |> 
      # summarising data 
      dplyr::summarise(value = case_when(n() == 1 ~ value[1], # needed with duplicate values 
                                         n() == 2 ~ mean(value, na.rm = TRUE),
                                         n() >= 3 ~ median(value, na.rm = TRUE)),
                       .by = c(Park,
                               MonitoringLocationName_Long,
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
                       .by = c(MonitoringLocationName_Long,
                               AxisName,
                               month,
                               month_name))
    
    return(list(current = data_current,
                historic = data_historic))
  }
}

### table data 
prep_table <- function(df, scope = c("filtered", "full")){
  # preps data for table
  # ARGS:
  # df: park filtered data
  # park_code: park code of data
  # type: type of park (river or lake)
  # scope: scope for park or network annual report (filtered or full)
  # RETURNS: A dataset for report table with either a limited or full list of 
  # parameters to plot.
  
  # selecting scope
  scope <- match.arg(scope)
  
  # checking periods 
  period_levels <- c("Historic", "Current")
  
  if(scope == "filtered"){
    
    # setting params based on type and releveling 
    if(identical(type, "river")){
      # param <- river_param
      param_order <- c("Discharge (ft^3/s)",
                       "Water Temperature (deg C)",
                       "Phosphorus (ug/L)",
                       "Chlorophyll a (mg/m3)",
                       "Total suspended solids (mg/L)",
                       "Nitrate (ug/L)", 
                       "Calcium (mg/L)", 
                       "Chloride (mg/L)")
    } else { # type == lake 
      # param <- lake_param
      param_order <- c("Water Temperature (deg C)", 
                       "Dissolved Oxygen (mg/L)",
                       "Phosphorus (ug/L)", 
                       "Chlorophyll a (mg/m3)", 
                       "Dissolved Organic Carbon (mg/L)", 
                       "Secchi Disk Depth (m)",
                       "Calcium (mg/L)",
                       "Water level (m)")
    }
  
      suppressWarnings({
        df |> 
        # filtering params 
        dplyr::filter(AxisName %in% selected_params) |> 
        # making sure each parameter has a row for each period
        tidyr::complete(AxisName = selected_params,
                        period = period_levels) |> 
        # summarising data, adding NA if no data exists
        dplyr::summarise(med = ifelse(all(is.na(value)), NA_real_, 
                                      median(value, na.rm = TRUE)),
                         stan_dev = ifelse(sum(!is.na(value)) <= 1, NA_real_,
                                           sd(value, na.rm = TRUE)),
                         min_val = ifelse(all(is.na(value)), NA_real_, 
                                          min(value, na.rm = TRUE)),
                         max_val = ifelse(all(is.na(value)), NA_real_, 
                                          max(value, na.rm = TRUE)),
                         .by = c("AxisName",
                                 "period")) |> 
        # pivoting wider for table formatting
        tidyr::pivot_wider(names_from = period,
                           values_from = c(med, 
                                           stan_dev,
                                           min_val,
                                           max_val)) |> 
        # rounding to 1 decimal place
        dplyr::mutate(dplyr::across(where(is.numeric), \(x) round(x, 1))) |> 
        # releveling parameters
        dplyr::mutate(AxisName = forcats::fct_relevel(AxisName,
                                                      param_order)) |> 
        # selecting needed column names
        dplyr::select(AxisName,
                      min_val_Historic, 
                      med_Historic, 
                      max_val_Historic, 
                      stan_dev_Historic,
                      min_val_Current,  
                      med_Current,  
                      max_val_Current,  
                      stan_dev_Current) #|> 
        # arranging by AxisName
        # dplyr::arrange(AxisName)
      })
  } else { # scope == full
    
    # getting params 
    param <- unique(df$AxisName)
    
    suppressWarnings({
      df |> 
        # making sure each parameter has a row for each period
        tidyr::complete(AxisName = param,
                        period = period_levels) |> 
        # summarising data, adding NA if no data exists
        dplyr::summarise(med = ifelse(all(is.na(value)), NA_real_, 
                                      median(value, na.rm = TRUE)),
                         stan_dev = ifelse(sum(!is.na(value)) <= 1, NA_real_,
                                           sd(value, na.rm = TRUE)),
                         min_val = ifelse(all(is.na(value)), NA_real_, 
                                          min(value, na.rm = TRUE)),
                         max_val = ifelse(all(is.na(value)), NA_real_, 
                                          max(value, na.rm = TRUE)),
                         .by = c("AxisName",
                                 "period")) |> 
        # pivoting wider for table formatting
        tidyr::pivot_wider(names_from = period,
                           values_from = c(med, 
                                           stan_dev,
                                           min_val,
                                           max_val)) |> 
        # rounding to 1 decimal place
        dplyr::mutate(dplyr::across(where(is.numeric), \(x) round(x, 1))) |> 
        # releveling parameters
        # dplyr::mutate(AxisName = forcats::fct_relevel(AxisName,
        #                                               param_order)) |> 
        # selecting needed column names
        dplyr::select(AxisName,
                      min_val_Historic, 
                      med_Historic, 
                      max_val_Historic, 
                      stan_dev_Historic,
                      min_val_Current,  
                      med_Current,  
                      max_val_Current,  
                      stan_dev_Current) #|> 
      # arranging by AxisName
      # dplyr::arrange(AxisName)
    })
  }
}

### depth profiles 
prep_profiles <- function(df, type, scope = c("filtered", "full")){
  # preps data for profile plots
  # ARGS:
  # df: park filtered data
  # park_code: park code of data
  # type: type of park (river or lake)
  # scope: scope for park or network annual report (filtered or full)
  # RETURNS: A dataset for boxplot with either a limited or full list of 
  # parameters to plot.
  
  # selecting scope
  scope <- match.arg(scope)
  
  if(scope == "filtered"){
    # filters depth params 
    param <- c("Dissolved Oxygen (mg/L)",
               "Water Temperature (deg C)")
  } else{ # scope == full
    # full depth params 
    param <- c("Dissolved Oxygen (%)",
               "Dissolved Oxygen (mg/L)",
               "pH",
               "Specific Conductance (uS/cm)",
               "Water Temperature (deg C)")  
  }

    # filtering based on type
    if(type == "lake"){
      
      # current years data
      current_data <- df |> 
        # filtering depth data and current year
        dplyr::filter(AxisName %in% param,
                      period == "Current") |> 
        dplyr::filter(!is.na(value)) |> 
        # adding month names 
        dplyr::mutate(month_long = format(as.Date(end_date), "%B")) |> 
        # arranging for plot
        dplyr::arrange(AxisName,
                month,
                depth) |> 
        dplyr::mutate(month_long = factor(month_long,
                                          levels = unique(month_long)))
      
        # getting unique months 
        valid_months <- unique(current_data$month)
        
      # all data 
      all_data <- df |> 
        # filtering valid months 
        dplyr::filter(month %in% valid_months,
                      AxisName %in% param) |> 
        # round depths
        dplyr::mutate(depth = round(depth, 0)) |> 
        dplyr::filter(!is.na(value)) |> 
        # adding month names
        dplyr::mutate(month_long = format(as.Date(end_date), "%B")) |>
        # getting min and max
        dplyr::summarise(min_value = min(value, na.rm = TRUE),
                         max_value = max(value, na.rm = TRUE),
                         .by = c("depth",
                                 "month",
                                 "AxisName",
                                 "month_long")) |> 
        # arranging for plot 
        dplyr::arrange(AxisName,
                       month,
                       depth) |> 
        dplyr::mutate(month_long = factor(month_long,
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
plot_map <- function(df, type){
  
  if(identical(type, "lake")){
  
  leaflet(df) |> 
    addProviderTiles("Esri.WorldStreetMap") |> 
    addCircleMarkers(lng = ~lon,
                     lat = ~lat,
                     radius = 5,
                     color = "blue",
                     popup = ~MonitoringLocationName_Long)
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
                       popup = ~MonitoringLocationName_Long)
    
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
              linewidth = 1) +
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
                                  setNames("black", date_labels[2]))) +
    labs(x = "Month",
         y = "Value",
         alt = "A timeseries figure showing monthly surface measurements in the upper 2m of the water column for the site and each parameter.") + 
    scale_x_continuous(breaks = seq(1, 12, by = 2),
                       labels = month.abb[seq(1, 12, by = 2)]) +
    facet_wrap(~AxisName,
               scales = "free",
               ncol = 4) +
    theme_bw() + 
    theme(text = element_text(size = 16))
}

### table 
create_table <- function(df){
  
  # Ensure spanner columns exist (fill missing ones with NA)
  needed <- c("min_val_Historic", "med_Historic", "max_val_Historic",
              "stan_dev_Historic", "min_val_Current", "med_Current",
              "max_val_Current", "stan_dev_Current")
  
  missing <- setdiff(needed, names(df))
  for (cols in missing) df[[cols]] <- NA_real_
  
  # Table
  df |> 
    gt::gt() |>
    gt::tab_spanner(label = html(paste0("<strong>2006 - ",
                                        as.numeric(current_year) - 1,
                                        "</strong>")),
                    columns = c(min_val_Historic,
                                med_Historic,
                                max_val_Historic,
                                stan_dev_Historic)) |> 
    gt::tab_spanner(label = html(paste0("<strong>",
                                        current_year,
                                        "</strong>")),
                    columns = c(min_val_Current,
                                med_Current,
                                max_val_Current,
                                stan_dev_Current)) |> 
    gt::cols_label(AxisName = html("<strong>Parameter</strong>"),
                   min_val_Historic = html("<strong>Minimum</strong>"),
                   med_Historic = html("<strong>Median</strong>"),
                   max_val_Historic = html("<strong>Maximum</strong>"),
                   stan_dev_Historic = html("<strong>Standard \nDeviation</strong>"),
                   min_val_Current = html("<strong>Minimum</strong>"),
                   med_Current = html("<strong>Median</strong>"),
                   max_val_Current = html("<strong>Maximum</strong>"),
                   stan_dev_Current = html("<strong>Standard \nDeviation</strong>")) |> 
    gt::cols_align_decimal(columns = c("min_val_Historic", 
                                       "med_Historic",
                                       "max_val_Historic", 
                                       "stan_dev_Historic",
                                       "min_val_Current",
                                       "med_Current",
                                       "max_val_Current",
                                       "stan_dev_Current")) |> 
    gt::opt_table_font(size = "14px")
    
}

### print table 
print_table <- function(tbl){
  cat(gt::as_raw_html(tbl))
  invisible(NULL)
}

### depth profile
plot_depthprofile <- function(current, historic, scope = c("filtered", "full")){

  # if river park
  if (is.null(current) || is.null(historic)) {
    return(invisible(NULL))   # do nothing
  }
  
  # if lake park
  if (scope == "filtered"){
  
  # if scope == filtered
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
    
  } else { # scope == full
    ggplot() +   
      # historic ribbons
      geom_ribbon(data = historic,
                  aes(xmin = min_value,
                      xmax = max_value,
                      y = depth),
                      # fill = AxisName),
                  size = 10,
                  # fill = "grey70",
                  alpha = 0.3) +
      geom_path(data = current,
                aes(x = value,
                    y = depth),
                    # color = AxisName),
                linewidth = 1.5) +
      geom_point(data = current,
                 aes(x = value,
                     y = depth),
                     # color = AxisName),
                 size = 3) +
      facet_grid(month_long~AxisName,
                 scales = "free") +
      labs(x = "Value",
           y = "Depth (m)",
           alt = "Depth profile plots for dissolved oxygen (mg/L) and water temperature in different months for the site. Historic minimum and maximum ranges are plotted as bands.") +
      # scale_fill_manual(values = c("#0982C8",
      #                              "#C8AF60")) +
      # scale_color_manual(values = c("#0982C8",
      #                               "#C8AF60")) +
      theme_bw() + 
      theme(text = element_text(size = 16))
    
  }
}
