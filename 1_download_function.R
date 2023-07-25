### Aquatic Forecast Workflow ###

### Step 1: Download Required Data

## Target data (Y-variables)

## Site metadata

## Past meteorological data for calibration (X-variables)

## Weather forecast (future X)

### Step 2: Calibrate forecast model

### Step 3: Make a forecast into the future



download_targets <- function(){
  readr::read_csv("https://data.ecoforecast.org/neon4cast-targets/aquatics/aquatics-targets.csv.gz", guess_max = 1e6)
}

##' Download Site metadata
##' @return metadata dataframe
download_site_meta <- function(){
  site_data <- readr::read_csv("https://raw.githubusercontent.com/eco4cast/neon4cast-targets/main/NEON_Field_Site_Metadata_20220412.csv") 
  site_data %>% filter(as.integer(aquatics) == 1)
}


##' append historical meteorological data into target file
##' @param target targets dataframef
##' @return updated targets dataframe with added weather data
merge_met_past <- function(target){
  
  ## connect to data
  df_past <- neon4cast::noaa_stage3()
  
  ## filter for site and variable
  sites <- unique(target$site_id)
  noaa_past <- df_past |> 
    dplyr::filter(site_id %in% sites,
                  variable == "air_temperature") |> 
    dplyr::collect()
  
  noaa_past_mean = noaa_past |> 
    dplyr::select(datetime, site_id, prediction, parameter) |>
    dplyr::mutate(date = as_date(datetime)) |>
    dplyr::group_by(date, site_id) |>
    dplyr::summarize(air_temperature = mean(prediction, na.rm = TRUE),
                     .groups = "drop") |>
    dplyr::rename(datetime = date) |>
    dplyr::mutate(air_temperature = air_temperature - 273.15) 
  
  ## Aggregate (to day) and convert units of drivers
  target <- target %>% 
    group_by(datetime, site_id,variable) %>%
    summarize(obs2 = mean(observation, na.rm = TRUE), .groups = "drop") %>%
    mutate(obs3 = ifelse(is.nan(obs2),NA,obs2)) %>%
    select(datetime, site_id, variable, obs3) %>%
    rename(observed = obs3) %>%
    filter(variable %in% c("temperature", "oxygen")) %>% 
    tidyr::pivot_wider(names_from = "variable", values_from = "observed")
  
  ## Merge in past NOAA data into the targets file, matching by date.
  target <- left_join(target, noaa_past_mean, by = c("datetime","site_id"))
  
}

##' Download NOAA GEFS weather forecast
##' @param forecast_date start date of forecast
##' @return dataframe
download_met_forecast <- function(forecast_date){
  ## connect to data
  df_future <- neon4cast::noaa_stage2()
  
  noaa_date <- forecast_date - lubridate::days(1)  #Need to use yesterday's NOAA forecast because today's is not available yet
  
  ## filter available forecasts by date and variable
  met_future <- df_future |> 
    dplyr::filter(reference_datetime == lubridate::as_datetime(noaa_date),
                  datetime >= lubridate::as_datetime(forecast_date), 
                  variable == "air_temperature") |> 
    dplyr::collect()
  
  ## aggregate to daily
  met_future <- met_future %>% 
    mutate(date = lubridate::as_date(datetime)) %>% 
    group_by(date, site_id, parameter) |> 
    summarize(air_temperature = mean(prediction), .groups = "drop") |> 
    mutate(air_temperature = air_temperature - 273.15) |> 
    select(date, site_id, air_temperature, parameter)
  
  return(met_future)
}