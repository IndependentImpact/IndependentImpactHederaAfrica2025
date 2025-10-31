
createGHGindicators <- function() {
  
  library(CTOWWS)
  library(tidyverse)
  library(DBI)
  library(units)
  library(openxlsx)
  
  con <- dbConnect_wws()
  
  x <- CTOWWS::doenAllesWWS(outdir = "~/tmp/dMRVToetsdata", 
                            outname = "dMRVToets", 
                            logdir = "~/tmp/dMRVToetsdata")
  
  smash <- tbl(con, "tbl_households") %>% 
    filter(place == "Smashblock") %>% 
    select(-stand_number, -respondent_name) %>% 
    collect() 
  
  dfCOEF <- tibble(fuel = c("wood", "charcoal"), COEF = c(units::as_units(1560, "g/kg"), units::as_units(2860, "g/kg")))
  dffNRB <- tibble(place = "Smashblock", year = c(2021, 2022), fNRB = c(0.3, 0.31))
  
  dfres <- x$res %>% 
    ungroup() %>%   
    select(household_qr_code , kg_p_month_m2) %>% 
    left_join(smash) %>% 
    filter(!is.na(assignment)) %>% 
    mutate(fuel = sample(c("wood", "charcoal"), size = nrow(smash), replace = TRUE), 
           year = sample(c(2022, 2021), size = nrow(smash), replace = TRUE)) %>% 
    select(place, year, fuel, assignment, household_qr_code, kg_p_month_m2) %>% 
    mutate(kg_p_month_m2 = units::as_units(kg_p_month_m2, "kg"))
  
  # Frequency approach ------------------------------------------------------
  ## Kg per fire for each place, fuel type and season
  dfKg.p.f <- tribble(~place, ~fuel, ~season, ~ kgpf,
                      "Smashblock", "wood", "summer",  x[[4]]$kg_w_p_fire_m2[[1]]@val$Mean, 
                      "Smashblock", "charcoal", "summer",  x[[4]]$kg_w_p_fire_m2[[1]]@val$Mean *0.5
  )
  
  ## Counting results per household per period
  ### Simulated frequency results for January 2023. 
  #### Data will come in from instrument platform in this format:
  dfFreqRes <- dfres %>% 
    filter(assignment == "LPG") %>% 
    select(-kg_p_month_m2) %>%
    mutate(freq = sample(x = 5:35*12, size = n(), replace = TRUE)) %>% 
    mutate(date_start_monitoring_period = as.Date("2021-01-01"),
           date_end_monitoring_period = as.Date("2021-01-31"),
           year = lubridate::year(date_end_monitoring_period))
  
  # Save simulated data artefacts -------------------------------------------
  
  save(dfFreqRes, dfKg.p.f, dfres, dfCOEF, dffNRD, file = "DemoMonitoringData.Rda")
  
  dbDisconnect(con)
  
  return(invisible(0))
}