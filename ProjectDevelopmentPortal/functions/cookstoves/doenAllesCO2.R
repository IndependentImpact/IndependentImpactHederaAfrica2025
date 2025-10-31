#' @title doenAllesCO2
#' @description Connect to the DB with wood-weighing results andf calculate CO2 ER
#' @param con Connection to DB. Use dbConnect_wws by default
#' @param dfCOEF dataframe
#' @param dffNRB dataframe
#' @param outdir Character
#' @param outname Character
#' @param logdir Character
#' @param place Character or NULL (default)
#' @export
#'
#'

doenAllesCO2 <- function(con = dbConnect_wws(),
                         dfCOEF = NULL,
                         dffNRB = NULL,
                         outdir = "",
                         outname = "",
                         logdir = "",
                         place = NULL,
                         fuel = "wood", # lelike hack - bou in DB in en as eienskap van HH?
                         control = "Control",
                         treatment = "LPG"
                         ){
  if (is.null(dfCOEF)) stop("dfCOEF cannot be NULL")
  if (is.null(dffNRB)) stop("dffNRB cannot be NULL")

  x <- CTOWWS::doenAllesWWS(outdir = outdir, outname = outname, logdir = logdir)

  if (is.null(place)) {
    places <- tbl(con, "tbl_households") %>%
      select(-stand_number, -respondent_name) %>%
      collect()
  } else {
    places <- tbl(con, "tbl_households") %>%
      filter(place %in% place) %>%
      select(-stand_number, -respondent_name) %>%
      collect()
  }

  dfres <- x$res %>%
    ungroup() %>%
    unnest(data) %>%
    select(Date, household_qr_code , kg_p_month_m2) %>%
    mutate(year = lubridate::year(Date),
           fuel = fuel) %>%
    select(-Date) %>%
    distinct() %>%
    left_join(places) %>%
    filter(!is.na(assignment)) %>%
    select(place, year, fuel, assignment, household_qr_code, kg_p_month_m2) %>%
    mutate(kg_p_month_m2 = units::as_units(kg_p_month_m2, "kg")) %>%
    arrange(year, place, fuel, assignment)

  CBfj <- dfres %>% filter(assignment == control)
  CPfj <- dfres %>% filter(assignment == treatment)
  eef <- calculatEEF(CBfj = CBfj, CPfj = CPfj)
  CBy <- calculateCBy(eef = eef, CPy = CPfj)
  CPy <- CPfj %>%  group_by(place, year, fuel, assignment) %>% summarise(CP = sum(kg_p_month_m2, na.rm = TRUE))
  CLy <- CPy %>% mutate(L = units::as_units(0, "kg"))
  BE <- calculateE(CBy, onlyOutcomeAndGroups = TRUE, COEF = dfCOEF, fNRB = dffNRB)
  PE <- calculateE(CPy, var = "CP", outcome = "PE", onlyOutcomeAndGroups = TRUE)
  LE <- calculateE(CLy, var = "L", outcome = "LE", onlyOutcomeAndGroups = TRUE)
  ER <- calculateER(BE, PE, LE)

  dfBPL <- left_join(eef, CBy) %>% left_join(CLy) %>% left_join(dfCOEF) %>% left_join(dffNRB)
  BPL <- left_join(BE, PE) %>% left_join(LE)

  list(dfBPL = dfBPL,
       BPL = BPL,
       ER = ER)

}
