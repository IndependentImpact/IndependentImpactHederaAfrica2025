
calculateCookstoveER <- function() {
  
  # Libraries ---------------------------------------------------------------
  library(Guardener)
  library(CTOWWS)
  library(tidyverse)
  
  # Load Demo Monitoring Data  ----------------------------------------------
  
  load("DemoMonitoringData.Rda")
  
  # Choose approach ---------------------------------------------------------
  
  approach <-  c("frequency", "weighing")[1]
  
  # Wood weighing approach --------------------------------------------------
  CBfj <- dfres %>% filter(assignment == "Control") 
  CPfj <- dfres %>% filter(assignment == "LPG")
  eef <- calculatEEF(CBfj = CBfj, CPfj = CPfj) %>% filter(year == "2021") 
  
  # Frequency Approach ------------------------------------------------------
  ### Calculate Project fuel use by multiplying mean fuel per fire with frequency
  CPfj_f <- dfFreqRes %>% 
    filter(assignment == "LPG") %>% 
    left_join(dfKg.p.f) %>% 
    mutate(kg_p_month_m2 = kgpf * freq) %>% 
    select(-kgpf, -freq) %>% 
    mutate(kg_p_month_m2 = units::as_units(kg_p_month_m2, "kg"))
  
  # Calculate ER ------------------------------------------------------------
  
  if (approach == "frequency") CPfj <- CPfj_f
  
  CBy <- calculateCBy(eef = eef, CPy = CPfj) 
  CPy <- CPfj %>% group_by(place, year, fuel, assignment) %>% summarise(CP = sum(kg_p_month_m2, na.rm = TRUE))
  CLy <- CPy %>% mutate(L = units::as_units(0, "kg"))
  BE <- calculateE(CBy, onlyOutcomeAndGroups = TRUE, COEF = dfCOEF, fNRB = dffNRB)
  PE <- calculateE(CPy, var = "CP", outcome = "PE", onlyOutcomeAndGroups = TRUE)
  LE <- calculateE(CLy, var = "L", outcome = "LE", onlyOutcomeAndGroups = TRUE)
  ER <- calculateER(BE, PE, LE)
  
  
  # Make Guardian Ready -----------------------------------------------------
  ATL <- Glogin(un = Sys.getenv("guardian_sr_un"), 
                pw = Sys.getenv("guardian_sr_pw"))
  AT <- ATL$accessToken
  dfSchemas <- GgetSchemas(AT,  returndf = TRUE)
  dfTemplates <- dfSchemas %>% 
    GmakeSchemaTemplate() %>% 
    mutate(templ = map(data, ~names2tibble(.) )) %>% 
    ungroup() %>% 
    select(-data)
  dfPolicies <- GgetPolicies(AT, returndf = TRUE)
  policyids <- dfPolicies$id
  idx <- which(dfPolicies$name[[1]] == "Improved Cookstove Policy - MR Verification Subpolicy")
  dfBlocks <- GgetPolicyBlocks(AT, policyId = policyids[[1]][[as.integer(idx)]])
  
  
  # Stop. dfTemplates is leeg
  dfM <- dfTemplates %>% filter(name == "Monitoring Report (MR)") %>% unnest(templ)
  
  dmt <- tibble(
    id_project = "testProj", 
    date_start_monitoring_period = CPfj_f$date_start_monitoring_period %>% unique(), 
    date_end_monitoring_period = CPfj_f$date_end_monitoring_period %>% unique(),
    reductions_emissions = ER$ER[[1]] %>% units::drop_units(),
    datapar_monitored = tibble(
      data_parameter = "ER", 
      data_unit = "tCO@eq", 
      description = "ER month 1 2021", 
      source_of_data = "WWS and Freq", 
      values_applied = ER$ER[[1]] %>% units::drop_units(), 
      methods_and_procedures = " ", 
      monitoring_frequency  = "monthly",
      qa_qc_procedures  = " ", 
      purpose = " ", 
      comment = " ")
  ) 
  # test
  all(names(dmt) %in% names(dfM))
  
  topicID <- dfSchemas %>% filter(uuid == dfM$uuid[[1]]) %>% select(topicId) %>% distinct() %>% pull()
  contextURL <- dfSchemas %>% filter(uuid == dfM$uuid[[1]]) %>% select(-description) %>% unnest(document) %>% select(contextURL) %>% distinct() %>% pull()
  
  blockInfo <- whichBlocksHasSchema(dfBlocks = dfBlocks, schemaId = dfM$uuid[[1]]) %>% 
    filter(blockType == "requestVcDocumentBlock")
  
  # inspect
  blockInfo$tag
  blockID <- blockInfo$id[[1]]
  blockPolicy <- blockInfo$policyId[[1]]
  
  # post
  Gpost2block(accessToken = Glogin()$accessToken, 
              document = dmt, 
              schemaID = dfM$uuid[[1]],
              schemaIPFSurl = ifelse(is.na(contextURL), " ", contextURL), 
              policyId = blockPolicy,
              BlockId = blockID)
  
  # Display 
  dfBPL <- left_join(eef, CBy) %>% left_join(CLy) %>% left_join(dfCOEF) %>% left_join(dffNRB)
  BPL <- left_join(BE, PE) %>% left_join(LE) 
  
  return(invisible(0))
}