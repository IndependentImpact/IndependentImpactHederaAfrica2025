
# Set paths.
appdir <- "REPLACE_ME" # The location of the IndependentImpactUI dir on your machine.
logdir <- sprintf("%slogs/", appdir)
moddir <- sprintf("%smodules/", appdir)
funcdir <- sprintf("%sfunctions/", appdir)
wrkflwdir <- sprintf("%sworkflows/", appdir)
schemadir <- sprintf("%sschemas/", appdir)
tbldefdir <- sprintf("%stable-definitions/", appdir)
tmpdir <- sprintf("%stmp/", appdir)
cstmshinyctrlsdir <- sprintf("%scustom-shiny-controls/", appdir)
cstmcssdir <- sprintf("%swww/", appdir)
draftdocdir <- sprintf("%sdraft-documents/", appdir)
flureemapdir <- sprintf("%sfluree-maps/", appdir) # TODO. Just a temporary thing.

# Load packages.
library(shiny)
library(shinyjs)
library(shinythemes)
library(tidyr)
library(tibble)
library(httr)
library(RJDBC)
library(RPostgreSQL)
library(PKI)
library(plotly)
library(hedera)
library(jellyfi3shR)
library(logger)
#library(shinyBS)
library(sf)
library(leaflet)
library(novaRush)
library(reticulate)

# Source additional functions and modules.
for (x in c(funcdir, moddir, wrkflwdir, cstmshinyctrlsdir, flureemapdir)) {
  fps <- dir(path = x, full.names = TRUE, recursive = TRUE,
             include.dirs = FALSE, pattern = ".R$", 
             all.files = (x == funcdir))
  for (fp in fps) { source(fp) }
}
rbind.fill <- plyr::rbind.fill

if (!dir.exists(logdir)) {
  message("WARNING: logdir does not exist... Creating it now...")
  dir.create(logdir, mode = "777")
}
if (!dir.exists(draftdocdir)) {
  message("WARNING: draftdocdir does not exist... Creating it now...")
  dir.create(draftdocdir, mode = "777")
}

load(sprintf("%suiguhk.Rda", tmpdir))
hederaNetwork <- c("testnet", "previewnet", "mainnet")[1]

DB_NAME <- switch(
  hederaNetwork,
  testnet = "REPLACE_ME",
  previewnet = "REPLACE_ME",
  mainnet = "REPLACE_ME")

DB_HOST <- "REPLACE_ME" # E.g., "127.0.0.1", "localhost" or "200.10.10.42".

DB_HOST_PORT <- as.integer("REPLACE_ME") # typically 5432

Sys.setlocale("LC_ALL", "English")

DEFAULT_INP_WIDTH <- '100%'

defaultFlureeConf <- novaRush::setConfig(
  host = "data.flur.ee", 
  ledger = "jelly1/independent-impact-4", 
  signMessages = FALSE)
defaultFlureeConf <- novaRush::setContext(
  currentConfig = defaultFlureeConf, 
  context = list(
    "f" = "https://ns.flur.ee/ledger#",
    "indimp" = "https://independentimpact.org/ns/")) 

HEDERA_ADDRESS_BOOK_TOPIC_ID <- "REPLACE_ME" # TODO. Not used.
reticulate::use_python(iwefdj$PYTHON_PATH, required = TRUE)
hiero <- reticulate::import("hiero_sdk_python")
hieroDid <- reticulate::import("hiero_did_sdk_python")
asyncio <- reticulate::import("asyncio")
