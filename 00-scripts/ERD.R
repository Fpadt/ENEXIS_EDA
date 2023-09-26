source('~/isdb/00-scripts/00_setup.R')

# get MARA and remove duplicates
MARA <- 
  f_read_mara() %>%
  .[, DUP := duplicated(EAN11)] %>%
  .[DUP == FALSE & EAN11 != ""]

LART <- 
  raw_data[["ARTICLE"]][, .(LOCAL_ARTICLE_ID, LOCAL_EAN, CATEGORY_CODE)] %>%
  setnames(paste0("LART_", names(.)))

SLSO <- raw_data[["SELL_OUT"]]

PMDM <- 
  copy(MARA[, .(GLOBAL_ARTICLE_NUMBER, MATKL)]) %>%
  setnames(paste0("PMDM_", names(.)))

MARA %>% setnames(., paste0("ECC_", names(.)) )

COMB <- 
  LART[SLSO, 
       on = .(LART_LOCAL_ARTICLE_ID == LOCAL_ARTICLE_ID),
       nomatch = NA] %>%
  setnames("LART_LOCAL_ARTICLE_ID", "LOCAL_ARTICLE_ID" ) %>%
  MARA[., 
       on = .(ECC_EAN11 == LART_LOCAL_EAN),
       nomatch = NA] %>%
  setnames("ECC_EAN11", "LART_LOCAL_EAN" ) %>%
  PMDM[., 
       on = .(PMDM_GLOBAL_ARTICLE_NUMBER == GLOBAL_ARTICLE_NUMBER),
       nomatch = NA] %>%
  setnames("PMDM_GLOBAL_ARTICLE_NUMBER", "GLOBAL_ARTICLE_NUMBER" ) 

# ------------------------------------------------------------------------
PMDM <- f_read_pmdm()
MARA <- f_read_mara()

PMDM <- MARA[PMDM, on =.(MATNR == SAPARTICLENUMBER), nomatch = 0]
