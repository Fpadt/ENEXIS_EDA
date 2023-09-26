library(readxl)
library(data.table)
library(magrittr)
library(stringdist)

# BRANDT <- read_excel("~/data/00-raw/src/BRANDT.xlsx")

# stringsim('nagesh','ganesh', method='osa')
# stringsim('nagesh','ganesh', method='dl', weight = c(d = 1, i = 1, s = 1, t = .1))

ART_BR <-
  f_read_opco_data(
    denv = params$denv,                    # environment acc | prd
    opco = "br",                           # 2 character opco    
    ds   = c("ARTICLE"),                   # specific Data Sources
    nrws = as.numeric(params$nrws)         # c(-1,0,1,n)
  ) %>%
  .[["ARTICLE"]] %>%
  .[, .(GLOBAL_ARTICLE_NUMBER, LOCAL_EAN, PRODUCT_BRAND )] %>%
  unique() %>%
  .[, OPCO:= "BR"]

ART_CH <-
  f_read_opco_data(
    denv = params$denv,                    # environment acc | prd
    opco = "ch",                    # 2 character opco    
    ds   = c("ARTICLE"),                   # specific Data Sources
    nrws = as.numeric(params$nrws)         # c(-1,0,1,n)
  ) %>%
  .[["ARTICLE"]] %>%
  .[, .(GLOBAL_ARTICLE_NUMBER, LOCAL_EAN, PRODUCT_BRAND )] %>%
  unique() %>%
  .[, OPCO:= "CH"]

ART_CO <-
  f_read_opco_data(
    denv = params$denv,                    # environment acc | prd
    opco = "co",                           # 2 character opco    
    ds   = c("ARTICLE"),                   # specific Data Sources
    nrws = as.numeric(params$nrws)         # c(-1,0,1,n)
  ) %>%
  .[["ARTICLE"]] %>%
  .[, .(GLOBAL_ARTICLE_NUMBER, LOCAL_EAN, PRODUCT_BRAND )] %>%
  unique() %>%
  .[, OPCO:= "CO"]

ART_IT <-
  f_read_opco_data(
    denv = params$denv,                    # environment acc | prd
    opco = "it",                           # 2 character opco    
    ds   = c("ARTICLE"),                   # specific Data Sources
    nrws = as.numeric(params$nrws)         # c(-1,0,1,n)
  ) %>%
  .[["ARTICLE"]] %>%
  .[, .(GLOBAL_ARTICLE_NUMBER, LOCAL_EAN, PRODUCT_BRAND )] %>%
  unique() %>%
  .[, OPCO:= "IT"]

ART_PE <-
  f_read_opco_data(
    denv = params$denv,                    # environment acc | prd
    opco = "PE",                           # 2 character opco    
    ds   = c("ARTICLE"),                   # specific Data Sources
    nrws = as.numeric(params$nrws)         # c(-1,0,1,n)
  ) %>%
  .[["ARTICLE"]] %>%
  .[, .(GLOBAL_ARTICLE_NUMBER, LOCAL_EAN, PRODUCT_BRAND )] %>%
  unique() %>%
  .[, OPCO:= "PE"]

ART_TR <-
  f_read_opco_data(
    denv = params$denv,                    # environment acc | prd
    opco = "tr",                           # 2 character opco    
    ds   = c("ARTICLE"),                   # specific Data Sources
    nrws = as.numeric(params$nrws)         # c(-1,0,1,n)
  ) %>%
  .[["ARTICLE"]] %>%
  .[, .(GLOBAL_ARTICLE_NUMBER, LOCAL_EAN, PRODUCT_BRAND )] %>%
  unique() %>%
  .[, OPCO:= "TR"]

ARTICLE <- 
  rbind(ART_BR, ART_CH, ART_CO, ART_IT, ART_PE, ART_TR ) %>%
  .[LOCAL_EAN != "" | GLOBAL_ARTICLE_NUMBER != ""]

# FUZZY_SEARCH_RESULT <- 
#   stringdistmatrix(
#     a = toupper(ART), 
#     b = toupper(BRANDT$ZZBRAND_DESC),
#     useNames = TRUE,
#     method = "osa",
#     weight = c(d = 1, i = 1, s = 1, t = 1)) %>%
#   as.data.table(keep.rownames = TRUE) %>%
#   melt.data.table(
#     id.vars = "rn",
#     variable.factor = FALSE,
#     value.factor = FALSE
#   ) %>%
#   setorder(rn, value) %>%
#   .[, .SD[1:5], by = .(rn)] %T>%
#   setnames(c("L_BRAND", "G_BRAND", "STRINGDIST"))

# remove all local_brands with dist 0
# phonetic(c('Euler','Gauss','Hilbert','Knuth','Lloyd','Lukasiewicz','Wachs'),method='soundex')

PMDM <- f_read_pmdm("20201123151232_PMDM_Full.csv")

# CHECK1 <- 
#   copy(ARTICLE) %>%
#   .[LOCAL_EAN != "" & GLOBAL_ARTICLE_NUMBER != "", 
#     .(PRODUCT_BRAND, LOCAL_EAN, GLOBAL_ARTICLE_NUMBER)] %>%
#   PMDM[, .(SAPARTICLENUMBER, G_BRAND = BRAND)][
#     ., on = .(SAPARTICLENUMBER ==GLOBAL_ARTICLE_NUMBER)] %>%
#   PMDM[, .(EANCODE, L_BRAND = BRAND)][
#     ., on = .(EANCODE == LOCAL_EAN)] %T>%
#   setcolorder(c("EANCODE", "SAPARTICLENUMBER", "L_BRAND", "G_BRAND", "PRODUCT_BRAND")) %T>%
#   setorder(L_BRAND, G_BRAND)
# 
# RESULT1 <- 
#   CHECK1[L_BRAND != PRODUCT_BRAND, .(L_BRAND, G_BRAND, PRODUCT_BRAND)] %>%
#   unique() %>%
#   .[, STRDIST:= stringdist(
#     a = toupper(.$L_BRAND), b = toupper(.$PRODUCT_BRAND), method = "osa")] %T>%
#   View()

# stringdist(a = RESULT1$L_BRAND, b = CHECK2$G_BRAND      , method = "osa")
# stringdist(a = RESULT1$L_BRAND, b = CHECK2$PRODUCT_BRAND, method = "osa")

CHECK2 <- 
  copy(ARTICLE) %>%
  .[LOCAL_EAN != "", 
    .(PRODUCT_BRAND, LOCAL_EAN, OPCO)] %>%
  PMDM[, .(EANCODE, PMDM_BRAND = BRAND)][
    ., on = .(EANCODE == LOCAL_EAN)] %>%
  .[!is.na(PMDM_BRAND) & PMDM_BRAND !=""] %T>%
  setcolorder(c("EANCODE", "PMDM_BRAND", "PRODUCT_BRAND")) %T>%
  setorder(PMDM_BRAND)

RESULT2 <- 
  CHECK2[PMDM_BRAND != PRODUCT_BRAND, .(PMDM_BRAND, PRODUCT_BRAND)] %>%
  unique() %>%
  .[, STRDIST:= stringdist(
    a = toupper(.$PMDM_BRAND), b = toupper(.$PRODUCT_BRAND), method = "osa")] 

RESULT0 <- 
  RESULT2[STRDIST == 0, .(PMDM_BRAND)] %>%
  unique()

View(RESULT2[!PMDM_BRAND %in% RESULT0$PMDM_BRAND])
write.xlsx(
  RESULT2[!PMDM_BRAND %in% RESULT0$PMDM_BRAND], file = "PMDM_EAN.xlsx")  



CHECK3 <- 
  copy(ARTICLE) %>%
  .[GLOBAL_ARTICLE_NUMBER != "", 
    .(PRODUCT_BRAND, GLOBAL_ARTICLE_NUMBER)] %>%
  PMDM[, .(SAPARTICLENUMBER, PMDM_BRAND = BRAND)][
    ., on = .(SAPARTICLENUMBER == GLOBAL_ARTICLE_NUMBER)] %>%
  .[!is.na(PMDM_BRAND) & PMDM_BRAND !=""] %T>%
  setcolorder(c("SAPARTICLENUMBER", "PMDM_BRAND", "PRODUCT_BRAND")) %T>%
  setorder(PMDM_BRAND)

RESULT3 <- 
  CHECK3[PMDM_BRAND != PRODUCT_BRAND, .(PMDM_BRAND, PRODUCT_BRAND)] %>%
  unique() %>%
  .[, STRDIST:= stringdist(
    a = toupper(.$PMDM_BRAND), b = toupper(.$PRODUCT_BRAND), method = "osa")] %T>%
  View()

RESULT0 <- 
  RESULT3[STRDIST == 0, .(PMDM_BRAND)] %>%
  unique()

View(RESULT3[!PMDM_BRAND %in% RESULT0$PMDM_BRAND])
write.xlsx(
  RESULT3[!PMDM_BRAND %in% RESULT0$PMDM_BRAND], file = "PMDM_GAN.xlsx")  
