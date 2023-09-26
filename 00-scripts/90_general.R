# General Functions
# https://www.countryflags.com/en/switzerland-flag-icon.html
# https://www.iban.com/country-codes
# https://fontawesome.com/icons/book-open?style=solid
# https://launchpad.support.sap.com/#/notes/173241
# http://da1.grandvision.com:8080/BOE/InfoSteward
# https://style.tidyverse.org/
# https://r4ds.had.co.nz/exploratory-data-analysis.html
# https://packagemanager.rstudio.com/client/#/repos/1/packages/readr
# http://r-pkgs.had.co.nz/description.html
# https://www.dexlabanalytics.com/blog/statistical-application-of-r-python-know-skewness-kurtosis-and-calculate-it-effortlessly
# https://regex101.com/
# https://launchpad.support.sap.com/#/notes/173241 

fTableOverview <- function(pTable){
  
  lstUnique    <- lapply(pTable   , FUN = unique)
  vUniqueCount <- sapply(lstUnique, FUN = length)
  vUniqueCount <- sort(vUniqueCount, decreasing = TRUE)
  
  dtRATIO    <- data.table(FLDNM  = names(vUniqueCount),
                           UCOUNT = vUniqueCount,
                           RATIO  = round(nrow(pTable)/vUniqueCount, 0)) 
  
  fHDR   <- function(x){
    l_ret <- paste(sort(x)[1:ifelse(length(x) < 11, length(x), 10)], collapse = "/")
    if (substr(l_ret, 1, 1) == "/") { l_ret <- paste0("#", l_ret)}
    return(l_ret)
  }
  
  
  dtTMP  <- data.table(FLDNM = names(lstUnique),
                       EXAMP = sapply(lstUnique, FUN = fHDR))
  
  dtRATIO <-
    merge(dtRATIO, dtTMP, by = "FLDNM")[order(UCOUNT, decreasing = TRUE)]
  
  return(list(UCOUNT = matrix(vUniqueCount,
                              ncol = 1,
                              dimnames = list(names(vUniqueCount), "Count")), 
              UNIQUE = lstUnique,
              dtRATIO = dtRATIO ))
}

# param: Tables = list with tables 
f_analyze_tables <- 
  function(tables) {
    
    # local function to analyze a single table
    # note that loop is over column_names as this is used 
    # as column name in the factor result
    f_analyze_table <- 
      function(x) {
        map(.x = names(x), .f = f_analyze_column, TABLE=x) %>%
          magrittr::set_names(names(x))
      }
    
    # local function to analyze a single column
    f_analyze_column <- 
      function(COLUMN_NAME, TABLE) {
        lst <- list()
        
        lst[["FACTOR"]] <-  f_create_factor(COLUMN_NAME, TABLE)
        
#        lst[["WIDGETS"]] <- f_create_widget()
        
        # note FACTORS are past for mode calculation
        if(is.numeric(TABLE[, get(COLUMN_NAME)]) ) {
          lst[["DSTATS"]] <- f_calc_dstats(COLUMN_NAME, TABLE, lst)
        }
        
        lst
      }
    
    # main loop over all tables in list    
    map(.x = tables, .f = f_analyze_table)  %>%
      magrittr::set_names(names(tables))
    
  }

f_get_column_analysis <- 
  function(x, .lst, .table, .resultset) {
    .lst[[.table]][[x]][[.resultset]]
  }

f_remove_duplicates <- 
  function(primary_key, lst_raw_data ) {
    
    f_remove_duplicates_by_key <- 
      function(x, pk, rw) { 
        if (pk[[x]][["valid"]] == F) {
          lst_raw_data[[x]] <- 
            unique(lst_raw_data[[x]], 
                   by = pk[[x]][["columns"]] )
        } else {
          lst_raw_data[[x]]
        }
      }
    
    lst <- 
      map(
      .x = names(primary_key), .f = f_remove_duplicates_by_key,
      pk = primary_key, rw = lst_raw_data) 
    names(lst) <- names(primary_key)
    lst
  }

f_remove_constants <- 
  function(x){
    
    f_is_constant <- 
      function(x, dt) {
        if (nrow(unique(dt[, x, with=F])) <= 1) {
          x
        } else {
          NA
        }
      }
    
    constants <- 
      map_chr(names(x), .f = f_is_constant, x) %>%
      .[!is.na(.)]
    
    x[, setdiff(names(x), constants), with=FALSE]
  }

f_remove_no_one_to <- 
  function(x){
    
    f_has_1 <- 
      function(x, dt) {
        if (nrow(dt[get(x) == "1", x, with=F]) == 0 & x != "NMS") {
          x
        } else {
          NA
        }
      }
    
    one_to <- 
      map_chr(names(x), .f = f_has_1, x) %>%
      .[!is.na(.)]
    
    x[, setdiff(names(x), one_to), with=FALSE]
  }

f_check_primkeys <- 
  function(lst_raw_data, dt_metadata_didp) {
    l <- lapply(
      names(lst_raw_data), 
      f_check_table_primkey,
      lst_raw_data     = lst_raw_data,
      dt_metadata_didp = dt_metadata_didp) 
    names(l) <- names(lst_raw_data)
    l
  }

f_check_table_primkey <- 
  function(x, lst_raw_data, dt_metadata_didp) {
    f_determine_unique_keys(
      lst_raw_data[[x]],
      primkey = dt_metadata_didp[PRIMKEY == "X" & DS == x, COLUMN_NAME] 
    )
  }

f_determine_unique_keys <- 
  function(x, primkey) {
    
    # return unique key if exists else NULL
    f_get_unique_key <- 
      function(x, DT){
        if (!any(duplicated(DT, by = x))) {
          x
        }
      }
    
    # remove existing unique keys with less columns
    f_no_subkeys <- 
      function(x, ukeys) {
        
        f_no_subkey <- 
          function(key, x) {
            length(setdiff(key, x)) != 0 | is.null(key) 
          }
        
        if (all(unlist(lapply(ukeys, f_no_subkey, x = x)))) {
          x
        } 
      }
    
    # initialization
    unique_keys <- list()
    lstReturn <- list()
    
    if (missing(primkey)) {
      nms       <- names(x)[x[, sapply(x, is.character)]]
    } else {
      nms <- primkey
    }
    
    lstReturn[["columns"]] <- nms

    if (length(nms) > 0 ) {
      for (i in 1:length(nms)) {
        
        # determine candicate keys without already found shorter keys
        candidate_keys <- 
          combn(x = nms, m = i, FUN = f_no_subkeys, 
                ukeys = unique_keys, simplify = F ) 
        
        if (length(candidate_keys) != 0) {
          candidate_keys[sapply(candidate_keys, is.null)] <- NULL
          
          # check whether candicate keys are unique
          unique_keys <- 
            c(unique_keys, lapply(candidate_keys, f_get_unique_key, DT = x))
          unique_keys[sapply(unique_keys, is.null)] <- NULL
        }
      }

      # create vector with key found
      sapply(unique_keys, paste0, collapse = "; ")
      
      if (!missing(primkey) & length(unique_keys) == 0) {
        
        lstReturn[["duplicates"]] <-
          copy(x) %>%
          .[, DUP := duplicated(., by = primkey)] %>%
          .[, DUP := any(DUP)    , by = primkey]  %>%
          .[DUP == TRUE] %>%
          .[, DUP := NULL] %T>%
          setkeyv(primkey) %>%
          .[, GRP := .GRP, by = primkey] %T>%
          setcolorder(c(
            c("GRP", primkey), setdiff(names(.), c("GRP", primkey))))
        
      } 
      
      lstReturn[["valid"]]    <- (length(unique_keys) > 0)
      lstReturn[["keys"]] <- unique_keys
    
      lstReturn  
    
    }
  }

f_update_md <- 
  function(new, old, deltaday = today(), pk) {
    
    if ( "TO"   %in% names(old)) {old[,   TO:= NULL] }
    if (!"FROM" %in% names(old)) {old[, FROM:= ymd("1000-01-01")] }
    if (!"FROM" %in% names(new)) {new[, FROM:= deltaday] }
    
    # delete 
    old <- old[FROM != deltaday]
    
    KEYS    <- str_subset(names(old), "FROM", negate = TRUE)
    enddate <- ymd("9999-12-31")
    
    rbindlist(list(new, old), use.names = TRUE) %>%
      # create FROM-TO Interval
      .[order(FROM),
        TO:= shift(FROM-1, n=1, fill=enddate, type="lead"), 
        by = pk] %T>%
      setkeyv(KEYS) %>%
      # create counters to identify Merge.Periods
      .[order(FROM), 
        Merge.Period := {
          tmp <- shift(TO, fill = FROM[1] - 1)
          #    stopifnot(all(Start.Date > tmp))
          cumsum(FROM > tmp + 1) }, 
        by = KEYS] %>%
      # aggregate to find the overal FROM and TO dates for  KEYS      
      .[, .(FROM = min(FROM), TO = max(TO)), 
        by = c(KEYS, "Merge.Period")] %>%
      .[, Merge.Period:= NULL] %T>%
      setkeyv( c(pk, "FROM"))
  }

# f_cardinality <- 
#   function(x, dt, unq = TRUE) {
#     
#     # make sure that current table is unqiue
#     if (unq == FALSE) { dt <- unique(dt)}
#     
#     DT <- 
#       dt[, .N, by = .(C = get(x))] %>%
#       .[N > 1, N:= 2]
#     
#     N  <- unique(DT[, N]) %>% 
#       sort() %>% 
#       sub(pattern = "2", replacement = "N", x = .) %>%
#       paste0(collapse = "") 
#     
#     # check if some table entries do not have a value for c
#     if (any(is.na(DT[, C]))) {
#       ret <- paste0("0", N)
#     } else {
#       ret <- N
#     }
#     ret
#   }


f_cardinality <- 
  function(x) {
    nms <- c(names(x))
    
    lapply(nms, f_cardinality1, x) %>%
      rbindlist(use.names = TRUE) %T>%
      setcolorder(nms)
  }


f_cardinality1 <- 
  function(x, dt) {
    
    nms <- setdiff(names(dt), x)
    sapply(nms, f_cardinality2, dt=dt, y = x) %>%
      as.data.table(x = t(.), keep.rownames = TRUE) %>%
      .[, c(x) := "X"]
        

  }

f_cardinality2 <- 
  function(x, dt, y) {
    
    unique(dt[, c(x, y), with = F]) %>%
    .[rowSums(is.na(.)) < length(names(.))] %>%
      f_col_cardinality(y, .)
  }

# f_cardinality3 <- 
#   function(x, dt) {
#     nms <- setdiff(names(dt), x)
#     l   <- sapply(nms, f_col_cardinality, dt )
# #    names(l) <- nms
#     l
#   }

f_col_cardinality <- 
  function(x, dt ) {
    dt1 <- 
      dt[, .N, by = eval(x)] %>%
      .[N > 2, N:= 2]
    
    ret <- na.omit(dt1) %>% .[, N] %>% 
      unique() %>% sort() %>% paste0(collapse = "")
    
    if (any(is.na(dt[, setdiff(names(dt), eval(x)), with = F]))) { 
      ret <- paste0("0", ret)}
    sub(pattern = "2", replacement = "N", x = ret)
    
  }

f_find_equals <- 
  function(x, dt) {
    # get names of columns which have a 1:1 relationship with x
    nms1 <- dt[get(x) == 1, NMS]
    
    tmp <- 
      dt[NMS == x, nms1, with=F] %>%
      t() %>%
      as.data.table() %>%
      .[, NMS:= nms1] 
    
    if ("V1" %in% names(tmp)) {
      tmp[V1 == 1, NMS] 
    } else {
       NULL 
      }
  }

f_extend_with_pmdm_mapping <- 
  function(article_md, mara, pmdm, flt = c("PG2020", "PG2030")){

    # Apply filter (flt) based upon the categorycode from Local Master data
    ART_MD <- 
      article_md %>% 
      .[CATEGORY_CODE %in% flt, 
        .(LOCAL_ARTICLE_ID, GLOBAL_ARTICLE_NUMBER, LOCAL_EAN, CATEGORY_CODE)]

    ####### PMDM ######                
    PMDM       <- pmdm
    names_pmdm <- c(names(PMDM))
    
    # Match based on GLOBAL_ARTICLE_NUMBER (GAN)
    setnames(PMDM, paste0("PMDM_GAN_", names_pmdm))
    ART_MD <- 
      ART_MD %>%
      .[, PMDM_GAN_SAPARTICLENUMBER:= GLOBAL_ARTICLE_NUMBER] %>%
      PMDM[., on = .(PMDM_GAN_SAPARTICLENUMBER), nomatch = NA] %>%
      .[is.na(PMDM_GAN_ID), PMDM_GAN_SAPARTICLENUMBER:= NA]  
    
    # Match based on LOCAL_EAN (EAN)
    setnames(PMDM, paste0("PMDM_EAN_", names_pmdm))
    ART_MD <- 
      ART_MD %>%  
      .[, PMDM_EAN_EANCODE:= LOCAL_EAN] %>%
      PMDM[., on = .(PMDM_EAN_EANCODE), nomatch = NA]  %>%
      .[is.na(PMDM_EAN_ID), PMDM_EAN_EANCODE:= NA]
    
    # reset names
    setnames(PMDM, names_pmdm)

    ####### MARA ######        
    MARA       <- mara
    names_mara <- c(names(MARA))
    
    # Match based on GLOBAL_ARTICLE_NUMBER (GAN)
    setnames(MARA, paste0("MARA_GAN_", names_mara))
    ART_MD <- 
      ART_MD %>%
      .[, MARA_GAN_MATNR:= GLOBAL_ARTICLE_NUMBER] %>%  
      MARA[., on = .(MARA_GAN_MATNR), nomatch = NA] %>%
      .[is.na(MARA_GAN_EAN11), MARA_GAN_MATNR:= NA]  
    
    # Match based on LOCAL_EAN (EAN)
    setnames(MARA, paste0("MARA_EAN_", names_mara))
    ART_MD <- 
      ART_MD %>%
      .[, MARA_EAN_EAN11:= LOCAL_EAN] %>%  
      MARA[., on = .(MARA_EAN_EAN11), nomatch = NA] %>%
      .[is.na(MARA_EAN_MATNR), MARA_EAN_EAN11:= NA] %T>%
      setcolorder(c(
        "LOCAL_ARTICLE_ID", "LOCAL_EAN", "CATEGORY_CODE", "GLOBAL_ARTICLE_NUMBER",
        "PMDM_GAN_SAPARTICLENUMBER", "PMDM_GAN_EANCODE", "PMDM_GAN_ID", "PMDM_GAN_PRODUCTTYPE_ID"  ,
        "PMDM_EAN_SAPARTICLENUMBER", "PMDM_EAN_EANCODE",  "PMDM_EAN_ID", "PMDM_EAN_PRODUCTTYPE_ID",  
        "MARA_GAN_MATNR"           , "MARA_GAN_EAN11"  , "MARA_GAN_MATKL",
        "MARA_EAN_MATNR"           , "MARA_EAN_EAN11"  , "MARA_EAN_MATKL"
      ))
    
    setnames(MARA, names_mara)    

    # Add column which shows teh prevaling mapping
    ART_MD <- 
      ART_MD %>%
      .[, MAP:= ifelse(!is.na(MARA_EAN_MATKL)         , "MARA_EAN", "?")] %>%
      .[, MAP:= ifelse(!is.na(MARA_GAN_MATKL)         , "MARA_GAN", MAP)] %>%
      .[, MAP:= ifelse(!is.na(PMDM_EAN_PRODUCTTYPE_ID), "PMDM_EAN", MAP)] %>%
      .[, MAP:= ifelse(!is.na(PMDM_GAN_PRODUCTTYPE_ID), "PMDM_GAN", MAP)] %>%
      .[, MAP:= factor(
        x= MAP, 
        levels = c("?", "MARA_EAN", "MARA_GAN", "PMDM_EAN", "PMDM_GAN"),
        labels = c("unknown", "iSyn-EAN", "iSyn-GAN", "pmdm-EAN", "pmdm-GAN"))] 
    
    ART_MD 
  }

f_get_opco_sell_out_pmdm_mapping <- 
  function(opco, mara, pmdm){

    ds_A <- "ARTICLE"
    ART <- 
      f_read_opco_data(
        denv  = "prd",                    # environment acc | prd
        opco  = opco,                           # 2 character opco    
        ds    = ds_A,                           # specific Data Sources
        nrws  = Inf         # c(-1,0,1,n)
      ) %>%
      .[[ds_A]] %>%
      f_extend_with_pmdm_mapping(., mara = mara, pmdm = pmdm)
    
    dsrc <- "SELL_OUT"
    SO <- 
      f_read_opco_data(
        denv  = "prd",                    # environment acc | prd
        opco  = opco,                           # 2 character opco    
        ds    = dsrc,                           # specific Data Sources
        nrws  = Inf         # c(-1,0,1,n)
      ) %>%
      .[[dsrc]] %>%
      ART[, .(LOCAL_ARTICLE_ID, CATEGORY_CODE, MAP) ][
        ., on = .(LOCAL_ARTICLE_ID), nomatch = 0 ] %>%
      .[, .(Q = sum(QUANTITY), V = sum(GROSS_VALUE_SALES_INC_VAT)), 
        by = .(Month    = floor_date(ymd(DATE), "month"), 
               Category = factor(
                 CATEGORY_CODE, labels = c("Frames", "Sunglasses")), 
               MAP)] 
  }

f_get_opcos_sell_out_pmdm_mapping <- 
  function(opcos, pmdm_file_name ="20200924193743_PMDM_Full.csv") {

    # read pmdm data
    PMDM <- 
      f_read_pmdm(pmdm_file_name) %>%
      .[, .(ID, SAPARTICLENUMBER, EANCODE, PRODUCTTYPE_ID)]
    
    # read iSynergy MasterData
    MARA <- 
      f_read_mara() %>%
      unique(by = "EAN11") %>%
      .[!is.na(EAN11) & EAN11 != ""] %>%
      .[, .(MATNR, EAN11, MATKL )]
    
    map(.x = opcos, .f = f_get_opco_sell_out_pmdm_mapping,
        mara = MARA, pmdm = PMDM) %>%
      set_names(opcos) %>%
      rbindlist( idcol = "OPCO") %>%
      f_read_opco_labels()[., on = .(OPCO)] %T>%
      setnames("LABEL", "Opco")
    
  }


f_PMDM_field_length <- 
  function(x) {
    
    f_read_pmdm(x = x) %>%
      .[, lapply(.SD, function(x){max(nchar(x))})] %>%
      data.table::transpose(l=., keep.names = "PMDM_FIELD") %>% 
      setDT() %T>%
      setnames("V1", "Max_length") %T>%
      write.xlsx(tst, file = "PMDM_LENGTH.xlsx", asTable = TRUE)
    
  }