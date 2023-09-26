
f_get_hdr_log <- 
  function() {
    
    data.table() %>%
      .[, DTIME:= POSIXct()] %>%
      .[, c("OPCO"       , "ARTICLE"  , "STORE"  , "SELL_OUT", 
            "SELL_OUT_WS", "DISCOUNTS", "STOCK"  , "SELL_IN", "PO") := 
          double(0)]
  }

f_write_primary_key_log <- 
  function(primary_key) {
    
    # function to be executed for each table
    f_get_duplicates <- 
      function(x, pk) {
        if (!pk[[x]][["valid"]]) { 
          # side effect, write results
          f_write_dt_to_csv(
            x    = pk[[x]][["duplicates"]], 
            path = file.path(p_qua, params$opco),
            fn   = x)
          # return number of duplicates
          list(duplicates = nrow(pk[[x]][["duplicates"]])) 
        } else {list(duplicates =  0) }
      }
    
    # execute function for all in list x
    l        <- lapply(names(primary_key), f_get_duplicates, primary_key)
    names(l) <- names(primary_key)

    # convert list to data.table and make it wide format
    DT <-
      rbindlist(l) %>%
      .[, `:=` (
        OPCO  = params$opco,
        DTIME = Sys.time(),
        TABLE = names(l) )] %>%
        dcast.data.table(
          DTIME + OPCO ~ TABLE, value.var = "duplicates")

      # write results to log file which returns if file exists on exit
      f_append_to_log(DT, "primary_key")

  }

f_write_global_article_log <- 
  function(x, raw_data) {
    
    # function to be executed for each table
    f_ga_mapped <- 
      function(x, MARA, raw_data) {
        
        table <- raw_data[[x]]
        ojoin <- MARA[table, on = .(GLOBAL_ARTICLE_NUMBER), nomatch = NA]
      
        data.table(
          rows  = nrow(ojoin) ,
          g_art = sum(nchar(ojoin$GLOBAL_ARTICLE_NUMBER) > 0),
          match = sum(!is.na(ojoin$MATKL)) ,
          perc  = round(100 * sum(!is.na(ojoin$MATKL)) / nrow(ojoin), 0),
          TAB   = x
        )
        
        # paste0(
        #   nrow(ojoin)                                           , " rows, \n", 
        #   sum(nchar(ojoin$GLOBAL_ARTICLE_NUMBER) > 0)           , " g-art, \n",
        #   sum(!is.na(ojoin$MATKL))                              , " match, \n",
        #   round(100 * sum(!is.na(ojoin$MATKL)) / nrow(ojoin), 0), " % ")
      }
    
    # create empty table with the right header
    DT <- f_get_hdr_log()
    DT[, `:=` (STORE = NULL, DISCOUNTS = NULL)]
    
    # load RP1-MARA table
    MARA <- f_read_mara()
    
    # execute function for all in list x
    l        <- lapply(x, f_ga_mapped, MARA, raw_data) 
    names(l) <- x
    
    # convert list to data.table and make it wide format
    DT <-
      rbindlist(l) %>%
      .[, `:=` (
        OPCO  = params$opco,
        DTIME = Sys.time(),
        TABLE = names(l),
        TAB   = NULL)] 

    # View(
    #   dcast.data.table(
    #     DT, DTIME + OPCO ~ TABLE, 
    #     value.var = c("rows", "g_art", "match", "perc"))
    #   )
    
    # write results to log file which returns if file exists on exit
    f_append_to_log(DT, "global_article")
  }

f_write_date_range_log <- 
  function(x, raw_data) {
    
    # function to be executed for each table    
    f_get_date_range <- 
      function(x, raw_data) {
        paste0(range(raw_data[[x]][, DATE]), collapse = " - ")
      }
    
    # create empty table with the right header
    DT <- f_get_hdr_log()
    DT[, `:=` (ARTICLE = NULL, STORE = NULL, DISCOUNTS = NULL)]
    
    # execute function for all in list x    
    l        <- lapply(x, f_get_date_range, raw_data) 
    names(l) <- x
    
    # write results to log file which returns if file exists on exit
    f_append_to_log(l, DT, "date_range")
  }

f_append_to_log <- 
  function(x, logfile) {

    ffn <- file.path("~", "isdb", "60-results", "log",
                    paste0(logfile, "_log.csv"))
    
    # Check if a log file is already available    
    if (file.exists(ffn)) {

      log <- fread(file = ffn) %>% 
        .[, DTIME:= as.POSIXct(DTIME)]

      x  <- rbindlist(
        list(log, x), use.names = TRUE, fill = TRUE)
    }
    
    f_write_dt_to_csv(
      x    = x, 
      path = file.path("~", "isdb", "60-results", "log"), 
      fn   = paste0(logfile, "_log"))

    # return if file exists
    x
    
  }
