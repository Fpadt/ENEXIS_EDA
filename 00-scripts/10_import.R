# https://rstudio.github.io/cheatsheets/html/strings.html

# Purpose: 
# functions to generate the mapping excel
# import the data_chunks and export the consolidated data

# Read data files ------------------------------------------
f_hdr <- 
  function(x){
    data.table::fread(
      file   =  x,
      header = TRUE, 
      nrows  = 0
    ) %>% 
     names() %>%
    data.table(
      src_ffln = x,
      src_base = basename(x),
      src_flds = .,
      src_posn = 1:length(.),
      # dst_file = file.path(p_prp, basename(x) ),
      dst_flds = .,
      dst_posn = 1:length(.)
    ) %>%
      .[, c("src_main", "period") :=
          tstrsplit(x = src_base, split = "_(?=[0-9]{8})", perl = TRUE)
      ] %>%
      .[, c("period", "type") :=
          tstrsplit(x = period, split = "\\.", perl = TRUE)
      ]
  }

# function to create the mapping csv
f_create_fieldmapping <- 
  function(){
    purrr::map(.x = list.files(p_raw, full.names = TRUE, pattern = "csv$"), 
               .f = f_hdr) %>%
      rbindlist() %>%
      fwrite(
        file = file.path(p_prp, paste(today(), "map.csv", sep = "_")), 
        sep = ";", col.names = TRUE)
  }

# raw chunck to combined file ---------------------------------------------
f_save_consolidated_file <- 
  function(x, dst_map){

    # function to read file chunks    
    f_read_file_chunk <- 
      function(x, dst_map){
        
        src2dst <- dst_map[
          src_base == x, 
          .(src_ffln, src_sep, src_flds, dst_flds)] %>%
          unique()
        
        fread(
          file        = src2dst[1, src_ffln],
          sep         = src2dst[1, src_sep ] %>% ifelse(. == "t", "\t", .), 
          select      = src2dst[ , src_flds],
          colClasses  ="character",
          encoding    = "Latin-1",
          header      = TRUE,      # file has header
          strip.white = TRUE,      # only header trailing spaces are removed.
          na.strings  = "N/A"      # character vector of strings to become NA
        ) %>%
          setnames(src2dst[, dst_flds])    %>%   # rename header
          .[rowSums(. == "") != ncol(.), ] %>%   # remove blank rows
          .[, src_base:= x]
      }
    
    # list of chunks of 1 table
    lstChunks <- 
      dst_map[src_main == x, .(x = src_base)] %>%
      unique() 
    
    # full file name of destination
    ffnDest <- 
      file.path(
        p_prp,
        paste0(
          dst_map[src_main == x, dst_main][1],
          ".csv"
        )
      )
    
    # read & append chunks and write as a consolidated csv file
    purrr::pmap(
      .l      = lstChunks[, .(x)], 
      .f      = f_read_file_chunk, 
      dst_map = dst_map) %>%
      rbindlist(use.names = TRUE, fill = TRUE) %T>%
      fwrite(
        file = ffnDest
      )
  }

# this function reimports/exports the raw data in case mapping table changed
f_refresh_tables <- 
  function(){
    
    dst_map <- 
      openxlsx::read.xlsx(
        xlsxFile = file.path(p_prp, "map.xlsx"),
        sheet    = "map" 
      ) %>%
      setDT()
    
    # loop over main, save and store tables
    lstTables <- 
      dst_map[, .(x = src_main)] %>% 
      unique() %>%
      purrr::pmap(
        .l      = ., 
        .f      = f_save_consolidated_file,
        dst_map = dst_map)
  }


f_nchr <- 
  function(x){
    
    encodeString(x) %>% nchar() %>% max()
  }

lst1 <- fTableOverview(lstTables[[1]])
lst2 <- fTableOverview(lstTables[[2]])
lst3 <- fTableOverview(lstTables[[3]])

names(lstTables[[1]])
lstTables[[1]][, .(.N, L = f_nchr(peildatum))         , by = .(peildatum         , src_base)]
lstTables[[1]][, .(.N, L = f_nchr(netbeheerder))      , by = .(netbeheerder      , src_base)]
lstTables[[1]][, .(.N, L = f_nchr(provincie))         , by = .(provincie         , src_base)]
lstTables[[1]][, .(.N, L = f_nchr(gemeente))          , by = .(gemeente          , src_base)]
lstTables[[1]][, .(.N, L = f_nchr(buurt_cbs))         , by = .(buurt_cbs         , src_base)]
lstTables[[1]][, .(.N, L = f_nchr(buurt_cbs_code))    , by = .(buurt_cbs_code    , src_base)]
lstTables[[1]][, .(.N, L = f_nchr(aansl_cbs_aantal))  , by = .(aansl_cbs_aantal  , src_base)]
lstTables[[1]][, .(.N, L = f_nchr(aansl_opwek_aantal)), by = .(aansl_opwek_aantal, src_base)]
lstTables[[1]][, .(.N, L = f_nchr(opg_vermogen))      , by = .(opg_vermogen      , src_base)]

lstTables[[1]][grepl("\\.", aansl_cbs_aantal)][, .N, by = .(src_base)]
lstTables[[1]][grepl(","  , aansl_cbs_aantal)][, .N, by = .(src_base)]

lstTables[[1]][grepl("\\.", aansl_opwek_aantal)][, .N, by = .(src_base)]
lstTables[[1]][grepl(","  , aansl_opwek_aantal)][, .N, by = .(src_base)]

lstTables[[1]][grepl("\\.", opg_vermogen)][, .N, by = .(src_base)]
lstTables[[1]][grepl(","  , opg_vermogen)][, .N, by = .(src_base)]


names(lstTables[[3]])
lstTables[[2]][, .(.N, L = f_nchr(netbeheerder))        , by = .(netbeheerder        , src_base)]
lstTables[[2]][, .(.N, L = f_nchr(netgebied))           , by = .(netgebied           , src_base)]
lstTables[[2]][, .(.N, L = f_nchr(straatnaam))          , by = .(straatnaam          , src_base)]
lstTables[[2]][, .(.N, L = f_nchr(postcode_van))        , by = .(postcode_van        , src_base)]
lstTables[[2]][, .(.N, L = f_nchr(postcode_tot))        , by = .(postcode_tot        , src_base)]
lstTables[[2]][, .(.N, L = f_nchr(woonplaats))          , by = .(woonplaats          , src_base)]
lstTables[[2]][, .(.N, L = f_nchr(land_code))           , by = .(land_code           , src_base)]
lstTables[[2]][, .(.N, L = f_nchr(productsoort))        , by = .(productsoort        , src_base)]
lstTables[[2]][, .(.N, L = f_nchr(verbr_segment))       , by = .(verbr_segment       , src_base)]
lstTables[[2]][, .(.N, L = f_nchr(lvr_richting_perc))   , by = .(lvr_richting_perc   , src_base)]
lstTables[[2]][, .(.N, L = f_nchr(aansl_aantal))        , by = .(aansl_aantal        , src_base)]
lstTables[[2]][, .(.N, L = f_nchr(fysieke_status_perc)) , by = .(fysieke_status_perc , src_base)]
lstTables[[2]][, .(.N, L = f_nchr(aansl_soort_perc))    , by = .(aansl_soort_perc    , src_base)]
lstTables[[2]][, .(.N, L = f_nchr(aansl_soort))         , by = .(aansl_soort         , src_base)]
lstTables[[2]][, .(.N, L = f_nchr(sjv_gemiddeld))       , by = .(sjv_gemiddeld       , src_base)]
lstTables[[2]][, .(.N, L = f_nchr(sjv_laag_tarief_perc)), by = .(sjv_laag_tarief_perc, src_base)]
lstTables[[2]][, .(.N, L = f_nchr(slimme_meter_perc))   , by = .(slimme_meter_perc   , src_base)]
lstTables[[2]][, .(.N, L = f_nchr(src_base))            , by = .(src_base            , src_base)]

lstTables[[2]][grepl("\\.", lvr_richting_perc)][, .N, by = .(src_base)]
lstTables[[2]][grepl(","  , lvr_richting_perc)][, .N, by = .(src_base)]

lstTables[[2]][grepl("\\.", aansl_aantal)][, .N, by = .(src_base)]
lstTables[[2]][grepl(","  , aansl_aantal)][, .N, by = .(src_base)]

lstTables[[2]][grepl("\\.", aansl_soort_perc)][, .N, by = .(src_base)]
lstTables[[2]][grepl(","  , aansl_soort_perc)][, .N, by = .(src_base)]

lstTables[[2]][grepl("\\.", sjv_gemiddeld)][, .N, by = .(src_base)]
lstTables[[2]][grepl(","  , sjv_gemiddeld)][, .N, by = .(src_base)]

lstTables[[2]][grepl("\\.", sjv_laag_tarief_perc)][, .N, by = .(src_base)]
lstTables[[2]][grepl(","  , sjv_laag_tarief_perc)][, .N, by = .(src_base)]

lstTables[[2]][grepl("\\.", slimme_meter_perc)][, .N, by = .(src_base)]
lstTables[[2]][grepl(","  , slimme_meter_perc)][, .N, by = .(src_base)]

names(lstTables[[3]])
lstTables[[3]][, .(.N, L = f_nchr(peildatum))     , by = .(peildatum     , src_base)]
lstTables[[3]][, .(.N, L = f_nchr(netbeheerder))  , by = .(netbeheerder  , src_base)]
lstTables[[3]][, .(.N, L = f_nchr(gemeente))      , by = .(gemeente      , src_base)]
lstTables[[3]][, .(.N, L = f_nchr(buurt_cbs))     , by = .(buurt_cbs     , src_base)]
lstTables[[3]][, .(.N, L = f_nchr(buurt_cbs_code)), by = .(buurt_cbs_code, src_base)]
lstTables[[3]][, .(.N, L = f_nchr(wia_capc_max))  , by = .(wia_capc_max  , src_base)]
lstTables[[3]][, .(.N, L = f_nchr(wib_capc_max))  , by = .(wib_capc_max  , src_base)]
lstTables[[3]][, .(.N, L = f_nchr(zia_capc_max))  , by = .(zia_capc_max  , src_base)]
lstTables[[3]][, .(.N, L = f_nchr(zib_capc_max))  , by = .(zib_capc_max  , src_base)]
lstTables[[3]][, .(.N, L = f_nchr(src_base))      , by = .(src_base      , src_base)]

lstTables[[3]][grepl("\\.", wia_capc_max)][, .N, by = .(src_base)]
lstTables[[3]][grepl(","  , wia_capc_max)][, .N, by = .(src_base)]

lstTables[[3]][grepl("\\.", wib_capc_max)][, .N, by = .(src_base)]
lstTables[[3]][grepl(","  , wib_capc_max)][, .N, by = .(src_base)]

lstTables[[3]][grepl("\\.", zia_capc_max)][, .N, by = .(src_base)]
lstTables[[3]][grepl(","  , zia_capc_max)][, .N, by = .(src_base)]

lstTables[[3]][grepl("\\.", zib_capc_max)][, .N, by = .(src_base)]
lstTables[[3]][grepl(","  , zib_capc_max)][, .N, by = .(src_base)]

######

wb <- 
  createWorkbook(
    title    = "Enexis_decentrale_opwek_kv_(zon_pv)",
    subject  = "EDA",
    category = "JADS"
  ) 

for (i in 1:length(lst)) {
  wsh <- lst[i] %>% names()
  dt  <- lst[[i]]
  addWorksheet(  wb = wb, sheet = wsh, gridLines = FALSE, tabColour = GV_BLUE_0)
  writeDataTable(wb = wb, sheet = wsh, x = dt, tableStyle = "TableStyleLight14")
  freezePane(    wb = wb, sheet = wsh, firstActiveRow = 2, firstActiveCol = 2)
  setColWidths(  wb = wb, sheet = wsh, cols = 1:1000, widths = "auto")  
}

FN <- 
  "SSBI-556.xlsx" %>%
  paste0(format(Sys.time(), "%Y%m%d%H%M%S"), "-", .)

saveWorkbook(wb, file = FN, overwrite = TRUE)
shell.exec(normalizePath(FN))

# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$


# f_read_opco_data(denv, opco, ds, nrows=Inf)
f_read_opco_data <- 
  function(denv = "prd", opco, ds, nrws = Inf) {
    
    # Local Function
    f_read_ds <- 
      function(FOLDER, FILENAME, FILEEXT, 
               FSEP  , QUOTE   , CODEPAGE, SKIP, HEADER, STRIPWS, 
               DSRC  , NROWS   , SAMPLE  , fcdt, denv  , nrws){
        
        # By setting nrows < 0 the sample will be taken
        if (nrws == -1) {
          nrws <- SAMPLE
        } else if (nrws == 1) {
          nrws <- NROWS
        } 
        
        # make named vector with columnsclasses ! DSRC as DS fails
        col_select_class        <- 
          fcdt[DS == DSRC & OPCO == toupper(opco), R_DATATYPE]     
        names(col_select_class) <- 
          fcdt[DS == DSRC & OPCO == toupper(opco), COLUMN_NAME]

        DT <- 
          fread(
            file        = file.path(
              p_raw, denv, FOLDER, 
              paste0(FILENAME, ".", FILEEXT)),  # file to load
            sep         = FSEP,                 # pipe preferred
            quote       = QUOTE,                # quote character, e.g. " or ~
            encoding    = CODEPAGE,
            header      = HEADER,               # file has header
            strip.white = STRIPWS,              # only header trailing spaces are removed.
            na.strings  = "N/A",                # character vector of strings to become NA
            select      = col_select_class,     # data Types
            verbose     = FALSE,                # Be chatty and report timings?
            nrows       = nrws                  # number of rows to load
          )  
      }    
    
    # DataSource Header Info for OPCO import
    ds_hdr <- 
      f_read_ds_hdr() %>% 
      .[IMPORT == TRUE & OPCO == toupper(opco)] 
    
    # restrict to DataSources requested    
    if (!missing(ds)){
      ds_hdr <- ds_hdr[DS %in% toupper(ds)]
    }
    
    # select the required fields
    ds_hdr <- 
      ds_hdr[, .(FOLDER, FILENAME, FILEEXT,
                 FSEP  , QUOTE   , CODEPAGE , SKIP,
                 HEADER, STRIPWS , DSRC = DS, 
                 NROWS , SAMPLE)]
    
    # Read meta Data to determine column Types
    fcdt <- 
      f_read_meta_data(opco = opco) %>%
      .[IMPORT == TRUE ]
    
    # Main ---- 
    output <-
      pmap(ds_hdr, f_read_ds, fcdt, denv, nrws )
    names(output) <- ds_hdr[, DSRC]
    output
  }

# f_read_raw_data(denv, opcos, dss, nrows=Inf)
f_read_raw_data <- 
  function(denv = "prd", opcos, dss, nrws = Inf){
    opco <- opcos
    output <- 
      map(.x = opco, .f = f_read_opco_data, denv= denv, ds = dss, nrws = nrws)
    names(output) <- opcos
    output
  }
    

f_read_storemd_em <- 
  function() {
    
    col_classes <- 
      c(rep("character", 6), rep("numeric", 2), rep("character", 2), 
        rep("numeric", 1), rep("character", 1), rep("character", 1))

    fread(
      file        = file.path(
        p_raw, "src", 
        "StoreMD_em.csv"),                # file to load
      sep         = ";",                  # pipe preferred
      header      = TRUE,                 # file has header
      strip.white = TRUE,                 # only header trailing spaces are removed.
      na.strings  = "N/A",                # character vector of strings to become NA
      colClasses  = col_classes,          # read everything as character 
      verbose     = FALSE,                # Be chatty and report timings?
      nrows       = Inf                   # number of rows to load
    ) %>%
      .[nchar(DATE_OPENED) != 8 , DATE_OPENED:= NA] %>%
      .[,  `:=`(
        DATE_OPENED = ymd(DATE_OPENED), 
        DATE_CLOSED = ymd(DATE_CLOSED),
        NUMBER_OF_PINS = {
          gsub(pattern = ",", replacement = "", x = NUMBER_OF_PINS ) %>%
            as.integer()}
      )]
  }
    

# Read meta data and Harmonize to DS
  f_read_meta_data <- 
  function(opco) {
    
    dtR_DATA_TYPE <- 
      fread(file = file.path(p_src, "R_DATA_TYPE.csv"))

    # DataSource Header Info for OPCO import ----
    ds_hdr <- 
      f_read_ds_hdr()
    
    if (!missing(opco)) {
      ds_hdr <- ds_hdr[OPCO == toupper(opco)]}
    
    # Read metadata_didp ----
    metadata_didp <-
      fread(
        file = file.path(p_src, "METADATA_DIDP.csv")
      )
    # %>%
    #   cdt_map[., on = .(COLUMN_DATATYPE)]
    
    # Harmonize names to DS ----
    metadata <- 
      ds_hdr[, .(ID, IMPORT, OPCO, TABLE_NAME, DS)][
      metadata_didp, on = .(OPCO, TABLE_NAME), nomatch = 0]  %>%
      dtR_DATA_TYPE[., on = .(DS, FIELD == COLUMN_NAME)]     %>%
      .[R_DATATYPE %in% c("date", "time", "datetime"), 
        R_DATATYPE:= "character"]  %T>%
      setnames("FIELD", "COLUMN_NAME") %T>%
      setkey(ID, OPCO, DS, COLUMN_NAME)
    
    # TODO: replace as soon as O9 is known per OPCO  ----- 
    if (!missing(opco)) {
      if(opco == "gv") {metadata <- metadata[O9 == "Y"]}
    }
    
    metadata
  }

f_read_ds_hdr <- 
  function() {
    
    # DataSource Header Info for OPCO import
    fread(
      file = file.path(p_src, "DS_HDR.csv"),
      quote = "")                                       
  }

# information on files and paths  -------------------------


# example: View(f_get_file_info(ch1, list.files(ch1)))
f_get_file_info <-
  function(path, file) {
    
    # local function to convert unit
    f_set_unit <-
      function(x) {
        ifelse(
          x >       0 & x < 1024000 , paste0(round(x/1024^1, 1), " Kb"),
          ifelse(x >=  1024000 & x < 1048576000,
                 paste0(round(x/(1024^2), 1), " Mb"),
                 paste0(round(x/(1024^3), 1), " Gb"))
        )
      }
    
    file.info(file.path(path, file))      %>%
      as.data.table()                     %>%
      .[, filename  := file]              %>%
      .[, filepath  := path]              %>%
      .[, size_unit := f_set_unit(size)]
  }

f_get_folder <- 
  function(path){
    if (!dir.exists(path)) {dir.create(path)}
    path
  }

f_write_dt_to_csv <- 
  function(x, path, fn) {
    
    if (!dir.exists(path)) {dir.create(path)}
    
    ffn <- file.path(path, paste0(fn, ".csv"))
    
    write.table(x = x, file = ffn, sep = ";", 
                row.names = F, col.names = T,
                quote = TRUE)
  }

