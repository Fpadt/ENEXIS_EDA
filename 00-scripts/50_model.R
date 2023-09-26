# Build of the Dashboard

# purpose  : script for creation of dashboard
# author   : Floris Padt
# createdon: 09-06-2020
# changedon: 09-06-2020

f_clean_build <- 
  function() {
    
    build_files <- 
      list.files(bld_path, full.names = TRUE) %>% 
      as.list()
    
    if (length(build_files) > 0 ) { 
      lapply(build_files, unlink)
    }
  }

# f_create_fdb("data_profiling")
f_create_fdb <- 
  function(name_fdb) {
    
    # 1.  copy the main file and put it at the start
    file.copy(
      from = file.path(skt_path, "main.Rmd"),
      to = file.path(bld_path, "000-main.Rmd"),
      overwrite = TRUE
    )
    
    # 2.  copy the files section and put it at the end
    file.copy(
      from = file.path(skt_path, "page_files.Rmd"),
      to = file.path(bld_path, "999-page_files.Rmd"),
      overwrite = TRUE
    )
    
    # 3. List all files in Build
    rmd_structure <- 
      list.files(bld_path)                                           %>% 
      as.list()
    
    f_append <- 
      function(x) {
        block <- 
          readLines(con = file.path(bld_path, x))                  %>% 
          str_flatten(collapse="\n") 
      }
    
    fdb <- 
      lapply(rmd_structure, f_append) %>% 
      unlist
    
    # create new flexdashboard.Rmd 
    new_fdb <- 
      file(description = file.path(
        fdb_path, paste0(name_fdb, ".Rmd")), open = "w")
    
    writeLines(text = fdb, con = new_fdb, sep = "\n")    
    
    # save/close file
    close(con= new_fdb, open = "w")
    
    f_clean_build()
  }

# ---- ---------

# get sidebar for the Opco table
f_get_sidebar <- 
  function(x, opco) {
    
    opco_path <- f_get_opco_path(opco = opco)
    
    # local function
    append_field <-
      function(x, t) {
        txt <- glue(
          "- [{x}](#", tolower(t), "_", tolower(x), ")",
          .sep = "")
      }
    
    # get & select & order field names of table x
    table_fields <-
      f_read_meta_data(opco = opco) %>% 
      .[DS == toupper(x) & OPCO == toupper(opco) & IMPORT == TRUE] %T>%
      setorder(DS, R_DATATYPE, COLUMN_NAME) %>%
      .[, COLUMN_NAME]
    
    # get header of side bar for specific table
    sidebar <-
      readLines(con = file.path(skt_path, "part_sidebar.Rmd"))     %>%
      str_flatten(collapse="\n")                                     %>%
      gsub(pattern = "<table>", replacement = tolower(x),  x = .)
    
    # create markdown coding for field list
    part_sidebar_fields <-
      lapply(table_fields, append_field, x)                          %>%
      unlist()                                                       %>%
      str_flatten(collapse = "\n")
    
    str_flatten(
      string = c(sidebar, part_sidebar_fields), collapse = "\n"
    )
  }

f_create_page_summary <- 
  function(x, opco) {
    
    x <- str_to_title(x)
    y <- f_get_sidebar(x, opco)
    
    readLines(
      con = file.path(skt_path, "page_table_summary.Rmd"))           %>% 
      str_flatten(collapse="\n")                                     %>% 
      gsub(pattern = "<table>"  , replacement = x, x = .)            %>% 
      gsub(pattern = "<sidebar>", replacement = y, x = .)
    
  }

f_create_page_basic_statistics <- 
  function(x, opco) {
    
    x <- str_to_title(x)
    y <- f_get_sidebar(x, opco)
    
    readLines(
      con = file.path(skt_path, "page_basic_statistics.Rmd"))        %>% 
      str_flatten(collapse="\n")                                     %>% 
      gsub(pattern = "<table>"  , replacement = x, x = .)            %>% 
      gsub(pattern = "<sidebar>", replacement = y, x = .)
    
  }

f_create_page_column_descriptions <- 
  function(x, opco) {
    
    x <- str_to_lower(x)
    y <- f_get_sidebar(x, opco)
    
    readLines(
      con = file.path(skt_path, "page_columns_description.Rmd"))     %>% 
      str_flatten(collapse="\n")                                     %>% 
      gsub(pattern = "<table>"  , replacement = x, x = .)            %>% 
      gsub(pattern = "<sidebar>", replacement = y, x = .)
    
  }

f_create_page_field <- 
  function(table, field, R_DATATYPE, opco) {
    
    x <- str_to_lower(table)     # lowercase table name
    y <- f_get_sidebar(x, opco)  # sidebar
    f <- str_to_lower(field)     # lowercase field name
    
    if (R_DATATYPE == "character") {
      page_rmd <- "page_field_categorical.Rmd"
    } else {
      page_rmd <- "page_field_numeric.Rmd"
    }
    
    readLines(
      con = file.path(skt_path, page_rmd))                           %>% 
      str_flatten(collapse="\n")                                     %>% 
      gsub(pattern = "<table>"  , replacement = x, x = .)            %>% 
      gsub(pattern = "<field>"  , replacement = f, x = .)            %>% 
      gsub(pattern = "<sidebar>", replacement = y, x = .)
    
  }


# --- Save Pages ----

f_create_filename <- 
  function(DS, append, i) {
    file.path(
      bld_path, paste0(
        str_pad(i, 3, side = "left", "0"), "-", 
        tolower(DS), "_", append, ".Rmd"))
  }

# f_save_page_summary("Article", "ch", 1)
f_save_page_summary <- 
  function(DS, opco, i) {
    mycon <-
      file(description = f_create_filename(
        DS = DS , append = "summary", i = i), 
        open = "w")
    
    f_create_page_summary(DS, opco) %>% 
      writeLines(text = ., con = mycon, sep = "\n")    
    
    close(con= mycon, open = "w")
  }

f_save_page_basic_statistics <- 
  function(DS, opco, i) {
    mycon <-
      file(description = f_create_filename(
        DS = DS , append = "basic_statistics", i = i), 
        open = "w")    

    f_create_page_basic_statistics(DS, opco) %>% 
      writeLines(text = ., con = mycon, sep = "\n")    
    
    close(con= mycon, open = "w")
  }


f_save_page_column_descriptions <- 
  function(DS, opco, i) {
    mycon <-
      file(description = f_create_filename(
        DS = DS , append = "cols_descr", i = i), 
        open = "w")

    f_create_page_column_descriptions(DS, opco) %>% 
      writeLines(text = ., con = mycon, sep = "\n")
    
    close(con= mycon, open = "w")
  }

f_save_page_field <- 
  function(DS, COLUMN_NAME, R_DATATYPE, opco, i) {
    mycon <-
      file(description = f_create_filename(
        DS = DS , append = tolower(COLUMN_NAME), i = i), 
        open = "w")

      f_create_page_field(DS, COLUMN_NAME, R_DATATYPE, opco) %>% 
        writeLines(text = ., con = mycon, sep = "\n")
      
      close(con= mycon, open = "w")
    }
    invisible()

#     
f_save_opco <- 
  function(opco) {
    
    fields <-
      f_read_meta_data(opco = opco) %>%
      .[IMPORT == TRUE, .(DS, COLUMN_NAME, R_DATATYPE)]
    
    # determine tables and create summary page
    tables <- fields[, .(DS)] %>% unique()
    
    pmap(tables, f_save_page_summary            , opco, 1)
    pmap(tables, f_save_page_column_descriptions, opco, 2)
    pmap(tables, f_save_page_basic_statistics   , opco, 3)    
    pmap(fields, f_save_page_field              , opco, 4)
  }

