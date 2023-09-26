
# transfer publish folder to reporting server udc folder
f_transfer_to_reporting_server <-
  function(
    prot          = "sftp",
    url           = "report.grandvision.global",
    port          = 22,
    userpwd, source_folder, target_folder) {
    
    # prepare names for archiving original and publish new
    from          <- target_folder
    to            <- paste0(
      from, "_arc", "/udb_", 
      format(with_tz(Sys.time(), "Europe/Amsterdam"), "%Y%m%d-%H%M%S"))
    argument      <- paste("rename", from, to)
    sftpurl       <- paste0(prot, "://", url)
    target_folder <- paste0(prot, "://", userpwd, "@", url, target_folder)
    
    # function to transfer a file from source
    f_transfer_file <- function(x, source_folder, target_folder) {
      
      source_file <- file.path(source_folder, x)
      target_file <- file.path(target_folder, x)
      opts        <- list(ftp.create.missing.dirs=TRUE)
      
      ftpUpload(what = source_file, to = target_file) #, .opts=opts)
#      curl_upload(file, url, verbose = TRUE, reuse = TRUE, ...)
      cat(source_file, "\n")
    }
    
    # archive the current folder
    # curlPerform(
    #   url     = sftpurl,
    #   port    = 22,
    #   userpwd = userpwd,
    #   verbose = TRUE,
    #   quote   = argument
    # )
    
    # copy flexdashboard to http://report.grandvision.global/bi-doc/udc/
    filesFTP <- list.files(
      path = source_folder, pattern = "[^Rmd]$")
    lapply(filesFTP, f_transfer_file, source_folder, target_folder)
  }

f_knit_pmdm_mapping <- 
  function(denv = "prd", nrws = Inf) {
    
    input_rmd <- file.path("~", "isdb", "20-flexdashboards", "exec_flex.Rmd")
    
    rmarkdown::render(
      input        = input_rmd, 
      params       = list(denv = denv, nrws = nrws),
      output_file  = file.path(
        "~", "isdb", "70-publish", "pmdm", paste0(today(), "_", "review" )),
      flexdashboard::flex_dashboard(
        #self_contained = FALSE,
        favicon = "https://report.grandvision.global/bi-doc/img/favicon.ico",
        logo    = "https://report.grandvision.global/bi-doc/img/flex_gv.png",
        theme   = "cosmo",
        navbar  = list(
          list(
            title  = format(today(), "%A, %d %B, %Y"),
            href   = "https://www.grandvision.com",
            align  = "right",
            target = "_blank"
          )
        ),
        orientation     = "columns",
        self_contained  = FALSE,
        vertical_layout = "fill"
      )
    )
  }

# knit the Rmd to HTML and save it in the publish folder
# f_knit_flexdashboard_to_publish(x = "data_profiling", denv = "prd", opco = "ch", nrws = -1)
f_knit_flexdashboard_to_publish <-
  function(x = "data_profiling", denv = "prd", opco, nrws = -1) {
    
    input_rmd <- file.path("~", "isdb", "20-flexdashboards", paste0(x, ".Rmd"))
    
    rmarkdown::render(
      input        = input_rmd, 
      params       = list(denv = denv, opco = opco, nrws = nrws),
      output_file  = file.path(
        "~", "isdb", "70-publish", paste0(opco, "1", "_", x)),
      flexdashboard::flex_dashboard(
        # self_contained = FALSE,
        favicon = paste0(tolower(opco), "1.png"),
        logo    = "flex_gv.png",
        theme   = "cosmo",
        navbar  = list(
          list(
            title  = "Data Catalog",
            icon   = "fas fa-book-open",
            href   = "http://report.grandvision.global/bi-doc/udc/swiss.html",
            align  = "right",
            target = "_blank"
          )
        ),
        orientation     = "columns",
        self_contained  = TRUE,
        vertical_layout = "fill"
      )
    )
  }

# f_all(opco = "ch", nrws = Inf, pub=TRUE)
# f_all(opco = "tr", nrws = Inf, pub=TRUE)
# f_all(opco = "gv", nrws = Inf, pub=TRUE)
# f_all(opco = "co", nrws = Inf, pub=TRUE)
# f_all(opco = "ch", nrws = Inf, pub=FALSE)
# f_all(opco = "tr", nrws = Inf, pub=FALSE)
# f_all(opco = "gv", nrws = Inf, pub=FALSE)
f_all <- 
  function(opco, nrws=50, pub=FALSE) {
    
    # tic()
    
    # delete contents of 40-build
    f_clean_build()
    
    # save the components of the OPCO dashboard in 40-build
    f_save_opco(opco = opco)
    
    # create FlexDashboard structure out of components and 
    # save in 20=flexdashboards
    f_create_fdb("data_profiling")

    ### Executing/Kniting the dashboard ###
    # Populate the dashboard with data and convert to HTML
    f_knit_flexdashboard_to_publish(
      x = "data_profiling", denv = "prd", opco = opco, nrws = nrws)
    
    # Transfer the FlexDashboard to webserver
    if (pub == TRUE) {
      f_transfer_to_reporting_server(
        userpwd       = "floris.padt:15minsofr",
        source_folder = file.path("~", "isdb", "70-publish"),
        target_folder = "/var/www/html/bi-doc/udb")
    }          
    
    # toc()
  }
