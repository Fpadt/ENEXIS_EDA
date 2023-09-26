#' LT is the result List from factor analysis
#' x is the column name
#' n = the maximum number of factors to desiplay
#' v is value, pattern or word
#' 
f_widget_value <-
  function(x, LT, n, v) {
    
    DT <- LT[[x]][["FACTOR"]]
      f_lump_n(x, DT, n=n)
    
    # Calculate and Format table
    TOT <- DT[, sum(N)]
    DT %>%
      .[, .(Percent = sum(N)/TOT,
            Count = sum(N)), by = v] %>%
      # setorder(-Percent) %>%
      formattable(
        list(
          Count   = normalize_bar(COL_FACTR, 0.2))) %>%
      as.datatable(
        rownames = FALSE,
        caption  = glue("{x}, Obs: {TOT}"),
        style    = 'bootstrap4',
        extensions = 'Scroller',
        #        width    = 300,
        options  = list(
          dom         = 't',
          # deferRender = TRUE,
          # scrollY     = 200,
          # scroller    = TRUE,
          pageLength = 50
        )) %>%
      formatPercentage('Percent', 2)
  }


f_display_value <-
  function(dt, c, n=5) {
    
    RET <- 
      copy(dt[, c, with = FALSE])                         %T>%
      setnames("Value")                                    %>%
      .[, .N, by = .(Value)]                              %T>%
      setorder(-N)   
    
    # Group all categories with rank n or higher in OTHER
    # note: table needs to be sorted first
    setorder(RET, -N)
    if (n < RET[, .N]) {RET[n:.N, Value := "Other"]}
    # Aggregate FACTORS again for collapsing in OTHER category
    RET <- RET[, .N, by = .(Value)]
    
    # Calculate indicators
    NROWS <- RET[, .N]
    RET                                                    %>%
      .[, .(Percent = sum(N)/NROWS, 
            Count = sum(N)), by =.(Value)]                %T>%
      setorder(-Percent)                                   %>%
      formattable(
        list(
          Count   = normalize_bar("#9CEB91", 0.2)))        %>%
      as.datatable(
        rownames = FALSE,
        caption  = glue("Obs: {NROWS}, Column: {c}, Attr.: Value"),
        style    = 'bootstrap4',
        extensions = 'Scroller',
        #        width    = 300,
        options  = list(
          dom         = 't',
          # deferRender = TRUE,
          # scrollY     = 200,
          # scroller    = TRUE,
          pageLength = 50
        ))                                                 %>%
      formatPercentage('Percent', 2)
  }

f_display_DT <- 
  function(x, caption=""){
    x %>%
      datatable(
        rownames = FALSE,
        caption  = caption,
        style    = 'bootstrap4',
        extensions = 'Scroller',
        #        width    = 300,
        options  = list(
          dom         = 't',
          # deferRender = TRUE,
          # scrollY     = 200,
          # scroller    = TRUE,
          pageLength = 5
        ))                                                 
  }


f_create_igraph_gan_mapping <- 
  function(x, DT) {

    
    x <- DT[OPCO == x]
    
    path <- 
      file.path("..", "80-figures", "pmdm_mapping") %>%
      f_get_folder()

    lst <- list()
        
    # map the GV-colors 
    #                    ?        , MARA_EAN , MARA_GAN , PMDM_EAN , PMDM_GAN
    #                    unknown  , iSyn-EAN , iSyn-GAN , PMDM-EAN , PMDM-GAN
    gvColors        <- c(GV_ORNG_0, GV_GREY_2, GV_GREY_1, GV_BLUE_1, GV_BLUE_0)
    
    names(gvColors) <- levels(x$MAP)
    colScale        <- scale_fill_manual(name = "pmdm", values = gvColors)
    colScale_col    <- scale_color_manual(name = "pmdm", values = gvColors)
    
    # Determine the breaks
    db <- paste(ceiling(length(unique(x$Month))/9), "month")
    br <- (1:10)/10
    et <- element_text(
      family = "DejaVu Sans", face = "plain", colour = GV_GREY_1, size = 12)
    at <- element_text(
      family = "DejaVu Sans", face = "plain", colour = GV_GREY_1, size = 9)

    # Map data, aesthetics, GV colors and time scale
    pQ <- 
      ggplot(data = x, 
             mapping = aes(x = Month, y = Q, fill = MAP, tooltip = Q)) +
      colScale + 
      scale_x_date(date_breaks = eval(db), date_labels = "%Y.%m") +
      scale_y_continuous(sec.axis = sec_axis(~ . )) +
      labs(
        title = x[1, Opco], 
        caption = format(today(), "%A, %d %B, %Y"),
        x = "Date") +  
      facet_wrap(facets = "Category", nrow = 2) +
      theme(text = at, title = et, legend.position = "bottom")

    # Plot Identity
    pQI <- 
      pQ + 
      geom_col() +
      # geom_hline(
      #   data = x[, .(yintercept = mean(Q)), by = .(Category, MAP)], 
      #   mapping= aes(yintercept = yintercept, color = MAP)) +
      # colScale_col +
      labs(y = "Sell-Out Vol. ")

    lst[["pQI"]] <- pQI
    
    # save plot per day
    # ggsave(plot = pQI, 
    #        file.path(path, paste0(params$opco, "_qi_", today(), ".png")))
    
    x_mean_cumQ <- 
      copy(x) %>%
      .[, .(tot = sum(Q)), by = .(Category, MAP)] %>%
      .[, yintercept:= tot/sum(tot), by = .(Category)] %T>%
      setorder(Category, -MAP) %>%
      .[, yintercept:=cumsum(yintercept), by = .(Category)]
    
    # Plot Percentage 
    pQP <- 
      pQ + 
      geom_col(position = position_fill())  + 
        labs(y = "Sell-Out Vol. % ") +
        # geom_hline(
        #   data = x_mean_cumQ, 
        #   mapping= aes(yintercept = yintercept, color = MAP), linetype = "dashed", size = 1) +
        # colScale_col +
        scale_y_continuous(
          labels = scales::percent_format(), breaks = br,
          sec.axis = sec_axis(~ ., labels = scales::percent_format(), breaks = br ))

    lst[["pQP"]] <- pQP
    
    # # save plot per day
    # ggsave(plot = pQP, 
    #        file.path(path, paste0(params$opco, "_qp_", today(), ".png")))
    
    pV <- 
      ggplot(data = x, 
             mapping = aes(x = Month, y = V, fill = MAP, tooltip = V)) +
      colScale + 
      scale_x_date(date_breaks = eval(db), date_labels = "%Y.%m") +
      scale_y_continuous(sec.axis = sec_axis(~ . )) +  
      labs(
        title = x[1, Opco], 
        caption = format(today(), "%A, %d %B, %Y "),
        x = "Date") +    
      facet_wrap(facets = "Category", nrow = 2) +
      theme(text = at, title = et, legend.position = "bottom")
    
    # Plot Identity
    pVI <- 
      pV + 
      geom_col() + 
      labs(y = "Sell-Out Val. ")

    lst[["pVI"]] <- pVI
    
    # # save plot per day
    # ggsave(plot = pVI, 
    #        file.path(path, paste0(params$opco, "_vi_", today(), ".png")))
    
    x_mean_cumV <- 
      copy(x) %>%
      .[, .(tot = sum(V)), by = .(Category, MAP)] %>%
      .[, yintercept:= tot/sum(tot), by = .(Category)] %T>%
      setorder(Category, -MAP) %>%
      .[, yintercept:=cumsum(yintercept), by = .(Category)]
    
    # Plot Percentage 
    pVP <- 
      pV + 
      geom_col(position = position_fill())  + 
      labs(y = "Sell-Out Val. % ") +    
      # geom_hline(
      #   data = x_mean_cumV, 
      #   mapping= aes(yintercept = yintercept, color = MAP), 
      #   linetype = "dashed", size = 1) +
      # colScale_col +    
      scale_y_continuous(
        labels = scales::percent_format(), breaks = br,
        sec.axis = sec_axis(~ ., labels = scales::percent_format(), 
                            breaks = br ))
    
    lst[["pVP"]] <- pVP
    
    lst
    # # save plot per day
    # ggsave(plot = pVP, 
    #        file.path(path, paste0(params$opco, "_vp_", today(), ".png")))
  }
