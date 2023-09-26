# Purpose: Compress the column to unique values and count the number of entries
# input: x, a single column in a data.table .[, .(x)]
f_create_factor <- 
  function(x, DT) {
    
    f_get_non_permitted_characters <- 
      function(x) {
        # https://launchpad.support.sap.com/#/notes/173241
        
        ALL_CAPITAL <- 
          " !\\\"%&'()\\*\\+,-\\./\\:;<=>?_0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ#"
        patRSKC <- paste0(
          "([^", ALL_CAPITAL, "])", "|", "(^!)", 
          "|", "(^#$)", "|", 
          "([\U0001-\U001F])")
        
        str_extract_all(string = x, pattern = patRSKC, simplify = F) %>%
          lapply(paste0, collapse = "; ") %>%
          unlist()
      }
    

    DT %>%
      .[, .N, by= c(x) ] %>%
      .[, `:=` (
        ValueLength = {as.character(get(x)) %>% nchar() },
        Values      = get(x))] %>%
      .[, `:=` (
        CaseCnv =
          { .[, .(get(x), y = toupper(get(x)))] %>%
            .[, N := .N, by = y] %>% .[, N] },
        AlphaCnv =
          { .[, .(x, y = str_pad(get(x), max(ValueLength),
                                 "left", "0"))] %>%
            .[, N := .N, by = y] %>% .[, N] },
        Patterns = {
          get(x) %>%
            gsub(pattern = "\\d"  , replacement = "9", x = .)    %>%
            gsub(pattern = "[A-Z]", replacement = "X", x = .)    %>%
            gsub(pattern = "[a-z]", replacement = "x", x = .) } ,
        ValueLength        = {ValueLength},
        MinLength          = {ValueLength %>% min()}, #{nchar(get(x)) %>% min()},
        MaxLength          = {ValueLength %>% max()},          
        isNA               = is.na(x = get(x)),
        isNull             = is.null(x = get(x)),
        isEmptyString      = get(x) == "",
        LeadingWhiteSpace  = grepl("^\\s", get(x)),
        TrailingWhiteSpace = grepl("\\$s", get(x)),
        LeadingZeros       = { str_extract(get(x), "^0+") %>%
            ifelse(is.na(.) == TRUE, "", .)},
        isNUMC             = !grepl(pattern = "\\D", x = get(x)),
        ContainsLowerCase  = grepl(pattern = "[a-z]+", x = get(x)),
        NonPermittedChars  = f_get_non_permitted_characters(get(x))
      )] %T>%
      #      .[, c(".FROM", ".TO") := .(Sys.Date(), Sys.Date())] %T>%      
      setorder(-N)
  }

f_calc_dstats <- 
  function(x, DT, lst) {
    
    c <- DT[, get(x)]
    
    # helper variables
    hCNT <- length(c)
    hQXX <- quantile(c, prob=c(.25,.5,.75), na.rm = TRUE)
    hCLS <- class(c)
    hIQR <- IQR(c, na.rm = TRUE)
    hNNA <- sum(is.na(c))
    hNUL <- sum(is.null(c))
    hUNQ <- length(unique(c))
    hMIN <- min(c, na.rm = TRUE)
    hMAX <- max(c, na.rm = TRUE)
    hMOD <- as.character(lst[["FACTOR"]][1, get(x)])
    hMED <- median(c, na.rm = TRUE)
    hAVG <- mean(c, na.rm = TRUE)
    hSUM <- sum(c, na.rm = TRUE)
    hRNG <- abs(hMAX - hMIN)
    h_SD <- sd(c, na.rm = TRUE)
    h_SE <- h_SD/sqrt(hCNT)
    h_CV <- h_SD/hAVG
    hSKW <- hCNT/((hCNT - 1) * (hCNT - 2)) * 
      sum( (c - hAVG)^3/h_SD^3, na.rm = TRUE)
    hKUR <- (hCNT *(hCNT + 1)/((hCNT - 1)*(hCNT - 2)*(hCNT - 3))) *
      sum( (c - hAVG)^4/h_SD^4, na.rm = TRUE) - 
      ( (3 * (hCNT - 1)^2) / ((hCNT - 2) * (hCNT - 3)) ) 
    hIS0 <- sum(c == 0, na.rm = TRUE)
    hTLO <- sum(c < (hQXX["25%"] - (1.5 * hIQR)), na.rm = TRUE )
    hTHO <- sum(c > (hQXX["75%"] + (1.5 * hIQR)), na.rm = TRUE )
    hNLO <- sum(c < (hAVG - 3 * h_SD), na.rm = TRUE )
    hNHO <- sum(c > (hAVG + 3 * h_SD), na.rm = TRUE )
    
    DT[, .(
      Feature           = x,
      Count             = hCNT,
      FeatureClass      = hCLS,
      FeatureType       = ifelse(
        hCLS %in% c("factor","character"), "Categorical",
        ifelse(hCLS == "integer", "Discrete", "Continuous")),
      CountNA           = hNNA,
      CountNull         = hNUL,
      PercentageMissing = round(100*hNNA/hCNT, 4) ,
      PercentageUnique  = round(100*hUNQ/hCNT, 4) ,
      ConstantFeature   = ifelse(hUNQ == 1, "Yes", "No"),
      ZeroSpreadFeature = ifelse(hIQR == 0, "Yes", "No"),
      CountZero         = hIS0,
      Min               = hMIN,
      Q25               = hQXX["25%"],
      Q50               = hQXX["50%"],
      Median            = hMED,
      Mean              = hAVG,
      Mode              = hMOD,
      Q75               = hQXX["75%"],
      Max               = hMAX,
      IQR               = hIQR,
      Range             = hRNG,
      Sum               = hSUM,
      StdDeviation      = h_SD,
      StdError          = h_SE,
      CoefficientOfVar  = h_CV,
      SampleVariance    = h_SD^2,
      Skewness          = hSKW,
      Kurtosis          = hKUR,
      KurtosisType      = ifelse(
        hKUR == 3, "Mesokurtic",
        ifelse(hKUR < 3, "Platykurtic", "Leptokurtic")),
      Tukey_NLOutliers  = hTLO,
      Tukey_NHOutliers  = hTHO,
      NDstr_NLOutliers  = hNLO,
      NDstr_NHOutliers  = hNHO,      
      #      ImputationValue   = NULL,      
      Tukey_LOV         = hQXX["25%"] - (1.5 * hIQR),
      Tukey_HOV         = hQXX["75%"] + (1.5 * hIQR), 
      NDstr_LOV         = hAVG - 3 * h_SD,
      NDstr_HOV         = hAVG + 3 * h_SD   ) 
      ]  %>%
      .[, `:=` (
        ZeroSpreadFeature = ifelse(is.na(ZeroSpreadFeature) == TRUE,
                                   "No", ZeroSpreadFeature)
      )
      ]
  }

# collapse small groups to keep n +1 
f_lump_n <- 
  function(x, DT, n=2) {
    # Select columns and Sort descending on N
    DT <- 
      DT[, c(x, "N"), with = FALSE] %T>% 
      setorder(-N)
    
    # Group all categories with rank n or higher in OTHER
    if (n < DT[, .N]) {
      DT[n:.N, x := "Other"]
    }    
    # Aggregate FACTORS again for collapsing in OTHER category
    DT[, .(N = sum(N)), by = x]
  }

# f_lump_n <- 
#   function(dt, n) {
#     # set n not higher than remaining number of rows
#     
#     
#     nms <- setdiff(names(dt), "Count")
#     lnm <- length(nms)
#     x   <- nms[1]
#     
#     if ("Count" %in% names(dt) & lnm == 1) {
#       DT <- copy(dt)
#     } else if (!"Count" %in% names(dt) & lnm == 1) {
#       DT <- f_get_levels_count_descending(x, dt)
#     } else {
#       stop("too many fields")      
#     }
# 
#     setorder(DT, -Count)         
#     N <- DT[, .N]
#     
#     if (n < N) { 
#       DT <- 
#         DT[n:.N, (x) := "Other"]             %>% 
#         .[, .(Count = sum(Count)), by = x]
#     }
#     DT
#   }
# 
# f_get_levels_count_descending <- 
#   function(x, dt) {
#     dt[, x, with = FALSE]                                  %>% 
#       .[, .(Count = .N), by = x]                          %T>%
#       setkey(Count)
#   }



# ---- Categorical Functions ----

f_pattern_dt <- 
  function(dt) {

    x   <- setdiff(names(dt), "Count")[1]
    p   <- fPattern(dt[, x, with = FALSE][[1]])
    DT <- 
      copy(dt)                                             %>% 
      .[, (x) := p]                                        %>%
      .[, .(Count = sum(Count)), by = x]
  }

fPattern <-
  function(x) {
    x %>%
      gsub(pattern = "\\d"  , replacement = "9", x = .)    %>%
      gsub(pattern = "[A-Z]", replacement = "X", x = .)    %>%
      gsub(pattern = "[a-z]", replacement = "x", x = .)    %>%
      as.data.table                                        %>%
     setnames("Value")
  }

fValue <-
  function(x) {
    x %>%
      as.data.table                                        %>%
      setnames("Value")
  }

fWord <-
  function(x) {
    x %>%
      str_c(collapse = " ")                                %>%
      str_extract_all(pattern = "\\w+")                    %>%
      as.data.table                                        %>%
      setnames("Words")
  }


# ---- get value -----

# global Functions
f_get_value <- 
  function(x) {
      fValue(x) %>% 
        f_display_value(c = "Value", n = 25)    
    }

f_get_pattern <- 
  function(x) {
    fPattern(x) %>% 
      f_display_value(c = "Value", n = 10)    
  }

f_get_word <- 
  function(x) {
    fWord(x) %>% 
      f_display_value(c = "Value")    
  }

# ----  Calc function ---

f_calc_cardinality <- 
  function(pTable){
    
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
    
    dtRATIO  
    # return(list(UCOUNT = matrix(vUniqueCount,
    #                             ncol = 1,
    #                             dimnames = list(names(vUniqueCount), "Count")), 
    #             UNIQUE = lstUnique,
    #             dtRATIO = dtRATIO ))
  }

f_card <- 
  function(x) {
    
    nrws <- x[, .N]
    
    f_measures <- 
      function(x, dt) {
        
        n <- dt[, .N]
        v <- dt[, x, with=FALSE][[1]]
        l <- nchar(v)
        
        data.table(
          Field  = x           ,
          Unique = uniqueN(v)  , 
          Ratio  = uniqueN(v)/n,
          min    = min(l       , na.rm = TRUE),
          max    = max(l       , na.rm = TRUE),
          avg    = mean(l      , na.rm = TRUE),
          med    = median(l    , na.rm = TRUE))
      }
    
    map(names(x), f_measures, x)  #                        %>%
    #   as.data.table(keep.rownames = TRUE)               %T>% 
    #   setnames(c("FieldName", "Unique"))                 %>% 
    #   .[, `:=` (Ratio = Unique/nrws)]                    %>%
    # 
    
  }

