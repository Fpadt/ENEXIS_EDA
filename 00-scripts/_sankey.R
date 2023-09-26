library(networkD3)
library(data.table)
library(magrittr)
library(tidyverse)


# Make a connection data frame
links <- 
  tribble(
    ~source           , ~target           , ~value, ~link_grp,
    "Swiss"           , "Profiling"       , 1000  , "R",
    "Turkey"          , "Profiling"       , 1500  , "R",
    "Profiling"       , "Integration"     , 2000  , "R",
    "Profiling"       , "Quarantine"      , 500   , "Q",
    "Integration"     , "KPI"             , 1750  , "R",
    "Integration"     , "Quarantine"      , 250   , "Q",
    "KPI"             , "O9"              , 1000  , "R",
    "KPI"             , "Quarantine"      , 750   , "Q",  
  ) %>% as.data.table()

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- 
  c(as.character(links$source), as.character(links$target)) %>% 
  unique() %>% 
  data.table(name = ., id = 0:(length(.) - 1))

# With networkD3, connection must be provided using id, not using real name 
# like in the links dataframe.. So we need to reformat it.
links <- 
  nodes[, .(source = name, IDsource = id)][links, on = .(source)] %>% 
  nodes[, .(target = name, IDtarget = id)][.    , on = .(target)]               

# prepare color scale: I give one specific color for each node.
my_color <- 
  'd3.scaleOrdinal() 
  .domain(["Swiss" , "Turkey" , "Profiling", "Integration", "KPI"    , "Quarantine", "O9"     , "R"         , "Q"]) 
  .range(["#FF0000", "#E30A17", "#F36E21"  , "#F68946"    , "#FCC098", "#636363"   , "#0971B8", "lightgreen", "#BCBEC0"])'

# .range(["#FF0000", "#E30A17", d3.rgb(0,0,255,0.5), "green", "green", "green"             , "#0971B8", "lightgreen", d3.rgb(255,0,0,0.5)])'
#  .range(d3.schemeCategory20c)'

# .range(["#FF0000", "#E30A17", "green"         , "#A3A3A3"         , "#636363", "#404040"   , "#0971B8", "lightgreen", "lightred"])'

# my_color <- 
#   'd3.scaleOrdinal()
#   .domain(["Swiss" , "Turkey"  , "Data Profiling", "Data Integration", "KPI"    , "Quarantine", "O9"     , "R"        , "Q"]) 
#   .range(d3.schemeRdBu[9])'


# Make the Network. I call my colour scale with the colourScale argument
p <- sankeyNetwork(
  Links       = links, 
  Nodes       = nodes, 
  Source      = "IDsource", 
  Target      = "IDtarget", 
  Value       = "value", 
  NodeID      = "name", 
  colourScale = my_color,
  fontSize    = 30,
  units       = "records",
  nodeWidth   = 60,
  sinksRight  = TRUE,
  nodePadding = 10,
  LinkGroup   = "link_grp" )
p
