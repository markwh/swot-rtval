# Make cached data for app defaults

source("rivertile-viz/global.R")

defaultdir <- "rivertile-viz/data/sac18"
cachedir <- "rivertile-viz/cache/"

rtdata_default <- get_rivertile_data(defaultdir)

badnodes_default <- c(min(rtdata_default$rt_nodes$node_id), 
                      max(rtdata_default$rt_nodes$node_id),
              flag_nodes(defaultdir))

save(rtdata_default, file = sprintf("%s/rtdata.RData", cachedir))
save(badnodes_default, file = sprintf("%s/badnodes.RData", cachedir))
