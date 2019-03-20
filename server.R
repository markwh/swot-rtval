library(shinydashboard)
library(shinyFiles)
# library(rivertile)
library(ncdf4)
library(fs)
library(ggplot2)
library(leaflet)
library(dplyr)
library(plotly)


devtools::load_all("../rivertile")

trybrowse <- function(expr) {
  out <- try(expr)
  if (is(out, "try-error")) browser()
  out
}

# Variable selection
pixc_vars_tokeep <- c("azimuth_index", "range_index", "classification", 
                      "num_rare_looks", "latitude", "longitude", "height", 
                      "cross_track", "num_med_looks")
pixcvec_vars_tokeep <- c("azimuth_index", "range_index", "latitude_vectorproc", 
                         "longitude_vectorproc", "height_vectorproc", "node_index", 
                         "reach_index")

# Colors
nodecolor_unsel <- "#668cff"
nodecolor_sel <- "#0039e6"

# Pixc(vec) color legend
classes <- c(1, 2, 3, 4, 22, 23, 24)
classlabs <- c("land", "land_near_water", "water_near_land", "open_water",
              "land_near_dark_water", "dark_water_edge", "dark_water")
classpal <- colorFactor(palette = "Set1", domain = classes)

# Data funcitons
get_rivertile_data <- function(dir, truth = "gdem") {
  rt_nodes <- rt_read(path(dir, "rt.nc"), group = "nodes")
  rt_reaches <- rt_read(path(dir, "rt.nc"), group = "reaches")
  gdem_nodes <- rt_read(path(dir, sprintf("rt_%s.nc", truth)), 
                        group = "nodes")
  gdem_reaches <- rt_read(path(dir, sprintf("rt_%s.nc", truth)), 
                          group = "reaches")
  
  rt_pixcvec <- pixcvec_read(path(dir, "pcv.nc"))[pixcvec_vars_tokeep] %>% 
    dplyr::rename(node_id = node_index, reach_id = reach_index)
  
  gdem_pixcvec <- pixcvec_read(path(dir, sprintf("pcv_%s.nc", truth))) %>% 
    `[`(pixcvec_vars_tokeep) %>% 
    dplyr::rename(node_id = node_index, reach_id = reach_index)
  
  rt_pixc <- pixc_read(path(dir, "pixel_cloud.nc"))[pixc_vars_tokeep] %>% 
    left_join(rt_pixcvec, y = ., by = c("azimuth_index", "range_index"))
  gdem_pixc <- pixc_read(path(dir, "fake_pixc.nc"))[pixc_vars_tokeep] %>% 
    inner_join(gdem_pixcvec, y = ., by = c("azimuth_index", "range_index"))
  
  out <- list(rt_nodes = rt_nodes, rt_reaches = rt_reaches, 
              gdem_nodes = gdem_nodes, gdem_reaches = gdem_reaches,
              rt_pixc = rt_pixc, gdem_pixc = gdem_pixc)
  out
}

#' Function to remove nodes from an rtdata set--that is, a list of data.frames.
purge_nodes <- function(rtdata, purgenodes = numeric(0)) {
  if (length(purgenodes) == 0) return(rtdata)
  reachinds <- grep("reach", names(rtdata))
  purgefun <- function(x) x[!(x[["node_id"]] %in% purgenodes), ]
  
  out <- rtdata
  out[-reachinds] <- lapply(rtdata[-reachinds], purgefun)
  out
}


####------------------------------------
#### START OF SERVER FUNCTION ----------
####------------------------------------
function(input, output, session) {
  
  #### DATA INPUT ####
  defaultdir <- "../swot-error/output"
  roots <- c(home = defaultdir)
  shinyDirChoose(input, 'inputdir', roots = roots)
  
  purgedNodes <- numeric(0) # Keep track of which nodes get manually purged
  
  datadir <- reactive({
    if (is.null(input$dir)) { # REMOVE THIS LATER
      parsed_dir <- "../swot-error/output/sac18/"
    } else {
      dir <- input$inputdir
      parsed_dir <- parseDirPath(roots = roots, selection = dir)
    }
    parsed_dir
  })
  
  # Full dataset from get_rivertile_data()
  rtdata_full <- reactive({ 
    
    if (length(datadir()) == 0) return(NULL)
    purgedNodes <<- numeric(0) # reset purgedNodes
    updateTabsetPanel(session, inputId = "inTabset", selected = "Map")
    
    get_rivertile_data(dir = datadir())
    })
  
  # Node selection and purging
  observeEvent(input$nodePurge, {
    purgedNodes <<- unique(c(purgedNodes, input$selNodes))
  })
  observeEvent(input$nodeRestore, {
    purgedNodes <<- numeric(0) 
  })
  observeEvent(input$nodeSelClear, {
    updateCheckboxGroupInput(session, "selNodes", selected = character(0))
  })
  observeEvent(input$flagtruth, {
    # browser()
    flagnodes <- flag_nodes(datadir())
    tosel <- intersect(currentNodes(), flagnodes)
    updateCheckboxGroupInput(session, "selNodes", selected = tosel)
  })
  
  # Current dataset (subset of rtdata_full)
  rtdata <- reactive({
    if (is.null(rtdata_full())) return(NULL)
    
    input$nodePurge
    input$nodeRestore
    out <- purge_nodes(rtdata_full(), purgenodes = purgedNodes)
    out
  })
  
  currentNodes <- reactive({
    rtdat <- rtdata()
    if (is.null(rtdat)) {
      return(checkboxGroupInput("selNodes", "Node", choices = NULL, 
                                inline = TRUE))
    }
    
    nodeids1 <- rtdat$rt_nodes$node_id
    nodeids2 <- rtdat$gdem_nodes$node_id
    nodeNums <- sort(unique(as.numeric(c(nodeids1, nodeids2))))
    # Add names, so that we can add all=0
    names(nodeNums) <- nodeNums
    # nodeNums <- c(All = 0, nodeNums)
    nodeNums
  })
  
  # dynamic UI element for node selection based on nodes in rtdata()
  output$nodeSelect <- renderUI({
    checkboxGroupInput("selNodes", "Node", choices = currentNodes(), inline = TRUE)
  })
  

  #### MAPPING ####

  # Start with a base map including tiles and nodes. That's the leaflet() object.
  # Everything beyond that will be added using leafletProxy(). 
  # Some of this is copied/modified from bus tracker app.
  
  # Store last zoom button value so we can detect when it's clicked
  lastZoomButtonValue <- NULL
  
  # Base map with node locations and tiles
  output$rtmap <- renderLeaflet({
    rezoom <- "first"
    # If zoom button was clicked this time, and store the value, and rezoom
    if (!identical(lastZoomButtonValue, input$zoomButton)) {
      lastZoomButtonValue <<- input$zoomButton
      rezoom <- "always"
    }
    
    locations <- riverNodeLocations()    
    basemap <- leaflet(locations) %>% 
      addTiles()

    if (length(locations) == 0)
      return(basemap)
    
    map <- basemap %>% 
      addCircleMarkers(
        ~longitude,
        ~latitude,
        popup = ~paste(sprintf("reach: %s\nnode: %s", reach_id, node_id)),
        opacity = 0.8,
        color = nodecolor_unsel,
        radius = 2
      )
    
    map %>% mapOptions(zoomToLimits = rezoom)
  })
  
  
  # Locations of nodes for a particular case
  riverNodeLocations <- reactive({
    if (is.null(rtdata())) return(NULL)
    nodedf <- rtdata()$gdem_nodes
    nodedf
  })
  
  # Data frame with locations of selected nodes' pixc(vec)
  pcv_selected <- reactive({
    input$pcv_geoloc
    if (!input$pcv_plot) return(NULL)
    
    plotdf <- rtdata()$rt_pixc %>% 
      dplyr::filter(node_id %in% input$selNodes)
    
    # Use well-done pixcvec locs if selected
    if (input$pcv_geoloc == "wd") {
      plotdf <- plotdf %>% 
        dplyr::select(-latitude, -longitude) %>% 
        dplyr::rename(plotdf, latitude = latitude_vectorproc,
                      longitude = longitude_vectorproc)
    } 
    plotdf
  })
  # Data frame with locations of selected nodes' gdem pixc(vec)  
  pcv_gdem_selected <- reactive({
    input$pcv_geoloc
    if (!input$gdem_pcv_plot) return(NULL)
    
    plotdf <- rtdata()$gdem_pixc %>% 
      dplyr::filter(node_id %in% input$selNodes)
    
    # Use well-done pixcvec locs if selected
    if (input$pcv_geoloc == "wd") {
      plotdf <- plotdf %>% 
        dplyr::select(-latitude, -longitude) %>% 
        dplyr::rename(plotdf, latitude = latitude_vectorproc,
                      longitude = longitude_vectorproc)
    } 
    plotdf
  })
  

  # Observer to add pcv points
  observe({
    input$pcv_plot
    if (is.null(pcv_selected()) || (nrow(pcv_selected()) == 0)) {
      leafletProxy("rtmap") %>% 
        clearGroup("pcv")
    } else {
      # browser()
      leafletProxy("rtmap", data = pcv_selected()) %>% 
        clearGroup("pcv") %>% 
        addCircleMarkers(~longitude, ~latitude, stroke = FALSE,
                         radius = 8, fillOpacity = 0.7, 
                         popup = ~paste(sprintf("reach: %s\nnode: %s", 
                                                reach_id, node_id)),
                         fillColor = ~classpal(classification),
                         group = "pcv")
    }
  })
  
  # Observer for pcv points legend
  observe({
    input$pcv_plot
    proxy <- leafletProxy("rtmap", data = pcv_selected())
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$pcv_plot) {
      proxy %>% addLegend(position = "topright",
        colors = RColorBrewer::brewer.pal(7, "Set1"),
        labels = c("land", "land_near_water", "water_near_land", "open_water",
                   "land_near_dark_water", "dark_water_edge", "dark_water"))
    }
  })  
  
  # Observer for selected nodes
  observe({
    nodedat_ss <- riverNodeLocations() %>% 
      dplyr::filter(node_id %in% input$selNodes)
    leafletProxy("rtmap", data = nodedat_ss) %>% 
      clearGroup("nodes_sel") %>% 
      addCircleMarkers(lng = ~longitude, lat = ~latitude,
                       radius = 2, color = nodecolor_sel, fillColor = NULL,
                       group = "nodes_sel")
  })
  
  # Observer to add gdem pcv points
  observe({
    input$gdem_pcv_plot
    if (is.null(pcv_gdem_selected()) || (nrow(pcv_gdem_selected()) == 0)) {
      leafletProxy("rtmap") %>% 
        clearGroup("pcv_gdem")
    } else {
      leafletProxy("rtmap", data = pcv_gdem_selected()) %>% 
        clearGroup("pcv_gdem") %>% 
        addCircleMarkers(~longitude, ~latitude, radius = 4, stroke = FALSE,
                         popup = ~paste(sprintf("reach: %s\nnode: %s", 
                                                reach_id, node_id)),
                         fillOpacity = 0.4,
                         color = "red", 
                         group = "pcv_gdem")  
    }
  })
  
  #### VALIDATION ####
  
  # Data objects 
  valdata <- reactive({
    rt_valdata_df(obs = rtdata()$rt_nodes, truth = rtdata()$gdem_nodes)
  })
  
  ## Plots
  output$val_hist <- renderPlot({
    # browser()
    gg <- rt_val_hist(valdata(), vars = input$plot_vars,
                      curve = input$hist_curve,
                      center = input$hist_center, 
                      scale = input$hist_scale)
    gg$data <- mutate(gg$data, isSel = (node_id %in% input$selNodes))
    # browser()
    gg <- gg + geom_rug(aes(alpha = (isSel * 1)), color = "red", size = 1) +
      scale_alpha_identity()
    gg
  })
  
  # gg object--plotly object will use this
  val_qq_plot <- reactive({
    gg <- valdata() %>% 
      dplyr::filter(variable %in% input$plot_vars) %>% 
      mutate(isSel = (node_id %in% input$selNodes),
             rel_err = pixc_err / sigma_est) %>% 
      group_by(variable) %>% 
      mutate(theoretical = qqnorm(rel_err, plot.it = FALSE)$x) %>% 
      ungroup() %>% 
      ggplot() + 
      geom_point(aes(x = theoretical, y = rel_err, text = node_id, 
                     color = isSel, size = isSel)) +
      scale_color_manual(values = c("black", "red"), guide = FALSE) +
      scale_size_manual(values = c(1, 3), guide = FALSE) +
      facet_wrap(~variable)
    
    if (input$qq_line) {
      gg <- gg + geom_abline(slope = 1, intercept = 0)
    }
    gg
  })
  
  # plotly object
  output$val_qq_plotly <- renderPlotly({
    ggplotly(val_qq_plot(), tooltip = "text")
  })
  
  # gg object for scatterplot
  val_scatter_plot <- reactive({
    # browser() 
    plotdata <- valdata() %>% 
      dplyr::filter(variable %in% input$plot_vars) %>% 
      dplyr::mutate(isSel = (node_id %in% input$selNodes),
                    rel_err = pixc_err / sigma_est)
    
    plotdata$yvalue <- plotdata[[input$scatter_y]]
    plotdata$xvalue <- plotdata[[input$scatter_x]]
    if (input$scatter_y == "pixc_val") {
      plotdata$ymiddle <- plotdata$gdem_val 
    } else {plotdata$ymiddle <- 0}
    if (input$scatter_y == "rel_err") {
      plotdata$ysigma <- 1 
    } else {plotdata$ysigma <- plotdata$sigma_est}
    
    gg <- ggplot(plotdata, aes(x = xvalue)) +
      geom_ribbon(aes(ymin = ymiddle - 1.96 * ysigma, 
                      ymax = ymiddle + 1.96 * ysigma), 
                  fill = "pink") +
      geom_ribbon(aes(ymin = ymiddle - ysigma, ymax = ymiddle + ysigma), 
                  fill = "#7780ff") +
      geom_point(aes(y = yvalue, color = isSel, size = isSel, text = node_id)) +
      scale_color_manual(values = c("#333333", "red"), guide = FALSE) +
      scale_size_manual(values = c(1, 3), guide = FALSE) +
      facet_wrap(~variable, scales = "free") +
      ylab(input$scatter_y) + xlab(input$scatter_x)
    
    gg
  })
  
  # plotly render for scatterplot
  output$val_scatter_plotly <- renderPlotly({
    ggplotly(val_scatter_plot(), tooltip = "text")
  })
  
  
  # Stats tables --------
  output$stat_table <- renderTable({
    stat_tbl <- valdata() %>% 
      mutate(pixc_relerr = pixc_err / sigma_est) %>% 
      group_by(variable) %>% 
      summarize(bias = mean(pixc_err),
                sd = sd(pixc_err),
                rel_bias = mean(pixc_relerr),
                rel_sd = sd(pixc_relerr)) %>% 
      mutate(rmse = sqrt(bias^2 + sd^2), 
             rel_rmse = sqrt(rel_bias^2 + rel_sd^2)) %>% 
      dplyr::select(variable, bias, sd, rmse, rel_bias, rel_sd, rel_rmse)
    # DT::datatable(stat_tbl)
    stat_tbl
  })
  
  output$coverage_table <- renderTable({
    stat_tbl <- val_coverage(valdata()) %>% 
      rename(`68 (1-sigma)` = ci68,
             `90` = ci90,
             `95 (2-sigma)` = ci95,
             `99` = ci99)
    stat_tbl
  })
  
  output$hyptest_table <- renderTable({
    htdf <- rt_hyptest(valdata(), debias = input$debias_hyptest)
    htdf
  })
}
