
# Option to use cached data instead of reading netcdf. If not NULL, netcdf reading won't 
# proceed
# cachedir <- "./cache"
# for (file in list.files(cachedir, full.names = TRUE)) {
#   load(file)
# }


# This function, modified from http://www.r-bloggers.com/safe-loading-of-rdata-files/, load the Rdata into a new environment to avoid side effects
LoadToEnvironment <- function(url, env=new.env()) {
  load(url(url), env)
  return(env)
}

# default_data_url <- "https://osu.box.com/shared/static/9ng2ys6kubcbkqu8riar0l89uzk101zr.rdata"
default_data_url <- NULL
run_manifest <- read.csv("./roruns.csv", stringsAsFactors = FALSE) %>% 
  dplyr::filter(!is.na(rtviz_url),
                nchar(rtviz_url) > 0) 

####------------------------------------
#### START OF SERVER FUNCTION ----------
####------------------------------------
function(input, output, session) {
  
  #### DATA INPUT ####
  
  # Table of riverobs runs
  output$runs_table <- renderDT({
    # browser()
    rundf <- run_manifest %>% 
      dplyr::transmute(run_id = outno, 
                       priordb = gsub("$PRIORLOC", "", priordb, fixed = TRUE), 
                       case, pass, day = bndry_cond, 
                       smearing, land_sig0, water_sig0, 
                       gdem_preproc = grepl("preproc", gdem_name), notes)
    rundf
  })
  
  # Load selected dataset when input$loadDataset is fired. 
  
  
  # 
  
  defaultdir <- "./data"
  roots <- c(home = defaultdir)
  shinyDirChoose(input, 'inputdir', roots = roots)
  
  
  datadir <- reactive({
    if (is.null(input$inputdir)) { 
      return(NULL)
    } else {
      dir <- input$inputdir
      parsed_dir <- parseDirPath(roots = roots, selection = dir)
    }
    parsed_dir
  })

  
  purgedNodes <- numeric(0) # Keep track of which nodes get manually purged
  
  # Full dataset from get_rivertile_data()
  loadCounter <- 0 # For data source branch logic in data_in
  data_in <- reactive({ 
    
    # Test whether loadDataset was fired--if so, get data from url
    if (input$loadDataset > loadCounter) {
      stopifnot(loadCounter == input$loadDataset - 1)
      loadCounter <<- loadCounter + 1
      
      isolate(selrow <- input$runs_table_rows_selected)
      if (!length(selrow)) {
        message("No data selected")
        return(data_in())
      } else {
        load(url(run_manifest$rtviz_url[selrow]))
      }
    } else if (length(datadir())) { # get data from folder selection
      rtdata_in <- get_rivertile_data(dir = datadir())
      badnodes1 <- ambiguous_nodes(datadir())
      badnodes2 <- mismatch_nodes(rtdata_in$rt_nodes, rtdata_in$gdem_nodes)
      badnodes_in <- union(badnodes1, badnodes2)
    } else if (length(default_data_url)) { # get default data from url
      load(url(default_data_url)) 
    } else return(NULL)

    purgedNodes <<- numeric(0) # reset purgedNodes
    
    updateTabsetPanel(session, inputId = "inTabset", selected = "Map")
    return(list(rtdata_in = rtdata_in, badnodes_in = badnodes_in))    
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
    badnodes <- data_in()$badnodes_in
    tosel <- intersect(currentNodes(), badnodes)
    updateCheckboxGroupInput(session, "selNodes", selected = tosel)
  })
  
  # Current dataset (subset of data_in)
  rtdata <- reactive({
    if (is.null(data_in()$rtdata_in)) return(NULL)
    input$nodePurge
    input$nodeRestore
    out <- purge_nodes(data_in()$rtdata_in, purgenodes = purgedNodes)
    out
  })
  
  currentNodes <- reactive({
    rtdat <- rtdata()
    if (is.null(rtdat)) {
      return(NULL)
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
  
  
  # Color palette for reaches
  reachpal <- reactive({
    reachids <- sort(unique(data_in()$rtdata_in$rt_reaches$reach_id))
    nreaches <- length(reachids)
    # viridisLite::viridis(n = nreaches) 
    dkcols <- RColorBrewer::brewer.pal(n = 8, name = "Set3")
    pal <- leaflet::colorNumeric(palette = rep(dkcols, length.out = nreaches), 
                                 domain = reachids)
    pal
  })

  #### MAPPING ####

  # Start with a base map including tiles only. That's the leaflet() object.
  # Everything beyond that will be added using leafletProxy(). 
  # Some of this is copied/modified from bus tracker app.
  
  # Store last zoom button value so we can detect when it's clicked
  lastZoomButtonValue <- NULL
  
  # Base map with tiles only
  output$rtmap <- renderLeaflet({
    rezoom <- "first"
    # If zoom button was clicked this time, and store the value, and rezoom
    if (!identical(lastZoomButtonValue, input$zoomButton)) {
      lastZoomButtonValue <<- input$zoomButton
      rezoom <- "always"
    }
    
    locations <- riverNodeLocations()    
    minlat <- min(locations$latitude)
    maxlat <- max(locations$latitude)
    minlon <- min(locations$longitude)
    maxlon <- max(locations$longitude)
    basemap <- leaflet(locations) %>% 
      addTiles() %>% 
      fitBounds(minlon, minlat, maxlon, maxlat)

    if (length(locations) == 0)
      return(basemap)
    
    
    
    basemap %>% mapOptions(zoomToLimits = rezoom)
  })
  
  
  # Locations of nodes for a particular case
  riverNodeLocations <- reactive({
    if (is.null(rtdata())) return(NULL)
    nodedf <- rtdata()$gdem_nodes
    nodedf
  })
  
  # Observer for node locations
  observe({
    req(input$inTabset == "Map") # https://github.com/rstudio/leaflet/issues/590#issuecomment-439444519
    locations <- riverNodeLocations()    
    proxy <- leafletProxy("rtmap", data = locations)
    if (length(locations) == 0)
      return(leafletProxy("rtmap"))
    if (input$showReaches) {
      proxy %>% 
        addCircleMarkers(
          ~longitude,
          ~latitude,
          popup = ~paste(sprintf("reach: %s\nnode: %s", reach_id, node_id)),
          opacity = 0.8,
          color = ~reachpal()(reach_id),
          radius = 2
        ) 
    } else {
      proxy %>% 
        addCircleMarkers(
          ~longitude,
          ~latitude,
          popup = ~paste(sprintf("reach: %s\nnode: %s", reach_id, node_id)),
          opacity = 0.8,
          color = nodecolor_unsel,
          radius = 2
        )
    }
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
    req(input$inTabset == "Map") # https://github.com/rstudio/leaflet/issues/590#issuecomment-439444519
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
    req(input$inTabset == "Map") # https://github.com/rstudio/leaflet/issues/590#issuecomment-439444519
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
    req(input$inTabset == "Map") # https://github.com/rstudio/leaflet/issues/590#issuecomment-439444519
    nodelocs <- riverNodeLocations()
    if (!length(nodelocs)) return(leafletProxy("rtmap"))
    nodedat_ss <- nodelocs %>% 
      dplyr::filter(node_id %in% input$selNodes)
    # browser()
    leafletProxy("rtmap", data = nodedat_ss) %>% 
      clearGroup("nodes_sel") %>% 
      addCircleMarkers(lng = ~longitude, lat = ~latitude,
                       radius = 2, color = nodecolor_sel, fillColor = NULL,
                       group = "nodes_sel")
  })
  
  # Observer to add gdem pcv points
  observe({
    req(input$inTabset == "Map") # https://github.com/rstudio/leaflet/issues/590#issuecomment-439444519
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
  valdata_node <- reactive({
    rt_valdata_df(obs = rtdata()$rt_nodes, truth = rtdata()$gdem_nodes)
  })
  valdata_reach <- reactive({
    rt_valdata_df(obs = rtdata()$rt_reaches, truth = rtdata()$gdem_reaches)
  })
  
  ## Plots
  output$val_hist <- renderPlot({
    plotdata <- valdata_node() %>% 
      mutate(plotcolor = reachpal()(reach_id))
    if (!input$showReaches) {
      plotdata$plotcolor = nodecolor_sel
    }
    
    gg <- rt_val_hist(plotdata, vars = input$plot_vars,
                      curve = input$hist_curve,
                      center = input$hist_center, 
                      scale = input$hist_scale)
    gg$data <- mutate(gg$data, isSel = (node_id %in% input$selNodes))
    # browser()
    gg <- gg + geom_rug(aes(alpha = (isSel * 1), color = plotcolor), 
                        size = 1) +
      scale_alpha_identity() +
      scale_color_identity()
    gg
  })
  
  # gg object--plotly object will use this
  val_qq_plot <- reactive({
    # browser()
    plotdata_node <- valdata_node() %>% 
      dplyr::filter(variable %in% input$plot_vars) %>% 
      mutate(isSel = (node_id %in% input$selNodes),
             rel_err = pixc_err / sigma_est,
             plotcolor = case_when(
               input$showReaches ~ reachpal()(reach_id),
               !input$showReaches & isSel ~ nodecolor_sel, 
               !input$showReaches & !isSel ~ nodecolor_unsel)) %>% 
      group_by(variable) %>% 
      mutate(theoretical = qqnorm(rel_err, plot.it = FALSE)$x) %>% 
      ungroup()
    
    gg <- ggplot(plotdata_node, 
                 aes(x = theoretical, y = rel_err, color = plotcolor)) + 
      geom_point(aes(text = node_id, size = isSel)) +
      scale_color_identity(guide = FALSE) +
      scale_size_manual(values = c(1, 3), guide = FALSE) +
      facet_wrap(~variable, scales = "free_y")
    
    if (input$qq_line) {
      gg <- gg + geom_abline(slope = 1, intercept = 0)
    }

    gg + theme(legend.position = "none")
  })
  
  # plotly object
  output$val_qq_plotly <- renderPlotly({
    ggplotly(val_qq_plot(), tooltip = "text")
  })
  
  # gg object for scatterplot
  val_scatter_plot <- reactive({

    plotdata <- valdata_node() %>% 
      dplyr::filter(variable %in% input$plot_vars) %>% 
      dplyr::mutate(isSel = (node_id %in% input$selNodes),
                    rel_err = pixc_err / sigma_est,
                    plotcolor = case_when(
                      input$showReaches ~ reachpal()(reach_id),
                      !input$showReaches & isSel ~ nodecolor_sel, 
                      !input$showReaches & !isSel ~ nodecolor_unsel))
    
    # Manually add in the selected x and y axis variable names
    plotdata$yvalue <- plotdata[[input$scatter_y]]
    plotdata$xvalue <- plotdata[[input$scatter_x]]
    if (input$scatter_y == "pixc_val") {
      plotdata$ymiddle <- plotdata$gdem_val 
    } else {plotdata$ymiddle <- 0}
    if (input$scatter_y == "rel_err") {
      plotdata$ysigma <- 1 
    } else {plotdata$ysigma <- plotdata$sigma_est}
    
    # create the plot
    gg <- ggplot(plotdata, aes(x = xvalue)) +
      geom_ribbon(aes(ymin = ymiddle - 1.96 * ysigma, 
                      ymax = ymiddle + 1.96 * ysigma), 
                  fill = "pink") +
      geom_ribbon(aes(ymin = ymiddle - ysigma, ymax = ymiddle + ysigma), 
                  fill = "#7780ff") +
      geom_point(aes(y = yvalue, color = plotcolor, size = isSel, text = node_id)) +
      scale_color_identity(guide = FALSE) +
      scale_size_manual(values = c(1, 3), guide = FALSE) +
      facet_wrap(~variable, scales = "free") +
      ylab(input$scatter_y) + xlab(input$scatter_x)
    
    # Overlay reach data
    if (input$showReaches) {
      
      # need a node_id for reaches (use median)
      nodeiddf <- plotdata %>% 
        group_by(reach_id) %>% 
        summarize(node_id = median(node_id))
      
      plotdata_reach <- valdata_reach() %>% 
        left_join(nodeiddf, by = "reach_id") %>% 
        dplyr::filter(variable %in% input$plot_vars) %>% 
        mutate(plotcolor = reachpal()(reach_id), 
               rel_err = pixc_err / sigma_est)
      plotdata_reach$yvalue <- plotdata_reach[[input$scatter_y]]
      plotdata_reach$xvalue <- plotdata_reach[[input$scatter_x]]
      
      if (input$scatter_y == "pixc_val") {
        plotdata_reach$ymiddle <- plotdata_reach$gdem_val 
      } else {plotdata_reach$ymiddle <- 0}
      if (input$scatter_y == "rel_err") {
        plotdata_reach$ysigma <- 1 
      } else {plotdata_reach$ysigma <- plotdata_reach$sigma_est}
      
      gg <- gg + 
        geom_linerange(aes(ymin = ymiddle - 1.96 * ysigma,
                           ymax = ymiddle + 1.96 * ysigma),
                       size = 0.5, data = plotdata_reach, 
                       color = "#b3b3b3") +
        geom_linerange(aes(ymin = ymiddle - ysigma,
                           ymax = ymiddle + ysigma),
                        size = 1, data = plotdata_reach,
                       color = "#666666") +
        geom_point(aes(y = yvalue, 
                       color = plotcolor,
                       text = paste0("Reach: ", reach_id)), 
                   size = 5, data = plotdata_reach)
    }
    
    gg + theme(legend.position = "none")
  })
  
  # plotly render for scatterplot
  output$val_scatter_plotly <- renderPlotly({
    ggplotly(val_scatter_plot(), tooltip = "text")
  })
  
  output$nodearea_plot <- renderPlot({
    plotdata <- rtdata()$rt_pixc
    plotdata$node_index <- plotdata$node_id
    plotdata$reach_index <- plotdata$reach_id
    trunode <- rtdata()$gdem_nodes
    
    outgg <- nodearea_plot(plotdata, nodes = input$selNodes, node_truth = trunode)
    outgg
  })
  
  # Stats tables --------
  output$stat_table <- renderTable({
    stat_tbl <- valdata_node() %>% 
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
    stat_tbl <- val_coverage(valdata_node()) %>% 
      rename(`68 (1-sigma)` = ci68,
             `90` = ci90,
             `95 (2-sigma)` = ci95,
             `99` = ci99)
    stat_tbl
  })
  
  output$hyptest_table <- renderTable({
    htdf <- rt_hyptest(valdata_node(), debias = input$debias_hyptest)
    htdf
  })
}
