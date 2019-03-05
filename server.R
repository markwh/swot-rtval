library(shinydashboard)
library(shinyFiles)
# library(rivertile)
library(ncdf4)
library(fs)
library(ggplot2)
library(leaflet)
library(dplyr)
library(plotly)

# copied from https://stackoverflow.com/questions/42678858/q-q-plot-with-ggplot2stat-qq-colours-single-group
my_stat_qq = function(data, colour.var) {
  
  data=cbind(data, setNames(qqnorm(data$.resid, plot.it=FALSE), c("Theoretical", "Sample")))
  
  ggplot(data) + 
    geom_point(aes_string(x="Theoretical", y="Sample", colour=colour.var))
  
}
# end copy

devtools::load_all("../rivertile")

trybrowse <- function(expr) {
  out <- try(expr)
  if (is(out, "try-error")) browser()
  out
}

# library(curl) # make the jsonlite suggested dependency explicit

# # 1=South, 2=East, 3=West, 4=North
# dirColors <-c("1"="#595490", "2"="#527525", "3"="#A93F35", "4"="#BA48AA")

# # Download data from the Twin Cities Metro Transit API
# # http://svc.metrotransit.org/NexTrip/help
# getMetroData <- function(path) {
#   url <- paste0("http://svc.metrotransit.org/NexTrip/", path, "?format=json")
#   jsonlite::fromJSON(url)
# }

# # Load static trip and shape data
# trips  <- readRDS("metrotransit-data/rds/trips.rds")
# shapes <- readRDS("metrotransit-data/rds/shapes.rds")


# Get the shape for a particular route. This isn't perfect. Each route has a
# large number of different trips, and each trip can have a different shape.
# This function simply returns the most commonly-used shape across all trips for
# a particular route.

get_rivertile_data <- function(dir, truth = "gdem") {
  rt_nodes <- rt_read(path(dir, "rt.nc"), group = "nodes")
  rt_reaches <- rt_read(path(dir, "rt.nc"), group = "reaches")
  gdem_nodes <- rt_read(path(dir, sprintf("rt_%s.nc", truth)), 
                        group = "nodes")
  gdem_reaches <- rt_read(path(dir, sprintf("rt_%s.nc", truth)), 
                          group = "reaches")
  
  rt_pixc <- pixcvec_read(path(dir, "pcv.nc")) %>% 
    dplyr::rename(node_id = node_index, reach_id = reach_index)
  
  gdem_pixc <- pixcvec_read(path(dir, sprintf("pcv_%s.nc", truth))) %>% 
    dplyr::rename(node_id = node_index, reach_id = reach_index)
  
  
  out <- list(rt_nodes = rt_nodes, rt_reaches = rt_reaches, 
              gdem_nodes = gdem_nodes, gdem_reaches = gdem_reaches,
              rt_pixc = rt_pixc, gdem_pixc = gdem_pixc)
  out
}

purge_nodes <- function(rtdata, purgenodes = numeric(0)) {
  if (length(purgenodes) == 0) return(rtdata)
  reachinds <- grep("reach", names(rtdata))
  purgefun <- function(x) x[!(x[["node_id"]] %in% purgenodes), ]
  
  out <- rtdata
  out[-reachinds] <- lapply(rtdata[-reachinds], purgefun)
  out
}


function(input, output, session) {
  
  #### DATA INPUT ####
  defaultdir <- "~/Documents/swot-error/output"
  roots <- c(home = defaultdir)
  shinyDirChoose(input, 'inputdir', roots = roots)
  
  purgedNodes <- numeric(0)
  
  rtdata_full <- reactive({ 
    dir <- input$inputdir
    
    parsed_dir <- parseDirPath(roots = roots, selection = dir)
    
    ## REMOVEME
    if (is.null(input$dir)) {
      parsed_dir <- "~/Documents/swot-error/output/sac04/"
    }
    
    if (length(parsed_dir) == 0) return(NULL)
    purgedNodes <<- numeric(0) # reset purgedNodes
    updateTabsetPanel(session, inputId = "inTabset", selected = "Map")
    
    get_rivertile_data(dir = parsed_dir)
    })
  
  #### NODE SELECTION ####
    
  observeEvent(input$nodePurge, {
    purgedNodes <<- unique(c(purgedNodes, input$selNodes))
  })
  observeEvent(input$nodeRestore, {
    purgedNodes <<- numeric(0) 
  })
  observeEvent(input$nodeSelClear, {
    updateCheckboxGroupInput(session, "selNodes", selected = character(0))
  })

  rtdata <- reactive({
    if (is.null(rtdata_full())) return(NULL)
    
    input$nodePurge
    input$nodeRestore
    out <- purge_nodes(rtdata_full(), purgenodes = purgedNodes)
    out
  })
  
  output$nodeSelect <- renderUI({
    
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
    checkboxGroupInput("selNodes", "Node", choices = nodeNums, inline = TRUE)
  })
  

  #### MAPPING ####

  # Locations of nodes for a particular case
  riverNodeLocations <- reactive({
    if (is.null(rtdata())) return(NULL)
    # browser()
    nodedf <- rtdata()$gdem_nodes
    nodedf
  })
  
  # Pixcvec circlemarkers
  pcv_markers <- reactive({
    if (!input$pcv_plot || length(input$selNodes) == 0) {
      # Return identity function if nothing to add
      return(function(x) x) 
      }

    plotdf <- rtdata()$rt_pixc %>% 
      dplyr::filter(node_id %in% input$selNodes)
    out <- function(x) {
      addCircleMarkers(
        x,
        ~longitude_vectorproc, ~latitude_vectorproc,
        popup = ~paste(sprintf("reach: %s\nnode: %s", reach_id, node_id)),
        color = "green",
        data = plotdf)
    }
  })
  gdem_pcv_markers <- reactive({
    if (!input$gdem_pcv_plot || length(input$selNodes) == 0) {
      # Return identity function if nothing to add
      return(function(x) x) 
    }
    
    plotdf <- rtdata()$gdem_pixc %>% 
      dplyr::filter(node_id %in% input$selNodes)
    out <- function(x) {
      addCircleMarkers(
        x,
        ~longitude_vectorproc, ~latitude_vectorproc,
        popup = ~paste(sprintf("reach: %s\nnode: %s", reach_id, node_id)),
        color = "red",
        data = plotdf)
    }
  })
  
  # Store last zoom button value so we can detect when it's clicked
  lastZoomButtonValue <- NULL
  
  output$rtmap <- renderLeaflet({

    locations <- riverNodeLocations()    
    basemap <- leaflet(locations) %>% 
      addTiles()
    # addTiles('http://{s}.tile.thunderforest.com/transport/{z}/{x}/{y}.png')
    
    if (length(locations) == 0)
      return(basemap)

    map <- basemap %>% 
      addCircleMarkers(
        ~longitude,
        ~latitude,
        popup = ~paste(sprintf("reach: %s\nnode: %s", reach_id, node_id)),
        # color = ~dirPal(Direction),
        opacity = 0.8,
        radius = 2
      )

    rezoom <- "first"
    # If zoom button was clicked this time, and store the value, and rezoom
    if (!identical(lastZoomButtonValue, input$zoomButton)) {
      lastZoomButtonValue <<- input$zoomButton
      rezoom <- "always"
    }

    map <- map %>% 
      pcv_markers()(.) %>% 
      gdem_pcv_markers()(.) %>% 
      mapOptions(zoomToLimits = rezoom)
    map
  })
  
  #### VALIDATION ####
  
  ### Data objects 
  
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
    gg <- gg + geom_rug(aes(size =  1 * (isSel)), color = "red") +
      scale_size_identity()
    gg
  })
  
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
      # my_stat_qq(aes(sample = rel_err), isSel) +
      scale_color_manual(values = c("black", "red"), guide = FALSE) +
      scale_size_manual(values = c(1, 3), guide = FALSE) +
      facet_wrap(~variable)
    
    if (input$qq_line) {
      gg <- gg + geom_abline(slope = 1, intercept = 0)
    }
    gg
  })
  
  output$val_qq_plotly <- renderPlotly({
    ggplotly(val_qq_plot(), tooltip = "text")
  })
  
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
    covfun <- function(x, sigma, pctl) {
      if (input$debias_table) x <- x - mean(x, na.rm = TRUE)
      pctl <- pctl / 100
      bnd <- -qnorm((1 - pctl) / 2, mean = 0, sd = 1)
      numin <- sum(abs(x / sigma) <= bnd)
      out <- numin / length(x) * 100
      out
    }
    
    stat_tbl <- valdata() %>% 
      group_by(variable) %>% 
      summarize(
        `68 (1-sigma)` = covfun(pixc_err, sigma_est, 68),
        `90` = covfun(pixc_err, sigma_est, 90),
        `95 (2-sigma)` = covfun(pixc_err, sigma_est, 95),
        `99` = covfun(pixc_err, sigma_est, 99)
      )
    stat_tbl
    # DT::datatable(stat_tbl)
  })
}
