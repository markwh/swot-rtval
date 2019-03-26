
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      actionButton("nodeSelClear", "Clear Selected Nodes"),
      checkboxInput("showReaches", "Show Reaches", value = FALSE),
      uiOutput("nodeSelect", inline = TRUE),
      actionButton("nodePurge", "Remove Selected Nodes"),
      actionButton("nodeRestore", "Restore All Nodes"), 
      actionButton("flagtruth", "Auto-select nodes with ambiguous truth")
    ),
    mainPanel(
      tabsetPanel(id = "inTabset", selected = "Map",
        tabPanel("Data",
                 shinyDirButton('inputdir', label = "input select",
                                title = "Select directory with rivertiles")
        ),
        tabPanel("Map",
          actionButton("zoomButton", "Zoom to data"),
          checkboxInput("pcv_plot", "Plot PIXC(vec) for selected nodes"),
          checkboxInput("gdem_pcv_plot", "Plot gdem PIXC(vec) for selected nodes"),
          radioButtons("pcv_geoloc", "PIXC geolocation type", 
                       choices = c(med = "medium", wd = "well-done (PIXCvec)")),
          leafletOutput("rtmap", height = 700)
          
        ),
        tabPanel("Plots",
          checkboxGroupInput("plot_vars", label = "Variables to Show",
                             choices = val_vars, inline = TRUE,
                                selected = c("height", "width", "area_total")),
           tabsetPanel(
             tabPanel("Histogram",
                    checkboxInput("hist_center", label = "Center Histogram"),
                    checkboxInput("hist_scale", label = "Scale Histogram"),
                    checkboxInput("hist_curve", label = "Overlay Normal curve"),
                    plotOutput("val_hist")),
             tabPanel("QQ Plot",
                      checkboxInput("qq_line", label = "1:1 line"),
                      # plotOutput("val_qq")),
                      plotlyOutput("val_qq_plotly")),
             tabPanel("Scatterplot",
                      radioButtons("scatter_y", "Y-axis Variable", inline = TRUE,
                         choiceNames = c("Value", "Raw Error", "Scaled Error"),
                         choiceValues = c("pixc_val", "pixc_err", "rel_err")),
                      radioButtons("scatter_x", "X-axis Variable", inline = TRUE,
                                   choiceNames = names(scatter_vars),
                                   choiceValues = unname(scatter_vars)),
                      plotlyOutput("val_scatter_plotly"))
          )
        ),
        tabPanel("Stats", 
                 tabsetPanel(
                   tabPanel("Summary Stats", 
                            h2("Statistics"),
                            tableOutput("stat_table")
                            ),
                   tabPanel("Coverage", 
                            h2("Coverage Rates"),
                            checkboxInput("debias_table", "Adjust Bias"),
                            tableOutput("coverage_table")
                            ),
                   tabPanel("Hypothesis Test",
                            checkboxInput("debias_hyptest", "Adjust Bias"),
                            tableOutput("hyptest_table"))
                            )
                 ),
        tabPanel("Settings",
                 numericInput("maxmappts", "Maximum points to render on map", 
                              value = 3000, min = 1, step = 1000),
                 textInput("truthname", "Base name for gdem truth file",
                           value = "gdem")),
        tabPanel("Report")
      )
    )
  )
)
