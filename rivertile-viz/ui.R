
ui <- fluidPage(theme = shinytheme("sandstone"),
  sidebarLayout(
    sidebarPanel(
      actionButton("nodeSelClear", "Clear Selected Nodes"),
      checkboxInput("showReaches", "Show Reaches", value = FALSE),
      uiOutput("reachSelect", inline = TRUE),
      uiOutput("nodeSelect", inline = TRUE),
      actionButton("nodePurge", "Remove Selected Nodes"),
      actionButton("nodeRestore", "Restore All Nodes"), 
      actionButton("flagtruth", "Auto-select nodes with ambiguous truth")
    ),
    mainPanel(
      tabsetPanel(id = "inTabset", selected = "Data",
        tabPanel("Data",
           h3("Select a model run or upload output from your own."),
           tabsetPanel(id = "dataSource", selected = "Prerun", 
             tabPanel("Prerun",
               DT::DTOutput("runs_table"),
               # tableOutput("runs_table"),
               actionButton("loadDataset", "Load Selected Dataset")),
             tabPanel("Upload",
               shinyDirButton('inputdir', label = "input select",
                              title = "Select directory with rivertiles")
             )
           )
        ),
        tabPanel("Map",
          actionButton("zoomButton", "Zoom to data"),
          checkboxInput("pcv_plot", "Plot PIXC(vec) for selected nodes"),
          checkboxInput("gdem_pcv_plot", "Plot gdem PIXC(vec) for selected nodes"),
          radioButtons("pcv_geoloc", "PIXC geolocation type", 
                       choiceNames = c("medium", "well-done (PIXCvec)"),
                       choiceValues = c("med", "wd")),
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
                         choiceValues = c("value", "err", "relerr")),
                      radioButtons("scatter_x", "X-axis Variable", inline = TRUE,
                                   choiceNames = names(scatter_vars),
                                   choiceValues = unname(scatter_vars)),
                      plotlyOutput("val_scatterplot")),
                      # plotOutput("val_scatter_gg")),
             tabPanel("Node Area", 
                      plotOutput("nodearea_plot"))
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
