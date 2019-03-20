library(shinydashboard)
library(shinyFiles)
library(leaflet)
library(plotly)

# header <- dashboardHeader(
#   title = "Rivertile Validation"
# )

val_vars <- c("height", "height2", "width", "area_total", "area_detct", 
              "latitude", "longitude")
scatter_vars <- c("Node ID" = "node_id", "No. Pixels" = "n_good_pix", 
                  "X-track Distance" = "xtrk_dist", 
                  "Est. Uncertainty" = "sigma_est")

theme_set(theme_bw())

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      actionButton("nodeSelClear", "Clear Selected Nodes"),
      uiOutput("nodeSelect", inline = TRUE),
      actionButton("nodePurge", "Remove Selected Nodes"),
      actionButton("nodeRestore", "Restore All Nodes")
    ),
    mainPanel(
      tabsetPanel(id = "inTabset", selected = "Map",
        tabPanel("Data",
                 shinyDirButton('inputdir', label = "input select",
                                title = "Select directory with rivertiles")),
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
                 h2("Statistics"),
                 tableOutput("stat_table"),
                 h2("Coverage Rates"),
                 checkboxInput("debias_table", "Adjust Bias"),
                 tableOutput("coverage_table")),
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
