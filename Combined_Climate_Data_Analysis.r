# Function to check and install packages
if (!require('nasapower')) install.packages("nasapower", repos = "https://ropensci.r-universe.dev")
if (!require('rnoaa')) remotes::install_github("ropensci/rnoaa")

install_if_missing <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    }
  }
}

# List of required packages
required_packages <- c(
  "shiny",
  "suncalc",
  "ggplot2",
  "tidyr",
  "dplyr",
  "lubridate",
  "DT",
  "shinyjs",
  "gridExtra",
  "grid",
  "ggpubr",
  "openxlsx"
)

# Load necessary libraries
library(shiny)
library(nasapower)
library(suncalc)
library(rnoaa)
library(ggplot2)
library(tidyr)
library(dplyr)
library(lubridate)
library(DT)
library(shinyjs)
library(gridExtra)
library(grid)
library(ggpubr)
library(openxlsx)

# Helper functions
get_processed_daily_data <- function(lonlat, year, location_name) {
  data_daily <- get_power(
    community = "ag",
    pars = "ALLSKY_SFC_SW_DWN",
    temporal_api = "daily",
    lonlat = lonlat,
    dates = c(paste0(year, "-01-01"), paste0(year, "-12-31")),
    time_standard = "UTC"
  )
  
  data_monthly <- data_daily %>%
    group_by(LON, LAT, YEAR, MM) %>%
    summarise(Value = round(mean(ALLSKY_SFC_SW_DWN, na.rm = TRUE), 2)) %>%
    ungroup() %>%
    mutate(
      PARAMETER = "ALLSKY_SFC_SW_DWN",
      Month = toupper(format(as.Date(paste0(YEAR, "-", MM, "-01")), "%b")),
      Location = location_name,
      Year = as.integer(YEAR)
    ) %>%
    select(LON, LAT, PARAMETER, Year, Month, Value, Location)
  
  return(data_monthly)
}

get_day_length_data <- function(latitudes) {
  day_length <- function(lat, month) {
    date_range <- seq.Date(as.Date(paste0("2024-", sprintf("%02d", month), "-01")),
                           as.Date(paste0("2024-", sprintf("%02d", month), "-01")) + days_in_month(as.Date(paste0("2024-", sprintf("%02d", month), "-01"))),
                           by = "day")
    lengths <- sapply(date_range, function(date) {
      times <- getSunlightTimes(date, lat = lat, lon = 0)
      day_len <- as.numeric(difftime(times$sunset, times$sunrise, units = "hours"))
      return(day_len)
    })
    round(mean(lengths, na.rm = TRUE), 3)
  }
  
  day_length_data <- expand.grid(Month = 1:12, Latitude = latitudes)
  day_length_data$DayLength <- mapply(day_length_data$Latitude, day_length_data$Month, FUN = day_length)
  return(day_length_data)
}

get_temperature_data <- function(lat, lon, year_min, year_max) {
  stations <- meteo_nearby_stations(lat_lon_df = data.frame(id = 1, latitude = lat, longitude = lon),
                                    var = "TMAX",
                                    year_min = year_min,
                                    year_max = year_max,
                                    limit = 1)
  station_id <- stations[[1]]$id[1]
  weather_data <- meteo_pull_monitors(monitors = station_id, var = c("TMAX", "TMIN"))
  
  weather_data <- weather_data %>%
    mutate(date = as.Date(date),
           Year = as.numeric(format(date, "%Y")),
           Month = factor(toupper(format(date, "%b")), levels = toupper(month.abb), ordered = TRUE),
           Temperature = round((tmax + tmin) / 20, 2),
           Location = paste("Lat:", lat, "Lon:", lon)) %>%
    filter(Year >= year_min & Year <= year_max) %>%
    group_by(Location, Year, Month) %>%
    summarize(Temperature = round(mean(Temperature, na.rm = TRUE), 2)) %>%
    arrange(Location, Year, Month)
  
  return(weather_data)
}


# Define UI
ui <- fluidPage(
    useShinyjs(),
    titlePanel("Combined Climate Data Analysis"),
    sidebarLayout(
        sidebarPanel(
            textInput("latitudes", "Enter Latitudes (comma-separated):", value = "63.27,63.82,66.70"),
            textInput("longitudes", "Enter Longitudes (comma-separated):", value = "16.01,20.26,22.53"),
            sliderInput("years", "Select Year Range:", min = 2020, max = 2024, value = c(2022, 2023), step = 1),
            actionButton("update", "Generate Results"),
            hr(),
            # Add false text under the "Generate Results" button
            HTML('<p style="color:red;">The process last some time while connecting to remote databases.</p>'),
            hr(),
            downloadButton("downloadPlots", "Download All Plots as PDF"),
            p(),
            downloadButton("downloadTables", "Download All Tables as Excel"),
            p(),
            downloadButton("downloadIrradiancePlot", "Download Irradiance Plot (300 DPI)"),
            p(),
            downloadButton("downloadDayLengthPlot", "Download Day Length Plot (300 DPI)"),
            p(),
            downloadButton("downloadTempPlot", "Download Temperature Plot (300 DPI)")
        ),
        mainPanel(
            tabsetPanel(
               tabPanel("About", 
                       # Adding placeholder text
                       p(""),
                       HTML("The <b>Combined Climate Data Analysis</b> tool is designed to send geographic 
                       coordinates and obtain environmental data related to light, its duration, its quality, as well as temperature data for academic purposes. <p><p>
                       For correct operation, the data is entered in the latitude and longitude boxes, separating each value by a comma. 
                       The pairs of values entered in the same order will be processed and presented in the different tabs with a brief description 
                       of the data source.
                       The public repositories used in the query are:"),
                       p("* suncalc library in R to calculate Day Lenght plot according to the latitude."),
                       p("* NASA Prediction Of Worldwide Energy Resources (POWER) database. Specifically a recalculation of the daily Averaged Daily 
                         CERES All Sky Surface Shortwave Downward Irradiance. The total solar irradiance incident (direct plus diffuse) 
                         on a horizontal plane at the surface of the earth under all sky conditions. An alternative term for the total 
                         solar irradiance is the \"Global Horizontal Irradiance\" or GHI."),
                       p("* National Oceanic and Atmospheric Administration (NOAA) database to obtain temperature data."),
                       HTML("You can select a <b>Year Range</b> from one to several years. The <b>only limitation</b> is a query to an unfinished year.
                         The recomendation is not to extend the range to unfinished years, as not all the data will be available in some of the databases."),
                       p(""),
                       fluidRow(
                         column(4, tags$a(href = "https://github.com/datastorm-open/suncalc", target = "_blank",
                                          tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/9/91/Octicons-mark-github.svg", height = "100px"))),
                         column(4, tags$a(href = "https://www.noaa.gov/", target = "_blank",
                                          tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/7/79/NOAA_logo.svg", height = "100px"))),
                         column(4, tags$a(href = "https://power.larc.nasa.gov/", target = "_blank",
                                          tags$img(src = "https://www.earthdata.nasa.gov/s3fs-public/2022-11/power_logo_event.png?VersionId=D1uPqBGEw0Yx2qsds8h2TFZCoQlLGApj", height = "100px")))
                       ),
                       p(""),
                       p("Contact information : david.lazaro.gimeno@gmail.com")
                ),
               tabPanel(
                 "Day Length Plot",
                 p(""),
                 HTML('The daylength plot includes areas indicating twilight or constant light period in <span style="background-color: rgba(255, 255, 0, 0.3);">yellow</span> 
        or <span style="background-color: rgba(128, 128, 128, 0.3);">gray</span> to indicate dawn and night as an orientation. More precise approaches can be visualized using other interesting platforms, such as www.timeanddate.com.'),
                 plotOutput("dayLengthPlot"),
                 DTOutput("dayLengthTable")
               ),
                tabPanel("Irradiance Plot", p(""),p("The irradiance plot retrieves information from the NASA Prediction Of Worldwide Energy Resources."),plotOutput("irradiancePlot"), DTOutput("irradianceTable")),
                tabPanel("Temperature Plot", p(""),p("The temperature plot retrieves information from The National Oceanic and Atmospheric Administration (NOAA)."),plotOutput("tempPlot"), DTOutput("tempTable")),
                
            )
        )
    )
)


# Define server
server <- function(input, output) {
    latitudes <- reactive(as.numeric(unlist(strsplit(input$latitudes, ","))))
    longitudes <- reactive(as.numeric(unlist(strsplit(input$longitudes, ","))))
    
    irradiance_data <- eventReactive(input$update, {
      data_list <- list()
      for (i in seq_along(latitudes())) {
        lat <- latitudes()[i]
        lon <- longitudes()[i]
        for (year in seq(input$years[1], input$years[2])) {
          data_list[[paste("Location", i, year, sep = "_")]] <- get_processed_daily_data(c(lon, lat), year, paste("Lat:", lat, "Lon:", lon))
        }
      }
        combined_data <- bind_rows(data_list)
        combined_data$Month <- factor(combined_data$Month, levels = toupper(month.abb))
        return(combined_data)
    })
    
    day_length_data <- eventReactive(input$update, {
        get_day_length_data(latitudes())
    })
    
    temperature_data <- eventReactive(input$update, {
        all_data <- data.frame()
        for (i in seq_along(latitudes())) {
            lat <- latitudes()[i]
            lon <- longitudes()[i]
            temp_data <- get_temperature_data(lat, lon, input$years[1], input$years[2])
            all_data <- rbind(all_data, temp_data)
        }
        return(all_data)
    })
    
    # Render plots
    irradiance_plot <- reactive({
        ggplot(irradiance_data(), aes(x = Month, y = Value, color = Location, group = interaction(Location, Year), linetype = as.factor(Year))) +
            geom_line() +
            geom_point() +
            scale_color_brewer(palette = "Set2") +
            labs(title = "Monthly Averaged Daily CERES All Sky Surface Shortwave Downward Irradiance",
                 x = "Month", y = "Irradiance (MJ/m^2/day)",
                 color = "Location", linetype = "Year") +
            theme_minimal()
    })
    
    output$irradiancePlot <- renderPlot({
        irradiance_plot()
    })
    
    output$irradianceTable <- renderDT({
        datatable(irradiance_data(), options = list(pageLength = 12, autoWidth = TRUE))
    })
    
    day_length_plot <- reactive({
        ggplot(day_length_data(), aes(x = Month, y = DayLength, color = as.factor(Latitude))) +
            geom_line() +
            scale_y_continuous(breaks = seq(0, 24, by = 4), limits = c(0, 24)) +
            scale_color_brewer(palette = "Set2") +
            labs(title = "Average Monthly Day Length",
                 x = "Month", y = "Day Length (hours)",
                 color = "Latitude") +
            scale_x_continuous(breaks = 1:12, labels = toupper(month.abb)) +
            annotate('rect', xmin = -Inf, xmax = Inf, ymin = 18, ymax = 24, alpha = .3, fill = 'yellow') +
            annotate('rect', xmin = -Inf, xmax = Inf, ymin = 0, ymax = 6, alpha = .3, fill = 'gray') +
            theme_minimal()
    })
    
    output$dayLengthPlot <- renderPlot({
        day_length_plot()
    })
    
    output$dayLengthTable <- renderDT({
        datatable(day_length_data(), options = list(pageLength = 12, autoWidth = TRUE))
    })
    
    temp_plot <- reactive({
        ggplot(temperature_data(), aes(x = Month, y = Temperature, color = Location, group = interaction(Location, Year), linetype = as.factor(Year))) +
            geom_line() +
            geom_point() +
            scale_color_brewer(palette = "Set2") +
            labs(title = "Average Monthly Temperature",
                 x = "Month", y = "Temperature (°C)",
                 color = "Location", linetype = "Year") +
            theme_minimal()
    })
    
    output$tempPlot <- renderPlot({
        temp_plot()
    })
    
    output$tempTable <- renderDT({
        datatable(temperature_data(), options = list(pageLength = 12, autoWidth = TRUE))
    })
    
    # Download individual plots at 300 DPI
    output$downloadIrradiancePlot <- downloadHandler(
        filename = function() { paste("irradiance_plot.png") },
        content = function(file) {
            ggsave(file, plot = irradiance_plot(), dpi = 300, width = 11, height = 8.5)
        }
    )
    
    output$downloadDayLengthPlot <- downloadHandler(
        filename = function() { paste("day_length_plot.png") },
        content = function(file) {
            ggsave(file, plot = day_length_plot(), dpi = 300, width = 11, height = 8.5)
        }
    )
    
    output$downloadTempPlot <- downloadHandler(
        filename = function() { paste("temp_plot.png") },
        content = function(file) {
            ggsave(file, plot = temp_plot(), dpi = 300, width = 11, height = 8.5)
        }
    )
    
    # Download all plots as a single PDF
    output$downloadPlots <- downloadHandler(
        filename = function() { paste("combined_plots.pdf") },
        content = function(file) {
            pdf(file, width = 11, height = 8.5) # landscape for plots
            plot_list <- list(irradiance_plot(), day_length_plot(), temp_plot())
            for (plot in plot_list) {
                print(plot)
            }
            dev.off()
        }
    )
    
    # Download all tables as an Excel file with different sheets
    output$downloadTables <- downloadHandler(
        filename = function() { paste("climate_data_tables.xlsx") },
        content = function(file) {
            wb <- createWorkbook()
            addWorksheet(wb, "Irradiance Data")
            writeData(wb, "Irradiance Data", irradiance_data())
            addWorksheet(wb, "Day Length Data")
            writeData(wb, "Day Length Data", day_length_data())
            addWorksheet(wb, "Temperature Data")
            writeData(wb, "Temperature Data", temperature_data())
            saveWorkbook(wb, file, overwrite = TRUE)
        }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)

