# Libraries
library(ggplot2)
library(lubridate)
library(dplyr)
library(tidyr)
library(shiny)
library(plotly)
library(stringr)
library(rvest)
library(DT)
library(bslib)
library(tuneR)
library(seewave)
library(av)
library(viridisLite)
library(RColorBrewer)
library(shinyjs)
library(glue)
library(hms)
library(shinycssloaders)
library(DBI)
library(RSQLite)

#################################################
# Set file path variables (Requires User Input) #
#################################################

# Reading in all data:
# Second line defines specific columns we want to read, remove unwanted columns to improve loading time
all_data <- fst::read_fst("/Users/mikeoconnor/Documents/BirdWorkbench/birdnet_analyzer_run_files_combined_FST/run_files_combined_FST.2020_2021_2022_2023_2024_2025.m20_Jul_28.fst",
                          columns = c("Begin.Time..s.", "End.Time..s.", "Common.Name", "Species.Code", "Confidence", 
                                      "Begin.Path", "Location", "Date", "Date.Time", "Month", "Week",
                                      "avg_windspeed", "avg_temperature", "Obs.Time"))

# Set root folder for audio files

audio_filepath <- "/Volumes/Bio/"

# Tiny Shiny path in the form "http://tiny.pgbwb.com/?"
tiny_shiny <- "https://tiny.pgbwb.com/?"

# File path to SQL weather database
weather_path <- "/Users/mikeoconnor/Documents/BirdWorkbench/weather_snoop_valley_weather/valley.weather.db"


footer_text <- p("This project is powered by ",
                 tags$a("BirdNET Analyzer", href = "https://github.com/birdnet-team/BirdNET-Analyzer", target = "_blank"),
                 ", ",
                 tags$a("the R Project", href = "https://www.r-project.org/", target = "_blank"), 
                 ", ",
                 tags$a("PGBWB-team", href = "https://github.com/PGBWB-team/bird-workbench", target = "_blank"),
                 ", ",
                 tags$a("Shiny", href = "https://shiny.posit.co/", target = "_blank"), 
                 ", and ",
                 tags$a("Prairie Haven", href = "https://www.earsinthedriftless.com/", target = "_blank")
                 )

# Photo links and text:
ph_overview_pic <- "https://www.prairiehaven.com/wp-content/uploads/2016/01/Sharp-shinned-Hawk-2-9-16-4.jpg"
species_loc_pic <- "https://www.prairiehaven.com/wp-content/uploads/2009/03/ruffed-grouse-2.jpg"
species_overview_pic <- "https://prairiehaven.com/uploads/img441049af98a2c.jpg"
file_drilldown_pic <- "https://www.prairiehaven.com/wp-content/uploads/2009/03/orchard-oriole-young-male-5-08.jpg"

ph_overview_text <- "Waiting for you to select 'Generate Table' in the Sidebar"
species_loc_text <- "Select a species from the Species Lookup in the sidebar or make a selection from the Praire Haven Overview page."
species_overview_text <- "Select a species from the Species Lookup in the sidebar or make a selection from the Praire Haven Overview page."
file_drilldown_text <- "Select a file from the File Lookup in the sidebar or make a selection from the File Analysis page."

##############
# Start Code #
##############

ui <- bslib::page_navbar(
  header = useShinyjs(),
  theme = bs_theme(version = 5),
  id = "main_nav",
  title = "BirdOMatic",
  selected = "ph_overview",
  
  bslib::nav_panel(
    title = "Prairie Haven Overview", 
    value = "ph_overview",
    layout_sidebar(
      sidebar = sidebar(
        width = "500px",
        actionButton(inputId = "go", label = "Generate Table",
                     width = "200px"),
        bslib::accordion(
          id = "overview_accordion",
          open = TRUE,
          accordion_panel(
            "Model-match Scale",
            icon = icon("filter"),
            tags$div(
              style = "position: relative; width: 86%; margin: 0 auto;",  # align block
              sliderInput(
                inputId = "confidence_selection_overview",
                label = "BirdNET Model-match Scale",
                min = 0,
                max = 1,
                value = c(0.7, 1),
                step = 0.01,
                width = "100%"
              ),
              tags$div(
                style = "display: flex; justify-content: space-between; font-size: 12px; margin-top: -10px;",
                tags$span("Prepare for adventure"),
                tags$span("Not bad"),
                tags$span("Pretty good")
              )
            )
          ),
          accordion_panel(
            "Species Lookup",
            icon = icon("magnifying-glass"),
            selectizeInput(inputId = "species_lookup_overview",
                           label = NULL,
                           choices = unique(all_data$Common.Name),
                           multiple = TRUE, 
                           selected = NULL)
          ),
          accordion_panel(
            "Pivot Table Selection",
            icon = icon("table"),
            selectInput(inputId = "sel_view",
                        label = NULL,
                        choices = list("Species by Month" = "by_month",
                                       "Species by Location" = "by_location",
                                       "Species by Year" = "by_year",
                                       "Species by Week" = "by_week"),
                        selected = "by_month")
          ),
          accordion_panel(
            "Pivot Table Filters",
            id = "pivot_filters",
            icon = icon("sliders"),
            conditionalPanel(
              condition = "input.main_nav == 'ph_overview'",
              uiOutput("overview_view") %>% withSpinner()
            )
            # uiOutput("overview_view") %>% withSpinner()
          ),
          p(),
          footer_text
        )
      ),
      conditionalPanel(
        condition = "input.main_nav == 'ph_overview'",
        uiOutput("pivot_dt")
      )
      # uiOutput("pivot_dt") 
    )
  ),
  
  bslib::nav_panel(
    title = "Species-Specific Location Drilldown",
    value = "species_loc_drilldown",
    layout_sidebar(
      sidebar = sidebar(
        width = "500px",
        open = FALSE,
        bslib::accordion(
          id = "species_drilldown_accordion",
          open = FALSE,
          accordion_panel(
            "Model-match Scale",
            icon = icon("filter"),
            tags$div(
              style = "position: relative; width: 86%; margin: 0 auto;",  # align block
              sliderInput(
                inputId = "confidence_selection_drilldown",
                label = "BirdNET Model-match Scale",
                min = 0,
                max = 1,
                value = c(0.7, 1),
                step = 0.01,
                width = "100%"
              ),
              tags$div(
                style = "display: flex; justify-content: space-between; font-size: 12px; margin-top: -10px;",
                tags$span("Prepare for adventure"),
                tags$span("Not bad"),
                tags$span("Pretty good")
              )
            )
          ),
          accordion_panel(
            "Species Lookup",
            icon = icon("magnifying-glass"),
            selectizeInput(inputId = "species_lookup_location",
                           label = NULL,
                           choices = unique(all_data$Common.Name),
                           multiple = FALSE,
                           selected = "Acadian Flycatcher")
          ),
          accordion_panel(
            "Plot Settings",
            icon = icon("chart-simple"),
            radioButtons(
              inputId = "time_interval",
              label = "Time Interval",
              choices = c("Week" = "weekly", "Month" = "monthly"),
              selected = "weekly"
            )
          ),
          accordion_panel(
            "Cornell Ornithology Links",
            icon = icon("crow"),
            uiOutput("allaboutbirds",
                     label = "All About Birds"),
            p(),
            uiOutput("macaulay",
                     label = "Macaulay Library")
          ),
          p(),
          footer_text
      )
      ),
      

    conditionalPanel(
      condition = "input.main_nav == 'species_loc_drilldown'",
      uiOutput("species_title")
    ),
    
    conditionalPanel(
      condition = "input.main_nav == 'species_loc_drilldown'",
      uiOutput("conditional_tabset")
    ),
    uiOutput("filtered_data_ui")
  )),
  
  bslib::nav_panel(
    title = "Species-Specific Overview",
    value = "species_overview",
    layout_sidebar(
      sidebar = sidebar(
        width = "500px",
        open = FALSE,
        bslib::accordion(
          id = "species_overview_accordion",
          open = FALSE,
          accordion_panel(
            "Model-match Scale",
            icon = icon("filter"),
            tags$div(
              style = "position: relative; width: 86%; margin: 0 auto;",  # align block
              sliderInput(
                inputId = "confidence_selection_species_overview",
                label = "BirdNET Model-match Scale",
                min = 0,
                max = 1,
                value = c(0.7, 1),
                step = 0.01,
                width = "100%"
              ),
              tags$div(
                style = "display: flex; justify-content: space-between; font-size: 12px; margin-top: -10px;",
                tags$span("Prepare for adventure"),
                tags$span("Not bad"),
                tags$span("Pretty good")
              )
            )
          ),
          accordion_panel(
            "Species Lookup",
            icon = icon("magnifying-glass"),
            selectizeInput(inputId = "species_lookup_species_overview",
                           label = NULL,
                           choices = unique(all_data$Common.Name),
                           multiple = FALSE, 
                           selected = "Acadian Flycatcher")
          ),
          p(),
          footer_text
        )
      ),
      
      conditionalPanel(
        condition = "input.main_nav == 'species_overview'",
        uiOutput("species_title_overview")
      ),
      
      conditionalPanel(
        condition = "input.main_nav == 'species_overview'",
        uiOutput("conditional_species_plots")
      )
    )
  ),
  #   fluidRow(
  #     lapply(1:6, function(i){
  #       column(width = 6,
  #              conditionalPanel(
  #                condition = "input.main_nav == 'species_overview'",
  #                plotlyOutput(outputId = paste0("overviewPlot_", i), height = "400px")
  #              )
  #       )
  #     })
  #   )
  # )
  # ),
  
  bslib::nav_panel(
    title = "File Analysis",
    value = "audio_file_overview",
    layout_sidebar(
      sidebar = sidebar(
        width = "500px",
        open = TRUE,
        bslib::accordion(
          id = "file_overview_accordion",
          open = TRUE,
          accordion_panel(
            "Model-match Scale",
            icon = icon("filter"),
            tags$div(
              style = "position: relative; width: 86%; margin: 0 auto;",  # align block
              sliderInput(
                inputId = "confidence_selection_file_overview",
                label = "BirdNET Model-match Scale",
                min = 0,
                max = 1,
                value = c(0.7, 1),
                step = 0.01,
                width = "100%"
              ),
              tags$div(
                style = "display: flex; justify-content: space-between; font-size: 12px; margin-top: -10px;",
                tags$span("Prepare for adventure"),
                tags$span("Not bad"),
                tags$span("Pretty good")
              )
            )
          ),
          accordion_panel(
            "File Table Filters",
            icon = icon("sliders"),
            conditionalPanel(
              condition = "input.main_nav == 'audio_file_overview'",
              uiOutput("species_to_exclude"),
              uiOutput("species_file_filter")
            )
            # uiOutput("species_to_exclude")
          ),
          p(),
          footer_text
        )
      ),
      
      conditionalPanel(
        condition = "input.main_nav == 'audio_file_overview'",
        uiOutput("audio_player_full")
      ),
      conditionalPanel(
        condition = "input.main_nav == 'audio_file_overview'",
        uiOutput("audio_file_pivot_view")
      )
    # uiOutput("audio_player_full"),
    # uiOutput("audio_file_pivot_view")
  )
  ),
  
  bslib::nav_panel(
    title = "File Drilldown",
    value = "audio_file_drilldown",
    layout_sidebar(
      sidebar = sidebar(
        width = "500px",
        open = FALSE,
        bslib::accordion(
          id = "file_drilldown_accordion",
          open = FALSE,
          accordion_panel(
            "Model-match Scale",
            icon = icon("filter"),
            tags$div(
              style = "position: relative; width: 86%; margin: 0 auto;",  # align block
              sliderInput(
                inputId = "confidence_selection_file_drilldown",
                label = "BirdNET Model-match Scale",
                min = 0,
                max = 1,
                value = c(0.7, 1),
                step = 0.01,
                width = "100%"
              ),
              tags$div(
                style = "display: flex; justify-content: space-between; font-size: 12px; margin-top: -10px;",
                tags$span("Prepare for adventure"),
                tags$span("Not bad"),
                tags$span("Pretty good")
              )
            )
          ),
          accordion_panel(
            "File Lookup",
            icon = icon("magnifying-glass"),
            selectizeInput(inputId = "file_lookup_drilldown",
                           label = NULL,
                           choices = NULL,
                           multiple = FALSE)
          ),
          accordion_panel(
            "Plot Settings",
            icon = icon("chart-simple"),
            radioButtons(
              inputId = "weather_context",
              label = "Weather Time Range",
              choices = c("24 Hour Context" = "day", "1 Hour Context" = "hour"),
              selected = "day"
            )
          ),
          p(),
          footer_text
        )
      ),
      

        conditionalPanel(
          condition = "input.main_nav == 'audio_file_drilldown'",
          uiOutput("file_title")
        ),
        
        accordion(
          open = TRUE,
          accordion_panel(
            "Weather Plots",
            layout_columns(
              plotOutput("file_wind_plot"),
              plotOutput("file_temp_plot"),
              col_widths = c(6, 6)
            )
          ),
          accordion_panel(
            "File Scatterplot",
            conditionalPanel(
              condition = "input.main_nav == 'audio_file_drilldown'",
              plotlyOutput("audio_file_drilldown_plot")
            )
          ),
          accordion_panel(
            "File Details Table",
            conditionalPanel(
              condition = "input.main_nav == 'audio_file_drilldown'",
              uiOutput("audio_file_drilldown_df")
            )
          )
        )
  )),
  
  footer = tagList(
    # Add your custom script for clearing Plotly selection
    tags$script(HTML("
  Shiny.addCustomMessageHandler('plotly-clearSelection', function(plotId) {
    var plot = document.getElementById(plotId);
    if (plot) {
      Plotly.restyle(plot, {selectedpoints: [null]});
    }
  });
")),
    
    tags$script(HTML("
  $(document).on('dblclick', 'div[data-table-id] table tbody tr', function() {
    var wrapperDiv = $(this).closest('div[data-table-id]');
    var tableId = wrapperDiv.attr('data-table-id');
    var table = wrapperDiv.find('table').DataTable();
    var rowData = table.row(this).data();
    var originalIndex = rowData[rowData.length - 1];
    
    Shiny.setInputValue(tableId + '_dblclick', originalIndex, {priority: 'event'});
  });
")),
    
    tags$head(
      tags$style(HTML("
    .navbar-nav {
      display: flex;
      justify-content: space-between;
      width: 100%;
    }

    .nav-tab-right:hover {
      background-color: #f5c6cb !important; /* slight hover change */
      color: #721c24 !important;
    }

    .navbar-nav > li:last-child {
      order: 2;
    }
    
    # .footer {
    #   position: absolute;
    #   bottom: 0;
    #   left: 0;
    #   text-align:right;
    #   width: 100%;
    #   height:2.5rem;
    # }

  "))
    ),
    
    # tags$footer("This project is powered by BirdNET Analyzer and the R Project.", class = "footer"),
    
    tags$style(HTML("
  .lightblue {
    background-color: #007bc2 !important;
    color: white !important;
  }
"))
  )
)
  
    




server <- function(input, output, session) {
  
  #################
  ## URL Routing ##
  #################
  # Disable table button on start
  shinyjs::addClass("go", "lightblue")

  
  # Reactive value to store the selected species code
  species_click <- reactiveVal(NULL) # Default species code
  file_click <- reactiveVal(NULL)
  
  # Send species_click to the client for conditional formatting in species-loc-drilldown
  observe({
    session$sendCustomMessage("species_click", species_click())
  })
  
  observeEvent(session$clientData$url_hash, {
    fullHash <- session$clientData$url_hash # e.g., "#species_overview?species=cedwre"
    fullHash <- sub("^#", "", fullHash)
    
    tab <- strsplit(fullHash, "\\?")[[1]][1]
    queryString <- strsplit(fullHash, "\\?")[[1]][2]
    
    species <- NULL
    file_name <- NULL
    if (!is.na(queryString) & tab == "species_loc_drilldown") {
      params <- strsplit(queryString, "&")[[1]]
      params_list <- setNames(
        sapply(strsplit(params, "="), `[`, 2),
        sapply(strsplit(params, "="), `[`, 1)
      )
      species <- params_list[["species"]]
    }
    
    if (!is.na(queryString) & tab == "audio_file_drilldown") {
      params <- strsplit(queryString, "&")[[1]]
      params_list <- setNames(
        sapply(strsplit(params, "="), `[`, 2),
        sapply(strsplit(params, "="), `[`, 1)
      )
      file_name <- params_list[["file"]]
    }
    
    if (!is.null(tab)) {
      freezeReactiveValue(input, "main_nav")
      updateNavbarPage(session, "main_nav", selected = tab)
    }
    
    if (!is.null(species)) {
      species_click(species)
    }
    
    if (!is.null(file_name)) {
      file_click(file_name)
    }

  }, priority = 1)
  
  
  observeEvent(input$main_nav, {
    currentHash <- sub("#", "", session$clientData$url_hash)
    
    # If species_click() is defined, include it in the URL
    if (input$main_nav %in% c("species_overview", "species_loc_drilldown") && !is.null(species_click())) {
      pushQueryString <- paste0("#", input$main_nav, "?species=", species_click())
    } else if (input$main_nav == "audio_file_drilldown") {
      pushQueryString <- paste0("#", input$main_nav, "?file=", file_click())
    } else {
      pushQueryString <- paste0("#", input$main_nav)
    }
    
    if (is.null(currentHash) || currentHash != input$main_nav) {
      freezeReactiveValue(input, "main_nav")  # avoid unnecessary reactivity loop
      updateQueryString(pushQueryString, mode = "push", session)
    }
    
  }, priority = 0)
  
  ###############################################
  ## Application Build: Prairie Haven Overview ##
  ###############################################
  
  # Function to get week from date
  get_week_from_date <- function(date) {
    if (anyNA(date)) return(NA_integer_)
    
    # Extract year from the given date
    year <- as.integer(format(date, "%Y"))
    
    # Get the first day of the year
    first_day <- as.Date(paste0(year, "-01-01"))
    
    # Find the first Sunday of the year
    first_sunday <- first_day + (7 - wday(first_day) + 1) %% 7
    
    # Calculate the difference in days between the given date and the first Sunday
    days_since_first_sunday <- as.integer(as.Date(date) - as.Date(first_sunday))
    
    # Determine week number
    week_number <- (days_since_first_sunday %/% 7) +1
    
    return(week_number)
  }

  create_table_pivot <- function(data, current_week = NULL) {
    # Add a helper column with row index for DT styling
    data$row_index <- seq_len(nrow(data))
    
    dt <- datatable(
      data,
      escape = FALSE,
      extensions = c("Buttons", "FixedHeader"),
      options = list(
        # Alternative: Custom positioning with div classes
        dom = 'rt<"row"<"col-sm-4"f><"col-sm-4"B><"col-sm-4"i>>',
        ordering = TRUE,
        buttons = list(
          list(extend = 'csv', className = 'btn-sm'),
          list(extend = 'copy', className = 'btn-sm')
        ),
        page_length = nrow(data),
        lengthMenu = list(c(nrow(data)), c("All")),
        columnDefs = list(list(visible=FALSE, targets= c("Species.Code", "row_index"))),
        fixedHeader = list(header = TRUE),
        scrollY = 500,
        # Add custom CSS for styling
        initComplete = JS(
          "function(settings, json) {",
          "  // Style the search input to be smaller",
          "  $('.dataTables_filter input').css({",
          "    'width': '200px',",
          "    'height': '30px',",
          "    'font-size': '12px'",
          "  });",
          "  // Style the search label",
          "  $('.dataTables_filter label').css({",
          "    'font-size': '12px',",
          "    'font-weight': 'normal'",
          "  });",
          "  // Make buttons smaller",
          "  $('.dt-buttons .btn').css({",
          "    'font-size': '11px',",
          "    'padding': '4px 8px',",
          "    'margin': '2px'",
          "  });",
          "  // Style the info display to match other elements",
          "  $('.dataTables_info').css({",
          "    'font-size': '12px',",
          "    'font-weight': 'normal',",
          "    'padding-top': '8px',",
          "    'margin': '0'",
          "  });",
          "  // Ensure all elements are vertically aligned",
          "  $('.row > div').css({",
          "    'display': 'flex',",
          "    'align-items': 'center'",
          "  });",
          "}"
        )
      ),
      rownames = FALSE,
      selection = list(mode = "single", target = "row"),
      fillContainer = TRUE
    ) %>%
      formatStyle(
        "row_index",
        target = "row",
        fontStyle = styleEqual(1, "italic")
      ) %>%
      formatStyle(
        "Total.Count",
        fontStyle = "italic"
      ) 
    
    if (!is.null(current_week) && current_week %in% colnames(data)) {
      dt <- dt %>%
        formatStyle(
          current_week,
          fontWeight = "bold",
          color = "#007bc2"
        )
    } 
    
    return(dt)
  }
  
  create_pivot_month <- function(df, yr_input = "All", loc_input = "All", species_input = NULL) {
    df <- df %>%
      filter(if (!is.null(species_input)) Common.Name %in% species_input else TRUE) %>%
      filter(if (yr_input != "All") lubridate::year(as.Date(Date)) == yr_input else TRUE) %>%
      filter(if (loc_input != "All") Location == loc_input else TRUE) %>%
      mutate(Month = factor(month.abb[lubridate::month(as.Date(Date))], levels = month.abb)) %>%
      group_by(Common.Name, Species.Code, Month) %>%
      summarize(Count_by_Species = n(), .groups = "drop") %>%
      pivot_wider(names_from = Month, values_from = Count_by_Species, values_fill = list(Count_by_Species = 0)) %>%
      mutate(Total.Count = rowSums(select(., -c(Common.Name, Species.Code)), na.rm = TRUE)) 
   
    num_birds <- nrow(df[df$Common.Name != "nocall",])   
    df <- df %>%
      bind_rows(summarise(.,
                          across(where(is.numeric), sum),
                          across(where(is.character), ~ paste(num_birds, "Unique Species"))))
    
    # Reorder the columns: Common.Name, Species.Code, then months in order
    month_order <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                     "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    
    # Add missing month columns with 0 values if they don't exist
    for (month in month_order) {
      if (!(month %in% names(df))) {
        df[[month]] <- 0  # Add missing months with 0 values
      }
    }
    
    df <- df %>% select(Common.Name, Total.Count, Species.Code, all_of(month_order))
    df <- rbind(df[nrow(df),], df[1:(nrow(df)-1),])
    return(df)
  }
  
  create_pivot_week <- function(df, yr_input = "All", loc_input = "All", week_sel, time_input = "All", species_input = NULL) {
    df <- df %>%
      filter(if (!is.null(species_input)) Common.Name %in% species_input else TRUE) %>%
      filter(if (yr_input != "All") lubridate::year(as.Date(Date)) == yr_input else TRUE) %>%
      filter(if (loc_input != "All") Location == loc_input else TRUE) %>%
      filter(Week >= week_sel[1] & Week <= week_sel[2]) %>%
      mutate(Time = as_hms(Date.Time)) %>%
      mutate(Time.Of.Day = case_when(
        Time < hms(0, 30, 11) ~ "Morning",
        Time == hms(0, 30, 11) ~ "Afternoon",
        Time > hms(0, 30, 11) ~ "Night"
      )) %>%
      filter(if (time_input != "All") Time.Of.Day == time_input else TRUE) %>%
      group_by(Common.Name, Species.Code, Week) %>%
      summarize(Count_by_Species = n(), .groups = "drop") 
    
    # Capture and sort year columns before pivoting
    week_cols <- sort(unique(df$Week))
    names(week_cols) <- get_week_date_range(lubridate::year(Sys.time()), week_cols, format = TRUE)

    df <- df %>%
      pivot_wider(names_from = Week, values_from = Count_by_Species, values_fill = list(Count_by_Species = 0)) %>%
      mutate(Total.Count = rowSums(select(., all_of(as.character(week_cols))), na.rm = TRUE)) %>%
      select(Common.Name, Species.Code, all_of(as.character(week_cols)), Total.Count)
    
    # Rename columns to use date ranges instead of week numbers
    old_names <- as.character(week_cols)
    new_names <- names(week_cols)
    names(df)[names(df) %in% old_names] <- new_names[match(names(df)[names(df) %in% old_names], old_names)]
    
    # Add summary row at top
    num_birds <- nrow(df[df$Common.Name != "nocall",])
    summary_row <- df %>%
      summarise(
        Common.Name = paste(num_birds, "Unique Species"),
        Species.Code = "",
        Total.Count = sum(df$Total.Count),
        across(all_of(new_names), sum)  # Use new_names here
      )
    
    df <- bind_rows(summary_row, df) %>%
      select(Common.Name, Total.Count, Species.Code, all_of(new_names))  # Use new_names here too
    
    return(df)
    
  }

  create_pivot_year <- function(df, loc_input = "All", month_sel, week_sel, species_input = NULL) {
    if (!identical(as.integer(week_sel), as.integer(c(1, 53)))) {
      month_sel <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                     "Aug", "Sep", "Oct", "Nov", "Dec")
    }
    
    else if (identical(as.integer(week_sel), as.integer(c(1,53))) & is.null(month_sel)) {
      month_sel <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                     "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    }
    df <- df %>%
      filter(if (!is.null(species_input)) Common.Name %in% species_input else TRUE) %>%
      filter(if (loc_input != "All") Location == loc_input else TRUE) %>%
      filter(Week >= week_sel[1] & Week <= week_sel[2]) %>%
      filter(Month %in% month_sel) %>%
      mutate(Date = as.Date(Date), Year = year(Date)) %>%
      group_by(Common.Name, Species.Code, Year) %>%
      summarize(Count_by_Species = n(), .groups = "drop")
    
    # Capture and sort year columns before pivoting
    year_cols <- sort(unique(df$Year))
    
    df <- df %>%
      pivot_wider(names_from = Year, values_from = Count_by_Species, values_fill = list(Count_by_Species = 0))
    
    df <- df %>%
      mutate(Total.Count = rowSums(select(., all_of(as.character(year_cols))), na.rm = TRUE)) %>%
      select(Common.Name, Species.Code, all_of(as.character(year_cols)), Total.Count)
    
    # Add summary row at top
    num_birds <- nrow(df[df$Common.Name != "nocall",])
    summary_row <- df %>%
      summarise(
        Common.Name = paste(num_birds, "Unique Species"),
        Species.Code = "",
        Total.Count = sum(df$Total.Count),
        across(all_of(as.character(year_cols)), sum)
      )
    
    df <- bind_rows(summary_row, df) %>%
      select(Common.Name, Total.Count, Species.Code, all_of(as.character(year_cols)))
    
    return(df)
  }


  create_pivot_location <- function(df, yr_input = "All", month_sel, week_sel, species_input = NULL) {
    
    if (!identical(as.integer(week_sel), as.integer(c(1, 53)))) {
      month_sel <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                     "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    }
    else if (identical(as.integer(week_sel), as.integer(c(1,53))) & is.null(month_sel)) {
      month_sel <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                     "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    }
    
    df <- df %>%
      filter(if (!is.null(species_input)) Common.Name %in% species_input else TRUE) %>%
      filter(if (yr_input != "All") lubridate::year(as.Date(Date)) == yr_input else TRUE) %>% 
      filter(Week >= week_sel[1] & Week <= week_sel[2]) %>%
      filter(Month %in% month_sel) %>%
      group_by(Common.Name, Species.Code, Location) %>%
      summarize(Count_by_Species = n(), .groups = "drop") %>%
      pivot_wider(names_from = Location, values_from = Count_by_Species, values_fill = list(Count_by_Species = 0)) %>%
      mutate(Total.Count = rowSums(select(., -c(Common.Name, Species.Code)), na.rm = TRUE)) %>%
      select(Common.Name, Total.Count, everything())
    
    num_birds <- nrow(df[df$Common.Name != "nocall",])    
    df <- df %>%
      bind_rows(summarise(.,
                          across(where(is.numeric), sum),
                          across(where(is.character), ~ paste(num_birds, "Unique Species"))))
    
    df <- rbind(df[nrow(df),], df[1:(nrow(df)-1),])
    return(df)
  }
  
  confidence_filter <- function(data, conf) {
    min_conf <- conf[1]
    max_conf <- conf[2]
    data <- subset(data, as.numeric(Confidence) >= min_conf & as.numeric(Confidence) <= max_conf)
    return(data)
  } 
  
 
  filtered_data <- reactiveVal(NULL)
  
  slider_touched <- reactiveVal(FALSE)
  pivot_ready <- reactiveVal(TRUE) 
  
  # Add this at the top of your server function
  confidence_value <- reactiveVal(c(0.7, 1))  # Single source of truth
  
  # Replace your existing debounced_conf with:
  debounced_conf <- debounce(confidence_value, 2000)
  
  # Run default filtering on app load
  observeEvent(all_data, {
    filtered_data(confidence_filter(all_data, confidence_value()))
  }, once = TRUE)
  
  # Add observers for each slider to update the central reactive value
  observeEvent(input$confidence_selection_overview, {
    confidence_value(input$confidence_selection_overview)
    slider_touched(TRUE)
    shinyjs::disable("go")
  }, ignoreInit = TRUE)
  
  observeEvent(input$confidence_selection_drilldown, {
    confidence_value(input$confidence_selection_drilldown)
    slider_touched(TRUE)
    shinyjs::disable("go")
  }, ignoreInit = TRUE)
  
  observeEvent(input$confidence_selection_species_overview, {
    confidence_value(input$confidence_selection_species_overview)
    slider_touched(TRUE)
    shinyjs::disable("go")
  }, ignoreInit = TRUE)
  
  observeEvent(input$confidence_selection_file_overview, {
    confidence_value(input$confidence_selection_file_overview)
    slider_touched(TRUE)
    shinyjs::disable("go")
  }, ignoreInit = TRUE)
  
  observeEvent(input$confidence_selection_file_drilldown, {
    confidence_value(input$confidence_selection_file_drilldown)
    slider_touched(TRUE)
    shinyjs::disable("go")
  }, ignoreInit = TRUE)
  
  # Add observers to keep all sliders in sync
  observeEvent(confidence_value(), {
    updateSliderInput(session, "confidence_selection_overview", value = confidence_value())
    updateSliderInput(session, "confidence_selection_drilldown", value = confidence_value())
    updateSliderInput(session, "confidence_selection_species_overview", value = confidence_value())
    updateSliderInput(session, "confidence_selection_file_overview", value = confidence_value())
    updateSliderInput(session, "confidence_selection_file_drilldown", value = confidence_value())
    
  }, ignoreInit = TRUE)
  

  observe({
    req(all_data)
    req(slider_touched() == TRUE)
    req(debounced_conf())
    
    new_data <- confidence_filter(all_data, debounced_conf())
    filtered_data(new_data)
    
    # Enable generate table button after filtering
    shinyjs::enable("go")
    shinyjs::addClass("go", "lightblue")
    shinyjs::html("go", "Regenerate Table")
  })
  
  observeEvent(input$sel_view, {
    pivot_ready(FALSE)
    # accordion_panel_open("overview_accordion", values = "pivot_filters")
    shinyjs::enable("go")
    shinyjs::addClass("go", "lightblue")
    shinyjs::html("go", "Generate Table")
  })

  observeEvent({
    list(input$year_sel, input$loc_sel, input$month_sel, input$week_sel, input$time_sel, input$species_lookup_overview)
    }, {
    shinyjs::enable("go")
    shinyjs::addClass("go", "lightblue")
  })

  observeEvent(input$go, {
    shinyjs::disable("go")
    shinyjs::removeClass("go", "lightblue")
    shinyjs::html("go", "Regenerate Table")
    pivot_ready(TRUE)
  })
  
  species_by_month <- eventReactive(input$go, {
    req(input$year_sel)
    req(input$loc_sel)
    req(filtered_data())
    
    create_pivot_month(filtered_data(), input$year_sel, input$loc_sel, input$species_lookup_overview)
  })
  
  species_by_location <- eventReactive(input$go, {
    req(input$year_sel)
    req(input$week_sel)
    req(filtered_data())
    
    create_pivot_location(filtered_data(), input$year_sel, input$month_sel, input$week_sel, input$species_lookup_overview)
  })
  
  species_by_year <- eventReactive(input$go, {
    req(input$loc_sel)
    req(input$week_sel)
    req(filtered_data())
    
    create_pivot_year(filtered_data(), input$loc_sel, input$month_sel, input$week_sel, input$species_lookup_overview)
  })
  
  species_by_week <- eventReactive(input$go, {
    req(input$loc_sel)
    req(input$week_sel)
    req(input$year_sel)
    req(input$time_sel)
    req(filtered_data())
    
    create_pivot_week(filtered_data(), input$year_sel,input$loc_sel, input$week_sel, input$time_sel, input$species_lookup_overview)
  })
  
  
  # Table rendering
  output$species_by_month_pivot <- DT::renderDT({
    req(pivot_ready())
    create_table_pivot(species_by_month())
  })
  
  output$species_by_location_pivot <- DT::renderDT({
    req(pivot_ready())
    create_table_pivot(species_by_location())
  })
  
  output$species_by_year_pivot <- DT::renderDT({
    req(pivot_ready())
    create_table_pivot(species_by_year())
  })
  
  output$species_by_week_pivot <- DT::renderDT({
    req(pivot_ready())
    create_table_pivot(species_by_week(), current_week = get_week_date_range(year = lubridate::year(Sys.time()),
                                                                             week = get_week_from_date(Sys.time()),
                                                                             format = TRUE))
  })
  

  year_choices <- reactive({
    years <- unique(as.character(lubridate::year(all_data$Date)))
    years <- years[order(years)]
    c(setNames(as.list(years), years), "All" = "All")
  }) %>% bindCache(all_data)
  
  loc_choices <- reactive({
    locs <- unique(all_data$Location)
    locs <- locs[order(locs)]
    c(setNames(as.list(locs), locs), "All" = "All")
  }) %>% bindCache(all_data)
  
  month_choices <- reactive({
    months <- unique(lubridate::month(all_data$Date, label = TRUE, abbr = TRUE))
    months <- factor(months, levels = month.abb)  # Keep order
    months <- sort(months)
    c(setNames(as.list(as.character(months)), as.character(months)))
  }) %>% bindCache(all_data)

  # Dynamic UI based on selection
  output$overview_view <- renderUI({
    req(input$sel_view)
    
    if (input$sel_view == "by_month") {
      tagList(
        selectInput("year_sel", label = "Year Selection", choices = year_choices(), selected = "All"),
        selectInput("loc_sel", label = "Location Selection", choices = loc_choices(), selected = "All")
      )

    } else if (input$sel_view =="by_location") {
      tagList(
        selectInput("year_sel", label = "Year Selection", choices = year_choices(), selected = "All"),
        selectInput("month_sel", label = "Month Selection", 
                    choices = month_choices(), 
                    selected = month_choices(),
                    multiple = TRUE),
        sliderInput("week_sel", label = "Week Selection",
                    min = 1,
                    max = 53,
                    value = c(1, 53),
                    step = 1),
        p(em("Note that week selection overrides month selection."), style = "font-size: 12px;")
      )
      
    } else if (input$sel_view == "by_year") {
      tagList(
        selectInput("loc_sel", label = "Location Selection", choices = loc_choices(), selected = "All"),
        selectInput("month_sel", label = "Month Selection", choices = month_choices(), selected = month_choices(),
                    multiple = TRUE),
        sliderInput("week_sel", label = "Week Selection",
                    min = 1,
                    max = 53,
                    value = c(1, 53),
                    step = 1),
        p(em("Note that week selection overrides month selection."), style = "font-size: 12px;")
      )
      
    } else if (input$sel_view == "by_week") {
      tagList(
        selectInput("loc_sel", label = "Location Selection", choices = loc_choices(), selected = "All"),
        selectInput("year_sel", label = "Year Selection", choices = year_choices(), selected = "All"),
        selectInput("time_sel", label = "Time Selection", 
                    choices = c("Morning", "Afternoon", "Night", "All"), 
                    selected = "All"),
        sliderInput("week_sel", label = "Week Selection",
                    min = 1,
                    max = 53,
                    value = c(1, 53),
                    step = 1),
        p(em(tagList("This week is ", strong("Week ", style = "font-size: 12px; color: #007bc2;"), 
                     strong(get_week_from_date(Sys.time()), style = "font-size: 12px; color: #007bc2;"))), style = "font-size: 12px;")
      )
    }
  }) 
  
  output$pivot_dt <- renderUI({
    req(input$sel_view)
    list(
      if (pivot_ready() != TRUE) {
        p("Click the Generate Table Button in the Sidebar to View Table")
        uiOutput("waiting_image")
        
      } else if (input$sel_view == "by_month") {
        tagList(
          p(),
          p(icon("circle-info"), strong("Double click a row below to view species specific details")),
          div(
            id = "species_by_month_pivot_wrapper",
            `data-table-id` = "species_by_month_pivot",
            DT::dataTableOutput("species_by_month_pivot") 
          )
        )
      } else if (input$sel_view == "by_location") {
        tagList(
          p(),
          p(icon("circle-info"), strong("Double click a row below to view species specific details")),
          div(
            id = "species_by_location_pivot_wrapper",
            `data-table-id` = "species_by_location_pivot",
            DT::dataTableOutput("species_by_location_pivot") 
          )
        )
      } else if (input$sel_view == "by_year") {
        tagList(
          p(),
          p(icon("circle-info"), strong("Double click a row below to view species specific details")),
          div(
            id = "species_by_year_pivot_wrapper",
            `data-table-id` = "species_by_year_pivot",
            DT::dataTableOutput("species_by_year_pivot") 
          )
        )
        
      } else if (input$sel_view == "by_week") {
        tagList(
          p(),
          p(icon("circle-info"), strong("Double click a row below to view species specific details")),
          div(
            id = "species_by_week_pivot_wrapper",
            `data-table-id` = "species_by_week_pivot",
            DT::dataTableOutput("species_by_week_pivot")
          )
        )
      }
    )
  })
  
  output$waiting_image <- renderUI({
    tagList(
      p(icon("circle-info"), strong(ph_overview_text)),
      div(
        tags$img(src = ph_overview_pic,
                 alt = "Sharp Shinned Hawk",
                 style = "max-width: 100%; height: auto;"),
        p("Photo by Marcie O'Connor at ",
          tags$a("Prairie Haven", href = "https://prairiehaven.com", target = "_blank"),
          style = "font-size: 12px; color: #666; margin-top: 5px;")
      )
    )
  })
  
  # Observe clicks on the tables
  observeEvent(input$species_by_month_pivot_dblclick, {
    selected_row <- input$species_by_month_pivot_dblclick
    if (!is.null(selected_row) && length(selected_row) > 0) {
      species_list <- species_by_month()
      if (selected_row[1] <= nrow(species_list)) {
        species_click(species_list$Species.Code[selected_row[1]])  
        updateNavbarPage(session, "main_nav", selected = "species_loc_drilldown")
      }
    }
  })
  
  observeEvent(input$species_by_location_pivot_dblclick, {
    selected_row <- input$species_by_location_pivot_dblclick
    if (!is.null(selected_row) && length(selected_row) > 0) {
      species_list <- species_by_location()
      if (selected_row[1] <= nrow(species_list)) {
        species_click(species_list$Species.Code[selected_row[1]])  
        updateNavbarPage(session, "main_nav", selected = "species_loc_drilldown")
      }
    }
  })
  
  observeEvent(input$species_by_year_pivot_dblclick, {
    selected_row <- input$species_by_year_pivot_dblclick
    if (!is.null(selected_row) && length(selected_row) > 0) {
      species_list <- species_by_year()
      if (selected_row[1] <= nrow(species_list)) {
        species_click(species_list$Species.Code[selected_row[1]])  
        updateNavbarPage(session, "main_nav", selected = "species_loc_drilldown")
      }
    }
  })
  
  observeEvent(input$species_by_week_pivot_dblclick, {
    selected_row <- input$species_by_week_pivot_dblclick
    if (!is.null(selected_row) && length(selected_row) > 0) {
      species_list <- species_by_week()
      if (selected_row[1] <= nrow(species_list)) {
        species_click(species_list$Species.Code[selected_row[1]])
        updateNavbarPage(session, "main_nav", selected = "species_loc_drilldown")
      }
    }
  })
  
  ############################################  
  ## Application Build: Audio File Overview ##
  ############################################
  
  species_values <- unique(all_data$Common.Name)
  default_species_vals <- c("American Crow", "American Goldfinch", "American Woodcock",
                            "Blue Jay", "Hairy Woodpecker", "Red-bellied Woodpecker", 
                            "White-breasted Nuthatch")
  
  # Debounced species to exclude value
  debounced_species_exclude <- debounce(reactive(input$species_exclude), 500)
  debounced_species_file_filter <- debounce(reactive(input$species_file_filter), 500)
  
  # Filtered data based on exclusion list
  cached_filtered_data <- reactiveVal(NULL)
  
  observeEvent({
    filtered_data()
    debounced_species_exclude()
    debounced_species_file_filter()
  }, {
    req(filtered_data(), debounced_species_exclude())
    # Filter out species we want to exclude
    filtered <- filtered_data()[!(filtered_data()$Common.Name %in% debounced_species_exclude()),]
    
    # Identify files with specified species 
    if (!is.null(debounced_species_file_filter())) {
      files <- filtered[filtered$Common.Name %in% debounced_species_file_filter(),]$Begin.Path
      filtered <- filtered[filtered$Begin.Path %in% files,]
    }
    
    cached_filtered_data(filtered)
  }, ignoreNULL = FALSE)
  
  # Optimized pivot creator
  create_pivot_files <- function(df) {
    
    find_audio_file_full <- function(row) {
      if (is.null(row[["Begin.Path"]]) || row[["Begin.Path"]] == "") {
        return(NULL)
      }
      
      file_name <- basename(row[["Begin.Path"]])
      year <- year(row[["Date"]])
      file_loc <- paste0(audio_filepath, "Bio ", as.character(year), "/From Recorders")
      
      audio_loc <- file.path(file_loc, file_name)
      
      if (is.null(audio_loc) || audio_loc == "" || !file.exists(audio_loc)) {
        return(NULL)
      }
      
      return(audio_loc)
    }
    
    # Step 3: Add overall metrics
    df_all_cols <- df %>%
      group_by(Begin.Path, Location, Date, Date.Time, avg_temperature, avg_windspeed) %>%
      summarise(
        Number.Observations = n(),
        Number.Unique.Species = n_distinct(Common.Name),
        Mean.Confidence = as.numeric(format(round(mean(as.numeric(Confidence)), 4), nsmall = 4 )),
        Median.Confidence = as.numeric(format(round(median(as.numeric(Confidence)), 4), nsmall = 4)),
        SD.Confidence = if (n()>1) as.numeric(format(round(sd(as.numeric(Confidence)), 4), nsmall = 4)) else 0,
        .groups = "drop"
      ) %>%
      mutate(Time = as_hms(Date.Time)) %>%
      mutate(Time.Of.Day = case_when(
        Time < hms(0, 30, 11) ~ "Morning",
        Time == hms(0, 30, 11) ~ "Afternoon",
        Time > hms(0, 30, 11) ~ "Night"
      )) %>%
      mutate(Time.Of.Day = as.character(Time.Of.Day)) %>%
      mutate(Year = as.character(lubridate::year(Date))) %>%
      mutate(Month = as.character(lubridate::month(Date, label = TRUE, abbr = FALSE))) %>%
      # left_join(top_species_df, by = "Begin.Path") %>%
      rowwise() %>%
      mutate(Play.Audio = {
        audio_id <- find_audio_file_full(pick(everything()))
        if (!is.null(audio_id)) {
          as.character(shinyInput(
            FUN = actionButton,
            id = audio_id,
            label = "Audio",
            onclick = 'Shiny.setInputValue(\"stream_audio\", this.id, {priority: \"event\"})'
          ))
        } else {
          "No Audio File"
        }
      }) 

    df_all_cols$Number.Observations.Pct = percent_rank(df_all_cols$Number.Observations)
    df_all_cols$Windspeed.Pct = 1 - percent_rank(df_all_cols$avg_windspeed)
    df_all_cols$Confidence.Pct = percent_rank(df_all_cols$Mean.Confidence)
    
    df_all_cols <- df_all_cols %>%
      mutate(Score = case_when(
        is.na(avg_windspeed) ~ (Number.Observations.Pct + Confidence.Pct + 0.5) / 3,
        !is.null(avg_windspeed) ~ (Number.Observations.Pct + Windspeed.Pct + Confidence.Pct) / 3
      )) %>%
      mutate_if(is.numeric, round, 3) 
      
    df_all_cols <- df_all_cols %>%
      mutate(Score_Pct = percent_rank(Score),
             Score_Color = case_when(
               Score >= 0.9 ~ "#007bc2",
               Score >= 0.8 & Score < 0.9 ~ "#358aca",  
               Score >= 0.7 & Score < 0.8 ~ "#5399d1",  
               Score >= 0.6 & Score < 0.7 ~ "#6da7d8",  
               Score >= 0.5 & Score < 0.6 ~ "#86b6df", 
               Score >= 0.4 & Score < 0.5 ~ "#9ec5e6",
               Score >= 0.3 & Score < 0.4 ~ "#b6d3ed",
               Score >= 0.2 & Score < 0.3 ~ "#cee2f3",
               Score >= 0.1 & Score < 0.2 ~ "#e7f0f9",
               Score < 0.1 ~ "#ffffff"               
             ))
    df_all_cols$row_index <- seq_len(nrow(df_all_cols))
    df_all_cols <- df_all_cols %>%
      select(Play.Audio, Begin.Path, Number.Observations, Number.Unique.Species, 
             Location, Date, Year, Month, Time, Time.Of.Day, 
             Mean.Confidence, avg_temperature, avg_windspeed,
             # Top.Species.1, 
             Score, Score_Color, row_index)

    return(df_all_cols)
  }
  
  # Final reactive for display data 
  file_list <- reactive({
    req(cached_filtered_data())
    create_pivot_files(cached_filtered_data())
  })
  
  # Render the pivot table
  output$file_list_pivot <- DT::renderDT({
    datatable(file_list(),
              escape = FALSE,
              extensions = c("Buttons", "FixedHeader"),
              filter = "top",
              rownames = FALSE,
              colnames = c("# Obs" = "Number.Observations", "# Unique Species" = "Number.Unique.Species",
                           "Time of Day" = "Time.Of.Day", "File Name" = "Begin.Path",
                           "Mean Conf" = "Mean.Confidence", 
                           "Wind Speed" = "avg_windspeed", "Temperature" = "avg_temperature"),
              fillContainer = TRUE,
              selection = "single",
              # class = "row-border",
              options = list(
                # dom = "Blfrtip",
                dom = 'lrt<"row"<"col-sm-4"f><"col-sm-4"B><"col-sm-4"i>>p',
                ordering = TRUE,
                buttons = list(
                  list(extend = 'csv', className = 'btn-sm'),
                  list(extend = 'copy', className = 'btn-sm')
                ),
                pageLength = 100,
                lengthMenu = list(c(25, 50, 100),
                                  c('25', '50', '100')),
                paging = TRUE,
                fixedHeader = list(header = TRUE),
                scrollY = 500,
                scrollX = TRUE,
                columnDefs = list(list(visible=FALSE, targets= c("Score_Color", "row_index"))),
                # Add custom CSS for styling
                initComplete = JS(
                  "function(settings, json) {",
                  "  // Style the search input to be smaller",
                  "  $('.dataTables_filter input').css({",
                  "    'width': '200px',",
                  "    'height': '30px',",
                  "    'font-size': '12px'",
                  "  });",
                  "  // Style the search label",
                  "  $('.dataTables_filter label').css({",
                  "    'font-size': '12px',",
                  "    'font-weight': 'normal'",
                  "  });",
                  "  // Make buttons smaller",
                  "  $('.dt-buttons .btn').css({",
                  "    'font-size': '11px',",
                  "    'padding': '4px 8px',",
                  "    'margin': '2px'",
                  "  });",
                  "  // Style the info display to match other elements",
                  "  $('.dataTables_info').css({",
                  "    'font-size': '12px',",
                  "    'font-weight': 'normal',",
                  "    'padding-top': '8px',",
                  "    'margin': '0'",
                  "  });",
                  "  // Ensure all elements are vertically aligned",
                  "  $('.row > div').css({",
                  "    'display': 'flex',",
                  "    'align-items': 'center'",
                  "  });",
                  "}"
                ))
              ) %>%
                formatStyle("Score",
                            valueColumns = "Score_Color",
                            target = "cell",
                            backgroundColor = JS("value"))
  }) 
  
  
  # UI for species exclusion and table display 
  
  output$species_to_exclude <- renderUI({
    selectizeInput(inputId = "species_exclude",
                   label = "Species to Exclude",
                   choices = species_values,
                   selected = default_species_vals,
                   multiple = TRUE,
                   width = "400px")
  })
  
  output$species_file_filter <- renderUI({
    selectizeInput(inputId = "species_file_filter",
                   label = "Filter Files by Species",
                   choices = species_values,
                   multiple = TRUE,
                   width = "400px")
  })
  
  output$audio_file_pivot_view <- renderUI({
    tagList(
      p(icon("circle-info"), strong("Double click a row below to view file specific details")),
      div(
        id = "file_list_pivot_wrapper",
        `data-table-id` = "file_list_pivot",
        DT::dataTableOutput("file_list_pivot") %>% withSpinner()
      )
    )
  })

  selected_audio_full <- reactiveValues(file = NULL)
  audio_file_path_full <- reactiveVal(NULL)

  observeEvent(input$stream_audio, {
    selectedRow <- input$stream_audio
    audio_file_path_raw <- unlist(selectedRow)

    selected_audio_full$file <- audio_file_path_raw
    
    preview_audio()
  })
  
  preview_audio <- reactive({
    req(selected_audio_full$file)
    
    if (!dir.exists("www")) {
      dir.create("www")
    }
    
    if (!is.null(audio_file_path_full())) {
      if (file.exists(audio_file_path_full())) {
        file.remove(audio_file_path_full())
      }
    }
    
    audio_src <- basename(selected_audio_full$file)
    dest_path <- file.path("www", audio_src)
    
    output_wav <- av_audio_convert(audio = selected_audio_full$file, 
                                   output = dest_path,
                                   total_time = 15)
    
    audio_file_path_full(dest_path)
    
    # Force UI refresh by generating a new audio tag
    output$audio_player_full <- renderUI({
      tags$audio(src = audio_src, type = "audio/wav", controlsList="nodownload", controls = NA, autoplay = TRUE)
    })
  })
  
  observeEvent(input$file_list_pivot_dblclick, {
    selected_row <- input$file_list_pivot_dblclick
    if (!is.null(selected_row) && length(selected_row) > 0) {
      file_list_df <- file_list()
      if (selected_row[1] <= nrow(file_list_df)) {
        file_click(file_list_df$Begin.Path[selected_row[1]])
        updateNavbarPage(session, "main_nav", selected = "audio_file_drilldown")
      }
    }
  })
  
  #############################################
  ## Applictaion Build: Audio File Drilldown ##
  #############################################
  
  # Server side selection options because there are a lot. 
  updateSelectizeInput(session, "file_lookup_drilldown", choices = c("Default", unique(all_data$Begin.Path)), selected = "Default", server = TRUE)
  
  # If a user manually searches for a species, update the species_click() reactive val
  observeEvent(input$file_lookup_drilldown, {
    file_name <- input$file_lookup_drilldown
    
    if (file_name != "Default"){
      file_click(file_name)
    }
    
    # Construct the new URL hash with species query
    newHash <- paste0("#", input$main_nav, "?file=", file_click())
    
    # Update the URL without reloading the app
    updateQueryString(newHash, mode = "push", session = session)
    
  }, ignoreInit = TRUE)
  
  
  file_details_data <- reactiveVal(NULL)
  
  observe({
    req(filtered_data())
    req(!is.null(file_click()))
    
    file_details_dt <- subset(filtered_data(), Begin.Path == file_click())
    file_details_dt <- file_details_dt %>%
      rowwise() %>%
      mutate(Sound.Button = list({
        url_val <- find_audio_file(.data)
        if (!is.null(url_val)) {
          paste0(
            '<a class="btn btn-primary" target="_blank" href="',
            url_val,
            '">Open Observation</a>'
          )
        } else {
          "No Audio Available"
        }
      }))
    
    file_details_data(file_details_dt)
  })
  
  get_date_time <- function(file_nm) {
    if (is.null(file_nm) || file_nm == "") {
      return(NULL)
    }
    
    date <- unlist(strsplit(basename(file_nm), split = "_"))[2]
    time <- substr(unlist(strsplit(basename(file_nm), split = "_"))[3], start = 1, stop = 6)
    date_time <- ymd_hms(paste(date, time), tz="UTC")
    return(date_time)
  }
  
  output$file_title <- renderUI({
    if (isTRUE(is.null(file_click()) || file_click() == "NA" || identical(file_click(), character(0)))) {
      tagList(
        p(icon("circle-info"), strong(file_drilldown_text)),
        div(
          tags$img(src = file_drilldown_pic,
                   alt = "Orchard Oriole",
                   style = "max-width: 100%; height: auto;"),
          p("Photo by Marcie O'Connor at",
            tags$a("Prairie Haven", href = "https://prairiehaven.com", target = "_blank"),
            style = "font-size: 12px; color: #666; margin-top: 5px;")
        )
      )
    } else {
      name_title <- file_click()
      div(
        p(h3(name_title))
      )
    }
  })
  
  output$file_wind_plot <- renderPlot({
    req(file_click())
    
    if (isFALSE(is.null(file_click()) || file_click() == "NA" || identical(file_click(), character(0)))) {
      date_time_start <- get_date_time(file_click())
      date_time_end <- date_time_start + 3600
      
      plot_time_start <- date_time_start - 43200 # 12 hours before audio file
      plot_time_end <- date_time_start + 43200 # 12 hours after audio file
      
      if (input$weather_context == "hour") {
        plot_time_start <- date_time_start
        plot_time_end <- date_time_end
      }
      
      # Connect to SQLite database
      con <- dbConnect(RSQLite::SQLite(), weather_path)
      
      wind_query <- "
    SELECT
      value AS windspeed,
      strftime('%Y-%m-%d %H:%M:%S', datetime(time, 'unixepoch')) AS time
    FROM windSpeed
    WHERE
      time >= ? AND
      time < ?
  "
      
      wind <- dbGetQuery(con, wind_query, params = list(as.numeric(plot_time_start), as.numeric(plot_time_end)))
      dbDisconnect(con)
      
      # Convert time back to POSIXct for ggplot
      wind$time <- as.POSIXct(wind$time, tz = "UTC")
      wind <- wind[format(as.POSIXct(wind$time), "%S") != "00",]
      
      # define region of audio file for plotting
      regions <- tibble(x1 = date_time_start, x2 = date_time_end, y1 = -Inf, y2 = Inf)
      
      ggplot(wind, aes(time, windspeed)) + 
        geom_line(linewidth = 1) +
        geom_rect(data = regions,
                  inherit.aes = FALSE,
                  mapping = aes(xmin = x1, xmax = x2,
                                ymin = y1, ymax = y2,
                                fill = "Audio File"),  # Just use a simple category name
                  color = "transparent",
                  alpha = 0.2) +
        scale_fill_manual(name = paste("Audio File:", format(date_time_start, "%Y-%m-%d %H:%M")),
                          values = c("Audio File" = "#007bc2")) +
        labs(title = "Wind Speed", x = "Date Time", y = "Wind Speed (mph)") +
        theme_minimal() +
        theme(legend.position = "bottom")
    }
  })
  
  output$file_temp_plot <- renderPlot({
    req(file_click())
    
    if (isFALSE(is.null(file_click()) || file_click() == "NA" || identical(file_click(), character(0)))) {
      date_time_start <- get_date_time(file_click())
      date_time_end <- date_time_start + 3600
      
      plot_time_start <- date_time_start - 43200 # 12 hours before audio file
      plot_time_end <- date_time_start + 43200 # 12 hours after audio file
      
      if (input$weather_context == "hour") {
        plot_time_start <- date_time_start
        plot_time_end <- date_time_end
      }
      
      # Connect to SQLite database
      con <- dbConnect(RSQLite::SQLite(), weather_path)
      
      temp_query <- "
    SELECT
      value AS temp,
      strftime('%Y-%m-%d %H:%M:%S', datetime(time, 'unixepoch')) AS time
    FROM outdoorTemperature
    WHERE 
      time >= ? AND
      time < ?
    "
      temp <- dbGetQuery(con, temp_query, params = list(as.numeric(plot_time_start), as.numeric(plot_time_end)))
      dbDisconnect(con)
      
      # Convert time back to POSIXct for ggplot
      temp$time <- as.POSIXct(temp$time, tz = "UTC")
      temp <- temp[format(as.POSIXct(temp$time), "%S") != "00",]
      
      # define region of audio file for plotting
      regions <- tibble(x1 = date_time_start, x2 = date_time_end, y1 = -Inf, y2 = Inf)
      
      ggplot(temp, aes(time, temp)) + 
        geom_line(linewidth = 1) +
        geom_rect(data = regions,
                  inherit.aes = FALSE,
                  mapping = aes(xmin = x1, xmax = x2,
                                ymin = y1, ymax = y2,
                                fill = "Audio File"),  # Just use a simple category name
                  color = "transparent",
                  alpha = 0.2) +
        scale_fill_manual(name = paste("Audio File:", format(date_time_start, "%Y-%m-%d %H:%M")),
                          values = c("Audio File" = "#007bc2")) +
        labs(title = "Temperature", x = "Date Time", y = "Temperature (°F)") +
        theme_minimal() +
        theme(legend.position = "bottom")
    }
  })
  
  

  
  output$audio_file_drilldown_plot <- renderPlotly({
    if (isFALSE(is.null(file_click()) || file_click() == "NA" || identical(file_click(), character(0)))) {
      file_details_plot_data <- file_details_data()
      
      p_file <- ggplot(file_details_plot_data, aes(x = Date.Time + as.numeric(Begin.Time..s.), y = Confidence)) +
        geom_point(aes(color = factor(Common.Name))) +
        labs(title = "File Details", x = "Time", y = "Confidence") +
        # scale_x_time() +
        theme_minimal()
      
      plotly_object <- ggplotly(p_file)
    }
  })
    


  output$audio_file_drilldown_df <- renderUI({
    if (isFALSE(is.null(file_click()) || file_click() == "NA" || identical(file_click(), character(0)))) {
      DT::renderDT(
        datatable(file_details_data(),
                  escape = FALSE,
                  extensions = "FixedHeader",
                  selection = "none",
                  filter = "top",
                  rownames = FALSE,
                  fillContainer = TRUE,
                  options = list(
                    fixedHeader = list(header = TRUE),
                    columnDefs = list(list(visible = FALSE, 
                                           targets = c("Begin.Time..s.", "End.Time..s.", "Species.Code", "Begin.Path", "Location", "Date", 
                                                       "Date.Time", "Month", "Week", "avg_windspeed", 
                                                       "avg_temperature"))),
                    scrollY = 500, 
                    scrollX = TRUE
                  ))
      )}
  })
  
  
  ##################################################
  ## Application Build: Species-Specific Overview ##
  ##################################################
  
  # If a user manually searches for a species, update the species_click() reactive val
  observeEvent(input$species_lookup_species_overview, {
    species_name <- unique(all_data$Species.Code[all_data$Common.Name == input$species_lookup_species_overview])
    species_click(species_name)
    
    # Construct the new URL hash with species query
    newHash <- paste0("#", input$main_nav, "?species=", species_click())
    
    # Update the URL without reloading the app
    updateQueryString(newHash, mode = "push", session = session)
    
  }, ignoreInit = TRUE)
  
  output$species_title_overview <- renderUI({
    if (isTRUE(is.null(species_click()) || species_click() == "NA" || identical(species_click(), character(0)))) {
      tagList(
        p(icon("circle-info"), strong(species_overview_text)),
        div(
          tags$img(src = species_overview_pic,
                   alt = "Northern Saw-whet Owl",
                   # width = "800px",
                   # height = "800px"),
                   style = "max-width: 100%; max-height: 100%;"),
          p("Photo by Marcie O'Connor at ",
            tags$a("Prairie Haven", href = "https://www.prairiehaven.com", target = "_blank"),
            style = "font-size: 12px; color: #666; margin-top: 5px;")
        )
      )
    } else {
      name_title <- unique(subset(filtered_data(), Species.Code==species_click())$Common.Name)
      p(h3(name_title))
    }
  })
  
  output$conditional_species_plots <- renderUI({
    if (isFALSE(is.null(species_click()) || species_click() == "NA" || identical(species_click(), character(0)))) {
      fluidRow(
        lapply(1:6, function(i){
          column(width = 6,
                 conditionalPanel(
                   condition = "input.main_nav == 'species_overview'",
                   plotlyOutput(outputId = paste0("overviewPlot_", i), height = "400px")
                 )
          )
        })
      )
    }
  })
  
  # Function for determining date range
  get_week_date_range <- function(year, week, format = FALSE) {
    # Get the first day of the year
    first_day <- as.Date(paste0(year, "-01-01"))
    
    # Find the first Sunday of the year
    first_sunday <- first_day + (7 - wday(first_day) + 1) %% 7
    
    # Calculate the start date of the target week
    start_date <- first_sunday + (week - 1) * 7
    
    # Calculate the end date of the target week
    end_date <- start_date + 6
    
    # Format the date range
    paste(format(start_date, "%m/%d/%Y"), "-", format(end_date, "%m/%d/%Y"))
    
    if (format == FALSE) {
      drange <- paste(format(start_date, "%m/%d/%Y"), "-", format(end_date, "%m/%d/%Y"))
    }
    
    else if (format == TRUE) {
      drange <- paste0("Week ", week, " (",format(start_date, "%m/%d"), "-", format(end_date, "%m/%d"), ")")
    }
    
    return(drange)
  }
  
  # List of locations
  location_list <- c("House", "Glen", "Prairie", "Wetland", "Savanna", "Forest")
  
  
  ## year_choices()
  for (i in seq_along(location_list)) {
    local({
      loc <- location_list[i]
      output[[paste0("overviewPlot_", i)]] <- renderPlotly({
        
        cols <- viridis(length(year_choices()), direction = 1, option = "plasma")
        names(cols) <- year_choices()
        
        line_size <- round(seq(.3, 1.2, length.out = length(year_choices())), 3)
        names(line_size) <- year_choices()
        
        # Subset by species:
        species_data <- subset(filtered_data(), Species.Code==species_click() & Location == loc)
        
        # Check if species_data has any rows
        validate(
          need(nrow(species_data) > 0, paste("\n No observations available for this species at the", loc, "location."))
        )
        
        # Extract Week, Year, and create Week.Year.Loc variable
        species_data <- species_data %>%
          mutate(
            Date = as.Date(Date),
            Year = year(Date),
            Month = month(Date),
            Month.Year = paste(Month, Year, sep = ","), 
            Month.Year.Loc = paste(Month, Year, Location, sep = ","),
            Week.Year = paste(Week, Year, sep = ","),
            Week.Year.Loc = paste(Week, Year, Location, sep= ",")
          )
        
        # Create the plot
        p <- ggplot(species_data, aes(x = Week
                                      # key = Week.Year.Loc,
                                      # text = paste(
                                      #   "Week:", Week,
                                      #   "<br>Date Range:", get_week_date_range(Year, Week),
                                      #   "<br>Count:", Count)
                                      )) +
          geom_freqpoly(binwidth=1,aes(color = as.factor(Year), linewidth = as.factor(Year))) +
          scale_x_continuous(breaks = 1:52, limits = c(1, 52)) +
          labs(title = loc, x = "Week", y = "Frequency") +
          scale_color_manual(name = "Year", values = cols) +
          scale_linewidth_manual(name = "Year", values = line_size) +
          theme_minimal()
        
        # Convert to an interactive plotly object and register the click event
        plotly_object <- ggplotly(p, dynamicTicks = TRUE
                                  # = "text"
                                  )
        
      })
    })
  }
  
  ############################################################
  ## Application Build: Species-Specific Location Drilldown ##
  ############################################################
  
  # If a user manually searches for a species, update the species_click() reactive val
  observeEvent(input$species_lookup_location, {
    species_name <- unique(all_data$Species.Code[all_data$Common.Name == input$species_lookup_location])
    species_click(species_name)
    
    # Construct the new URL hash with species query
    newHash <- paste0("#", input$main_nav, "?species=", species_click())
    
    # Update the URL without reloading the app
    updateQueryString(newHash, mode = "push", session = session)

  }, ignoreInit = TRUE)
  
  output$species_title <- renderUI({
      if (isTRUE(is.null(species_click()) || species_click() == "NA" || identical(species_click(), character(0)))) {
        tagList(
          p(icon("circle-info"), strong(species_loc_text)),
          div(
            tags$img(src = species_loc_pic,
                     alt = "Ruffed Grouse",
                     style = "max-width: 100%; height: auto;"),
            p("Photo by Marcie O'Connor at ",
              tags$a("Prairie Haven", href = "https://prairiehaven.com", target = "_blank"),
              style = "font-size: 12px; color: #666; margin-top: 5px;")
          )
        )
      } else {
        name_title <- unique(subset(filtered_data(), Species.Code==species_click())$Common.Name)
        div(
          p(h3(name_title)),
          p(),
          p(icon("circle-info"), strong("Select a bar from the plot to view data table."))
        )
      }
  })
  
  output$allaboutbirds <- renderUI({
    req(species_click())
    shiny::a(
      h4("All About Birds",
      class = "btn btn-default action-button shiny-bound-input lightblue",
      style = "width:200px"),
    target = "_blank",
    href = paste0("https://allaboutbirds.org/guide/",
                  species_click(),"/overview")
    )
  })
  
  output$macaulay <- renderUI({
    req(species_click())
    shiny::a(
      h4("Macaulay Library",
         class = "btn btn-default action-button shiny-bound-input lightblue",
         style = "width:200px"),
      target = "_blank",
      href = paste0("https://search.macaulaylibrary.org/catalog?taxonCode=", 
                    species_click(), "&mediaType=audio&sort=rating_rank_desc")
    )
  })
  
  output$conditional_tabset <- renderUI({
    if (isFALSE(is.null(species_click()) || species_click() == "NA" || identical(species_click(), character(0)))) {
      tabsetPanel(
        id = "tab_selection",
        !!!lapply(seq_along(c("House", "Glen", "Prairie", "Wetland", "Savanna", "Forest")), function(i) {
          location <- c("House", "Glen", "Prairie", "Wetland", "Savanna", "Forest")[i]
          tabPanel(
            title = location,
            conditionalPanel(
              condition = paste0("input.main_nav == 'species_loc_drilldown' && input.tab_selection == '", location, "'"),
              plotlyOutput(outputId = paste0("frequencyPlot_", i)) %>% withSpinner()
            ),
            br()
          )
        })
      )
    }
  })
  

  
  for (i in seq_along(location_list)) {
    local({
      loc <- location_list[i]
      output[[paste0("frequencyPlot_", i)]] <- renderPlotly({
        
        cols <- viridis(length(year_choices()), direction = 1, option = "plasma")
        names(cols) <- year_choices()
        
        min_conf <- debounced_conf()[1]
        max_conf <- debounced_conf()[2]
        
        # Subset by species:
        species_data <- subset(filtered_data(), Species.Code==species_click() & Location == loc)
        
        # Check if species_data has any rows
        validate(
          need(nrow(species_data) > 0, "No observations available for this species at this location.")
        )
        
        # Extract Week, Year, and create Week.Year.Loc variable
        species_data <- species_data %>%
          mutate(
            Date = as.Date(Date),
            Year = year(Date),
            Month = month(Date),
            Month.Year = paste(Month, Year, sep = ","), 
            Month.Year.Loc = paste(Month, Year, Location, sep = ","),
            Week.Year = paste(Week, Year, sep = ","),
            Week.Year.Loc = paste(Week, Year, Location, sep= ",")
          )
        
        # Fill in the gaps when we don't have weeks with data
        complete_data <- species_data %>%
          complete(Year = unique(species_data$Year), Week = 1:52, fill = list(Count = 0))
        complete_data <- complete_data %>%
          group_by(Year) %>%
          mutate(YearlyCount = n()) %>%
          ungroup()
        
        complete_data <- subset(complete_data,
                                as.numeric(Confidence) >= min_conf &
                                  as.numeric(Confidence) <= max_conf)
        
        count_data_week <- complete_data %>%
          group_by(Week.Year.Loc) %>%
          summarise(Count = n(), .groups = 'drop')
        
        count_data_month <- complete_data %>%
          group_by(Month.Year.Loc) %>%
          summarise(Count = n(), .groups = 'drop')
        
        complete_data_week <- left_join(complete_data, count_data_week, by = "Week.Year.Loc")
        
        complete_data_month <- left_join(complete_data, count_data_month, by = "Month.Year.Loc")
        
        if (input$time_interval == "weekly") {
          # Create the plot
          p <- ggplot(complete_data_week, aes(x = Week,
                                         key = Week.Year.Loc,
                                         text = paste(
                                           "Week:", Week,
                                           "<br>Date Range:", get_week_date_range(Year, Week),
                                           "<br>Count:", Count)
                                         )) +
            geom_bar(position = "identity", alpha = 0.8, aes(fill = as.factor(Year)), width = 0.9) +
            scale_x_continuous(breaks = c(1, 5, 9, 13, 17, 21, 26, 30, 35, 39, 44, 48), 
                               limits = c(0, 54),
                               labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
            labs(title = loc, x = "Month (Approximate)", y = "Frequency") +
            scale_fill_manual(name = "Year", values = cols) +
            theme_minimal() +
            theme(
              axis.text.x = element_text(angle = 0, hjust = 1)
            )
          
        }
        
        if (input$time_interval == "monthly") {
          # Create the plot
          p <- ggplot(complete_data_month, aes(x = Month,
                                         key = Month.Year.Loc,
                                         text = paste(
                                           "Month:", Month,
                                           "<br>Count:", Count
                                         ))) +
            geom_bar(position = "identity", alpha = 0.8, aes(fill = as.factor(Year)), width = 0.9) +
            scale_x_continuous(breaks = 1:12, limits = c(0, 13)) +
            labs(title = loc, x = "Month", y = "Frequency") +
            scale_fill_manual(name = "Year", values = cols) +
            theme_minimal()
          
        }
        
        # Convert to an interactive plotly object and register the click event
        plotly_object <- ggplotly(p, tooltip = "text")
        event_register(plotly_object, "plotly_click")
        
        # Identify the three most recent years
        recent_years <- tail(sort(unlist(year_choices())), 4)
        
        # Update visibility of traces
        plotly_object$x$data <- lapply(plotly_object$x$data, function(trace) {
          if (!is.null(trace$name) && !(trace$name %in% as.character(recent_years))) {
            trace$visible <- "legendonly"
          }
          return(trace)
        })
        
        plotly_object %>%
          layout(clickmode = "event+select")  
        
      })
    })
  }
  
  # Placeholder for filtered data reactive value
  filtered_data <- reactiveVal(NULL)
  selected_data <- reactiveVal(NULL)
  
  # Create reactive values to store audio and spectrogram
  audio_file_path <- reactiveVal(NULL)
  
  observeEvent(input$main_nav, {
    selected_data(NULL)
    output$filtered_data_ui <- NULL
    
    if (!is.null(audio_file_path())) {
      if (file.exists(audio_file_path())) {
        file.remove(audio_file_path())
      }
    }
    
    if (!is.null(audio_file_path_full())) {
      if (file.exists(audio_file_path_full())) {
        file.remove(audio_file_path_full())
      }
    }
    
    audio_file_path(NULL)
    audio_file_path_full(NULL)
    
    # Clear the plotly selection
    for (i in seq_along(location_list)) {
      session$sendCustomMessage("plotly-clearSelection", paste0("frequencyPlot_", i))
    }
  })

  observeEvent(input$tab_selection, {
    selected_data(NULL)
    output$filtered_data_ui <- NULL
    
    if (!is.null(audio_file_path())) {
      if (file.exists(audio_file_path())) {
        file.remove(audio_file_path())
      }
    }
    
    if (!is.null(audio_file_path_full())) {
      if (file.exists(audio_file_path_full())) {
        file.remove(audio_file_path_full())
      }
    }
    
    audio_file_path(NULL)
    audio_file_path_full(NULL)
    
    # Clear the plotly selection
    for (i in seq_along(location_list)) {
      session$sendCustomMessage("plotly-clearSelection", paste0("frequencyPlot_", i))
    }
  })

  
  # Function to create sound button
  shinyInput <- function(FUN, id, ...) {
      as.character(FUN(paste0(id), ...))
  }
  
  find_audio_file <- function(row) {
    if (is.null(row[["Begin.Path"]]) || row[["Begin.Path"]] == "" || identical(row$Begin.Path, character(0))) {
      return(NULL)
    }
    
    file_name <- basename(row[["Begin.Path"]])
    year_val <- year(row[["Date"]])
    file_loc <- paste0(audio_filepath, "Bio ", as.character(year_val), "/From Recorders")
    audio_loc <- file.path(file_loc, file_name)
    
    if (!file.exists(audio_loc)) {
      return(NULL)
    }
    
    begin_time <- as.numeric(row[["Begin.Time..s."]])
    species_name <- row[["Common.Name"]]
    species_code <- row[["Species.Code"]]
    conf <- row[["Confidence"]]
    loc <- row[["Location"]]
    temp <- paste(round(row[["avg_temperature"]], 3), "°F")
    wind <- paste(round(row[["avg_windspeed"]], 3), "MPH")
    
    # Encode for URL
    species_name <- URLencode(species_name)
    file_name <- URLencode(file_name)
    loc <- URLencode(loc)
    
    url <- paste0(
      tiny_shiny,
      "species_name=", species_name,
      "&species_code=", species_code,
      "&conf=", conf,
      "&loc=", loc,
      "&temp=", temp,
      "&wind=", wind,
      "&file_name=", file_name,
      "&begin_time=", begin_time
    )
    
    return(url)
  }

  # Observe the selected data and filter the original dataframe
  observe({
    req(event_data("plotly_selected"))
    
    # Combine file listings for all species
    selected_data(event_data("plotly_selected"))
    
    # filter data based on the selected bin center
    output$filtered_data <- DT::renderDT({
      
      min_conf <- debounced_conf()[1]
      max_conf <- debounced_conf()[2]
      
      # Subset by species:
      species_data <- subset(filtered_data(), Species.Code==species_click())
      
      # Extract Week, Year, and create Week.Year.Loc variable
      species_data <- species_data %>%
        mutate(
          Date = as.Date(Date),
          Year = year(Date),
          Month = month(Date),
          Month.Year = paste(Month, Year, sep = ","), 
          Month.Year.Loc = paste(Month, Year, Location, sep = ","),
          Week.Year = paste(Week, Year, sep = ","),
          Week.Year.Loc = paste(Week, Year, Location, sep= ",")
        )
      
      # Fill in the gaps when we don't have weeks with data
      complete_data <- species_data %>%
        complete(Year = unique(species_data$Year), Week = 1:52, fill = list(Count = 0))
      complete_data <- complete_data %>%
        group_by(Year) %>%
        mutate(YearlyCount = n()) %>%
        ungroup()
      
      if (!is.null(selected_data())) {
        if (input$time_interval == "weekly") {
          out_df <- subset(complete_data, (Week.Year.Loc %in% selected_data()$key & 
                                             as.numeric(Confidence)>=min_conf & 
                                             as.numeric(Confidence)<= max_conf), select=c("Begin.Time..s.", "End.Time..s.", "Week", 
                                                                                          "Confidence", "Location", "Begin.Path", "Species.Code", 
                                                                                          "Date", "Common.Name", "Obs.Time", "avg_windspeed", "avg_temperature"))
        }
        
        if (input$time_interval == "monthly") {
          out_df <- subset(complete_data, (Month.Year.Loc %in% selected_data()$key & 
                                             as.numeric(Confidence)>=min_conf & 
                                             as.numeric(Confidence)<= max_conf), select=c("Begin.Time..s.", "End.Time..s.", "Week", 
                                                                                          "Confidence", "Location", "Begin.Path", "Species.Code", 
                                                                                          "Date", "Common.Name", "Obs.Time", "avg_windspeed", "avg_temperature"))
        }
        
        # Add action buttons for opening the website
        out_df <- out_df %>%
          rowwise() %>%
          mutate(Sound.Button = list({
            url_val <- find_audio_file(.data)
            if (!is.null(url_val)) {
              paste0(
                '<a class="btn btn-primary" target="_blank" href="',
                find_audio_file(.data),
                '">Open Observation</a>'
              )
            } else {
              "No Audio Available"
            }
          })) %>%
          mutate(across(c("avg_windspeed", "avg_temperature"), ~ round(.x, 3))) %>%
          select("Sound.Button", "Confidence", 
                 "Begin.Time..s.", "Obs.Time", "Date", 
                 "Begin.Path", "Species.Code",
                 "avg_windspeed", "avg_temperature") %>%
          ungroup()
        
        datatable(out_df, 
                  escape = FALSE,
                  extensions = "FixedHeader",
                  selection = "none",
                  filter = "top",
                  options = list(
                    fixedHeader = list(header = TRUE),
                    columnDefs = list(list(visible=FALSE, targets= "Begin.Time..s."))
                  ),
                  colnames = c("Wind Speed (MPH)" = "avg_windspeed", "Temp (F)" = "avg_temperature"))
      } else {
        data.frame()
      }
    })
    
    output$filtered_data_ui <- renderUI({
      req(selected_data())  # only show the table + spinner if there is selected data
      
      if (isFALSE(is.null(species_click()) || species_click() == "NA" || identical(species_click(), character(0)))) {
        div(
          p(),
          p(icon("circle-info"), strong("Select sound button to view observation.")),
          DT::dataTableOutput("filtered_data") %>% withSpinner()
        ) 
      }

      # div(
      #   p(),
      #   p(icon("circle-info"), strong("Select sound button to view observation.")),
      #   DT::dataTableOutput("filtered_data") %>% withSpinner()
      # )
    })
    
  })
  
  
}

shinyApp(ui, server)

