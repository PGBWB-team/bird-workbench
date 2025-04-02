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
library(later)
library(RColorBrewer)
library(shinyjs)

# Reading in all data:
all_data <- fst::read_fst("/Users/laurenwick/Dropbox/Lauren Wick/Plotly App/70conf_2020_to_2024.fst")

# Set root folder for audio files
audio_filepath <- "/Users/laurenwick/Dropbox/Lauren Wick/"

ui <- navbarPage(
  theme = bs_theme(version = 5),
  id = "main_nav",
  title = "BirdNet-Analyzer Data Visualization",
  selected = "ph_overview",
  tabPanel(title = "Prairie Haven Overview",
           value = "ph_overview",
           
           fixedPanel(
             top = 70, left = 30, width = "700px", height = "250px",
             style = "z-index: 1000",
             draggable = TRUE,
             layout_column_wrap(
               width = 1/2,
               card(
                 height = "225px",
                 card_body(
                   tags$div(
                     style = "position: relative;",
                     sliderInput(
                       inputId = "confidence_selection_overview", 
                       label = "BirdNET Happiness Scale",
                       min = 0, 
                       max = 1, 
                       value = c(0.7, 1),
                       step = 0.01
                     ),
                     tags$div(
                       style = "display: flex; justify-content: space-between; padding: 0 12px; margin-top: -10px; font-size: 11px;",
                       tags$span("Not Happy"),
                       tags$span("Okay"),
                       tags$span("Happy")
                     )
                   )
                 )
               )
               ,
               card(
                 selectInput(inputId = "sel_view", 
                             label = "Pivot Table Selection",
                             choices = list("Species by Month" = "by_month",
                                            "Species by Location" = "by_location", 
                                            "Species by Year" = "by_year"),
                             selected = "by_month")
               )
             )
           ),
           
           # sliderInput(
           #   inputId = "confidence_selection_overview", 
           #   label = "Confidence Level", 
           #   min = 0, 
           #   max = 1, 
           #   value = c(0.7, 1)
           # ),
           # 
           # selectInput(inputId = "sel_view", 
           #             label = h3("View Selection"),
           #             choices = list("Species by Month" = "by_month",
           #                            "Species by Location" = "by_location", 
           #                            "Species by Year" = "by_year"),
           #             selected = "by_month"),
           
          uiOutput("overview_view")),
  
  tabPanel(title = "Species-Specific Location Drilldown", 
           value = "species_loc_drilldown",
           
           layout_column_wrap(
             width = 1/2,
             card(
               card_body(
                 sliderInput(
                   inputId = "confidence_selection", 
                   label = "Confidence Level",
                   min = 0, 
                   max = 1, 
                   value = c(0.7, 1)
                 )
               )
               
             ),
             card(
               radioButtons(
                 inputId = "time_interval",
                 label = "Time Interval",
                 choices = c("Week" = "weekly", "Month" = "monthly"), 
                 selected = "weekly"
               )
             )
           ),
             
           uiOutput("species_title"),
           uiOutput("species_link"),
           
           tabsetPanel(
             id = "tab_selection",
             !!!lapply(seq_along(c("House", "Glen", "Prairie", "Wetland", "Savanna", "Forest")), function(i) {
               tabPanel(
                 title = c("House", "Glen", "Prairie", "Wetland", "Savanna", "Forest")[i],
                 plotlyOutput(outputId = paste0("frequencyPlot_", i)),
                 br()
               )
             })
           ),
           
           DT::dataTableOutput("filtered_data"),
           
           br(),
           
           card(
             card_header("Audio Segment Parameters"),
             card_body(
               sliderInput(
                 inputId = "recording_length",
                 label = "Recording Length (seconds)",
                 min = 3,
                 max = 30,
                 value = 10,
                 step = 1,
                 round = TRUE
               ),
               
               sliderInput(
                 inputId = "recording_offset",
                 label = "Recording Offset (seconds)",
                 min = 0,
                 max = 30,
                 value = 3,
                 step = 1,
                 round = TRUE
               )
             )
           ),
           
           uiOutput("audio_player"),
           downloadButton("audioDownload", "Download Audio"),
           plotOutput("spectrogram")),
           
  
  tabPanel(title = "Species-Specific Overview",
           value = "species_overview",
           uiOutput("species_title_overview"),
           fluidRow(
             lapply(1:6, function(i){
               column(width = 6,
                      plotlyOutput(outputId = paste0("overviewPlot_", i), height = "400px"))
             })
           )
  ),
  # Add your custom script for clearing Plotly selection
  tags$script(HTML("
  Shiny.addCustomMessageHandler('plotly-clearSelection', function(plotId) {
    var plot = document.getElementById(plotId);
    if (plot) {
      Plotly.restyle(plot, {selectedpoints: [null]});
    }
  });
"))
  )




server <- function(input, output, session) {
  
  ###############################################
  ## Application Build: Prairie Haven Overview ##
  ###############################################
  # Reactive value to store the selected species code
  species_click <- reactiveVal("acafly") # Default species code

  create_table_pivot <- function(data) {
    datatable(
      data,
      escape = FALSE,
      extensions = "Buttons",
      options = list(
        dom = 'Bfrtip',
        ordering = TRUE,
        buttons = c('csv', 'copy'),
        page_length = nrow(data),
        lengthMenu = list(c(nrow(data)), c("All")),
        columnDefs = list(list(visible=FALSE, targets="Species.Code"))
      ),
      rownames = FALSE,
      selection = list(mode = "single", target = "row")
    )
  }
  
  create_pivot_month <- function(df, yr_input = "All") {
    df <- df %>%
      filter(if (yr_input != "All") lubridate::year(as.Date(Date)) == yr_input else TRUE) %>%
      mutate(Month = factor(month.abb[lubridate::month(as.Date(Date))], levels = month.abb)) %>%
      group_by(Common.Name, Species.Code, Month) %>%
      summarize(Count_by_Species = n(), .groups = "drop") %>%
      pivot_wider(names_from = Month, values_from = Count_by_Species, values_fill = list(Count_by_Species = 0)) %>%
      mutate(Total.Count = rowSums(select(., -c(Common.Name, Species.Code)), na.rm = TRUE))
    # Reorder the columns: Common.Name, Species.Code, then months in order
    month_order <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                     "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    
    # Add missing month columns with 0 values if they don't exist
    for (month in month_order) {
      if (!(month %in% names(df))) {
        df[[month]] <- 0  # Add missing months with 0 values
      }
    }
    
    df <- df %>% select(Common.Name, Species.Code, Total.Count, all_of(month_order))
    return(df)
  }

  create_pivot_year <- function(df, loc_input = "All") {
    df <- df %>%
      filter(if (loc_input != "All") Location == loc_input else TRUE) %>%
      mutate(Date = as.Date(Date), Year = year(Date)) %>%
      group_by(Common.Name, Species.Code, Year) %>%
      summarize(Count_by_Species = n(), .groups = "drop") %>%
      pivot_wider(names_from = Year, values_from = Count_by_Species, values_fill = list(Count_by_Species = 0)) %>%
      mutate(Total.Count = rowSums(select(., -c(Common.Name, Species.Code)), na.rm = TRUE)) %>%
      select(Common.Name, Total.Count, everything())
    return(df)
  }

  create_pivot_location <- function(df, yr_input = "All") {
    df <- df %>%
      filter(if (yr_input != "All") lubridate::year(as.Date(Date)) == yr_input else TRUE) %>%
      group_by(Common.Name, Species.Code, Location) %>%
      summarize(Count_by_Species = n(), .groups = "drop") %>%
      pivot_wider(names_from = Location, values_from = Count_by_Species, values_fill = list(Count_by_Species = 0)) %>%
      mutate(Total.Count = rowSums(select(., -c(Common.Name, Species.Code)), na.rm = TRUE)) %>%
      select(Common.Name, Total.Count, everything())
    return(df)
  }
  
  confidence_filter <- function(data, conf) {
    min_conf <- conf[1]
    max_conf <- conf[2]
    data <- subset(data, as.numeric(Confidence) >= min_conf & as.numeric(Confidence) <= max_conf)
    return(data)
  }
  
  # Predefined values for caching
  year_values <- unique(as.character(lubridate::year(all_data$Date)))
  year_values <- as.list(year_values[order(year_values)])
  names(year_values) <- as.list(year_values)
  year_values <- c(year_values, "All"="All")
  
  loc_values <- unique(all_data$Location)
  loc_values <- as.list(loc_values[order(loc_values)])
  names(loc_values) <- as.list(loc_values)
  loc_values <- c(loc_values, "All" = "All")
  
  default_confidence <- c(0.7, 1) 
  
  # Initialize an environment to store precomputed results
  preload_cache <- new.env()
  
  filtered_data <- reactiveVal(confidence_filter(all_data, default_confidence))
  
  observe({
    req(all_data)
    req(input$confidence_selection_overview)
    new_data <- confidence_filter(all_data, input$confidence_selection_overview)
    filtered_data(new_data)
  })
  
  # Function to preload and store results
  preload_data <- function() {
    
    for (year in year_values) {
      preload_cache[[paste0("species_by_month_", year, "_", default_confidence[1], "_", default_confidence[2])]] <- create_pivot_month(filtered_data(), year)
      preload_cache[[paste0("species_by_location_", year, "_", default_confidence[1], "_", default_confidence[2])]] <- create_pivot_location(filtered_data(), year)
    }
    for (loc in loc_values) {
      preload_cache[[paste0("species_by_year_", loc, "_", default_confidence[1], "_", default_confidence[2])]] <- create_pivot_year(filtered_data(), loc)
    }
  }
  
  # Run preloading at startup
  observe({
    isolate(preload_data())
  })
  
  # Cached reactives with preloaded fallback
  cached_species_by_month <- reactive({
    req(input$year_sel)
    req(input$confidence_selection_overview)
    key <- paste0("species_by_month_", input$year_sel, "_", input$confidence_selection_overview[1], "_", input$confidence_selection_overview[2])
    if (exists(key, preload_cache)) {
      preload_cache[[key]]
    } else {
      create_pivot_month(filtered_data(), input$year_sel)
    }
  }) %>% bindCache(input$confidence_selection_overview, input$year_sel)
  
  cached_species_by_location <- reactive({
    req(input$year_sel)
    req(input$confidence_selection_overview)
    key <- paste0("species_by_location_", input$year_sel, "_", input$confidence_selection_overview[1], "_", input$confidence_selection_overview[2])
    if (exists(key, preload_cache)) {
      preload_cache[[key]]
    } else {
      create_pivot_location(filtered_data(), input$year_sel)
    }
  }) %>% bindCache(input$confidence_selection_overview, input$year_sel)
  
  cached_species_by_year <- reactive({
    req(input$loc_sel)
    req(input$confidence_selection_overview)
    key <- paste0("species_by_year_", input$loc_sel, "_", input$confidence_selection_overview[1], "_", input$confidence_selection_overview[2])
    if (exists(key, preload_cache)) {
      preload_cache[[key]]
    } else {
      create_pivot_year(filtered_data(), input$loc_sel)
    }
  }) %>% bindCache(input$confidence_selection_overview, input$loc_sel)
  
  # Observe Data Tables
  output$species_by_month_pivot <- renderDataTable({ create_table_pivot(cached_species_by_month()) })
  output$species_by_location_pivot <- renderDataTable({ create_table_pivot(cached_species_by_location()) })
  output$species_by_year_pivot <- renderDataTable({ create_table_pivot(cached_species_by_year()) })
  
  # Dynamic UI based on selection
  output$overview_view <- renderUI({
    # Creating valid choices for year filtering
    year_vals <- unique(as.character(lubridate::year(all_data$Date)))
    year_vals <- as.list(year_vals[order(year_vals)])
    names(year_vals) <- as.list(year_vals)
    year_vals <- c(year_vals, "All"="All")
    
    # Create valid choices for year filtering
    loc_vals <- unique(all_data$Location)
    loc_vals <- as.list(loc_vals[order(loc_vals)])
    names(loc_vals) <- as.list(loc_vals)
    loc_vals <- c(loc_vals, "All" = "All")
      
    if (input$sel_view == "by_month") {
      tagList(
        layout_columns(
          card(
            height = "285px",
            card_body(
              selectInput("year_sel", label = "Year Selection", choices = year_vals, selected = "All"))
          ),
          col_widths = c(-8, 4)
        ),
        br(),
        # selectInput("year_sel", label = "Year Selection", choices = year_vals, selected = "All"),
        DT::dataTableOutput("species_by_month_pivot")
      )

    } else if (input$sel_view =="by_location") {
      tagList(
        layout_columns(
          card(
            height = "285px",
            card_body(
              selectInput("year_sel", label = "Year Selection", choices = year_vals, selected = "All"))
          ),
          col_widths = c(-8, 4)
        ),
        br(),
        # selectInput("year_sel", label = "Year Selection", choices = year_vals, selected = "All"),
        DT::dataTableOutput("species_by_location_pivot")
      )
      
    } else if (input$sel_view == "by_year") {
      tagList(
        layout_columns(
          card(
            height = "285px",
            card_body(
              selectInput("loc_sel", label = "Location Selection", choices = loc_vals, selected = "All"))
          ),
          col_widths = c(-8, 4)
        ),
        br(),
        # selectInput("loc_sel", label = "Location Selection", choices = loc_vals, selected = "All"),
        DT::dataTableOutput("species_by_year_pivot")
      )
    }
  }) 
  
  # Observe clicks on the tables
  observeEvent(input$species_by_month_pivot_rows_selected, {
    selected_row <- input$species_by_month_pivot_rows_selected
    if (!is.null(selected_row) && length(selected_row) > 0) {
      species_list <- cached_species_by_month()
      if (selected_row[1] <= nrow(species_list)) {
        species_click(species_list$Species.Code[selected_row[1]])  
        updateNavbarPage(session, "main_nav", selected = "species_loc_drilldown")
      }
    }
  })
  
  observeEvent(input$species_by_location_pivot_rows_selected, {
    selected_row <- input$species_by_location_pivot_rows_selected
    if (!is.null(selected_row) && length(selected_row) > 0) {
      species_list <- cached_species_by_location()
      if (selected_row[1] <= nrow(species_list)) {
        species_click(species_list$Species.Code[selected_row[1]])  
        updateNavbarPage(session, "main_nav", selected = "species_loc_drilldown")
      }
    }
  })
  
  observeEvent(input$species_by_year_pivot_rows_selected, {
    selected_row <- input$species_by_year_pivot_rows_selected
    if (!is.null(selected_row) && length(selected_row) > 0) {
      species_list <- cached_species_by_year()
      if (selected_row[1] <= nrow(species_list)) {
        species_click(species_list$Species.Code[selected_row[1]])  
        updateNavbarPage(session, "main_nav", selected = "species_loc_drilldown")
      }
    }
  })
  
  ##################################################
  ## Application Build: Species-Specific Overview ##
  ##################################################
  output$species_title_overview <- renderUI({
    name_title <- unique(subset(all_data, Species.Code==species_click())$Common.Name)
    print(h3(name_title))
  })
  
  # Function for determining date range
  get_week_date_range <- function(year, week) {
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
  }
  
  get_week_from_date <- function(date) {
    date <- as.Date(date, format = "%Y-%m-%d")
    
    # Extract year from the given date
    year <- as.integer(format(date, "%Y"))
    
    # Get the first day of the year
    first_day <- as.Date(paste0(year, "-01-01"))
    
    # Find the first Sunday of the year
    first_sunday <- first_day + (7 - lubridate::wday(first_day) + 1) %% 7
    
    # Calculate the difference in days between the given date and the first Sunday
    days_since_first_sunday <- as.integer(as.Date(date) - as.Date(first_sunday))
    
    # Determine week number
    week_number <- (days_since_first_sunday %/% 7) +1
    
    return(week_number)
    
  }
  
  # List of locations
  location_list <- c("House", "Glen", "Prairie", "Wetland", "Savanna", "Forest")
  
  # Set colors based on year. You'll want to add more colors with each new year. 
  # cols <- c("2020" = "#F8766D",
  #           "2021" = "#7CAE00",
  #           "2022" = "#00BFC4",
  #           "2023" = "#C77CFF",
  #           "2024" = "#E68613")
  # cols <- brewer.pal(5, "Greens")
  # cols <- brewer.pal(5, "YlGn")
  # cols <- brewer.pal(5, "YlGnBu")
  cols <- brewer.pal(5, "YlOrBr")
  names(cols) <- c("2020", "2021", "2022", "2023", "2024")
  
  for (i in seq_along(location_list)) {
    local({
      loc <- location_list[i]
      output[[paste0("overviewPlot_", i)]] <- renderPlotly({
        
        # Subset by species:
        species_data <- subset(all_data, Species.Code==species_click() & Location == loc)
        
        # Extract Week, Year, and create Week.Year.Loc variable
        species_data <- species_data %>%
          mutate(
            Date = as.Date(Date),
            Year = year(Date),
            Month = month(Date),
            Week = get_week_from_date(Date),
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
      
        
        # Create the plot
        p <- ggplot(species_data, aes(x = Week)) +
          geom_freqpoly(binwidth=1,aes(color = as.factor(Year))) +
          scale_x_continuous(breaks = 1:52, limits = c(1, 52)) +
          labs(title = loc, x = "Week", y = "Frequency") +
          scale_color_manual(name = "Year", values = cols) +
          theme_minimal()
        
        # Convert to an interactive plotly object and register the click event
        plotly_object <- ggplotly(p, dynamicTicks = TRUE)
        
      })
    })
  }
  
  ############################################################
  ## Application Build: Species-Specific Location Drilldown ##
  ############################################################
  
  output$species_title <- renderUI({
    name_title <- unique(subset(all_data, Species.Code==species_click())$Common.Name)
    h3(name_title)
  })
  
  output$species_link <- renderUI({
    req(species_click())  # Ensure species_click() is not NULL
    tags$a(
      href = paste0("https://search.macaulaylibrary.org/catalog?taxonCode=", 
                    species_click(), "&mediaType=audio&sort=rating_rank_desc"),
      target = "_blank",
      h5("[Open Bird Guide]")
    )
  })
  
  for (i in seq_along(location_list)) {
    local({
      loc <- location_list[i]
      output[[paste0("frequencyPlot_", i)]] <- renderPlotly({
        
        min_conf <- input$confidence_selection[1]
        max_conf <- input$confidence_selection[2]
        
        # Subset by species:
        species_data <- subset(all_data, Species.Code==species_click() & Location == loc)
        
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
            Week = get_week_from_date(Date),
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
        plotly_object %>%
          layout(clickmode = "event+select")  
        
      })
    })
  }
  
  # Placeholder for filtered data reactive value
  filtered_data <- reactiveVal(NULL)
  selected_data <- reactiveVal(NULL)
  
  # Create reactive values to store audio and spectrogram
  audio_player <- reactiveVal(NULL)
  spectrogram <- reactiveVal(NULL)
  audio_file_path <- reactiveVal(NULL)

  observeEvent(input$tab_selection, {
    selected_data(NULL)
    audio_player(NULL)
    spectrogram(NULL)
    audio_player(NULL)
    output$audio_player <- NULL
    
    if (!is.null(audio_file_path())) {
      if (file.exists(audio_file_path())) {
        file.remove(audio_file_path())
      }
    }
    audio_file_path(NULL)
    
    # Clear the plotly selection
    for (i in seq_along(location_list)) {
      session$sendCustomMessage("plotly-clearSelection", paste0("frequencyPlot_", i))
    }
  })

  
  # Function to create sound button
  shinyInput <- function(FUN, id, ...) {
      as.character(FUN(paste0(id), ...))
  }
  

  # Observe the selected data and filter the original dataframe
  observe({
    req(event_data("plotly_selected"))
    
    # Combine file listings for all species
    selected_data(event_data("plotly_selected"))
    
    find_audio_file <- function(row) {
      if (is.null(row[["Begin.Path"]]) || row[["Begin.Path"]] == "") {
        return(NULL)
      }
      
      file_name <- basename(row[["Begin.Path"]])
      year <- year(row[["Date"]])
      file_loc <- paste0(audio_filepath, "Bio ", as.character(year), "/From Recorders")
      
      if (is.null(row[["Begin.Time..s."]])) {
        return(NULL)
      }
      
      start_time <- as.numeric(row[["Begin.Time..s."]])
      
      
      audio_loc <- file.path(file_loc, file_name)
      
      if (is.null(audio_loc) || audio_loc == "" || !file.exists(audio_loc)) {
        return(NULL)
      }
      
      return(paste0(audio_loc, "***", start_time))
    }
    
    
    # filter data based on the selected bin center
    output$filtered_data <- DT::renderDataTable({
      
      min_conf <- input$confidence_selection[1]
      max_conf <- input$confidence_selection[2]
      
      # Subset by species:
      species_data <- subset(all_data, Species.Code==species_click())
      
      # Extract Week, Year, and create Week.Year.Loc variable
      species_data <- species_data %>%
        mutate(
          Date = as.Date(Date),
          Year = year(Date),
          Month = month(Date),
          Week = get_week_from_date(Date),
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
                                             as.numeric(Confidence)<= max_conf), select=c("Begin.Time..s.", "End.Time..s.", "Week", "Confidence", "Location", "Begin.Path", "Species.Code", "Date"))
        }
        
        if (input$time_interval == "monthly") {
          out_df <- subset(complete_data, (Month.Year.Loc %in% selected_data()$key & 
                                             as.numeric(Confidence)>=min_conf & 
                                             as.numeric(Confidence)<= max_conf), select=c("Begin.Time..s.", "End.Time..s.", "Week", "Confidence", "Location", "Begin.Path", "Species.Code", "Date"))
        }
        
        # Add action buttons for opening the website
        out_df <- out_df %>%
          rowwise() %>%
          mutate(
            Sound.Button = list({
              audio_id <- find_audio_file(.data)

              if (!is.null(audio_id)) {
                shinyInput(
                  FUN = actionButton,
                  id = audio_id,
                  label = "Audio", 
                  onclick = 'Shiny.setInputValue(\"play_button\", this.id, {priority: \"event\"})'
                )
              }
              else {
                "No Audio File"
              }
            })
            
          ) %>%
          select("Sound.Button", "Confidence", 
                 "Begin.Time..s.", "Date", 
                 "Begin.Path", "Species.Code") %>%
          ungroup()
        
        datatable(out_df, 
                  escape = FALSE,
                  selection = "none")
      } else {
        data.frame()
      }
    })
  })
  
  selected_audio <- reactiveValues(file = NULL, begin_time = NULL)
  
  observeEvent(input$play_button, {
    selectedRow <- input$play_button
    audio_file_path_raw <- unlist(strsplit(selectedRow, "***", fixed=TRUE))[[1]]
    begin_time <- as.numeric(unlist(strsplit(selectedRow, "***", fixed=TRUE))[[2]])
    
    selected_audio$file <- audio_file_path_raw
    selected_audio$begin_time <- begin_time
    
    update_audio_and_spectrogram()
  })
  
  observeEvent(c(input$recording_length, input$recording_offset), {
    if (!is.null(selected_audio$file)) {
      update_audio_and_spectrogram()
    }
  })
  
  update_audio_and_spectrogram <- reactive({
    req(selected_audio$file)
    
    recording_secs <- input$recording_length
    recording_offset <- input$recording_offset
    
    if (!dir.exists("www")) {
      dir.create("www")
    }
    
    audio_src <- paste0(sub("\\.wav$", "", basename(selected_audio$file)), "_", selected_audio$begin_time, "s.wav")
    dest_path <- file.path("www", audio_src)
    
    output_wav <- av_audio_convert(audio = selected_audio$file, 
                                   output = dest_path,
                                   start_time = selected_audio$begin_time - as.numeric(recording_offset),
                                   total_time = as.numeric(recording_secs))
    
    audio_file_path(dest_path)
    
    # Force UI refresh by generating a new audio tag
    output$audio_player <- renderUI({
      tags$audio(src = audio_src, type = "audio/wav", controls = TRUE, autoplay = TRUE)
    })
    
    audio_wav <- tuneR::readWave(output_wav)
    
    v <- seewave::ggspectro(audio_wav, ovlp = 50) +
      geom_tile(aes(fill = amplitude)) +
      ylim(0, 12) +
      scale_fill_gradientn(colours = viridis(256, option = "B"),
                           limits = c(-90, 0))
    
    spectrogram(v)
  })
  
  output$audioDownload <- downloadHandler(
    filename = function() {
      req(audio_file_path())
      paste(basename(audio_file_path()))
    },
    content = function(file) {
      req(audio_file_path())
      file.copy(audio_file_path(), file)
    },
    contentType = "audio/wav"
  )
  
  # Show the name of the employee that has been clicked on
  output$audio_player <- renderUI({
    req(audio_player())
    audio_player()
  })
  
  output$spectrogram <- renderPlot({
    req(spectrogram())
    spectrogram()
  })
  
  
}

shinyApp(ui, server)

