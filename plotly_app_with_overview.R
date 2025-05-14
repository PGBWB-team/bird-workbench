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

# Reading in all data:
# Second line defines specific columns we want to read, remove unwanted columns to improve loading time
all_data <- fst::read_fst("/Users/laure/Dropbox/Lauren Wick/Plotly App/70conf_2020_to_2024.fst",
                          columns = c("Begin.Time..s.", "End.Time..s.", "Common.Name", "Species.Code", "Confidence", 
                                      "Begin.Path", "Location", "Date", "Date.Time"))

# Set root folder for audio files
audio_filepath <- "/Users/laure/Dropbox/Lauren Wick/"

ui <- navbarPage(
  theme = bs_theme(version = 5),
  id = "main_nav",
  title = "BirdNet-Analyzer Data Visualization",
  selected = "ph_overview",
  
  # Shared sliderInput for confidence
    card(
      height = "185px",
      card_body(
        tags$div(
          style = "position: relative; width: 86%; margin: 0 auto;",  # align block
          sliderInput(
            inputId = "confidence_selection_overview", 
            label = "BirdNET Happiness Scale",
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
      )
  ),
  
  tabPanel(title = "Prairie Haven Overview", value = "ph_overview", 
           selectInput(inputId = "sel_view", 
                       label = "Pivot Table Selection",
                       choices = list("Species by Month" = "by_month",
                                      "Species by Location" = "by_location", 
                                      "Species by Year" = "by_year"),
                       selected = "by_month"),
           uiOutput("overview_view") %>% withSpinner()
  ),
  
  tabPanel(title = "Species-Specific Location Drilldown", 
           value = "species_loc_drilldown",
    
             card(
               radioButtons(
                 inputId = "time_interval",
                 label = "Time Interval",
                 choices = c("Week" = "weekly", "Month" = "monthly"), 
                 selected = "weekly"
               )
             ),
             
           uiOutput("species_title"),
           uiOutput("species_link"),
           
           tabsetPanel(
             id = "tab_selection",
             !!!lapply(seq_along(c("House", "Glen", "Prairie", "Wetland", "Savanna", "Forest")), function(i) {
               tabPanel(
                 title = c("House", "Glen", "Prairie", "Wetland", "Savanna", "Forest")[i],
                 plotlyOutput(outputId = paste0("frequencyPlot_", i)) %>% withSpinner(),
                 br()
               )
             })
           ),
           
           uiOutput("filtered_data_ui"),
           
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
           
           uiOutput("audio_player") %>% withSpinner(),
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
  
  tabPanel(title = "File Analysis",
           value = "audio_file_overview",
           uiOutput("audio_player_full"),
           uiOutput("audio_file_pivot_view")
  ),
  
  # Add your custom script for clearing Plotly selection
  tags$script(HTML("
  Shiny.addCustomMessageHandler('plotly-clearSelection', function(plotId) {
    var plot = document.getElementById(plotId);
    if (plot) {
      Plotly.restyle(plot, {selectedpoints: [null]});
    }
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
    
  "))
  )
  
  
  )




server <- function(input, output, session) {
  
  ###############################################
  ## Application Build: Prairie Haven Overview ##
  ###############################################
  # Reactive value to store the selected species code
  species_click <- reactiveVal("acafly") # Default species code

  create_table_pivot <- function(data) {
    # Add a helper column with row index for DT styling
    data$row_index <- seq_len(nrow(data))
    
    datatable(
      data,
      escape = FALSE,
      extensions = c("Buttons", "FixedHeader"),
      options = list(
        dom = 'Bfrtip',
        ordering = TRUE,
        buttons = c('csv', 'copy'),
        page_length = nrow(data),
        lengthMenu = list(c(nrow(data)), c("All")),
        columnDefs = list(list(visible=FALSE, targets= c("Species.Code", "row_index"))),
        fixedHeader = list(header = TRUE)
      ),
      rownames = FALSE,
      selection = list(mode = "single", target = "row")
    ) %>%
      formatStyle(
        "row_index",
        target = "row",
        fontStyle = styleEqual(1, "italic")
      )
  }
  
  create_pivot_month <- function(df, yr_input = "All", loc_input = "All") {
    df <- df %>%
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
    
    df <- df %>% select(Common.Name, Species.Code, Total.Count, all_of(month_order))
    df <- rbind(df[nrow(df),], df[1:(nrow(df)-1),])
    return(df)
  }

  create_pivot_year <- function(df, loc_input = "All", month_sel) {
    df <- df %>%
      filter(if (loc_input != "All") Location == loc_input else TRUE) %>%
      filter(lubridate::month(as.Date(Date), label = TRUE) %in% month_sel) %>%
      mutate(Date = as.Date(Date), Year = year(Date)) %>%
      group_by(Common.Name, Species.Code, Year) %>%
      summarize(Count_by_Species = n(), .groups = "drop")
    
    # Capture and sort year columns before pivoting
    year_cols <- sort(unique(df$Year))
    
    df <- df %>%
      pivot_wider(names_from = Year, values_from = Count_by_Species, values_fill = list(Count_by_Species = 0))
    
    df <- df %>%
      mutate(Total.Count = rowSums(select(., all_of(as.character(year_cols))), na.rm = TRUE)) %>%
      select(Common.Name, Species.Code, Total.Count, all_of(as.character(year_cols)))
    
    # Add summary row at top
    num_birds <- nrow(df[df$Common.Name != "nocall",])
    summary_row <- df %>%
      summarise(
        Common.Name = paste(num_birds, "Unique Species"),
        Species.Code = "",
        Total.Count = sum(df$Total.Count),
        across(all_of(as.character(year_cols)), sum)
      )
    
    df <- bind_rows(summary_row, df)
    
    return(df)
  }


  create_pivot_location <- function(df, yr_input = "All", month_sel) {
    df <- df %>%
      filter(if (yr_input != "All") lubridate::year(as.Date(Date)) == yr_input else TRUE) %>% 
      filter(lubridate::month(as.Date(Date), label = TRUE) %in% month_sel) %>%
      # filter(as.Date(Date) >= date_input[1] & as.Date(Date) <= date_input[2]) %>%
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
  
  # Confidence default
  default_confidence <- c(0.7, 1)
  
  # Reactive filtered data
  filtered_data <- reactiveVal(confidence_filter(all_data, default_confidence))

  observe({
    req(all_data)
    req(input$confidence_selection_overview)
    new_data <- confidence_filter(all_data, input$confidence_selection_overview)
    filtered_data(new_data)
  })
  
  
  # Reactives that generate pivots on demand
  species_by_month <- reactive({
    req(input$year_sel)
    req(input$loc_sel)
    req(filtered_data())
    create_pivot_month(filtered_data(), input$year_sel, input$loc_sel)
  }) %>% bindCache(input$year_sel, input$loc_sel, input$confidence_selection_overview)
  
  debounced_month_sel <- debounce(reactive(input$month_sel), 500)
  
  species_by_location <- reactive({
    req(input$year_sel)
    req(debounced_month_sel())
    req(filtered_data())
    create_pivot_location(filtered_data(), input$year_sel, debounced_month_sel())
  }) %>% bindCache(input$year_sel, debounced_month_sel(), input$confidence_selection_overview)
  
  species_by_year <- reactive({
    req(input$loc_sel)
    req(debounced_month_sel())
    req(filtered_data())
    create_pivot_year(filtered_data(), input$loc_sel, debounced_month_sel())
  }) %>% bindCache(input$loc_sel, debounced_month_sel(), input$confidence_selection_overview)
  
  # Table rendering
  output$species_by_month_pivot <- renderDataTable({
    create_table_pivot(species_by_month())
  })
  
  output$species_by_location_pivot <- renderDataTable({
    create_table_pivot(species_by_location())
  })
  
  output$species_by_year_pivot <- renderDataTable({
    create_table_pivot(species_by_year())
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
        selectInput("loc_sel", label = "Location Selection", choices = loc_choices(), selected = "All"),
        DT::dataTableOutput("species_by_month_pivot") %>% withSpinner()
      )

    } else if (input$sel_view =="by_location") {
      tagList(
        selectInput("year_sel", label = "Year Selection", choices = year_choices(), selected = "All"),
        selectInput("month_sel", label = "Month Selection", choices = month_choices(), selected = month_choices(),
                    multiple = TRUE),
        DT::dataTableOutput("species_by_location_pivot") %>% withSpinner()
      )
      
    } else if (input$sel_view == "by_year") {
      tagList(
        selectInput("loc_sel", label = "Location Selection", choices = loc_choices(), selected = "All"),
        selectInput("month_sel", label = "Month Selection", choices = month_choices(), selected = month_choices(),
                    multiple = TRUE),
        DT::dataTableOutput("species_by_year_pivot") %>% withSpinner()
      )
    }
  }) 
  
  # Observe clicks on the tables
  observeEvent(input$species_by_month_pivot_rows_selected, {
    selected_row <- input$species_by_month_pivot_rows_selected
    if (!is.null(selected_row) && length(selected_row) > 0) {
      species_list <- species_by_month()
      if (selected_row[1] <= nrow(species_list)) {
        species_click(species_list$Species.Code[selected_row[1]])  
        updateNavbarPage(session, "main_nav", selected = "species_loc_drilldown")
      }
    }
  })
  
  observeEvent(input$species_by_location_pivot_rows_selected, {
    selected_row <- input$species_by_location_pivot_rows_selected
    if (!is.null(selected_row) && length(selected_row) > 0) {
      species_list <- species_by_location()
      if (selected_row[1] <= nrow(species_list)) {
        species_click(species_list$Species.Code[selected_row[1]])  
        updateNavbarPage(session, "main_nav", selected = "species_loc_drilldown")
      }
    }
  })
  
  observeEvent(input$species_by_year_pivot_rows_selected, {
    selected_row <- input$species_by_year_pivot_rows_selected
    if (!is.null(selected_row) && length(selected_row) > 0) {
      species_list <- species_by_year()
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
  
  # Filtered data based on exclusion list
  cached_filtered_data <- reactiveVal(NULL)
  
  observeEvent({
    filtered_data()
    debounced_species_exclude()
  }, {
    req(filtered_data(), debounced_species_exclude())
    filtered <- filtered_data()[!(filtered_data()$Common.Name %in% debounced_species_exclude()),]
    cached_filtered_data(filtered)
  })
  
  # Optimized pivot creator
  create_pivot_files <- function(df) {
    
    # First, get the top 3 species per file group
    top_species_df <- df %>%
      group_by(Begin.Path, Common.Name) %>%
      summarise(Species.Count = n(), .groups = "drop") %>%
      arrange(Begin.Path, desc(Species.Count)) %>%
      group_by(Begin.Path) %>%
      slice_head(n = 3) %>%
      mutate(Rank = paste0("Top.Species.", row_number())) %>%
      select(Begin.Path, Rank, Common.Name) %>%
      pivot_wider(
        names_from = Rank,
        values_from = Common.Name,
        values_fill = NA
      )
    
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
      group_by(Begin.Path, Location, Date, Date.Time) %>%
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
      mutate(Year = as.character(year(Date))) %>%
      mutate(Month = as.character(month(Date, label = TRUE, abbr = FALSE))) %>%
      left_join(top_species_df, by = "Begin.Path") %>%
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
      }) %>%
      select(Play.Audio, Begin.Path, Number.Observations, Number.Unique.Species, 
             Location, Date, Year, Month, Time, Time.Of.Day, 
             Mean.Confidence, Median.Confidence, SD.Confidence,
             Top.Species.1, Top.Species.2, Top.Species.3)
      # left_join(df_top_birds, by = "Begin.Path")
    
    return(df_all_cols)
  }
  
  # Final reactive for display data 
  file_list <- reactive({
    req(cached_filtered_data())
    create_pivot_files(cached_filtered_data())
  })
  
  # Render the pivot table
  output$file_list_pivot <- renderDataTable(
    # server = FALSE, 
    {
    datatable(file_list(),
              escape = FALSE,
              extensions = "Buttons",
              filter = "top",
              options = list(
                dom = "Bfrtip",
                ordering = TRUE,
                buttons = c("csv", "copy"),
                pageLength = 100
              ),
              rownames = FALSE,
              colnames = c("# Obs" = "Number.Observations", "# Unique Species" = "Number.Unique.Species",
                           "Time of Day" = "Time.Of.Day", "File Name" = "Begin.Path",
                           "Mean Conf" = "Mean.Confidence", "Median Conf" = "Median.Confidence", "SD Conf" = "SD.Confidence"),
              selection = "single")
      
    }) 
  
  
  # UI for species exclusion and table display 
  output$audio_file_pivot_view <- renderUI({
    tagList(
      selectizeInput(inputId = "species_exclude",
                     label = "Species to Exclude",
                     choices = species_values,
                     selected = default_species_vals,
                     multiple = TRUE,
                     width = "400px"),
      DT::dataTableOutput("file_list_pivot") %>% withSpinner()
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
                                   output = dest_path)
    
    audio_file_path_full(dest_path)
    
    # Force UI refresh by generating a new audio tag
    output$audio_player_full <- renderUI({
      tags$audio(src = audio_src, type = "audio/wav", controlsList="nodownload", controls = NA, autoplay = TRUE)
    })
  })
  
  
  ##################################################
  ## Application Build: Species-Specific Overview ##
  ##################################################
  output$species_title_overview <- renderUI({
    name_title <- unique(subset(filtered_data(), Species.Code==species_click())$Common.Name)
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
  
  
  ## year_choices()
  for (i in seq_along(location_list)) {
    local({
      loc <- location_list[i]
      output[[paste0("overviewPlot_", i)]] <- renderPlotly({
        
        cols <- viridis(length(year_choices()), direction = 1, option = "plasma")
        names(cols) <- year_choices()
        
        line_size <- round(seq(.3, 1, length.out = length(year_choices())), 3)
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
          geom_freqpoly(binwidth=1,aes(color = as.factor(Year), size = as.factor(Year))) +
          scale_x_continuous(breaks = 1:52, limits = c(1, 52)) +
          labs(title = loc, x = "Week", y = "Frequency") +
          scale_color_manual(name = "Year", values = cols) +
          scale_size_manual(name = "Year", values = line_size) +
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
    name_title <- unique(subset(filtered_data(), Species.Code==species_click())$Common.Name)
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
        
        cols <- viridis(length(year_choices()), direction = 1, option = "plasma")
        names(cols) <- year_choices()
        
        min_conf <- input$confidence_selection_overview[1]
        max_conf <- input$confidence_selection_overview[2]
        
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
  audio_player <- reactiveVal(NULL)
  spectrogram <- reactiveVal(NULL)
  audio_file_path <- reactiveVal(NULL)

  observeEvent(input$tab_selection, {
    selected_data(NULL)
    audio_player(NULL)
    spectrogram(NULL)
    output$audio_player <- NULL
    
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
      
      min_conf <- input$confidence_selection_overview[1]
      max_conf <- input$confidence_selection_overview[2]
      
      # Subset by species:
      species_data <- subset(filtered_data(), Species.Code==species_click())
      
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
    
    output$filtered_data_ui <- renderUI({
      req(selected_data())  # only show the table + spinner if there is selected data
      
      withSpinner(
        DT::dataTableOutput("filtered_data")
      )
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
    
    if (!is.null(audio_file_path())) {
      if (file.exists(audio_file_path())) {
        file.remove(audio_file_path())
      }
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

