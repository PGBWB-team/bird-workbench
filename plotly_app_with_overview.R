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

# Reading in all data:
all_data <- fst::read_fst("/Users/laurenwick/Dropbox/Lauren Wick/Plotly App/70conf_2020_to_2024.fst")

ui <- navbarPage(
  id = "main_nav",
  title = "BirdNet-Analyzer Data Visualization",
  selected = "ph_overview",
  tabPanel(title = "Prairie Haven Overview",
           value = "ph_overview",
           
           sliderInput(
             inputId = "confidence_selection_overview", 
             label = "Confidence Level", 
             min = 0, 
             max = 1, 
             value = c(0.7, 1)
           ),
           
           selectInput(inputId = "sel_view", 
                       label = h3("View Selection"),
                       choices = list("Species Totals" = "totals", "Species by Month" = "by_month",
                                      "Species by Location" = "by_location", "Species by Year" = "by_year"),
                       selected = "totals"),
          uiOutput("overview_view")),
  
  tabPanel(title = "Species-Specific Location Drilldown", 
           value = "species_loc_drilldown",
           
           sidebarLayout(
             
             sidebarPanel(
               radioButtons(
                 inputId = "time_interval",
                 label = "Time Interval", 
                 choices = c("Week" = "weekly", "Month" = "monthly"), 
                 selected = "weekly"
               ),
               
               sliderInput(
                 inputId = "confidence_selection", 
                 label = "Confidence Level", 
                 min = 0, 
                 max = 1, 
                 value = c(0.7, 1)
               ),
               
               sliderInput(
                 inputId = "recording_length",
                 label = "Recording Length (seconds)",
                 min = 3,
                 max = 30,
                 value = 10,
                 step = 1,
                 round = TRUE
               )
             ),
             
             mainPanel(
               uiOutput("species_title"),
               tabsetPanel(
                 id = "tab_selection",
                 !!!lapply(seq_along(c("House", "Glen", "Prairie", "Wetland", "Savanna", "Forest")), function(i) {
                   tabPanel(
                     title = c("House", "Glen", "Prairie", "Wetland", "Savanna", "Forest")[i],
                     plotlyOutput(outputId = paste0("frequencyPlot_", i))
                   )
                 })
               ),
               
               DT::dataTableOutput("filtered_data"),
               uiOutput("audio_player"),
               plotOutput("spectrogram")
               
             )
           )
     
  ),
  
  tabPanel(title = "Species-Specific Overview",
           value = "species_overview",
           uiOutput("species_title_overview"),
           fluidRow(
             lapply(1:6, function(i){
               column(width = 6,
                      plotlyOutput(outputId = paste0("overviewPlot_", i), height = "400px"))
             })
           )
  )
)


server <- function(input, output, session) {
  
  ###############################################
  ## Application Build: Prairie Haven Overview ##
  ###############################################
  # Reactive value to store the selected species code
  species_click <- reactiveVal("acafly") # Default species code
  
  # Function to create table:
  create_table <- function(data) {
    datatable(
      data.frame(Display = data$Display),
      escape = FALSE,
      extensions = "Buttons",
      options = list(
        dom = 'Bfrtip',
        ordering = FALSE,
        page_length = nrow(data),
        buttons = c('csv', 'copy'),
        lengthMenu = list(c(nrow(data)), c("All")) # Allow selecting "All"
      ),
      rownames = FALSE,
      colnames = NULL,
      # selection = "single"
      selection = list(mode = "single", target = "cell")
    )
  }

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
      selection = list(mode = "single", target = "cell")
    )
  }
  
  create_pivot_month <- function(df, yr_input="All") {
    if (yr_input == "All") {
      out_df <- df %>%
        mutate(Date = as.Date(Date), Month = month(Date)) %>%
        group_by(Common.Name, Species.Code, Month) %>%
        summarize(Count_by_Species = n()) %>%
        pivot_wider(names_from = Month, values_from = Count_by_Species, values_fill = list(Count_by_Species = 0)) %>%
        select(Common.Name, Species.Code, as.character(1:12)) %>%
        rename_with( ~ month.name[as.numeric(.)], -c(Common.Name, Species.Code))
      return(out_df)
    } else {
      out_df <- df %>%
        filter(lubridate::year(as.Date(Date)) == yr_input) %>%
        mutate(Date = as.Date(Date), Month = month(Date)) %>%
        group_by(Common.Name, Species.Code, Month) %>%
        summarize(Count_by_Species = n()) %>%
        pivot_wider(names_from = Month, values_from = Count_by_Species, values_fill = list(Count_by_Species = 0)) %>%
        select(Common.Name, Species.Code, as.character(1:12)) %>%
        rename_with( ~ month.name[as.numeric(.)], -c(Common.Name, Species.Code))
      return(out_df)
    }
  }

  create_pivot_year <- function(df, loc_input = "All") {
    if (loc_input == "All") {
      out_df <- df %>%
        mutate(Date = as.Date(Date), Year = year(Date))

       yrs <- unique(out_df$Year)

       out_df <- out_df %>%
        group_by(Common.Name, Species.Code, Year) %>%
        summarize(Count_by_Species = n()) %>%
        pivot_wider(names_from = Year, values_from = Count_by_Species, values_fill = list(Count_by_Species = 0)) %>%
        select(Common.Name, Species.Code, as.character(yrs))

       return(out_df)
    } else {
      out_df <- df %>%
        filter(Location == loc_input) %>%
        mutate(Date = as.Date(Date), Year = year(Date))

      yrs <- unique(out_df$Year)

      out_df <- out_df %>%
        group_by(Common.Name, Species.Code, Year) %>%
        summarize(Count_by_Species = n()) %>%
        pivot_wider(names_from = Year, values_from = Count_by_Species, values_fill = list(Count_by_Species = 0)) %>%
        select(Common.Name, Species.Code, as.character(yrs))

      return(out_df)
    }
  }

  create_pivot_location <- function(df, yr_input = "All") {

    if (yr_input == "All") {
      locs <- unique(df$Location)

      out_df <- df %>%
        group_by(Common.Name, Species.Code, Location) %>%
        summarize(Count_by_Species = n()) %>%
        pivot_wider(names_from = Location, values_from = Count_by_Species, values_fill = list(Count_by_Species = 0)) %>%
        select(Common.Name, Species.Code, locs)

      return(out_df)
    } else {
      out_df <- df %>%
        filter(lubridate::year(as.Date(Date)) == yr_input)

      locs <- unique(out_df$Location)

      out_df <- out_df %>%
        group_by(Common.Name, Species.Code, Location) %>%
        summarize(Count_by_Species = n()) %>%
        pivot_wider(names_from = Location, values_from = Count_by_Species, values_fill = list(Count_by_Species = 0)) %>%
        select(Common.Name, Species.Code, locs)

      return(out_df)
    }
  }
  
  confidence_filter <- function(data, conf) {
    min_conf <- conf[1]
    max_conf <- conf[2]
    
    data <- subset(data,
                   as.numeric(Confidence) >= min_conf &
                     as.numeric(Confidence) <= max_conf)
    
    return(data)
  }
  
  # Reactive expression for species_by_month based on year selection
  total_species <- reactive({
    req(input$confidence_selection_overview)
    all_data <- confidence_filter(all_data, input$confidence_selection_overview)
    
    # New df of total count by species:
    count_by_species <- all_data %>% count(Common.Name, Species.Code, name = "Count")
  })
  
  species_by_month <- reactive({
    req(input$year_sel)  # Ensure input exists before proceeding
    req(input$confidence_selection_overview)
    all_data <- confidence_filter(all_data, input$confidence_selection_overview)
    create_pivot_month(all_data, input$year_sel)
  })

  species_by_location <- reactive({
    req(input$year_sel) # Ensure input exists before proceeding
    req(input$confidence_selection_overview)
    all_data <- confidence_filter(all_data, input$confidence_selection_overview)
    create_pivot_location(all_data, input$year_sel)
  })

  species_by_year <- reactive({
    req(input$loc_sel)
    req(input$confidence_selection_overview)
    all_data <- confidence_filter(all_data, input$confidence_selection_overview)
    create_pivot_year(all_data, input$loc_sel)
  })

  # Observe Data Tables
  observe({
    output$species_by_month_pivot <- renderDataTable({ create_table_pivot(species_by_month()) })
  })

  observe({
    output$species_by_location_pivot <- renderDataTable({ create_table_pivot(species_by_location()) })
  })

  observe({
    output$species_by_year_pivot <- renderDataTable({ create_table_pivot(species_by_year())})
  })

  observe({
    output$species_counts_t1 <- renderDataTable({ create_table_pivot(total_species()) })
  })

  # Dynamic UI based on selection
  output$overview_view <- renderUI({
    if (input$sel_view == "totals") {

      DT::dataTableOutput("species_counts_t1")

    } else if (input$sel_view == "by_month") {

      # Creating valid choices for year filtering
      year_vals <- unique(as.character(lubridate::year(all_data$Date)))
      year_vals <- as.list(year_vals[order(year_vals)])
      names(year_vals) <- as.list(year_vals)
      year_vals <- c(year_vals, "All"="All")

      tagList(
        selectInput("year_sel",
                    label = "Year Selection",
                    choices = year_vals,
                    selected = "All"),
        DT::dataTableOutput("species_by_month_pivot")
      )

    } else if (input$sel_view =="by_location") {

      # Creating valid choices for year filtering
      year_vals <- unique(as.character(lubridate::year(all_data$Date)))
      year_vals <- as.list(year_vals[order(year_vals)])
      names(year_vals) <- as.list(year_vals)
      year_vals <- c(year_vals, "All" = "All")

      tagList(
        selectInput("year_sel",
                    label = "Year Selection",
                    choices = year_vals,
                    selected = "All"),
        DT::dataTableOutput("species_by_location_pivot")
      )
    } else if (input$sel_view == "by_year") {

      # Create valid choices for year filtering
      loc_vals <- unique(all_data$Location)
      loc_vals <- as.list(loc_vals[order(loc_vals)])
      names(loc_vals) <- as.list(loc_vals)
      loc_vals <- c(loc_vals, "All" = "All")

      tagList(
        selectInput("loc_sel",
                    label = "Location Selection",
                    choices = loc_vals,
                    selected = "All"),
        DT::dataTableOutput("species_by_year_pivot")
      )
    }
  })

  # Observe clicks on the tables
  observeEvent(input$species_by_month_pivot_cells_selected, {
    selected_row <- input$species_by_month_pivot_cells_selected
    if (nrow(selected_row) > 0) {
      new_species <- species_by_month()$Species.Code[selected_row]
      species_click(new_species) # Update the reactive value
      updateNavbarPage(session, "main_nav", selected = "species_loc_drilldown")
    }
  })

  observeEvent(input$species_by_location_pivot_cells_selected, {
    selected_row <- input$species_by_location_pivot_cells_selected
    if (nrow(selected_row) > 0) {
      new_species <- species_by_location()$Species.Code[selected_row]
      species_click(new_species) # Update the reactive value
      updateNavbarPage(session, "main_nav", selected = "species_loc_drilldown")
    }
  })

  observeEvent(input$species_by_year_pivot_cells_selected, {
    selected_row <- input$species_by_year_pivot_cells_selected
    if (nrow(selected_row) > 0) {
      new_species <- species_by_year()$Species.Code[selected_row]
      species_click(new_species) # Update the reactive value
      updateNavbarPage(session, "main_nav", selected = "species_loc_drilldown")
    }
  })

  observeEvent(input$species_counts_t1_cells_selected, {
    selected_row <- input$species_counts_t1_cells_selected
    if (nrow(selected_row) > 0) {
      new_species <- total_species()$Species.Code[selected_row]
      species_click(new_species) # Update the reactive value
      updateNavbarPage(session, "main_nav", selected = "species_loc_drilldown")
    }
  })
  
  ##################################################
  ## Application Build: Species-Specific Overview ##
  ##################################################
  output$species_title_overview <- renderUI({
    name_title <- unique(subset(all_data, Species.Code==species_click())$Common.Name)
    print(h3(name_title))
  })
  
  # List of locations
  location_list <- c("House", "Glen", "Prairie", "Wetland", "Savanna", "Forest")
  
  # Set colors based on year. You'll want to add more colors with each new year. 
  # cols <- c("2020" = "#F8766D",
  #           "2021" = "#7CAE00",
  #           "2022" = "#00BFC4",
  #           "2023" = "#C77CFF",
  #           "2024" = "#E68613")
  # cols <- brewer.pal(5, "Greens")
  cols <- brewer.pal(5, "YlGn")
  # cols <- brewer.pal(5, "YlGnBu")
  # cols <- brewer.pal(5, "YlOrBr")
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
    # Extract year from the given date
    year <- as.integer(format(as.Date(date), "%Y"))
    
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

  output$species_title <- renderUI({
    name_title <- unique(subset(all_data, Species.Code==species_click())$Common.Name)
    print(h3(name_title))
  })
  
  for (i in seq_along(location_list)) {
    local({
      loc <- location_list[i]
      output[[paste0("frequencyPlot_", i)]] <- renderPlotly({
        
        min_conf <- input$confidence_selection[1]
        max_conf <- input$confidence_selection[2]
        
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
                               limits = c(1, 52),
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
            scale_x_continuous(breaks = 1:12, limits = c(1, 12)) +
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

  observeEvent(input$tab_selection, {
    selected_data(NULL)
    audio_player(NULL)
    spectrogram(NULL)
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
      
      file_name <- basename(row[["Begin.Path"]])
      start_time <- as.numeric(row[["Begin.Time..s."]])
      
      # TEMPORARY LOCAL DIRECTORY FOR TESTING
      file_loc <- "/Users/laurenwick/Dropbox/Lauren Wick/Test audio"
      
      audio_loc <- file.path(file_loc, file_name)
      
      if (!file.exists(audio_loc)) {
        return(NULL)
      }
      
      return(paste0(file_name, "***", start_time))
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
            Website = paste0(
              "<a href='", "https://search.macaulaylibrary.org/catalog?taxonCode=", Species.Code,
              "&mediaType=audio&sort=rating_rank_desc",
              "' target='_blank'>Open Bird Guide</a>"
            ),
            
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
          select("Sound.Button", "Website", "Confidence", 
                 "Begin.Time..s.", "End.Time..s.", "Date", "Week", 
                 "Location", "Begin.Path", "Species.Code") %>%
          ungroup()
        
        datatable(out_df, 
                  escape = FALSE,
                  selection = "none")
      } else {
        data.frame()
      }
    })
  })

  
  observeEvent(input$play_button, {
    # TEMPORARY LOCAL DIRECTORY FOR TESTING
    file_loc <- "/Users/laurenwick/Dropbox/Lauren Wick/Test audio"
    
    # Grab value from slider for length of recording to extract
    recording_secs <- input$recording_length
    
    # Define the temporary directory and stream the data
    if (!dir.exists("www")) {
      dir.create("www")
    }
    
    # Take the value of input$select_button
    selectedRow <- input$play_button
    audio_file <- unlist(strsplit(selectedRow, "***", fixed=TRUE))[[1]]
    begin_time <- as.numeric(unlist(strsplit(selectedRow, "***", fixed=TRUE))[[2]])
    
    audio_src <- paste0(strsplit(audio_file, ".wav"), "_", begin_time, "s.wav")
    dest_path <- file.path("www", audio_src)
    
    later::later(function() {
      if (file.exists(dest_path)) file.remove(dest_path)
    }, delay = 10)
    
    audio_loc <- file.path(file_loc, audio_file)
    output_wav <- av_audio_convert(audio = audio_loc, 
                                   output = dest_path,
                                   start_time = begin_time,
                                   total_time = as.numeric(recording_secs))
    
    audio_player(tags$audio(src = audio_src, type = "audio/wav", controls = NA, autoplay = NA))
    
    audio_wav <- tuneR::readWave(output_wav)
    
    v <- ggspectro(audio_wav, ovlp = 50) +
      geom_tile(aes(fill = amplitude)) +
      ylim(0, 12) +
      scale_fill_gradientn(colours = viridis(256, option = "B"))
    
    spectrogram(v)
  })
  
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

