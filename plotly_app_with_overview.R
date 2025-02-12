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

# Reading in all data:
all_data <- fst::read_fst("/Users/laurenwick/Dropbox/Lauren Wick/Plotly App/70conf_2020_to_2024.fst")

ui <- navbarPage(
  id = "main_nav",
  title = "BirdNet-Analyzer Data Visualization",
  selected = "ph_overview",
  tabPanel(title = "Prairie Haven Overview",
           value = "ph_overview",
           selectInput(inputId = "sel_view", 
                       label = h3("View Selection"),
                       choices = list("Species Totals" = "totals", "Species by Month" = "by_month",
                                      "Species by Location" = "by_location", "Species by Year" = "by_year"),
                       selected = "totals"),
          uiOutput("overview_view")),
  
  tabPanel(title = "Species-Specific Overview",
           value = "species_overview",
           fluidRow(
             lapply(1:6, function(i){
               column(width = 6,
                      plotlyOutput(outputId = paste0("overviewPlot_", i), height = "400px"))
             })
             )
  ), 
  
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
               )
             ),
             
             mainPanel(
               uiOutput("species_title"),
               tabsetPanel(
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
     
  )
)


server <- function(input, output, session) {
  
  ###############################################
  ## Application Build: Prairie Haven Overview ##
  ###############################################
  # Reactive value to store the selected species code
  species_click <- reactiveVal("acafly") # Default species code
  
  # New df of total count by species:
  count_by_species <- all_data %>% count(Common.Name, Species.Code)
  
  # Create display column that contains count and species name
  count_by_species$Display <- paste(count_by_species$Common.Name, "<br>", count_by_species$n, sep = "")
  
  # Calculate number of rows that will be in each column
  rows_per_column <- ceiling(nrow(count_by_species) / 4)
  
  # Split the data into four parts
  col1 <- count_by_species %>% slice(1:rows_per_column)
  col2 <- count_by_species %>% slice((rows_per_column + 1):(2 * rows_per_column))
  col3 <- count_by_species %>% slice((2 * rows_per_column + 1):(3 * rows_per_column))
  col4 <- count_by_species %>% slice((3 * rows_per_column + 1):n())
  
  # Function to create table:
  create_table <- function(data) {
    datatable(
      data.frame(Display = data$Display),
      escape = FALSE,
      options = list(
        dom = "t", 
        ordering = FALSE, 
        page_length = nrow(data),
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
      options = list(
        dom = "t",
        ordering = TRUE,
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
      
      out_df <- df %>%
        group_by(Common.Name, Species.Code, Location) %>%
        summarize(Count_by_Species = n()) %>%
        pivot_wider(names_from = Location, values_from = Count_by_Species, values_fill = list(Count_by_Species = 0)) %>%
        select(Common.Name, Species.Code, locs)
      
      return(out_df)
    }
  }
  
  # Reactive expression for species_by_month based on year selection
  species_by_month <- reactive({
    req(input$year_sel)  # Ensure input exists before proceeding
    create_pivot_month(all_data, input$year_sel)
  })
  
  species_by_location <- reactive({
    req(input$year_sel) # Ensure input exists before proceeding
    create_pivot_location(all_data, input$year_sel)
  })
  
  species_by_year <- reactive({
    req(input$loc_sel)
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
    output$species_counts_t1 <- renderDataTable({ create_table(col1) })
    output$species_counts_t2 <- renderDataTable({ create_table(col2) })
    output$species_counts_t3 <- renderDataTable({ create_table(col3) })
    output$species_counts_t4 <- renderDataTable({ create_table(col4) })
  })
  
  # Dynamic UI based on selection
  output$overview_view <- renderUI({
    if (input$sel_view == "totals") {
      
      fluidRow(
        column(3, DT::dataTableOutput("species_counts_t1")),
        column(3, DT::dataTableOutput("species_counts_t2")),
        column(3, DT::dataTableOutput("species_counts_t3")),
        column(3, DT::dataTableOutput("species_counts_t4"))
      )
    
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
      updateNavbarPage(session, "main_nav", selected = "species_overview")
    }
  })
  
  observeEvent(input$species_by_location_pivot_cells_selected, {
    selected_row <- input$species_by_location_pivot_cells_selected
    if (nrow(selected_row) > 0) {
      new_species <- species_by_location()$Species.Code[selected_row]
      species_click(new_species) # Update the reactive value
      updateNavbarPage(session, "main_nav", selected = "species_overview")
    }
  })
  
  observeEvent(input$species_by_year_pivot_cells_selected, {
    selected_row <- input$species_by_year_pivot_cells_selected
    if (nrow(selected_row) > 0) {
      new_species <- species_by_year()$Species.Code[selected_row]
      species_click(new_species) # Update the reactive value
      updateNavbarPage(session, "main_nav", selected = "species_overview")
    }
  })
  
  observeEvent(input$species_counts_t1_cells_selected, {
    selected_row <- input$species_counts_t1_cells_selected
    if (nrow(selected_row) > 0) {
      new_species <- col1$Species.Code[selected_row]
      species_click(new_species) # Update the reactive value
      updateNavbarPage(session, "main_nav", selected = "species_overview")
    }
  })
  
  observeEvent(input$species_counts_t2_cells_selected, {
    selected_row <- input$species_counts_t2_cells_selected
    if (nrow(selected_row) > 0) {
      new_species <- col2$Species.Code[selected_row]
      species_click(new_species) # Update the reactive value
      updateNavbarPage(session, "main_nav", selected = "species_overview")
    }
  })
  
  observeEvent(input$species_counts_t3_cells_selected, {
    selected_row <- input$species_counts_t3_cells_selected
    if (nrow(selected_row) > 0) {
      new_species <- col3$Species.Code[selected_row]
      species_click(new_species) # Update the reactive value
      updateNavbarPage(session, "main_nav", selected = "species_overview")
    }
  })
  
  observeEvent(input$species_counts_t4_cells_selected, {
    selected_row <- input$species_counts_t4_cells_selected
    if (nrow(selected_row) > 0) {
      new_species <- col4$Species.Code[selected_row]
      species_click(new_species) # Update the reactive value
      updateNavbarPage(session, "main_nav", selected = "species_overview")
    }
  })
  
  ##################################################
  ## Application Build: Species-Specific Overview ##
  ##################################################
  # List of locations
  location_list <- c("House", "Glen", "Prairie", "Wetland", "Savanna", "Forest")
  
  # Set colors based on year. You'll want to add more colors with each new year. 
  cols <- c("2020" = "#F8766D",
            "2021" = "#7CAE00",
            "2022" = "#00BFC4",
            "2023" = "#C77CFF",
            "2024" = "#E68613")
  
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
            Week = epiweek(Date),
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
  
  # Function to fetch files for each species with error handling
  fetch_files <- function(folder) {
    url <- paste0(base_url, folder)
    
    # Attempt to fetch the HTML content
    tryCatch({
      page <- rvest::read_html(url)
      files <- page %>% html_nodes("a") %>% html_attr("href")
      
      # Check if files are empty
      if (length(files) == 0) {
        warning(paste("No files found in folder:", folder))
        return(character(0))  # Return an empty character vector
      }
      
      return(files)
    }, error = function(e) {
      warning(paste("Error fetching files for folder:", folder, "-", e$message))
      return(character(0))  # Return an empty character vector in case of error
    })
  }
  
  output$species_title <- renderUI({
    name_title <- unique(subset(all_data, Species.Code==species_click())$Common.Name)
    # name_title <- unique(species_data$Common.Name)
    print(h3(name_title))
    # print(h3(species_click()))
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
            Week = epiweek(Date),
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
        
        if (input$time_interval == "weekly") {
          # Create the plot
          p <- ggplot(complete_data, aes(x = Week,
                                         key = Week.Year.Loc,
                                         text = paste(
                                           "Week:", Week,
                                           "<br>Date Range:", get_week_date_range(Year, Week),
                                           "<br>Count:", nrow(Week.Year.Loc)
                                         ))) +
            geom_bar(position = "identity", alpha = 0.8, aes(fill = as.factor(Year)), width = 0.9) +
            scale_x_continuous(breaks = 1:52, limits = c(1, 52)) +
            labs(title = loc, x = "Week", y = "Frequency") +
            scale_fill_manual(name = "Year", values = cols) +
            theme_minimal()
        }
        
        if (input$time_interval == "monthly") {
          # Create the plot
          p <- ggplot(complete_data, aes(x = Month,
                                         key = Month.Year.Loc,
                                         text = paste(
                                           "Month:", Month,
                                           "<br>Date Range:",
                                           "<br>Count:", nrow(Month.Year.Loc)
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
  
  # Pre-fetch the file list from the server during app initialization
  base_url <- "https://earsinthedriftless.com/BirdNET_Segments_test/Test_output_folder_v2/"
  
  # Function to create sound button
  shinyInput <- function(FUN, id, ...) {
      as.character(FUN(paste0(id), ...))
  }
  

  # Observe the selected data and filter the original dataframe
  observe({
    
    species_folders <- paste0(species_click(), "/")
    
    # Combine file listings for all species
    all_files <- fetch_files(species_folders)
    selected_data <- event_data("plotly_selected")
    
    find_audio <- function(row, species_files, folder_url) {
      # Create the regex pattern for the current row
      pattern <- paste0(
        "^", as.character(round(as.numeric(row[["Confidence"]]), digits=3)), "_[0-9]+_", gsub(".wav", "", basename(row[["Begin.Path"]])),
        "_", row[["Begin.Time..s."]], "s_", row[["End.Time..s."]], "s.wav", "$"
      )
      
      # Find matching file
      match <- species_files[str_detect(species_files, pattern)]
      if (length(match) == 1) {
        return(paste0(
          URLencode(folder_url), row[["Species.Code"]],"/", match)
        )
      } else {
        return(NULL)
      }
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
          Week = epiweek(Date),
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
      
      if (!is.null(selected_data)) {
        if (input$time_interval == "weekly") {
          out_df <- subset(complete_data, (Week.Year.Loc %in% selected_data$key & 
                                             as.numeric(Confidence)>=min_conf & 
                                             as.numeric(Confidence)<= max_conf), select=c("Begin.Time..s.", "End.Time..s.", "Week", "Confidence", "Location", "Begin.Path", "Species.Code"))
        }
        
        if (input$time_interval == "monthly") {
          out_df <- subset(complete_data, (Month.Year.Loc %in% selected_data$key & 
                                             as.numeric(Confidence)>=min_conf & 
                                             as.numeric(Confidence)<= max_conf), select=c("Begin.Time..s.", "End.Time..s.", "Week", "Confidence", "Location", "Begin.Path", "Species.Code"))
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
              audio_id <- find_audio(.data, all_files, base_url)
              if (!is.null(audio_id)) {
                shinyInput(
                  FUN = actionButton,
                  id = audio_id,
                  label = "Play Sound", 
                  onclick = 'Shiny.setInputValue(\"play_button\", this.id, {priority: \"event\"})'
                )
              }
              else {
                "No Audio File"
              }
            }),
            
            Spectrogram.Button = list({
              audio_id <- find_audio(.data, all_files, base_url)
              if (!is.null(audio_id)) {
                shinyInput(
                  FUN = actionButton,
                  id = audio_id,
                  label = "Display Spectrogram", 
                  onclick = 'Shiny.setInputValue(\"spectrogram_button\", this.id, {priority: \"event\"})'
                )
              }
              else {
                "No Audio File"
              }
            })
          ) %>%
          select("Sound.Button", "Spectrogram.Button", "Website", "Confidence", 
                 "Begin.Time..s.", "End.Time..s.", "Week", 
                 "Location", "Begin.Path", "Species.Code") %>%
          ungroup()
        
        datatable(out_df, escape = FALSE)
      } else {
        data.frame()
      }
    })
  })
  
  play_recording <- eventReactive(input$play_button, {
    # define the temporary directory and download the data
    if (!dir.exists("www")) {
      dir.create("www")
    }
    
    dest_path <- file.path("www", "temp_sound.wav")
    
    # Delete previous file if it exists
    if (file.exists(dest_path)) file.remove(dest_path)
    
    # take the value of input$select_button
    selectedRow <- input$play_button
    download.file(selectedRow, destfile = dest_path, mode = "wb")
    
    audio_src <- "temp_sound.wav"
    
    # Return an audio player element
    tags$audio(src = audio_src, type = "audio/wav", controls = NA, autoplay = NA)

  })
  
  show_spectrogram <- eventReactive(input$spectrogram_button, {
    dest_path <- file.path(tempdir(), "temp_sound_spec.wav")
    
    # Delete previous file if it exists
    if (file.exists(dest_path)) file.remove(dest_path)
    
    # Take the value of input$select_button and download file
    selectedRow <- input$spectrogram_button
    download.file(selectedRow, destfile = dest_path, mode = "wb")
    
    # Read wav file
    temp_wav <- tuneR::readWave(dest_path)

    # spectro(temp_wav, f= 48000, wl=512, ovlp=75, flim = c(1, 15), palette = reverse.gray.colors.1)
    v <- ggspectro(temp_wav, ovlp=50)
    v + geom_tile(aes(fill = amplitude))

  })
  
  # Show the name of the employee that has been clicked on
  output$audio_player <- renderUI({
    play_recording()
  })
  
  output$spectrogram <- renderPlot({
    show_spectrogram()
  })
  
  
}

shinyApp(ui, server)

