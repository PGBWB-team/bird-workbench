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

# Reading in all data:
all_data <- fst::read_fst("/Users/laurenwick/Dropbox/Lauren Wick/Plotly App/70conf_2020_to_2024.fst")

ui <- navbarPage(
  id = "main_nav",
  title = "BirdNet-Analyzer Data Visualization",
  selected = "ph_overview",
  tabPanel(title = "Prairie Haven Overview",
           value = "ph_overview",
           selectInput(inputId = "sel_view", 
                       label = "View Selection",
                       choices = list("Species Totals" = "totals", "Species by Month" = "by_month"),
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
               tabsetPanel(
                 !!!lapply(seq_along(c("House", "Glen", "Prairie", "Wetland", "Savanna", "Forest")), function(i) {
                   tabPanel(
                     title = c("House", "Glen", "Prairie", "Wetland", "Savanna", "Forest")[i],
                     plotlyOutput(outputId = paste0("frequencyPlot_", i))
                   )
                 })
               ),
               
               DT::dataTableOutput("filtered_data")
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
      data.frame(Common.Name = data$Common.Name,
                 January = data$January, February = data$February, March = data$March, April = data$April,
                 May = data$May, June = data$June, July = data$July, August = data$August, 
                 September = data$September, October = data$October, November = data$November, December = data$December),
      escape = FALSE,
      options = list(
        dom = "t",
        ordering = FALSE,
        page_length = nrow(data), 
        lengthMenu = list(c(nrow(data)), c("All"))
      ),
      rownames = FALSE,
      selection = list(mode = "single", target = "cell")
    )
  }
  
  # Pivot table:
  species_by_month <- all_data %>%
    mutate(Date = as.Date(Date), Month = month(Date)) %>%
    group_by(Common.Name, Species.Code, Month) %>%
    summarize(Count_by_Species = n()) %>%
    pivot_wider(names_from = Month, values_from = Count_by_Species, values_fill = list(Count_by_Species = 0)) %>%
    select(Common.Name, Species.Code, as.character(1:12)) %>%
    rename_with( ~ month.name[as.numeric(.)], -c(Common.Name, Species.Code))
  
  # Observe Data Tables
  observe({
    output$species_by_month_pivot <- renderDataTable({ create_table_pivot(species_by_month) })
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
    
    } else {
      DT::dataTableOutput("species_by_month_pivot")
    }
  })

  # Observe clicks on the tables
  observeEvent(input$species_by_month_pivot_cells_selected, {
    selected_row <- input$species_by_month_pivot_cells_selected
    if (nrow(selected_row) > 0) {
      new_species <- species_by_month$Species.Code[selected_row]
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
  
  # Observe the selected data and filter the original dataframe
  # observe({
  #   
  #   species_folders <- paste0(species_click(), "/")
  #   
  #   # Combine file listings for all species
  #   all_files <- fetch_files(species_folders)
  #   selected_data <- event_data("plotly_selected")
  #   
  # })
    
  # base_url <- "https://earsinthedriftless.com/BirdNET_Segments_test/Test_output_folder/"
  # species_folders <- paste0(species, "/")
  # 
  # # Combine file listings for all species
  # all_files <- fetch_files(species_folders)
  # 
  # Observe the selected data and filter the original dataframe
  observe({
    
    species_folders <- paste0(species_click(), "/")
    
    # Combine file listings for all species
    all_files <- fetch_files(species_folders)
    selected_data <- event_data("plotly_selected")
    
    if (!is.null(selected_data)) {
      # Extract the selected weeks (x-values of bars clicked)
      selected_weeks <- selected_data$x
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
            Sound.File = {
              # Create the regex pattern for the current row
              pattern <- paste0(
                "^", as.character(round(as.numeric(Confidence), digits=3)), "_[0-9]+_", gsub(".wav", "", basename(Begin.Path)),
                "_", Begin.Time..s., "s_", End.Time..s., "s.wav", "$"
              )
              
              # Find matching file
              match <- all_files[str_detect(all_files, pattern)]
              if (length(match) == 1) {
                paste0(
                  "<a href='", URLencode(base_url), Species.Code,"/", match,
                  "' target=`_blank'>Open Sound File</a>"
                )
              } else {
                "No File"
              }
            }
          ) %>%
          ungroup()
        
        datatable(out_df, escape = FALSE)
      } else {
        data.frame()
      }
    })
  })
  
  
}

shinyApp(ui, server)

