# Required packages:
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

# Set species code:
species <- "leabit"

ui <- navbarPage(
  title = "Main Observations",
  id = "main_nav",
  bg = "darkgreen",
  inverse = TRUE,
  tabPanel(title = "Prairie Haven Overview",
           value = "ph_overview",
           DTOutput("species_counts")
  ),
  
  tabPanel(title = "Test",
           value = "testing"),
  
  tabPanel(title = "Overview",
           value = "species_overview",
            fluidRow(
              lapply(1:6, function(i){
                column(width = 6,
                       plotlyOutput(outputId = paste0("overviewPlot_", i), height = "400px"))
              })
            )),
  
  tabPanel(title = "Location Drilldown",
           value = "loc_drilldown",
            page_sidebar(
              title = paste0(unique(subset(all_data, Species.Code==species)$Common.Name), " Observations"),
              
              sidebar = sidebar(
                bg = "white",
                radioButtons(
                  "time_interval", 
                  label = "Time Interval",
                  choices = c("Week" = "weekly", "Month" = "monthly"),
                  selected = "weekly"
                ),
                sliderInput(
                  "confidence_selection", label = "Confidence level: ",
                  min = 0, value = c(0.7, 1), max = 1
                )
              ),
              
              navset_card_underline(
                title = "Histograms by Location",
                !!!lapply(seq_along(c("House", "Glen", "Prairie", "Wetland", "Savanna", "Forest")), function(i) {
                  nav_panel(
                    title = c("House", "Glen", "Prairie", "Wetland", "Savanna", "Forest")[i],
                    plotlyOutput(outputId = paste0("frequencyPlot_", i))
                  )
                })
              ),
              
              accordion(
                accordion_panel(
                  "Data Table",
                  DT::dataTableOutput("filtered_data")
                )
              ) )
  ) )


server <- function(input, output, session) {
  
  ###############
  ## Functions ##
  ###############
  
  # Function to create data table
  create_table <- function(data) {
    # Select only the 'Display' column
    data <- data.frame(Display = data$Display)
    
    datatable(
      data, 
      escape = FALSE,
      options = list(
        dom = 't', # display only (no search / no pagination)
        ordering = FALSE,
        pageLength = nrow(data)
      ),
      rownames = FALSE,
      selection = 'single'
    )  
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
  
  ##################################################                                        
  ## Data Processing: Prairie Haven Overview Page ##
  ##################################################                                                                                
  
  # New df of total count by species:
  count_by_species <- all_data %>% count(Common.Name, Species.Code)
  
  # Create display column that contains count and species name
  count_by_species$Display <- paste(count_by_species$Common.Name, "<br>", count_by_species$n, sep = "")
  
  # # Calculate the number of rows per column 
  # rows_per_column <- ceiling(nrow(count_by_species) / 4)
  # 
  # # Split the data into three parts
  # column1 <- count_by_species[1:rows_per_column, ]
  # column2 <- count_by_species[(rows_per_column + 1):(2 * rows_per_column), ]
  # column3 <- count_by_species[(2 * rows_per_column + 1):(3 * rows_per_column), ]
  # column4 <- count_by_species[(3 * rows_per_column + 1):nrow(count_by_species), ]
  # 
  ################################################
  ## Data Processing: Species-Specific Overview ##
  ################################################
  
  # Subset by species:
  species_data <- subset(all_data, Species.Code==species)
  
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
  
  # Calculate the global maximum count to be used as y-axis limit in freqpoly plots
  global_max <- species_data %>%
    group_by(Location, Week) %>%
    summarise(Count = n(), .groups = "drop") %>%
    summarise(MaxCount = max(Count)) %>%
    pull(MaxCount)
  
  # List of locations
  location_list <- c("House", "Glen", "Prairie", "Wetland", "Savanna", "Forest")
  
  # Set colors based on year. You'll want to add more colors with each new year. 
  cols <- c("2020" = "#F8766D",
            "2021" = "#7CAE00",
            "2022" = "#00BFC4",
            "2023" = "#C77CFF",
            "2024" = "#E68613")
  
  ###################################################
  ## Data Processing: Species / Location Drilldown ##
  ###################################################
  
  # Pre-fetch the file list from the server during app initialization
  base_url <- "https://earsinthedriftless.com/BirdNET_Segments_test/Test_output_folder/"
  species_folders <- paste0(species, "/")
  
  # Combine file listings for all species
  all_files <- fetch_files(species_folders)
  
  ###############################################
  ## Application Build: Prairie Haven Overview ##
  ###############################################
  
  # Render the 3 tables (aka the three columns of data)
  output$species_counts <- renderDT({ 
    datatable(count_by_species, selection = "single", options = list(dom = "t")) 
    })
  
  # Observe clicks on the tables
  observeEvent(input$species_counts_rows_selected, {
    selected_row <- input$species_counts_rows_selected
    if (!is.null(selected_row)) {
      print(paste("Switching to overview for row:", selected_row))
      nav_show(id="main_nav", target = "testing")
    }
  })
   
  # observeEvent(input$table2_cell_clicked, {
  #   info <- input$table2_cell_clicked
  #   if (!is.null(info$value)) {
  #     species_code <- column2$Species.Code[info$row]
  #     print(paste("Updating navbar to 'overview' for species code:", species_code))
  #     updateNavbarPage(session=session, inputId = "main_nav",
  #                      selected = "species_overview")
  #   }
  # })
  # 
  # observeEvent(input$table3_cell_clicked, {
  #   info <- input$table3_cell_clicked
  #   if (!is.null(info$value)) {
  #     species_code <- column3$Species.Code[info$row]
  #     print(paste("Updating navbar to 'overview' for species code:", species_code))
  #     updateNavbarPage(session=session, inputId = "main_nav",
  #                      selected = "species_overview")
  #   }
  # })
  # 
  # observeEvent(input$table4_cell_clicked, {
  #   info <- input$table4_cell_clicked
  #   if (!is.null(info$value)) {
  #     species_code <- column4$Species.Code[info$row]
  #     print(paste("Updating navbar to 'overview' for species code:", species_code))
  #     updateNavbarPage(session=session, inputId = "main_nav",
  #                      selected = "species_overview")
  #   }
  # })
  
  # Placeholder for filtered data
  filtered_data <- reactiveVal(NULL)
  
  for (i in seq_along(location_list)) {
    local({
      loc <- location_list[i]
      output[[paste0("overviewPlot_", i)]] <- renderPlotly({
        # Subset to location
        species_data <- subset(species_data, 
                               Location == loc)
        
        # Create the plot
        p <- ggplot(species_data, aes(x = Week)) +
          geom_freqpoly(binwidth=1,aes(color = as.factor(Year))) +
          scale_x_continuous(breaks = 1:52, limits = c(1, 52)) +
          scale_y_continuous(limits = c(0, global_max + 50)) +
          labs(title = loc, x = "Week", y = "Frequency") +
          scale_color_manual(name = "Year", values = cols) +
          theme_minimal()
        
        # Convert to an interactive plotly object and register the click event
        plotly_object <- ggplotly(p, dynamicTicks = TRUE)
        
      })
    })
  }
  
  for (i in seq_along(location_list)) {
    local({
      loc <- location_list[i]
      output[[paste0("frequencyPlot_", i)]] <- renderPlotly({
        
        min_conf <- input$confidence_selection[1]
        max_conf <- input$confidence_selection[2]
        
        complete_data <- subset(complete_data,
                                as.numeric(Confidence) >= min_conf &
                                  as.numeric(Confidence) <= max_conf &
                                  Location == loc)
        
        if (input$time_interval == "weekly") {
          # Create the plot
          p <- ggplot(complete_data, aes(x = Week,
                                         key = Week.Year.Loc,
                                         text = paste(
                                           "Week:", Week,
                                           "<br>Date Range:", get_week_date_range(Year, Week),
                                           "<br>Count:", nrow(Week.Year.Loc)
                                         ))) +
            geom_bar(position = "identity", alpha = 0.8, aes(fill = as.factor(Year))) +
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
           geom_bar(position = "identity", alpha = 0.8, aes(fill = as.factor(Year))) +
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
  
  # Observe the selected data and filter the original dataframe
  observe({
    selected_data <- event_data("plotly_click")
    
    if (!is.null(selected_data)) {
      # Extract the selected weeks (x-values of bars clicked)
      selected_weeks <- selected_data$x
    }
    
    # filter data based on the selected bin center
    output$filtered_data <- DT::renderDataTable({
      
      min_conf <- input$confidence_selection[1]
      max_conf <- input$confidence_selection[2]
      
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
