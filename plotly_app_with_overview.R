# Reading in all data:
all_data <- fst::read_fst("/Users/laurenwick/Dropbox/Lauren Wick/Plotly App/70conf_2020_to_2024.fst")

# Set species code:
species <- "leabit"

ui <- navbarPage(
  id = "main_nav",
  title = "BirdNet-Analyzer Data Visualization",
  tabPanel(title = "Prairie Haven Overview",
           value = "ph_overview",
           DTOutput("species_counts")
  ),
  
  tabPanel(title = "Species-Specific Overview",
           value = "species_overview",
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
  
  # New df of total count by species:
  count_by_species <- all_data %>% count(Common.Name, Species.Code)
  
  # Create display column that contains count and species name
  count_by_species$Display <- paste(count_by_species$Common.Name, "<br>", count_by_species$n, sep = "")
  
  # Render the 3 tables (aka the three columns of data)
  output$species_counts <- renderDT({ 
    datatable(
      data.frame(Display = count_by_species$Display), 
      escape = FALSE,
      options = list(
        dom = 't', # display only (no search / no pagination)
        ordering = FALSE,
        pageLength = nrow(data)
      ),
      rownames = FALSE,
      selection = 'single'
    ) 
    })
  
  # Observe clicks on the tables
  observeEvent(input$species_counts_rows_selected, {
    selected_row <- input$species_counts_rows_selected
    if (!is.null(selected_row)) {
      new_species <- count_by_species$Species.Code[selected_row]
      species_click(new_species) # Update the reactive value
      print(paste("Updating navbar to 'overview' for species code:", species_click()))
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
          # scale_y_continuous(limits = c(0, global_max + 50)) +
          labs(title = loc, x = "Week", y = "Frequency") +
          scale_color_manual(name = "Year", values = cols) +
          theme_minimal()
        
        # Convert to an interactive plotly object and register the click event
        plotly_object <- ggplotly(p, dynamicTicks = TRUE)
        
      })
    })
  }
  
}

shinyApp(ui, server)

