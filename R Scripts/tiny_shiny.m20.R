library(shiny)
library(bslib)
library(dplyr)
library(shinycssloaders)
library(shinyjs)
library(ggplot2)
library(lubridate)
library(av)
library(seewave)
library(viridisLite)
library(DBI)
library(RSQLite)

#################################################
# Set file path variables (Requires User Input) #
#################################################

# Set root folder for audio files
audio_filepath <- "/Volumes/Bio/"

# parent_shiny_url format: "http://pgbwb.com/"
parent_shiny_url <- "https://pgbwb.com/"

# File path to SQL weather database
weather_path <- "/Users/mikeoconnor/Documents/BirdWorkbench/weather_snoop_valley_weather/valley.weather.db"

##############
# Start Code #
##############

# UI code:
ui <- page_sidebar(
  title = uiOutput("species_title"),
  sidebar = sidebar(
    
    card(
      card_header("Observation Details"),
      uiOutput("obs_details")
    ),
    
    card(
      card_header("Audio Parameters"),
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
    ),
    
    width = 370
  ),

   # card(
   #   style = "height: 70vh; display: flex; flex-direction: column;",
   #   card_header("Spectrogram"),
   #   div(
   #     style = "flex-grow: 1;",
   #     plotOutput("spectrogram", height = "500px") %>% withSpinner()
   #   )
   # ),

  fluidRow(
    plotOutput("spectrogram") %>% withSpinner(),
    uiOutput("audio_player") %>% withSpinner(),
    
    #  uiOutput("scrolling_spectro") %>% withSpinner(),
    
    layout_columns(
      downloadButton("audioDownload", "Download Audio"),
      downloadButton("videoDownload", "Download Video"),
      uiOutput("parent_url"),
      uiOutput("file_page_url"),
      uiOutput("cornell_link")
    )
  )
 #   plotOutput("spectrogram") %>% withSpinner(),
 #   uiOutput("audio_player") %>% withSpinner(),
 # 
 # #  uiOutput("scrolling_spectro") %>% withSpinner(),
 #  
 #  layout_columns(
 #    downloadButton("audioDownload", "Download Audio"),
 #    downloadButton("videoDownload", "Download Video"),
 #    uiOutput("parent_url"),
 #    uiOutput("cornell_link")
 #  ),
 # 
 # plotOutput("wind_plot") %>% withSpinner()
  )

# Server Code:
server <- function(input, output, session) {
  values <- reactiveValues(
    species_name = NULL,
    species_code = NULL,
    conf = NULL,
    loc = NULL,
    temp = NULL,
    wind = NULL,
    file_name = NULL,
    begin_time = NULL
  )
  
  observe({
    query <- parseQueryString(session$clientData$url_search)
    
    values$species_name <- query[["species_name"]]
    values$species_code <- query[["species_code"]]
    values$conf <- query[["conf"]]
    values$loc <- query[["loc"]]
    values$temp <- query[["temp"]]
    values$wind <- query[["wind"]]
    values$file_name <- query[["file_name"]]
    values$begin_time <- query[["begin_time"]]
  })
  
  # Function to find audio file
  find_audio_file <- function(file_nm) {
    if (is.null(file_nm) || file_nm == "") {
      return(NULL)
    }
    
    year <- year(as.Date(unlist(strsplit(basename(file_nm), split = "_"))[2], "%Y%m%d"))
    file_loc <- paste0(audio_filepath, "Bio ", as.character(year), "/From Recorders")
    audio_loc <- file.path(file_loc, file_nm)
    if (is.null(audio_loc) || audio_loc == "" || !file.exists(audio_loc)) {
      return(NULL)
    }
    
    
    return(audio_loc)
  }
  
  get_date_time <- function(file_nm) {
    if (is.null(file_nm) || file_nm == "") {
      return(NULL)
    }
    
    date <- unlist(strsplit(basename(file_nm), split = "_"))[2]
    time <- substr(unlist(strsplit(basename(file_nm), split = "_"))[3], start = 1, stop = 6)
    date_time <- ymd_hms(paste(date, time), tz="UTC")
    return(date_time)
  }
  
  debounced_recording_length <- debounce(reactive(input$recording_length), 1000)
  debounced_recording_offset <- debounce(reactive(input$recording_offset), 1000)
  
  observeEvent({
    debounced_recording_length()
    debounced_recording_offset()
  }, {
    req(debounced_recording_length(), debounced_recording_offset())
    if (!is.null(values$file_name)) {
      generate_audio_and_spectrogram()
    }
  })
  

  generate_audio_and_spectrogram <- reactive({
    req(values$file_name)
    og_file <- find_audio_file(values$file_name)

    output$species_title <- renderUI(
      values$species_name
    )
    
    output$obs_details <- renderUI(
      tagList(
        div(HTML("<strong>Confidence:</strong> "), values$conf),
        div(HTML("<strong>Location:</strong> "), values$loc),
        div(HTML("<strong>Date Time:</strong> "), get_date_time(values$file_name)),
        div(HTML("<strong>File Avg Temp:</strong> "), values$temp),
        div(HTML("<strong>File Avg Windspeed:</strong> "), values$wind),
        div(HTML("<strong>File Name:</strong> "), values$file_name)
      )
      
    )
    
    # Extract recording length and offset parameters from widgets
    recording_secs <- debounced_recording_length()
    recording_offset <- debounced_recording_offset()
    
    # Create www/ directory for storing / playing audio
    if (!dir.exists("www")) {
      dir.create("www")
    }
    
    # Create new file name by appending begin time to original wav file name
    audio_src <- paste0(sub("\\.wav$", "", basename(values$file_name)), "_", values$begin_time, "s.wav")
    dest_path <- file.path("www", audio_src)
    
    vid_src <- paste0(sub("\\.wav$", "", basename(values$file_name)), "_", values$begin_time, "s.mp4")
    dest_vid_path <- file.path("www", vid_src)
    
    # Stream audio clip and rewrite
    output_wav <- av_audio_convert(audio = og_file,
                                   output = dest_path,
                                   start_time = as.numeric(values$begin_time) - as.numeric(recording_offset),
                                   total_time = as.numeric(recording_secs))
    
    # av_spectro_vid <- av_spectrogram_video(output_wav,
    #                                        output = dest_vid_path,
    #                                        width = 1200, 
    #                                        height = 550, 
    #                                        res = 72,
    #                                        framerate = 20)
    
    # Generate audio tag
    output$audio_player <- renderUI({
      tags$audio(src = audio_src,
                 type = "audio/wav",
                 controls = NA,
                 autoplay = NA)
    })
    
    # output$scrolling_spectro <- renderUI({
    #   tags$video(src = vid_src,
    #              type = "video/mp4", 
    #              autoplay = NA,
    #              controls = NA)
    # })
    # 
    audio_wav <- tuneR::readWave(output_wav)
    
    v <- seewave::ggspectro(audio_wav, ovlp = 50) +
      geom_tile(aes(fill = amplitude)) +
      ylim(0, 12) +
      scale_fill_gradientn(colours = viridis(256, option = "B"),
                           limits = c(-90, 0))
    
    output$spectrogram <- renderPlot(v)
    
    output$audioDownload <- downloadHandler(
      filename = function() {
        req(values$file_name)
        paste(basename(dest_path))
      },
      content = function(file_path) {
        req(values$file_name) 
        file.copy(dest_path, file_path)
      }, 
      contentType = "audio/wav"
    )
    
    output$videoDownload <- downloadHandler(
      filename = function() {
        req(values$file_name)
        paste0(tools::file_path_sans_ext(basename(values$file_name)), "_spectrogram.mp4")
      },
      content = function(file_path) {
        req(values$file_name)
        
        # Show progress indicator that updates immediately
        withProgress(message = 'Generating spectrogram video', value = 0.1, {
          incProgress(0.1, detail = "Starting video generation...")
          
          # Force progress update to display
          Sys.sleep(0.1)
          incProgress(0.1, detail = "This can take a minute...")
          
          # Generate the video here - this only happens when button is clicked
          av_spectro_vid <- av_spectrogram_video(output_wav,
                                                 output = file_path,
                                                 width = 1200, 
                                                 height = 550, 
                                                 res = 72,
                                                 framerate = 20)
          
          incProgress(0.7, detail = "Video generation complete!")
        })
      },
      contentType = "video/mp4"
    )
    
    routing_text <- "#species_loc_drilldown?species="
    url_full <- paste0(parent_shiny_url, routing_text, values$species_code)
    
    output$parent_url <- renderUI(
      HTML(paste0('<a class="btn btn-primary" target="_blank" href="',
             url_full,
             '">Species Drilldown</a>')
    ))
    
    routing_text_file <- "#audio_file_drilldown?file="
    url_file_full <- paste0(parent_shiny_url, routing_text_file, values$file_name)
    
    output$file_page_url <- renderUI(
      HTML(paste0('<a class="btn btn-primary" target="_blank" href="',
                  url_file_full,
                  '">File Drilldown</a>'))
    )
    
    cornell_page <- paste0("https://search.macaulaylibrary.org/catalog?taxonCode=", 
                           values$species_code, "&mediaType=audio&sort=rating_rank_desc")
    output$cornell_link <- renderUI(
      HTML(paste0('<a class="btn btn-primary" target="_blank" href="',
                  cornell_page,
                  '">Macaulay Library</a>'))
    )
  })
  
  

}
  

shinyApp(ui, server)

