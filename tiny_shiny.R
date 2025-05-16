
# Set root folder for audio files
audio_filepath <- "/Users/laure/Dropbox/Lauren Wick/"


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
  
  plotOutput("spectrogram") %>% withSpinner(),
  uiOutput("audio_player") %>% withSpinner(),
  downloadButton("audioDownload", "Download Audio")
  )

# Server Code:
server <- function(input, output, session) {
  values <- reactiveValues(
    species_name = NULL,
    species_code = NULL,
    conf = NULL,
    loc = NULL,
    file_name = NULL,
    begin_time = NULL
  )
  
  observe({
    query <- parseQueryString(session$clientData$url_search)
    
    values$species_name <- query[["species_name"]]
    values$species_code <- query[["species_code"]]
    values$conf <- query[["conf"]]
    values$loc <- query[["loc"]]
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
  
  
  observeEvent(c(input$recording_length, input$recording_offset), {
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
        div(HTML("<strong>File Name:</strong> "), values$file_name)
      )
      
    )
    
    # Extract recording length and offset parameters from widgets
    recording_secs <- input$recording_length
    recording_offset <- input$recording_offset
    
    # Create www/ directory for storing / playing audio
    if (!dir.exists("www")) {
      dir.create("www")
    }
    
    # Create new file name by appending begin time to original wav file name
    audio_src <- paste0(sub("\\.wav$", "", basename(values$file_name)), "_", values$begin_time, "s.wav")
    dest_path <- file.path("www", audio_src)
    
    # Stream audio clip and rewrite
    output_wav <- av_audio_convert(audio = og_file,
                                   output = dest_path,
                                   start_time = as.numeric(values$begin_time) - as.numeric(recording_offset),
                                   total_time = as.numeric(recording_secs))
    
    # Generate audio tag
    output$audio_player <- renderUI({
      tags$audio(src = audio_src,
                 type = "audio/wav",
                 controls = NA,
                 autoplay = NA)
    })
    
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
  })
  

}
  

shinyApp(ui, server)

