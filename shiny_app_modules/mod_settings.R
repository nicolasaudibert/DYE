# =============================================================================
# Module: Global Settings
# =============================================================================
# Handles global configuration settings including:
# - Default colors for text, backgrounds, and buttons
# - Audio playback settings (loop, timeout)
# - Export settings
# =============================================================================

# -----------------------------------------------------------------------------
# UI Function
# -----------------------------------------------------------------------------
mod_settings_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    br(),
    
    # -------------------------------------------------------------------------
    # Color Settings Section
    # -------------------------------------------------------------------------
    h4("Default Colors"),
    helpText("These colors will be used as defaults for all elements."),
    
    fluidRow(
      column(
        3,
        colourInput(
          ns("text_color"),
          "Text color:",
          value = "#000000",
          showColour = "both"
        )
      ),
      column(
        3,
        colourInput(
          ns("bg_color"),
          "Button background:",
          value = "#FFFFE0",
          showColour = "both"
        )
      ),
      column(
        3,
        colourInput(
          ns("img_bg_color"),
          "Image background:",
          value = "#000000",
          showColour = "both"
        )
      ),
      column(
        3,
        colourInput(
          ns("likert_item_color"),
          "Likert item color:",
          value = "#FFFFE0",
          showColour = "both"
        )
      )
    ),
    
    br(),
    
    # Quick color presets
    tags$details(
      tags$summary(icon("magic"), " Color presets"),
      br(),
      fluidRow(
        column(
          3,
          actionButton(
            ns("preset_light"),
            "Light",
            class = "btn-sm btn-block",
            style = "background: #FFFFFF; color: #000000; border: 1px solid #ccc;"
          )
        ),
        column(
          3,
          actionButton(
            ns("preset_dark"),
            "Dark",
            class = "btn-sm btn-block",
            style = "background: #2C2C2C; color: #FFFFFF;"
          )
        ),
        column(
          3,
          actionButton(
            ns("preset_classic"),
            "Classic",
            class = "btn-sm btn-block",
            style = "background: #FFFFE0; color: #000000; border: 1px solid #ccc;"
          )
        ),
        column(
          3,
          actionButton(
            ns("preset_blue"),
            "Blue",
            class = "btn-sm btn-block",
            style = "background: #1E3A5F; color: #FFFFFF;"
          )
        )
      )
    ),
    
    hr(),
    
    # -------------------------------------------------------------------------
    # Audio Settings Section
    # -------------------------------------------------------------------------
    h4("Audio Settings"),
    
    # Loop audio
    checkboxInput(
      ns("audio_loop"),
      "Loop audio playback",
      value = FALSE
    ),
    helpText("If enabled, stimuli will play continuously until the participant responds."),
    
    br(),
    
    # Global timeout (informational)
    helpText(
      tags$em(
        "Note: Response timeouts are configured separately for forced choice ",
        "and Likert scales in their respective tabs."
      )
    ),
    
    hr(),
    
    # -------------------------------------------------------------------------
    # Display Settings Section
    # -------------------------------------------------------------------------
    h4("Display Settings"),
    
    fluidRow(
      column(
        6,
        # Resolution
        selectInput(
          ns("resolution"),
          "Resolution:",
          choices = c(
            "800 x 600 (default)" = "800x600",
            "1024 x 768" = "1024x768",
            "1280 x 720 (HD)" = "1280x720",
            "1280 x 800" = "1280x800",
            "1920 x 1080 (Full HD)" = "1920x1080"
          ),
          selected = "800x600"
        )
      ),
      column(
        6,
        # Fullscreen option
        div(style = "margin-top: 25px;",
          checkboxInput(
            ns("fullscreen"),
            "Fullscreen display in PsyToolkit",
            value = FALSE
          )
        )
      )
    ),
    helpText(
      "Resolution can only be changed for new projects. ",
      "Fullscreen mode displays the experiment in full browser window."
    ),
    div(
      class = "alert alert-warning",
      style = "font-size: 12px; padding: 8px 12px; margin-top: 10px;",
      icon("flask"),
      tags$strong(" Experimental: "),
      "Using a resolution other than the default 800×600 is still experimental. ",
      "Please check that the display in the Live Preview and in PsyToolkit matches your expectations."
    ),
    
    hr(),
    
    # -------------------------------------------------------------------------
    # Export Settings Section
    # -------------------------------------------------------------------------
    h4("Export Settings"),
    
    # Task name
    textInput(
      ns("task_name"),
      "Task name (for PSY code):",
      value = "evaluate_audio_stimuli",
      placeholder = "evaluate_audio_stimuli"
    ),
    helpText("Used as the task identifier in the generated PsyToolkit code."),
    
    # Block name
    textInput(
      ns("block_name"),
      "Block name:",
      value = "test",
      placeholder = "test"
    ),
    
    hr(),
    
    # -------------------------------------------------------------------------
    # Information Section
    # -------------------------------------------------------------------------
    h4("About"),
    
    div(
      class = "well",
      style = "font-size: 12px;",
      tags$p(
        tags$strong("PsyToolkit Listening Test Designer"),
        br(),
        "A Shiny application for creating listening tests ",
        "compatible with the PsyToolkit platform."
      ),
      tags$p(
        "Generated files include:",
        tags$ul(
          tags$li(tags$code("code.psy"), " - PsyToolkit experiment script"),
          tags$li("PNG images for instructions and response buttons"),
          tags$li(tags$code("INFO.txt"), " - Generation information")
        )
      ),
      tags$p(
        "For more information about PsyToolkit, visit: ",
        tags$a(
          href = "https://www.psytoolkit.org/",
          target = "_blank",
          "psytoolkit.org"
        )
      )
    )
  )
}

# -----------------------------------------------------------------------------
# Server Function
# -----------------------------------------------------------------------------
mod_settings_server <- function(id, settings_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # -------------------------------------------------------------------------
    # Sync color settings
    # -------------------------------------------------------------------------
    observe({
      settings_data$text_color <- input$text_color %||% "#000000"
      settings_data$bg_color <- input$bg_color %||% "#FFFFE0"
      settings_data$img_bg_color <- input$img_bg_color %||% "#000000"
      settings_data$likert_item_color <- input$likert_item_color %||% "#FFFFE0"
    })
    
    # -------------------------------------------------------------------------
    # Sync audio settings
    # -------------------------------------------------------------------------
    observe({
      settings_data$audio_loop <- input$audio_loop %||% FALSE
    })
    
    # -------------------------------------------------------------------------
    # Sync export settings
    # -------------------------------------------------------------------------
    observe({
      settings_data$task_name <- input$task_name %||% "evaluate_audio_stimuli"
      settings_data$block_name <- input$block_name %||% "test"
    })
    
    # -------------------------------------------------------------------------
    # Sync display settings
    # -------------------------------------------------------------------------
    observe({
      settings_data$resolution <- input$resolution %||% "800x600"
      settings_data$fullscreen <- input$fullscreen %||% FALSE
    })
    
    # -------------------------------------------------------------------------
    # Lock resolution after project has content
    # -------------------------------------------------------------------------
    observe({
      # Check if project has any content (this will be passed from app.R)
      has_content <- settings_data$project_has_content %||% FALSE
      
      if (has_content) {
        shinyjs::disable("resolution")
      } else {
        shinyjs::enable("resolution")
      }
    })
    
    # -------------------------------------------------------------------------
    # Color presets
    # -------------------------------------------------------------------------
    
    # Light preset
    observeEvent(input$preset_light, {
      updateColourInput(session, "text_color", value = "#000000")
      updateColourInput(session, "bg_color", value = "#FFFFFF")
      updateColourInput(session, "img_bg_color", value = "#F5F5F5")
      updateColourInput(session, "likert_item_color", value = "#FFFFFF")
    })
    
    # Dark preset
    observeEvent(input$preset_dark, {
      updateColourInput(session, "text_color", value = "#FFFFFF")
      updateColourInput(session, "bg_color", value = "#444444")
      updateColourInput(session, "img_bg_color", value = "#2C2C2C")
      updateColourInput(session, "likert_item_color", value = "#444444")
    })
    
    # Classic preset
    observeEvent(input$preset_classic, {
      updateColourInput(session, "text_color", value = "#000000")
      updateColourInput(session, "bg_color", value = "#FFFFE0")
      updateColourInput(session, "img_bg_color", value = "#000000")
      updateColourInput(session, "likert_item_color", value = "#FFFFE0")
    })
    
    # Blue preset
    observeEvent(input$preset_blue, {
      updateColourInput(session, "text_color", value = "#FFFFFF")
      updateColourInput(session, "bg_color", value = "#4A90D9")
      updateColourInput(session, "img_bg_color", value = "#1E3A5F")
      updateColourInput(session, "likert_item_color", value = "#4A90D9")
    })
    
  })
}
