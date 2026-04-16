# =============================================================================
# PsyToolkit Listening Test Designer
# Main application entry point
# =============================================================================
# This Shiny application allows users to graphically design listening tests
# for deployment on the PsyToolkit platform (https://www.psytoolkit.org/)
# =============================================================================

library(shiny)
library(shinyjs)
library(shinyWidgets)
library(colourpicker)
library(DT)
library(tidyverse)
library(readxl)
library(writexl)
library(zip)
library(digest)

# Note: Occasional "[error] handle_read_frame error: websocketpp.transport:7 (End of File)"
# messages in the console are normal WebSocket disconnection events and can be safely ignored.
# They occur when the browser tab is closed or the connection is interrupted.

# -----------------------------------------------------------------------------
# Source all modules
# -----------------------------------------------------------------------------
modules_path <- file.path("shiny_app_modules")
module_files <- list.files(modules_path, pattern = "\\.R$", full.names = TRUE)
sapply(module_files, source)

# -----------------------------------------------------------------------------
# Global constants
# -----------------------------------------------------------------------------
IMG_WIDTH <- 800
IMG_HEIGHT <- 600
VERTICAL_LINE_SPACING <- 0.7

# Default colors
DEFAULT_TEXT_COLOR <- "#000000"
DEFAULT_BG_COLOR <- "#FFFFE0"
DEFAULT_IMG_BG_COLOR <- "#000000"

# Default Likert scale settings
DEFAULT_LIKERT_POINTS <- 7
DEFAULT_LIKERT_TIMEOUT <- 10000

# Default forced choice settings
DEFAULT_CHOICE_TIMEOUT <- 10000

# Audio settings
DEFAULT_AUDIO_LOOP <- FALSE
DEFAULT_AUDIO_TIMEOUT <- 10000

# -----------------------------------------------------------------------------
# UI Definition
# -----------------------------------------------------------------------------
ui <- fluidPage(
  useShinyjs(),
  
  # Application title
  titlePanel("DYE (Design Your Experiment) - PsyToolkit Listening Test Designer"),
  
  # Project controls at the top
  mod_project_ui("project"),
  
  # Main layout with sidebar
  sidebarLayout(
    sidebarPanel(
      width = 4,
      tabsetPanel(
        id = "main_tabs",
        type = "tabs",
        
        # Tab 1: Audio files management
        tabPanel(
          "Audio Files",
          icon = icon("music"),
          mod_audio_files_ui("audio")
        ),
        
        # Tab 2: Forced choice configuration
        tabPanel(
          "Forced Choice",
          icon = icon("hand-pointer"),
          mod_forced_choice_ui("forced_choice")
        ),
        
        # Tab 3: Likert scales configuration
        tabPanel(
          "Likert Scales",
          icon = icon("sliders-h"),
          mod_likert_scales_ui("likert")
        ),
        
        # Tab 4: Instructions editor
        tabPanel(
          "Instructions",
          icon = icon("info-circle"),
          mod_instructions_ui("instructions")
        ),
        
        # Tab 5: Global settings
        tabPanel(
          "Settings",
          icon = icon("cog"),
          mod_settings_ui("settings")
        )
      )
    ),
    
    mainPanel(
      width = 8,
      tabsetPanel(
        id = "preview_tabs",
        type = "tabs",
        
        # Preview tab: live preview
        tabPanel(
          "Live Preview",
          icon = icon("eye"),
          mod_preview_ui("preview")
        ),
        
        # Export tab: final preview and download
        tabPanel(
          "Export",
          icon = icon("download"),
          mod_export_ui("export")
        ),
        
        # Help tab
        tabPanel(
          "Help",
          icon = icon("question-circle"),
          uiOutput("documentation_ui")
        )
      )
    )
  )
)

# -----------------------------------------------------------------------------
# Server Definition
# -----------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # ---------------------------------------------------------------------------
  # Shared reactive values across modules
  # ---------------------------------------------------------------------------
  

  # Audio files data
  audio_data <- reactiveValues(
    files = tibble(
      filename = character(),
      has_file = logical(),
      datapath = character()
    ),
    metadata_cols = character(),
    metadata = tibble(),
    # Predefined orders support
    use_predefined_orders = FALSE,
    predefined_orders = list()  # List of data frames, one per order
  )
  
  # Forced choice configuration
  forced_choice_data <- reactiveValues(
    enabled = FALSE,
    num_choices = 4,
    num_rows = 1,
    labels = list(),
    use_images = rep(FALSE, 4),
    images = list(),
    y_position = -100,
    timeout = DEFAULT_CHOICE_TIMEOUT,
    order_position = 1  
  )
  
 # Likert scales configuration
  likert_data <- reactiveValues(
    enabled = FALSE,
    num_scales = 1,
    scales = list(
      list(
        num_points = DEFAULT_LIKERT_POINTS,
        left_label = "Left",
        right_label = "Right",
        left_use_image = FALSE,
        right_use_image = FALSE,
        left_image = NULL,
        right_image = NULL,
        y_position = 100,
        timeout = DEFAULT_LIKERT_TIMEOUT,
        item_shape = "disc",
        item_image = NULL
      )
    ),
    auto_spacing = TRUE,
    shared_item_style = TRUE,
    global_item_shape = "disc",
    global_item_image = NULL
  )
  
  # Instructions configuration
  instructions_data <- reactiveValues(
    use_editor = TRUE,
    editor_content = "",
    uploaded_image = NULL,
    font_family = "sans",
    font_size = 24,
    font_color = "#FFFFFF",
    font_weight = "normal",
    display_instructions = TRUE
  )
  
  # Global settings
  settings_data <- reactiveValues(
    text_color = DEFAULT_TEXT_COLOR,
    bg_color = DEFAULT_BG_COLOR,
    img_bg_color = DEFAULT_IMG_BG_COLOR,
    audio_loop = DEFAULT_AUDIO_LOOP,
    audio_timeout = DEFAULT_AUDIO_TIMEOUT,
    resolution = "800x600",
    fullscreen = FALSE,
    project_has_content = FALSE
  )
  
  # Generated images for export
  export_images <- reactiveValues(
    instructions = NULL,
    forced_choice = list(),
    likert_labels = list(),
    likert_items = NULL
  )
  
  # ---------------------------------------------------------------------------
  # Call modules
  # ---------------------------------------------------------------------------
  
  # Audio files module
  mod_audio_files_server("audio", audio_data)
  
  # Project module
  mod_project_server(
    "project",
    audio_data,
    forced_choice_data,
    likert_data,
    instructions_data,
    settings_data
  )
  
  # Forced choice module
  mod_forced_choice_server("forced_choice", forced_choice_data, settings_data)
  
  # Likert scales module
  mod_likert_scales_server("likert", likert_data, settings_data)
  
  # Instructions module
  mod_instructions_server("instructions", instructions_data, settings_data)
  
  # Settings module
  mod_settings_server("settings", settings_data)
  
  # Preview module
  mod_preview_server(
    "preview",
    audio_data,
    forced_choice_data,
    likert_data,
    instructions_data,
    settings_data
  )
  
  # Export module
  mod_export_server(
    "export",
    audio_data,
    forced_choice_data,
    likert_data,
    instructions_data,
    settings_data,
    export_images,
    project_name = reactive({ input$`project-project_name` })
  )
  
  # ---------------------------------------------------------------------------
  # Sync left tabs with preview page
  # ---------------------------------------------------------------------------
  observeEvent(input$main_tabs, {
    # When user selects Instructions tab, switch preview to instructions
    if (input$main_tabs == "Instructions") {
      updateSelectInput(
        session,
        inputId = "preview-preview_page",
        selected = "instructions"
      )
    }
    # When user selects Forced Choice or Likert Scales, switch to response page
    else if (input$main_tabs %in% c("Forced Choice", "Likert Scales")) {
      updateSelectInput(
        session,
        inputId = "preview-preview_page",
        selected = "response"
      )
    }
  })
  
  # ---------------------------------------------------------------------------
  # Detect if project has content (to lock resolution)
  # ---------------------------------------------------------------------------
  observe({
    has_audio <- nrow(audio_data$files) > 0
    has_fc <- forced_choice_data$enabled
    has_likert <- likert_data$enabled
    has_instructions <- !is.null(instructions_data$editor_html) && 
                        nchar(instructions_data$editor_html %||% "") > 0 &&
                        instructions_data$editor_html != "<p><br></p>"
    
    settings_data$project_has_content <- has_audio || has_fc || has_likert || has_instructions
  })
  
  # ---------------------------------------------------------------------------
  # Documentation tab content
  # ---------------------------------------------------------------------------
  output$documentation_ui <- renderUI({
    doc_content <- generate_documentation_html()
    div(
      style = "padding: 20px; max-height: 80vh; overflow-y: auto;",
      HTML(doc_content)
    )
  })
}

#' Generate documentation HTML content
#' @return HTML string with documentation
generate_documentation_html <- function() {
  md_content <- '
# DYE (Design Your Experiment) - PsyToolkit Listening Test Designer

## Overview

This application helps you create audio perception experiments for PsyToolkit without writing code. Design your experiment visually, then export a ready-to-use ZIP file.

> **Important:** This application is **not part of PsyToolkit** and covers only a very small subset of PsyToolkit capabilities. Using this tool to design an experiment **does not exempt you from the obligation to cite PsyToolkit** in your publications. Please refer to the [PsyToolkit citation guidelines](https://www.psytoolkit.org/faq.html#citations).

## Quick Start

1. **Add audio stimuli** in the Audio Files tab (import a list or upload MP3 files directly)
2. **Configure responses** using Forced Choice buttons and/or Likert scales
3. **Create instructions** for participants in the Instructions tab
4. **Preview** your experiment in the Live Preview tab
5. **Export** to PsyToolkit from the Export tab

---

## Audio Files Tab

### Import Stimulus List
Upload an Excel, CSV, or TXT file containing your stimulus filenames and optional metadata columns.

- **Single file**: Stimuli will be presented in random order
- **ZIP archive**: Import multiple files for predefined presentation orders (one order per file)

### Upload MP3 Files
Directly upload your audio files. Files not uploaded here can be added later in PsyToolkit.

### Metadata Columns
Select which columns from your list should be saved in the results file (useful for analysis).

---

## Forced Choice Tab

Configure button-based responses for categorization tasks.

- **Number of choices**: 2-8 response options
- **Layout**: Arrange buttons in rows
- **Labels**: Text or uploaded images
- **Colors**: Customize text and background colors per button

---
## Likert Scales Tab

Add rating scales for subjective judgments.

> **Note**: PsyToolkit uses the `rate` command which displays scales only during response collection. When combining with forced choice or using multiple scales, elements appear sequentially.

- **Number of scales**: Add up to 5 scales per trial
- **Scale points**: 3-10 points per scale
- **Labels**: Text or images for scale endpoints
- **Item style**: Customize selection markers (shared or per-scale)

---

## Instructions Tab

Create the instruction screen shown before the experiment.

- **Rich text editor**: Format text with fonts, sizes, colors, and alignment
- **Image upload**: Alternatively, upload a pre-made PNG image
- **Display toggle**: Optionally disable instructions for experiments embedded in surveys

---

## Settings Tab

### Display Options
- **Resolution**: Screen size (default 800×600). Lock after adding content to prevent layout issues.
- **Background color**: Image background color for PsyToolkit
- **Fullscreen**: Enable fullscreen mode

### Audio Options
- **Loop audio**: Repeat until response is given
- **Timeout**: Maximum response time

---

## Live Preview Tab

Real-time preview of your experiment layout.

- **Page selector**: Switch between Instructions and Response pages
- **Alignment grid**: Toggle grid overlay to check element positions
- **Drag elements**: Reorder Forced Choice and Likert scale positions (not yet implemented)

---

## Export Tab

Generate and download your PsyToolkit experiment.

### Validation
The application checks for:
- Audio stimuli defined
- At least one response type enabled
- Instructions content (if enabled)

### Generated Files
- `code.psy`: Main experiment script
- `instructions.png`: Instructions image
- `choice_*.png`: Forced choice button images
- `left.png`, `right.png`: Likert scale labels
- `selection_item.png`: Likert scale marker
- `stimuli_and_orders.txt`: (Only for predefined orders)

### Import to PsyToolkit
1. Create new experiment using "Method 2: From ZIP file"
2. Upload the generated ZIP
3. Compile the experiment

### Running from a Survey
To embed the experiment in a PsyToolkit survey, add:
```
l: expe
t: experiment
- psytoolkit_experiment_YourProjectName
```

---

## Tips

- Use **consistent audio file naming** for easier data analysis
- **Test your experiment** in PsyToolkit before collecting data
- For **long experiments**, consider adding breaks
- **Metadata columns** help identify conditions during analysis

---

## Limitations

- Likert scales cannot be displayed simultaneously with other elements
- Maximum 8 forced choice options, 5 Likert scales
- Audio must be MP3 format

---

## About

**DYE (Design Your Experiment)** - A Shiny application for creating audio perception experiments compatible with PsyToolkit.

This tool is an independent project and is not affiliated with or endorsed by PsyToolkit.

For PsyToolkit documentation and citation guidelines, visit: [psytoolkit.org](https://www.psytoolkit.org/)
'
  
  # Convert markdown to HTML using markdown package if available
  # Otherwise use basic conversion
  if (requireNamespace("markdown", quietly = TRUE)) {
    html <- markdown::markdownToHTML(text = md_content, fragment.only = TRUE)
  } else {
    # Basic conversion for headers and formatting
    html <- md_content
    html <- gsub("^# (.+)$", "<h1>\\1</h1>", html, perl = TRUE)
    html <- gsub("^## (.+)$", "<h2>\\1</h2>", html, perl = TRUE)
    html <- gsub("^### (.+)$", "<h3>\\1</h3>", html, perl = TRUE)
    html <- gsub("\\*\\*(.+?)\\*\\*", "<strong>\\1</strong>", html)
    html <- gsub("\\*(.+?)\\*", "<em>\\1</em>", html)
    html <- gsub("`(.+?)`", "<code>\\1</code>", html)
    html <- gsub("^- (.+)$", "<li>\\1</li>", html, perl = TRUE)
    html <- gsub("^> (.+)$", "<blockquote>\\1</blockquote>", html, perl = TRUE)
    html <- gsub("\n\n", "</p><p>", html)
    html <- paste0("<p>", html, "</p>")
    html <- gsub("---", "<hr>", html)
  }
  
  # Add styling
  styled_html <- sprintf('
    <style>
      .doc-container h1 { color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 10px; }
      .doc-container h2 { color: #34495e; margin-top: 25px; }
      .doc-container h3 { color: #7f8c8d; }
      .doc-container code { background-color: #f4f4f4; padding: 2px 6px; border-radius: 3px; font-family: monospace; }
      .doc-container pre { background-color: #f4f4f4; padding: 10px; border-radius: 5px; overflow-x: auto; }
      .doc-container blockquote { border-left: 4px solid #f39c12; padding-left: 15px; color: #7f8c8d; margin: 15px 0; }
      .doc-container ul { padding-left: 25px; }
      .doc-container li { margin: 5px 0; }
      .doc-container hr { border: none; border-top: 1px solid #eee; margin: 20px 0; }
      .doc-container a { color: #3498db; }
    </style>
    <div class="doc-container">%s</div>
  ', html)
  
  return(styled_html)
}

# -----------------------------------------------------------------------------
# Run Application
# -----------------------------------------------------------------------------
shinyApp(ui = ui, server = server)
