# =============================================================================
# Module: Forced Choice Configuration
# =============================================================================
# Handles configuration of forced choice response options including:
# - Number of choices and their arrangement (rows/columns)
# - Text labels or image uploads for each option
# - Position and timeout settings
# =============================================================================

# -----------------------------------------------------------------------------
# UI Function
# -----------------------------------------------------------------------------
mod_forced_choice_ui <- function(id) {

  ns <- NS(id)
  
  tagList(
    br(),
    
    # Enable/disable forced choice
    checkboxInput(
      ns("enabled"),
      tags$strong("Enable Forced Choice"),
      value = FALSE
    ),
    
    # Conditional panel - only show when enabled
    conditionalPanel(
      condition = sprintf("input['%s']", ns("enabled")),
      
      hr(),
      
      # Number of choices
      fluidRow(
        column(
          6,
          numericInput(
            ns("num_choices"),
            "Number of choices:",
            value = 4,
            min = 2,
            max = 10
          )
        ),
        column(
          6,
          numericInput(
            ns("num_rows"),
            "Number of rows:",
            value = 1,
            min = 1,
            max = 5
          )
        )
      ),
      
      # Spacing settings
      fluidRow(
        column(
          6,
          sliderInput(
            ns("h_spacing"),
            "Horizontal spacing:",
            value = 50,
            min = 10,
            max = 200,
            step = 10
          )
        ),
        column(
          6,
          sliderInput(
            ns("v_spacing"),
            "Vertical spacing:",
            value = 50,
            min = 10,
            max = 150,
            step = 10
          )
        )
      ),
      
      # Box height
      sliderInput(
        ns("box_height"),
        "Button height (px):",
        value = 80,
        min = 40,
        max = 200,
        step = 10,
        width = "100%"
      ),
      
      # Timeout
      numericInput(
        ns("timeout"),
        "Timeout (ms):",
        value = 10000,
        min = 1000,
        max = 99999,
        step = 1000
      ),
      
      hr(),
      
      # Choice labels section
      h4("Response Options"),
      helpText("Enter text labels or upload images for each choice:"),
      
      # Button to validate text changes
      actionButton(
        ns("validate_text"),
        "Validate text changes",
        icon = icon("check"),
        class = "btn-info btn-sm"
      ),
      
      br(), br(),
      
      # Dynamic UI for choice inputs
      uiOutput(ns("choice_inputs_ui"))
    )
  )
}

# -----------------------------------------------------------------------------
# Server Function
# -----------------------------------------------------------------------------
mod_forced_choice_server <- function(id, forced_choice_data, settings_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # -------------------------------------------------------------------------
    # Internal reactive values
    # -------------------------------------------------------------------------
    internal <- reactiveValues(
      labels = list(),
      use_images = list(),
      images = list(),
      text_colors = list(),
      bg_colors = list(),
      font_sizes = list()  # For manual font size override
    )
    
    # -------------------------------------------------------------------------
    # Sync enabled state
    # -------------------------------------------------------------------------
    observeEvent(input$enabled, {
      forced_choice_data$enabled <- input$enabled
    })
    
    # -------------------------------------------------------------------------
    # Sync basic settings (non-text, reactive)
    # -------------------------------------------------------------------------
    observe({
      forced_choice_data$num_choices <- input$num_choices
      forced_choice_data$num_rows <- input$num_rows
      forced_choice_data$timeout <- input$timeout
      forced_choice_data$h_spacing <- input$h_spacing
      forced_choice_data$v_spacing <- input$v_spacing
      forced_choice_data$box_height <- input$box_height
    })
    
    # -------------------------------------------------------------------------
    # Update num_rows max based on num_choices
    # -------------------------------------------------------------------------
    observeEvent(input$num_choices, {
      updateNumericInput(
        session,
        "num_rows",
        max = input$num_choices,
        value = min(input$num_rows, input$num_choices)
      )
    })
    
    # -------------------------------------------------------------------------
    # Generate dynamic UI for choice inputs
    # -------------------------------------------------------------------------
    output$choice_inputs_ui <- renderUI({
      n <- input$num_choices
      req(n > 0)
      
      # Initialize internal lists if needed
      for (i in 1:n) {
        if (length(internal$labels) < i || is.null(internal$labels[[i]])) {
          internal$labels[[i]] <- paste("Choice", i)
        }
        if (length(internal$use_images) < i) {
          internal$use_images[[i]] <- FALSE
        }
        if (length(internal$text_colors) < i || is.null(internal$text_colors[[i]])) {
          internal$text_colors[[i]] <- settings_data$text_color %||% "#000000"
        }
        if (length(internal$bg_colors) < i || is.null(internal$bg_colors[[i]])) {
          internal$bg_colors[[i]] <- settings_data$bg_color %||% "#FFFFE0"
        }
        if (length(internal$font_sizes) < i) {
          internal$font_sizes[[i]] <- 0  # 0 = auto
        }
      }
      
      tagList(
        lapply(1:n, function(i) {
          div(
            class = "well well-sm",
            style = "margin-bottom: 10px; padding: 10px;",
            
            fluidRow(
              column(
                7,
                # Text input or file upload toggle
                checkboxInput(
                  ns(paste0("use_image_", i)),
                  paste("Choice", i, "- Use image"),
                  value = isolate(internal$use_images[[i]] %||% FALSE)
                ),
                
                # Conditional: text input
                conditionalPanel(
                  condition = sprintf("!input['%s']", ns(paste0("use_image_", i))),
                  textAreaInput(
                    ns(paste0("label_", i)),
                    label = NULL,
                    value = isolate(internal$labels[[i]] %||% paste("Choice", i)),
                    rows = 2,
                    width = "100%",
                    placeholder = "Enter label text..."
                  ),
                  # Font size override
                  numericInput(
                    ns(paste0("fontsize_", i)),
                    "Font size (0=auto):",
                    value = isolate(internal$font_sizes[[i]] %||% 0),
                    min = 0,
                    max = 20,
                    step = 0.5,
                    width = "120px"
                  )
                ),
                
                # Conditional: image upload
                conditionalPanel(
                  condition = sprintf("input['%s']", ns(paste0("use_image_", i))),
                  fileInput(
                    ns(paste0("image_", i)),
                    label = NULL,
                    accept = c(".png", ".jpg", ".jpeg", "image/png", "image/jpeg"),
                    placeholder = "Upload image"
                  ),
                  uiOutput(ns(paste0("image_status_", i)))
                )
              ),
              column(
                5,
                # Per-choice color settings using colourInput
                colourInput(
                  ns(paste0("text_color_", i)),
                  "Text:",
                  value = isolate(internal$text_colors[[i]] %||% "#000000"),
                  showColour = "background"
                ),
                colourInput(
                  ns(paste0("bg_color_", i)),
                  "Background:",
                  value = isolate(internal$bg_colors[[i]] %||% "#FFFFE0"),
                  showColour = "background"
                )
              )
            )
          )
        })
      )
    })
    
    # -------------------------------------------------------------------------
    # Observe non-text settings changes (reactive)
    # -------------------------------------------------------------------------
    observe({
      n <- input$num_choices
      req(n > 0)
      
      use_images <- list()
      text_colors <- list()
      bg_colors <- list()
      
      for (i in 1:n) {
        # Get use_image checkbox state
        use_img <- input[[paste0("use_image_", i)]]
        use_images[[i]] <- use_img %||% FALSE
        internal$use_images[[i]] <- use_images[[i]]
        
        # Get colors (using colourInput which works with Shiny)
        text_col <- input[[paste0("text_color_", i)]]
        if (!is.null(text_col)) {
          text_colors[[i]] <- text_col
          internal$text_colors[[i]] <- text_col
        } else {
          # Safe access to internal list
          if (length(internal$text_colors) >= i && !is.null(internal$text_colors[[i]])) {
            text_colors[[i]] <- internal$text_colors[[i]]
          } else {
            text_colors[[i]] <- settings_data$text_color %||% "#000000"
          }
        }
        
        bg_col <- input[[paste0("bg_color_", i)]]
        if (!is.null(bg_col)) {
          bg_colors[[i]] <- bg_col
          internal$bg_colors[[i]] <- bg_col
        } else {
          # Safe access to internal list
          if (length(internal$bg_colors) >= i && !is.null(internal$bg_colors[[i]])) {
            bg_colors[[i]] <- internal$bg_colors[[i]]
          } else {
            bg_colors[[i]] <- settings_data$bg_color %||% "#FFFFE0"
          }
        }
        
        # Get uploaded image
        img_input <- input[[paste0("image_", i)]]
        if (!is.null(img_input)) {
          internal$images[[i]] <- list(
            name = img_input$name,
            datapath = img_input$datapath
          )
        }
      }
      
      # Update shared data (font_sizes from internal, updated via validate button)
      forced_choice_data$use_images <- use_images
      forced_choice_data$images <- internal$images
      forced_choice_data$text_colors <- text_colors
      forced_choice_data$bg_colors <- bg_colors
      forced_choice_data$font_sizes <- internal$font_sizes
    })
    
    # -------------------------------------------------------------------------
    # Validate text changes (only on button click)
    # -------------------------------------------------------------------------
    observeEvent(input$validate_text, {
      n <- input$num_choices
      req(n > 0)
      
      labels <- list()
      font_sizes <- list()
      
      for (i in 1:n) {
        label <- input[[paste0("label_", i)]]
        labels[[i]] <- label %||% paste("Choice", i)
        internal$labels[[i]] <- labels[[i]]
        
        fs <- input[[paste0("fontsize_", i)]]
        font_sizes[[i]] <- fs %||% 0
        internal$font_sizes[[i]] <- font_sizes[[i]]
      }
      
      forced_choice_data$labels <- labels
      forced_choice_data$font_sizes <- font_sizes
      
      showNotification("Text labels and sizes updated", type = "message", duration = 2)
    })
    
    # -------------------------------------------------------------------------
    # Initialize labels on first load
    # -------------------------------------------------------------------------
    observe({
      n <- input$num_choices
      req(n > 0)
      
      # Only initialize if labels are empty
      if (length(forced_choice_data$labels) == 0) {
        labels <- list()
        for (i in 1:n) {
          # Safe access to internal labels
          if (length(internal$labels) >= i && !is.null(internal$labels[[i]])) {
            labels[[i]] <- internal$labels[[i]]
          } else {
            labels[[i]] <- paste("Choice", i)
          }
        }
        forced_choice_data$labels <- labels
      }
    }) |> bindEvent(input$num_choices, once = TRUE)
    
    # -------------------------------------------------------------------------
    # Generate image status outputs
    # -------------------------------------------------------------------------
    observe({
      n <- input$num_choices
      req(n > 0)
      
      for (i in 1:n) {
        local({
          idx <- i
          output[[paste0("image_status_", idx)]] <- renderUI({
            img <- internal$images[[idx]]
            if (!is.null(img)) {
              tags$small(
                class = "text-success",
                icon("check"),
                img$name
              )
            }
          })
        })
      }
    })
    
  })
}

# -----------------------------------------------------------------------------
# Helper Functions
# -----------------------------------------------------------------------------

#' Calculate layout for forced choice buttons
#' @param n_choices Total number of choices
#' @param n_rows Number of rows
#' @param img_width Image width in pixels
#' @param h_spacing Horizontal spacing
#' @param v_spacing Vertical spacing
#' @param box_height Button height
#' @param y_center Y center position
#' @return Data frame with layout coordinates
calculate_forced_choice_layout <- function(n_choices, n_rows, img_width = 800,
                                           h_spacing = 50, v_spacing = 50,
                                           box_height = 80, y_center = -100) {
  
  # Calculate number of choices per row
  base_per_row <- n_choices %/% n_rows
  remainder <- n_choices %% n_rows
  
  # Distribute extras to top rows
  choices_per_row <- rep(base_per_row, n_rows)
  if (remainder > 0) {
    for (i in 1:remainder) {
      choices_per_row[i] <- choices_per_row[i] + 1
    }
  }
  
  # Calculate box width based on max choices in a row
  max_per_row <- max(choices_per_row)
  box_width <- (img_width - h_spacing * (max_per_row + 1)) / max_per_row
  
  # Calculate total height of the forced choice block
  total_height <- n_rows * box_height + (n_rows - 1) * v_spacing
  
  # Calculate Y positions for each row (centered on y_center)
  y_top <- y_center + total_height / 2 - box_height / 2
  
  # Build layout data frame
  layout <- tibble(
    choice_id = integer(),
    row = integer(),
    col = integer(),
    x = numeric(),
    y = numeric(),
    xmin = numeric(),
    xmax = numeric(),
    ymin = numeric(),
    ymax = numeric(),
    width = numeric(),
    height = numeric()
  )
  
  choice_idx <- 1
  for (row in 1:n_rows) {
    n_in_row <- choices_per_row[row]
    row_width <- n_in_row * box_width + (n_in_row - 1) * h_spacing
    x_start <- -row_width / 2 + box_width / 2
    
    y_pos <- y_top - (row - 1) * (box_height + v_spacing)
    
    for (col in 1:n_in_row) {
      x_pos <- x_start + (col - 1) * (box_width + h_spacing)
      
      layout <- layout %>%
        add_row(
          choice_id = choice_idx,
          row = row,
          col = col,
          x = x_pos,
          y = y_pos,
          xmin = x_pos - box_width / 2,
          xmax = x_pos + box_width / 2,
          ymin = y_pos - box_height / 2,
          ymax = y_pos + box_height / 2,
          width = box_width,
          height = box_height
        )
      
      choice_idx <- choice_idx + 1
    }
  }
  
  return(layout)
}

#' Calculate total height of forced choice block
#' @param n_rows Number of rows
#' @param box_height Height of each button
#' @param v_spacing Vertical spacing between rows
#' @return Total height in pixels
calculate_fc_total_height <- function(n_rows, box_height = 80, v_spacing = 50) {
  n_rows * box_height + (n_rows - 1) * v_spacing
}
