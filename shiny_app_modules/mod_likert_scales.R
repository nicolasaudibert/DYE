# =============================================================================
# Module: Likert Scales Configuration
# =============================================================================
# Handles configuration of one or more Likert scales including:
# - Number of scales and points
# - Labels (text or images) for each scale
# - Item shapes/images for scale points
# - Position, spacing and timeout settings
# =============================================================================

# -----------------------------------------------------------------------------
# UI Function
# -----------------------------------------------------------------------------
mod_likert_scales_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    br(),
    
    # Enable/disable Likert scales
    checkboxInput(
      ns("enabled"),
      tags$strong("Enable Likert Scale(s)"),
      value = FALSE
    ),
    
    # Conditional panel - only show when enabled
    conditionalPanel(
      condition = sprintf("input['%s']", ns("enabled")),
      
      # Warning about PsyToolkit rate command behavior
      div(
        class = "alert alert-warning",
        style = "font-size: 12px; padding: 10px; margin-top: 10px;",
        icon("exclamation-triangle"),
        tags$strong(" Note: "),
        "Likert scales are implemented using PsyToolkit's ",
        tags$code("rate"), " command, which displays scales only during response collection. ",
        "When combining Likert scales with forced choice buttons, or using multiple Likert scales, ",
        "these elements will appear sequentially rather than simultaneously in the final experiment."
      ),
      
      hr(),
      
      # Number of scales
      numericInput(
        ns("num_scales"),
        "Number of scales:",
        value = 1,
        min = 1,
        max = 5
      ),
      
      hr(),
      
      # Global settings section
      h4("Global Settings"),
      
      # Auto spacing checkbox
      checkboxInput(
        ns("auto_spacing"),
        "Automatic vertical spacing",
        value = TRUE
      ),
      
      # Shared item style checkbox
      checkboxInput(
        ns("shared_item_style"),
        "Same item style for all scales",
        value = TRUE
      ),
      
      # Global item shape (when shared)
      conditionalPanel(
        condition = sprintf("input['%s']", ns("shared_item_style")),
        
        fluidRow(
          column(
            4,
            selectInput(
              ns("global_item_shape"),
              "Item shape:",
              choices = c(
                "Filled circle" = "disc",
                "Empty circle" = "circle",
                "Filled square" = "square_filled",
                "Empty square" = "square",
                "Triangle" = "triangle",
                "Diamond" = "diamond",
                "Custom image" = "custom"
              ),
              selected = "disc"
            )
          ),
          column(
            4,
            colourInput(
              ns("global_item_color"),
              "Item color:",
              value = "#666666",
              showColour = "background"
            )
          ),
          column(
            4,
            conditionalPanel(
              condition = sprintf("input['%s'] == 'custom'", ns("global_item_shape")),
              fileInput(
                ns("global_item_image"),
                "Upload item image:",
                accept = c(".png", "image/png")
              )
            )
          )
        )
      ),
      
      # Item size and spacing
      fluidRow(
        column(
          6,
          sliderInput(
            ns("item_size"),
            "Item size (px):",
            value = 30,
            min = 15,
            max = 60,
            step = 5
          )
        ),
        column(
          6,
          sliderInput(
            ns("item_spacing"),
            "Item spacing (px):",
            value = 20,
            min = 5,
            max = 50,
            step = 5
          )
        )
      ),
      
      # Label size (default)
      sliderInput(
        ns("label_font_size"),
        "Default label font size:",
        value = 8,
        min = 4,
        max = 12,
        step = 0.5,
        width = "100%"
      ),
      
      hr(),
      
      # Button to validate text changes
      actionButton(
        ns("validate_text"),
        "Validate text changes",
        icon = icon("check"),
        class = "btn-info btn-sm"
      ),
      
      br(), br(),
      
      # Individual scale configurations
      h4("Scale Configuration"),
      uiOutput(ns("scales_ui"))
    )
  )
}

# -----------------------------------------------------------------------------
# Server Function
# -----------------------------------------------------------------------------
mod_likert_scales_server <- function(id, likert_data, settings_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # -------------------------------------------------------------------------
    # Internal reactive values
    # -------------------------------------------------------------------------
    internal <- reactiveValues(
      scales = list(),
      global_item_image_path = NULL,
      last_settings_item_color = NULL
    )
    
    # -------------------------------------------------------------------------
    # Sync enabled state
    # -------------------------------------------------------------------------
    observeEvent(input$enabled, {
      likert_data$enabled <- input$enabled
    })
    
    # -------------------------------------------------------------------------
    # Sync global_item_color from Settings (react to Settings changes)
    # -------------------------------------------------------------------------
    observe({
      settings_color <- settings_data$likert_item_color
      req(settings_color)
      
      # Update global_item_color if it hasn't been manually changed from Settings value
      current_val <- input$global_item_color
      # Update if current is NULL, default gray, or matches a previous Settings value
      if (is.null(current_val) || current_val == "#666666" || 
          (!is.null(internal$last_settings_item_color) && current_val == internal$last_settings_item_color)) {
        updateColourInput(session, "global_item_color", value = settings_color)
      }
      internal$last_settings_item_color <- settings_color
    })
    
    # -------------------------------------------------------------------------
    # Sync global settings
    # -------------------------------------------------------------------------
    observe({
      likert_data$num_scales <- input$num_scales
      likert_data$auto_spacing <- input$auto_spacing
      likert_data$shared_item_style <- input$shared_item_style
      likert_data$global_item_shape <- input$global_item_shape
      likert_data$item_size <- input$item_size
      likert_data$item_spacing <- input$item_spacing
      likert_data$label_font_size <- input$label_font_size
    })
    
    # -------------------------------------------------------------------------
    # Handle global item image upload
    # -------------------------------------------------------------------------
    observeEvent(input$global_item_image, {
      req(input$global_item_image)
      internal$global_item_image_path <- input$global_item_image$datapath
      likert_data$global_item_image <- list(
        name = input$global_item_image$name,
        datapath = input$global_item_image$datapath
      )
    })
    
    # -------------------------------------------------------------------------
    # Generate dynamic UI for individual scales
    # -------------------------------------------------------------------------
    output$scales_ui <- renderUI({
      n <- input$num_scales
      req(n > 0)
      
      # Initialize internal scales list if needed
      for (idx in 1:n) {
        if (length(internal$scales) < idx || is.null(internal$scales[[idx]])) {
          internal$scales[[idx]] <- list(
            num_points = 7,
            left_label = "Left",
            right_label = "Right",
            left_font_size = 0,
            right_font_size = 0,
            left_use_image = FALSE,
            right_use_image = FALSE,
            left_image = NULL,
            right_image = NULL,
            y_position = 0,
            timeout = 10000,
            item_shape = "disc",
            item_image = NULL,
            label_color = settings_data$text_color %||% "#000000",
            label_bg = settings_data$bg_color %||% "#FFFFE0",
            item_color = settings_data$likert_item_color %||% "#666666"
          )
        }
      }
      
      tagList(
        lapply(1:n, function(i) {
          scale_data <- isolate(internal$scales[[i]])
          
          div(
            class = "panel panel-default",
            style = "margin-bottom: 15px;",
            
            div(
              class = "panel-heading",
              style = "padding: 8px 12px;",
              tags$strong(paste("Scale", i))
            ),
            
            div(
              class = "panel-body",
              style = "padding: 12px;",
              
              # Number of points and timeout
              fluidRow(
                column(
                  6,
                  numericInput(
                    ns(paste0("points_", i)),
                    "Points:",
                    value = scale_data$num_points %||% 7,
                    min = 2,
                    max = 11
                  )
                ),
                column(
                  6,
                  numericInput(
                    ns(paste0("timeout_", i)),
                    "Timeout (ms):",
                    value = scale_data$timeout %||% 10000,
                    min = 1000,
                    max = 99999999,
                    step = 1000
                  )
                )
              ),
              
              # Y position (only if auto spacing is off)
              conditionalPanel(
                condition = sprintf("!input['%s']", ns("auto_spacing")),
                sliderInput(
                  ns(paste0("y_position_", i)),
                  "Y position:",
                  value = min(max(scale_data$y_position %||% 0, -280), 280),
                  min = -280,
                  max = 280,
                  step = 10
                )
              ),
              
              hr(style = "margin: 10px 0;"),
              
              # Left label
              fluidRow(
                column(
                  8,
                  tags$strong("Left label:"),
                  checkboxInput(
                    ns(paste0("left_use_image_", i)),
                    "Use image",
                    value = scale_data$left_use_image %||% FALSE
                  ),
                  
                  conditionalPanel(
                    condition = sprintf("!input['%s']", ns(paste0("left_use_image_", i))),
                    textInput(
                      ns(paste0("left_label_", i)),
                      label = NULL,
                      value = scale_data$left_label %||% "Left",
                      placeholder = "Left label text"
                    ),
                    numericInput(
                      ns(paste0("left_font_size_", i)),
                      "Font size (0=default):",
                      value = scale_data$left_font_size %||% 0,
                      min = 0,
                      max = 10,
                      step = 0.5,
                      width = "140px"
                    )
                  ),
                  
                  conditionalPanel(
                    condition = sprintf("input['%s']", ns(paste0("left_use_image_", i))),
                    fileInput(
                      ns(paste0("left_image_", i)),
                      label = NULL,
                      accept = c(".png", ".jpg", ".jpeg", "image/png", "image/jpeg")
                    ),
                    uiOutput(ns(paste0("left_image_status_", i)))
                  )
                )
              ),
              
              # Right label
              fluidRow(
                column(
                  8,
                  tags$strong("Right label:"),
                  checkboxInput(
                    ns(paste0("right_use_image_", i)),
                    "Use image",
                    value = scale_data$right_use_image %||% FALSE
                  ),
                  
                  conditionalPanel(
                    condition = sprintf("!input['%s']", ns(paste0("right_use_image_", i))),
                    textInput(
                      ns(paste0("right_label_", i)),
                      label = NULL,
                      value = scale_data$right_label %||% "Right",
                      placeholder = "Right label text"
                    ),
                    numericInput(
                      ns(paste0("right_font_size_", i)),
                      "Font size (0=default):",
                      value = scale_data$right_font_size %||% 0,
                      min = 0,
                      max = 10,
                      step = 0.5,
                      width = "140px"
                    )
                  ),
                  
                  conditionalPanel(
                    condition = sprintf("input['%s']", ns(paste0("right_use_image_", i))),
                    fileInput(
                      ns(paste0("right_image_", i)),
                      label = NULL,
                      accept = c(".png", ".jpg", ".jpeg", "image/png", "image/jpeg")
                    ),
                    uiOutput(ns(paste0("right_image_status_", i)))
                  )
                )
              ),
              
              # Per-scale item style (only if not shared)
              conditionalPanel(
                condition = sprintf("!input['%s']", ns("shared_item_style")),
                
                hr(style = "margin: 10px 0;"),
                tags$strong("Item style:"),
                
                fluidRow(
                  column(
                    6,
                    selectInput(
                      ns(paste0("item_shape_", i)),
                      label = NULL,
                      choices = c(
                        "Filled circle" = "disc",
                        "Empty circle" = "circle",
                        "Filled square" = "square_filled",
                        "Empty square" = "square",
                        "Triangle" = "triangle",
                        "Diamond" = "diamond",
                        "Custom image" = "custom"
                      ),
                      selected = scale_data$item_shape %||% "disc"
                    )
                  ),
                  column(
                    6,
                    conditionalPanel(
                      condition = sprintf("input['%s'] == 'custom'", 
                                         ns(paste0("item_shape_", i))),
                      fileInput(
                        ns(paste0("item_image_", i)),
                        label = NULL,
                        accept = c(".png", "image/png")
                      )
                    )
                  )
                )
              ),
              
              hr(style = "margin: 10px 0;"),
              
              # Color settings for labels
              fluidRow(
                column(
                  4,
                  colourInput(
                    ns(paste0("label_color_", i)),
                    "Label text:",
                    value = scale_data$label_color %||% "#000000",
                    showColour = "background"
                  )
                ),
                column(
                  4,
                  colourInput(
                    ns(paste0("label_bg_", i)),
                    "Label background:",
                    value = scale_data$label_bg %||% "#FFFFE0",
                    showColour = "background"
                  )
                ),
                # Item color only shown when not using shared item style
                column(
                  4,
                  conditionalPanel(
                    condition = sprintf("!input['%s']", ns("shared_item_style")),
                    colourInput(
                      ns(paste0("item_color_", i)),
                      "Item color:",
                      value = scale_data$item_color %||% (settings_data$likert_item_color %||% "#666666"),
                      showColour = "background"
                    )
                  )
                )
              )
            )
          )
        })
      )
    })
    
    # -------------------------------------------------------------------------
    # Observe non-text settings and sync to shared data
    # -------------------------------------------------------------------------
    observe({
      n <- input$num_scales
      req(n > 0)
      
      scales <- list()
      
      for (i in 1:n) {
        # Initialize scale with current or default values
        scale <- list(
          num_points = input[[paste0("points_", i)]] %||% 7,
          timeout = input[[paste0("timeout_", i)]] %||% 10000,
          y_position = input[[paste0("y_position_", i)]] %||% 0,
          left_use_image = input[[paste0("left_use_image_", i)]] %||% FALSE,
          right_use_image = input[[paste0("right_use_image_", i)]] %||% FALSE,
          item_shape = input[[paste0("item_shape_", i)]] %||% "disc"
        )
        
        # Get colors
        label_col <- input[[paste0("label_color_", i)]]
        scale$label_color <- if (!is.null(label_col)) label_col else settings_data$text_color
        
        label_bg <- input[[paste0("label_bg_", i)]]
        scale$label_bg <- if (!is.null(label_bg)) label_bg else settings_data$bg_color
        
        # Get item color - use global if shared style, otherwise per-scale
        if (input$shared_item_style %||% TRUE) {
          # Use global item color
          global_col <- input$global_item_color
          scale$item_color <- if (!is.null(global_col) && nchar(global_col) > 0) {
            global_col
          } else {
            settings_data$likert_item_color %||% "#666666"
          }
        } else {
          # Use per-scale item color
          item_col <- input[[paste0("item_color_", i)]]
          scale$item_color <- if (!is.null(item_col) && nchar(item_col) > 0) {
            item_col
          } else {
            settings_data$likert_item_color %||% "#666666"
          }
        }
        
        # Keep labels and font sizes from internal (updated via validate button)
        if (length(internal$scales) >= i && !is.null(internal$scales[[i]])) {
          scale$left_label <- internal$scales[[i]]$left_label %||% "Left"
          scale$right_label <- internal$scales[[i]]$right_label %||% "Right"
          scale$left_font_size <- internal$scales[[i]]$left_font_size %||% 0
          scale$right_font_size <- internal$scales[[i]]$right_font_size %||% 0
        } else {
          scale$left_label <- "Left"
          scale$right_label <- "Right"
          scale$left_font_size <- 0
          scale$right_font_size <- 0
        }
        
        # Handle image uploads
        left_img <- input[[paste0("left_image_", i)]]
        if (!is.null(left_img)) {
          scale$left_image <- list(name = left_img$name, datapath = left_img$datapath)
        } else if (length(internal$scales) >= i && !is.null(internal$scales[[i]]$left_image)) {
          scale$left_image <- internal$scales[[i]]$left_image
        }
        
        right_img <- input[[paste0("right_image_", i)]]
        if (!is.null(right_img)) {
          scale$right_image <- list(name = right_img$name, datapath = right_img$datapath)
        } else if (length(internal$scales) >= i && !is.null(internal$scales[[i]]$right_image)) {
          scale$right_image <- internal$scales[[i]]$right_image
        }
        
        item_img <- input[[paste0("item_image_", i)]]
        if (!is.null(item_img)) {
          scale$item_image <- list(name = item_img$name, datapath = item_img$datapath)
        } else if (length(internal$scales) >= i && !is.null(internal$scales[[i]]$item_image)) {
          scale$item_image <- internal$scales[[i]]$item_image
        }
        
        scales[[i]] <- scale
        internal$scales[[i]] <- scale
      }
      
      likert_data$scales <- scales
    })
    
    # -------------------------------------------------------------------------
    # Validate text changes (only on button click)
    # -------------------------------------------------------------------------
    observeEvent(input$validate_text, {
      n <- input$num_scales
      req(n > 0)
      
      for (i in 1:n) {
        left <- input[[paste0("left_label_", i)]]
        right <- input[[paste0("right_label_", i)]]
        left_fs <- input[[paste0("left_font_size_", i)]]
        right_fs <- input[[paste0("right_font_size_", i)]]
        
        if (length(internal$scales) >= i) {
          internal$scales[[i]]$left_label <- left %||% "Left"
          internal$scales[[i]]$right_label <- right %||% "Right"
          internal$scales[[i]]$left_font_size <- left_fs %||% 0
          internal$scales[[i]]$right_font_size <- right_fs %||% 0
        }
      }
      
      # Trigger update
      likert_data$scales <- internal$scales
      
      showNotification("Label text and sizes updated", type = "message", duration = 2)
    })
    
    # -------------------------------------------------------------------------
    # Initialize labels on first load
    # -------------------------------------------------------------------------
    observe({
      n <- input$num_scales
      req(n > 0)
      
      # Initialize with defaults if needed
      if (length(likert_data$scales) == 0) {
        likert_data$scales <- internal$scales
      }
    }) |> bindEvent(input$num_scales, once = TRUE)
    
    # -------------------------------------------------------------------------
    # Generate image status outputs for each scale
    # -------------------------------------------------------------------------
    observe({
      n <- input$num_scales
      req(n > 0)
      
      for (i in 1:n) {
        local({
          idx <- i
          
          # Left image status
          output[[paste0("left_image_status_", idx)]] <- renderUI({
            if (length(internal$scales) >= idx && !is.null(internal$scales[[idx]]$left_image)) {
              tags$small(class = "text-success", icon("check"), 
                        internal$scales[[idx]]$left_image$name)
            }
          })
          
          # Right image status
          output[[paste0("right_image_status_", idx)]] <- renderUI({
            if (length(internal$scales) >= idx && !is.null(internal$scales[[idx]]$right_image)) {
              tags$small(class = "text-success", icon("check"), 
                        internal$scales[[idx]]$right_image$name)
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

#' Generate shape drawing function for Likert items
#' @param shape Shape type
#' @return ggplot shape code
get_shape_geom <- function(shape) {
  shape_map <- list(
    disc = 16,        # Filled circle
    circle = 1,       # Empty circle
    square_filled = 15,
    square = 0,
    triangle = 17,
    diamond = 18
  )
  
  return(shape_map[[shape]] %||% 16)
}

#' Create Likert item image
#' @param shape Shape type
#' @param size Size in pixels
#' @param color Color
#' @param output_path Output file path
#' @return Path to created image
create_likert_item_image <- function(shape, size = 30, color = "#666666", 
                                     output_path = NULL) {
  
  if (is.null(output_path)) {
    output_path <- tempfile(fileext = ".png")
  }
  
  shape_code <- get_shape_geom(shape)
  
  p <- ggplot() +
    geom_point(aes(x = 0, y = 0), shape = shape_code, size = size / 3, 
               color = color, fill = color) +
    xlim(-1, 1) +
    ylim(-1, 1) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA)
    )
  
  ggsave(output_path, p, width = size, height = size, units = "px", 
         dpi = 72, bg = "transparent")
  
  return(output_path)
}
