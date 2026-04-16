# =============================================================================
# Module: Instructions Editor
# =============================================================================
# Handles creation of the instructions page with:
# - Rich text editor (Quill-based) with full formatting options
# - HTML preview with live update
# - HTML to PNG conversion via webshot2 for export
# - Direct image upload as alternative
# =============================================================================

# -----------------------------------------------------------------------------
# UI Function
# -----------------------------------------------------------------------------
mod_instructions_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    br(),
    
    h4("Instructions Page"),
    helpText("Create the instructions shown before the listening test."),
    
    # Checkbox to enable/disable instructions
    checkboxInput(
      ns("display_instructions"),
      "Display instructions before running task",
      value = TRUE
    ),
    
    hr(),
    
    # Content shown only when instructions are enabled
    conditionalPanel(
      condition = sprintf("input['%s'] == true", ns("display_instructions")),
      
      # Toggle between editor and image upload
      radioButtons(
        ns("input_mode"),
        "Input method:",
        choices = c(
          "Rich text editor" = "editor",
          "Upload image" = "image"
        ),
        selected = "editor",
        inline = TRUE
      ),
      
      # =========================================================================
      # Rich text editor mode
      # =========================================================================
      conditionalPanel(
        condition = sprintf("input['%s'] == 'editor'", ns("input_mode")),
        
        # Color controls
        fluidRow(
          column(
            6,
            colourInput(
              ns("bg_color"),
              "Background color:",
              value = "#FFFFE0",
              showColour = "both"
            )
          ),
          column(
            6,
            colourInput(
              ns("default_text_color"),
              "Default text color:",
              value = "#000000",
              showColour = "both"
          )
        )
      ),
      
      br(),
      
      # Quill editor container with background color applied via CSS
      div(
        id = ns("editor_wrapper"),
        style = "border: 1px solid #ccc; margin-bottom: 10px;",
        
        # Full toolbar
        div(
          id = ns("quill_toolbar"),
          style = "background: #f8f8f8; border-bottom: 1px solid #ccc;",
          # Font family
          tags$select(
            class = "ql-font",
            tags$option(value = "sans-serif", selected = TRUE),
            tags$option(value = "serif"),
            tags$option(value = "monospace")
          ),
          # Font size - using explicit pixel values
          tags$select(
            class = "ql-size",
            tags$option(value = "12px", "Small"),
            tags$option(value = "16px", selected = TRUE, "Normal"),
            tags$option(value = "24px", "Large"),
            tags$option(value = "32px", "Huge")
          ),
          # Text formatting
          tags$button(class = "ql-bold", title = "Bold"),
          tags$button(class = "ql-italic", title = "Italic"),
          tags$button(class = "ql-underline", title = "Underline"),
          tags$button(class = "ql-strike", title = "Strikethrough"),
          # Text color
          tags$select(class = "ql-color"),
          # Alignment
          tags$select(
            class = "ql-align",
            tags$option(value = "", selected = TRUE),
            tags$option(value = "center"),
            tags$option(value = "right"),
            tags$option(value = "justify")
          ),
          # Lists
          tags$button(class = "ql-list", value = "bullet", title = "Bullet list"),
          tags$button(class = "ql-list", value = "ordered", title = "Numbered list"),
          # Sub/superscript
          tags$button(class = "ql-script", value = "sub", title = "Subscript"),
          tags$button(class = "ql-script", value = "super", title = "Superscript"),
          # Clear
          tags$button(class = "ql-clean", title = "Clear formatting")
        ),
        
        # Editor area - use default colors matching Button background and Text color
        div(
          id = ns("quill_editor"),
          style = "min-height: 200px; font-size: 16px; background-color: #FFFFE0 !important;"
        )
      ),
      
      # CSS to ensure all Quill elements have correct colors
      tags$style(HTML(sprintf("
        #%s {
          background-color: #FFFFE0 !important;
        }
        #%s .ql-container {
          background-color: #FFFFE0 !important;
          border: none !important;
        }
        #%s .ql-editor {
          background-color: #FFFFE0 !important;
          color: #000000 !important;
          min-height: 180px;
        }
        #%s .ql-editor.ql-blank::before {
          color: #000000 !important;
          opacity: 0.5;
          font-style: italic;
        }
      ", ns("quill_editor"), ns("quill_editor"), ns("quill_editor"), ns("quill_editor")))),
      
      # Hidden inputs to store Quill content
      tags$input(type = "hidden", id = ns("editor_html"), value = ""),
      tags$input(type = "hidden", id = ns("editor_text"), value = ""),
      
      # Include Quill CSS and JS
      tags$head(
        tags$link(
          rel = "stylesheet",
          href = "https://cdn.quilljs.com/1.3.7/quill.snow.css"
        ),
        tags$script(src = "https://cdn.quilljs.com/1.3.7/quill.min.js")
      ),
      
      # Initialize Quill with proper timing for color sync
      tags$script(HTML(sprintf("
        $(document).ready(function() {
          // Function to update editor colors
          var updateEditorColors = function(bgColor, textColor) {
            var container = document.getElementById('%s');
            var qlContainer = document.querySelector('#%s .ql-container');
            var editorEl = document.querySelector('#%s .ql-editor');
            
            if (container) container.style.backgroundColor = bgColor;
            if (qlContainer) qlContainer.style.backgroundColor = bgColor;
            if (editorEl) {
              editorEl.style.backgroundColor = bgColor;
              editorEl.style.color = textColor;
            }
            
            // Update dynamic CSS
            var styleId = '%s_dynamic_style';
            var styleEl = document.getElementById(styleId);
            if (!styleEl) {
              styleEl = document.createElement('style');
              styleEl.id = styleId;
              document.head.appendChild(styleEl);
            }
            styleEl.textContent = 
              '#%s { background-color: ' + bgColor + ' !important; }' +
              '#%s .ql-container { background-color: ' + bgColor + ' !important; }' +
              '#%s .ql-editor { background-color: ' + bgColor + ' !important; color: ' + textColor + ' !important; }' +
              '#%s .ql-editor.ql-blank::before { color: ' + textColor + ' !important; opacity: 0.5; }';
            
            if (window.quillEditor_%s) {
              window.quillEditor_%s.format('color', textColor);
            }
          };
          
          // Initialize Quill editor
          var initQuill = function() {
            if (typeof Quill === 'undefined') {
              setTimeout(initQuill, 100);
              return;
            }
            
            if (window.quillEditor_%s) {
              return;
            }
            
            // Register fonts
            var Font = Quill.import('formats/font');
            Font.whitelist = ['sans-serif', 'serif', 'monospace'];
            Quill.register(Font, true);
            
            // Register sizes with pixel values
            var SizeStyle = Quill.import('attributors/style/size');
            SizeStyle.whitelist = ['12px', '16px', '24px', '32px'];
            Quill.register(SizeStyle, true);
            
            var quill = new Quill('#%s', {
              modules: {
                toolbar: '#%s'
              },
              theme: 'snow',
              placeholder: 'Enter your instructions here...'
            });
            
            window.quillEditor_%s = quill;
            
            // Apply initial colors from inputs after delay
            setTimeout(function() {
              var bgInput = document.querySelector('#%s input');
              var textInput = document.querySelector('#%s input');
              var bgColor = bgInput ? bgInput.value : '#FFFFE0';
              var textColor = textInput ? textInput.value : '#000000';
              updateEditorColors(bgColor, textColor);
            }, 200);
            
            // Sync content to Shiny
            quill.on('text-change', function() {
              var html = quill.root.innerHTML;
              var text = quill.getText();
              Shiny.setInputValue('%s', html);
              Shiny.setInputValue('%s', text);
            });
          };
          
          setTimeout(initQuill, 500);
        });
      ", 
        ns("quill_editor"), ns("quill_editor"), ns("quill_editor"),
        ns("quill_editor"),
        ns("quill_editor"), ns("quill_editor"), ns("quill_editor"), ns("quill_editor"),
        gsub("-", "_", id), gsub("-", "_", id),
        gsub("-", "_", id),
        ns("quill_editor"), ns("quill_toolbar"),
        gsub("-", "_", id),
        ns("bg_color"), ns("default_text_color"),
        ns("editor_html"), ns("editor_text")
      ))),
      
      br(),
      
      div(
        class = "alert alert-info",
        style = "padding: 8px; font-size: 12px;",
        icon("info-circle"),
        " Use the 'Live Preview' panel (right side) and select 'Instructions' to see the formatted preview."
      ),
      
      helpText(
        icon("lightbulb"),
        " The rich text editor supports fonts, sizes, colors, and formatting. ",
        "Use the toolbar to style your text."
      )
    ),
    
    # =========================================================================
    # Image upload mode
    # =========================================================================
    conditionalPanel(
      condition = sprintf("input['%s'] == 'image'", ns("input_mode")),
      
      fileInput(
        ns("image_upload"),
        "Upload instructions image (PNG):",
        accept = c(".png", "image/png")
      ),
      
      helpText(
        "Recommended size: 800 x 600 pixels.",
        "The image will be displayed as-is in PsyToolkit."
      ),
      
      # Show uploaded image preview
      uiOutput(ns("image_preview_ui"))
    )
    
    )  # End of conditionalPanel for display_instructions
  )
}

# -----------------------------------------------------------------------------
# Server Function
# -----------------------------------------------------------------------------
mod_instructions_server <- function(id, instructions_data, settings_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # -------------------------------------------------------------------------
    # Internal state
    # -------------------------------------------------------------------------
    internal <- reactiveValues(
      uploaded_image_path = NULL,
      initialized = FALSE,
      last_settings_bg = NULL,
      last_settings_text = NULL
    )
    
    # -------------------------------------------------------------------------
    # Initialize colors from settings and observe changes
    # -------------------------------------------------------------------------
    observe({
      # Get current settings values
      # Instructions background should use bg_color (Button background)
      bg_col <- settings_data$bg_color %||% "#FFFFE0"
      text_col <- settings_data$text_color %||% "#000000"
      
      # Check if settings have changed (for preset changes)
      settings_changed <- !internal$initialized ||
        (!is.null(internal$last_settings_bg) && internal$last_settings_bg != bg_col) ||
        (!is.null(internal$last_settings_text) && internal$last_settings_text != text_col)
      
      if (settings_changed) {
        # Update color inputs
        updateColourInput(session, "bg_color", value = bg_col)
        updateColourInput(session, "default_text_color", value = text_col)
        
        # Update internal tracking
        internal$last_settings_bg <- bg_col
        internal$last_settings_text <- text_col
        
        # Update editor styling via JavaScript - target ALL Quill layers
        shinyjs::delay(300, {
          shinyjs::runjs(sprintf("
            // Update all Quill layers
            var container = document.getElementById('%s');
            var qlContainer = document.querySelector('#%s .ql-container');
            var editorEl = document.querySelector('#%s .ql-editor');
            
            if (container) container.style.backgroundColor = '%s';
            if (qlContainer) qlContainer.style.backgroundColor = '%s';
            if (editorEl) {
              editorEl.style.backgroundColor = '%s';
              editorEl.style.color = '%s';
            }
            
            // Update dynamic CSS
            var styleId = '%s_dynamic_style';
            var styleEl = document.getElementById(styleId);
            if (!styleEl) {
              styleEl = document.createElement('style');
              styleEl.id = styleId;
              document.head.appendChild(styleEl);
            }
            styleEl.textContent = 
              '#%s { background-color: %s !important; }' +
              '#%s .ql-container { background-color: %s !important; }' +
              '#%s .ql-editor { background-color: %s !important; color: %s !important; }' +
              '#%s .ql-editor.ql-blank::before { color: %s !important; opacity: 0.5; }' +
              '#%s .ql-editor p { color: %s; }';
            
            // Update Quill format for new text
            if (window.quillEditor_%s) {
              window.quillEditor_%s.format('color', '%s');
              
              // Update existing text without explicit color
              var quill = window.quillEditor_%s;
              var length = quill.getLength();
              if (length > 1) {
                var delta = quill.getContents();
                var index = 0;
                delta.ops.forEach(function(op) {
                  if (op.insert && typeof op.insert === 'string') {
                    var len = op.insert.length;
                    if (!op.attributes || !op.attributes.color) {
                      quill.formatText(index, len, 'color', '%s', 'silent');
                    }
                    index += len;
                  }
                });
              }
            }
            
            console.log('Settings colors applied - bg:', '%s', 'text:', '%s');
          ", ns("quill_editor"), ns("quill_editor"), ns("quill_editor"),
             bg_col, bg_col, bg_col, text_col,
             ns("quill_editor"),
             ns("quill_editor"), bg_col,
             ns("quill_editor"), bg_col,
             ns("quill_editor"), bg_col, text_col,
             ns("quill_editor"), text_col,
             ns("quill_editor"), text_col,
             gsub("-", "_", id), gsub("-", "_", id), text_col,
             gsub("-", "_", id),
             text_col,
             bg_col, text_col))
        })
        
        internal$initialized <- TRUE
      }
    })
    
    # -------------------------------------------------------------------------
    # Sync settings to shared data
    # -------------------------------------------------------------------------
    observe({
      instructions_data$use_editor <- (input$input_mode == "editor")
      instructions_data$bg_color <- input$bg_color %||% "#FFFFE0"
      instructions_data$default_text_color <- input$default_text_color %||% "#000000"
      instructions_data$display_instructions <- input$display_instructions %||% TRUE
    })
    
    # -------------------------------------------------------------------------
    # Update editor colors when local color inputs change
    # -------------------------------------------------------------------------
    observeEvent(input$bg_color, {
      bg_col <- input$bg_color
      text_col <- input$default_text_color %||% "#000000"
      
      # Use session$sendCustomMessage for more reliable execution
      shinyjs::runjs(sprintf("
        (function() {
          var bgColor = '%s';
          var textColor = '%s';
          
          console.log('Updating Quill bg color to:', bgColor);
          
          // Get all relevant elements
          var container = document.getElementById('%s');
          var qlContainer = container ? container.querySelector('.ql-container') : null;
          var editorEl = container ? container.querySelector('.ql-editor') : null;
          
          console.log('Found elements:', {container: !!container, qlContainer: !!qlContainer, editorEl: !!editorEl});
          
          // Apply styles with !important via cssText
          if (container) {
            container.style.cssText += '; background-color: ' + bgColor + ' !important;';
          }
          if (qlContainer) {
            qlContainer.style.cssText += '; background-color: ' + bgColor + ' !important; border: none !important;';
          }
          if (editorEl) {
            editorEl.style.cssText += '; background-color: ' + bgColor + ' !important;';
          }
          
          // Also update dynamic stylesheet
          var styleId = '%s_dynamic_style';
          var styleEl = document.getElementById(styleId);
          if (!styleEl) {
            styleEl = document.createElement('style');
            styleEl.id = styleId;
            document.head.appendChild(styleEl);
          }
          styleEl.textContent = 
            '#%s { background-color: ' + bgColor + ' !important; }' +
            '#%s .ql-container { background-color: ' + bgColor + ' !important; border: none !important; }' +
            '#%s .ql-editor { background-color: ' + bgColor + ' !important; color: ' + textColor + ' !important; }' +
            '#%s .ql-editor.ql-blank::before { color: ' + textColor + ' !important; opacity: 0.5; }';
        })();
      ", bg_col, text_col,
         ns("quill_editor"),
         ns("quill_editor"),
         ns("quill_editor"),
         ns("quill_editor"),
         ns("quill_editor"),
         ns("quill_editor")))
    }, ignoreInit = TRUE)
    
    observeEvent(input$default_text_color, {
      text_col <- input$default_text_color
      bg_col <- input$bg_color %||% "#FFFFE0"
      
      shinyjs::runjs(sprintf("
        (function() {
          var textColor = '%s';
          var bgColor = '%s';
          
          console.log('Updating Quill text color to:', textColor);
          
          var container = document.getElementById('%s');
          var editorEl = container ? container.querySelector('.ql-editor') : null;
          
          if (editorEl) {
            editorEl.style.cssText += '; color: ' + textColor + ' !important;';
          }
          
          // Update dynamic stylesheet
          var styleId = '%s_dynamic_style';
          var styleEl = document.getElementById(styleId);
          if (!styleEl) {
            styleEl = document.createElement('style');
            styleEl.id = styleId;
            document.head.appendChild(styleEl);
          }
          styleEl.textContent = 
            '#%s { background-color: ' + bgColor + ' !important; }' +
            '#%s .ql-container { background-color: ' + bgColor + ' !important; }' +
            '#%s .ql-editor { background-color: ' + bgColor + ' !important; color: ' + textColor + ' !important; }' +
            '#%s .ql-editor.ql-blank::before { color: ' + textColor + ' !important; opacity: 0.5; }' +
            '#%s .ql-editor p { color: ' + textColor + ' !important; }';
          
          // Set Quill format for new text
          if (window.quillEditor_%s) {
            window.quillEditor_%s.format('color', textColor);
            
            // Update existing text without explicit color
            var quill = window.quillEditor_%s;
            var length = quill.getLength();
            if (length > 1) {
              var delta = quill.getContents();
              var index = 0;
              delta.ops.forEach(function(op) {
                if (op.insert && typeof op.insert === 'string') {
                  var len = op.insert.length;
                  if (!op.attributes || !op.attributes.color) {
                    quill.formatText(index, len, 'color', textColor, 'silent');
                  }
                  index += len;
                }
              });
            }
          }
        })();
      ", text_col, bg_col,
         ns("quill_editor"),
         ns("quill_editor"),
         ns("quill_editor"),
         ns("quill_editor"),
         ns("quill_editor"),
         ns("quill_editor"),
         ns("quill_editor"),
         gsub("-", "_", id), gsub("-", "_", id), gsub("-", "_", id)))
    }, ignoreInit = TRUE)
    
    # -------------------------------------------------------------------------
    # Sync editor content
    # -------------------------------------------------------------------------
    observe({
      html_content <- input$editor_html
      text_content <- input$editor_text
      
      if (!is.null(html_content)) {
        instructions_data$editor_html <- html_content
      }
      if (!is.null(text_content)) {
        instructions_data$editor_content <- text_content
      }
    })
    
    # -------------------------------------------------------------------------
    # Handle image upload
    # -------------------------------------------------------------------------
    observeEvent(input$image_upload, {
      req(input$image_upload)
      internal$uploaded_image_path <- input$image_upload$datapath
      instructions_data$uploaded_image <- list(
        name = input$image_upload$name,
        datapath = input$image_upload$datapath
      )
    })
    
    # -------------------------------------------------------------------------
    # Image preview UI
    # -------------------------------------------------------------------------
    output$image_preview_ui <- renderUI({
      req(internal$uploaded_image_path)
      
      tagList(
        br(),
        tags$strong("Uploaded image:"),
        br(),
        tags$img(
          src = base64enc::dataURI(
            file = internal$uploaded_image_path,
            mime = "image/png"
          ),
          style = "max-width: 100%; max-height: 300px; border: 1px solid #ccc;"
        )
      )
    })
    
  })
}

# -----------------------------------------------------------------------------
# Helper Functions
# -----------------------------------------------------------------------------

#' Create instructions image from HTML content using webshot2
#' @param html_content HTML content from Quill editor
#' @param bg_color Background color
#' @param default_text_color Default text color
#' @param output_path Output file path
#' @param img_width Image width
#' @param img_height Image height
#' @return Path to created image
create_instructions_from_html <- function(html_content, 
                                          bg_color = "#000000",
                                          default_text_color = "#FFFFFF",
                                          output_path = NULL,
                                          img_width = 800,
                                          img_height = 600) {
  
  if (is.null(output_path)) {
    output_path <- tempfile(fileext = ".png")
  }
  
  # Create HTML file with styling
  # Base font-size is 16px to match Quill's "Normal" size
  # Quill sizes: 12px (Small), 16px (Normal), 24px (Large), 32px (Huge)
  html_template <- sprintf('
<!DOCTYPE html>
<html>
<head>
  <meta charset="UTF-8">
  <style>
    * {
      margin: 0;
      padding: 0;
      box-sizing: border-box;
    }
    body {
      width: %dpx;
      height: %dpx;
      background-color: %s;
      color: %s;
      font-family: Arial, sans-serif;
      font-size: 16px;
      display: flex;
      align-items: center;
      justify-content: center;
      padding: 40px;
      overflow: hidden;
    }
    .content {
      max-width: 100%%;
      max-height: 100%%;
      width: 100%%;
      overflow: hidden;
    }
    .content p {
      margin-bottom: 0.5em;
    }
    .ql-font-serif {
      font-family: Georgia, serif !important;
    }
    .ql-font-monospace {
      font-family: "Courier New", monospace !important;
    }
    p.ql-align-center, .ql-align-center {
      text-align: center !important;
    }
    p.ql-align-right, .ql-align-right {
      text-align: right !important;
    }
    p.ql-align-justify, .ql-align-justify {
      text-align: justify !important;
    }
  </style>
</head>
<body>
  <div class="content">
    %s
  </div>
</body>
</html>
', img_width, img_height, bg_color, default_text_color, html_content)
  
  # Write HTML to temp file
  html_file <- tempfile(fileext = ".html")
  writeLines(html_template, html_file)
  
  # Use webshot2 to convert to PNG with higher resolution
  # Suppress informational messages about Chrome reconnection
  tryCatch({
    if (requireNamespace("webshot2", quietly = TRUE)) {
      suppressMessages({
        invisible(capture.output({
          webshot2::webshot(
            html_file,
            file = output_path,
            vwidth = img_width,
            vheight = img_height,
            cliprect = "viewport",
            delay = 0.5  # Small delay to ensure fonts are loaded
          )
        }, type = "message"))
      })
    } else {
      # Fallback to basic ggplot rendering if webshot2 not available
      warning("webshot2 not available, using fallback text rendering")
      return(create_instructions_fallback(html_content, bg_color, default_text_color, output_path,
                                          img_width, img_height))
    }
  }, error = function(e) {
    warning("webshot2 failed, using fallback: ", e$message)
    return(create_instructions_fallback(html_content, bg_color, default_text_color, output_path,
                                        img_width, img_height))
  })
  
  # Clean up
  unlink(html_file)
  
  return(output_path)
}

#' Fallback function to create instructions image using ggplot2
#' @param html_content HTML content (will strip tags)
#' @param bg_color Background color
#' @param text_color Text color
#' @param output_path Output file path
#' @param img_width Image width
#' @param img_height Image height
#' @return Path to created image
create_instructions_fallback <- function(html_content, bg_color, text_color, output_path,
                                         img_width = 800, img_height = 600) {
  
  # Strip HTML tags to get plain text
  text <- gsub("<br>|<br/>|</p>", "\n", html_content)
  text <- gsub("<[^>]+>", "", text)
  text <- trimws(text)
  
  if (nchar(text) == 0) {
    text <- "Instructions"
  }
  
  # Scale coordinates based on resolution
  x_scale <- img_width / 800
  y_scale <- img_height / 600
  
  p <- ggplot() +
    annotate("text", x = 0, y = 0, label = text,
             size = 8, color = text_color, lineheight = 1.2) +
    xlim(-400 * x_scale, 400 * x_scale) +
    ylim(-300 * y_scale, 300 * y_scale) +
    theme_void() +
    theme(plot.background = element_rect(fill = bg_color, color = NA))
  
  ggsave(output_path, p, width = img_width, height = img_height, units = "px", dpi = 72)
  
  return(output_path)
}

#' Create instructions image from plain text (legacy function)
#' @param text Instruction text
#' @param font_family Font family
#' @param font_size Font size in points
#' @param font_weight Font weight
#' @param font_color Text color
#' @param bg_color Background color
#' @param img_width Image width
#' @param img_height Image height
#' @return ggplot object
create_instructions_image <- function(text, 
                                      font_family = "sans",
                                      font_size = 24,
                                      font_weight = "plain",
                                      font_color = "#FFFFFF",
                                      bg_color = "#000000",
                                      img_width = 800,
                                      img_height = 600) {
  
  family_map <- c("sans" = "sans", "serif" = "serif", "mono" = "mono")
  family <- family_map[[font_family]] %||% "sans"
  
  # Ensure font_weight is valid
  valid_weights <- c("plain", "bold", "italic", "bold.italic")
  if (!font_weight %in% valid_weights) {
    font_weight <- "plain"
  }
  
  p <- ggplot() +
    annotate("text", x = 0, y = 0, label = text,
             size = font_size / 2.8, family = family,
             fontface = font_weight, color = font_color,
             lineheight = 1.2) +
    xlim(-img_width / 2, img_width / 2) +
    ylim(-img_height / 2, img_height / 2) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = bg_color, color = NA),
      panel.background = element_rect(fill = bg_color, color = NA)
    )
  
  return(p)
}

#' Save instructions image to file
#' @param instructions_data Instructions data from module
#' @param output_path Output file path
#' @return Path to saved image
save_instructions_image <- function(instructions_data, output_path, 
                                    img_width = 800, img_height = 600) {
  
  if (!(instructions_data$use_editor %||% TRUE) && !is.null(instructions_data$uploaded_image)) {
    # Copy uploaded image
    file.copy(instructions_data$uploaded_image$datapath, output_path, overwrite = TRUE)
  } else if (!is.null(instructions_data$editor_html) && 
             nchar(instructions_data$editor_html) > 0 &&
             instructions_data$editor_html != "<p><br></p>") {
    # Generate from HTML
    create_instructions_from_html(
      html_content = instructions_data$editor_html,
      bg_color = instructions_data$bg_color %||% "#000000",
      default_text_color = instructions_data$default_text_color %||% "#FFFFFF",
      output_path = output_path,
      img_width = img_width,
      img_height = img_height
    )
  } else {
    # Generate blank or with placeholder text
    text <- instructions_data$editor_content
    if (is.null(text) || nchar(trimws(text)) == 0) {
      text <- "Instructions"
    }
    
    # Scale coordinates based on resolution
    x_scale <- img_width / 800
    y_scale <- img_height / 600
    
    p <- ggplot() +
      annotate("text", x = 0, y = 0, label = text,
               size = 8, color = instructions_data$default_text_color %||% "#FFFFFF",
               lineheight = 1.2) +
      xlim(-400 * x_scale, 400 * x_scale) +
      ylim(-300 * y_scale, 300 * y_scale) +
      theme_void() +
      theme(plot.background = element_rect(
        fill = instructions_data$bg_color %||% "#000000", color = NA))
    
    ggsave(output_path, p, width = img_width, height = img_height, units = "px", dpi = 72)
  }
  
  return(output_path)
}
