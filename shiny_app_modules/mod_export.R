# =============================================================================
# Module: Export
# =============================================================================
# Handles generation of PsyToolkit experiment files:
# - code.psy script generation
# - PNG images for all visual elements
# - ZIP archive packaging
# =============================================================================

# -----------------------------------------------------------------------------
# UI Function
# -----------------------------------------------------------------------------
mod_export_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    br(),
    
    h4("Export PsyToolkit Experiment"),
    
    # Validation status
    uiOutput(ns("validation_ui")),
    
    hr(),
    
    # Preview generated images
    h5("Generated Images Preview"),
    
    tabsetPanel(
      id = ns("image_preview_tabs"),
      type = "pills",
      
      tabPanel(
        "Instructions",
        br(),
        uiOutput(ns("preview_instructions"))
      ),
      
      tabPanel(
        "Response Buttons",
        br(),
        uiOutput(ns("preview_buttons_ui"))
      ),
      
      tabPanel(
        "Likert Elements",
        br(),
        uiOutput(ns("preview_likert_ui"))
      )
    ),
    
    hr(),
    
    # PSY code preview
    h5("Generated PsyToolkit Code"),
    tags$details(
      tags$summary(icon("code"), " Show/hide code preview"),
      br(),
      verbatimTextOutput(ns("psy_code_preview"))
    ),
    
    hr(),
    
    # Export options
    h5("Export Options"),
    uiOutput(ns("mp3_include_ui")),
    
    # Export actions
    fluidRow(
      column(
        6,
        uiOutput(ns("generate_btn_ui"))
      ),
      column(
        6,
        uiOutput(ns("download_btn_ui"))
      )
    ),
    
    # Warning message for missing MP3 files
    uiOutput(ns("mp3_warning_ui")),
    
    # Instructions for importing into PsyToolkit
    uiOutput(ns("psytoolkit_instructions_ui")),
    
    br(),
    
    # Status messages with file links
    uiOutput(ns("export_status_ui"))
  )
}

# -----------------------------------------------------------------------------
# Server Function
# -----------------------------------------------------------------------------
mod_export_server <- function(id, audio_data, forced_choice_data, 
                              likert_data, instructions_data, 
                              settings_data, export_images, project_name = reactive("Untitled_Project")) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # -------------------------------------------------------------------------
    # Internal state
    # -------------------------------------------------------------------------
    internal <- reactiveValues(
      is_valid = FALSE,
      has_content = FALSE,  # TRUE if project has any elements
      validation_messages = list(),
      generated_files = list(),
      psy_code = "",
      export_ready = FALSE,
      project_name = "Untitled_Project"
    )
    
    # -------------------------------------------------------------------------
    # Check if project has content
    # -------------------------------------------------------------------------
    observe({
      has_audio <- nrow(audio_data$files) > 0
      has_fc <- forced_choice_data$enabled %||% FALSE
      has_likert <- likert_data$enabled %||% FALSE
      has_instructions <- !is.null(instructions_data$editor_html) && 
                          nchar(trimws(instructions_data$editor_html %||% "")) > 0 &&
                          instructions_data$editor_html != "<p><br></p>"
      
      internal$has_content <- has_audio || has_fc || has_likert || has_instructions
    })
    
    # -------------------------------------------------------------------------
    # Render Generate button (enabled only if project has content)
    # -------------------------------------------------------------------------
    output$generate_btn_ui <- renderUI({
      if (internal$has_content) {
        actionButton(
          ns("generate_btn"),
          "Generate All Files",
          icon = icon("cogs"),
          class = "btn-primary btn-lg btn-block"
        )
      } else {
        tags$button(
          type = "button",
          class = "btn btn-primary btn-lg btn-block disabled",
          disabled = "disabled",
          icon("cogs"),
          " Generate All Files"
        )
      }
    })
    
    # -------------------------------------------------------------------------
    # Render Download button (enabled only after generation)
    # -------------------------------------------------------------------------
    output$download_btn_ui <- renderUI({
      if (internal$export_ready) {
        downloadButton(
          ns("download_zip"),
          "Download ZIP",
          class = "btn-success btn-lg btn-block"
        )
      } else {
        tags$button(
          type = "button",
          class = "btn btn-success btn-lg btn-block disabled",
          disabled = "disabled",
          icon("download"),
          " Download ZIP"
        )
      }
    })
    
    # -------------------------------------------------------------------------
    # MP3 inclusion checkbox (shown only if audio files have been uploaded)
    # -------------------------------------------------------------------------
    output$mp3_include_ui <- renderUI({
      # Check if audio files have actual file data (not just filenames)
      has_uploaded_mp3 <- nrow(audio_data$files) > 0 && 
                          any(audio_data$files$has_file %||% FALSE)
      
      if (has_uploaded_mp3) {
        checkboxInput(
          ns("include_mp3"),
          "Include MP3 audio files in ZIP archive",
          value = TRUE
        )
      } else {
        NULL
      }
    })
    
    # -------------------------------------------------------------------------
    # Warning message for missing MP3 files
    # -------------------------------------------------------------------------
    output$mp3_warning_ui <- renderUI({
      # Check if audio files have actual file data
      has_uploaded_mp3 <- nrow(audio_data$files) > 0 && 
                          any(audio_data$files$has_file %||% FALSE)
      
      # Check if checkbox is unchecked or no MP3 uploaded
      include_mp3 <- input$include_mp3 %||% TRUE
      
      should_warn <- nrow(audio_data$files) > 0 && (!has_uploaded_mp3 || !include_mp3)
      
      if (should_warn && internal$export_ready) {
        div(
          class = "alert alert-warning",
          style = "margin-top: 15px;",
          icon("exclamation-triangle"),
          tags$strong(" Note: "),
          "The MP3 audio files are not included in the ZIP archive. ",
          "You will need to upload them separately to PsyToolkit before compiling the experiment."
        )
      } else {
        NULL
      }
    })
    
    # -------------------------------------------------------------------------
    # PsyToolkit import instructions (shown after generation)
    # -------------------------------------------------------------------------
    output$psytoolkit_instructions_ui <- renderUI({
      if (!internal$export_ready) return(NULL)
      
      # Generate sanitized project name for survey code example
      proj_name <- project_name()
      sanitized_name <- sanitize_filename(proj_name)
      
      div(
        class = "alert alert-info",
        style = "margin-top: 15px;",
        icon("info-circle"),
        tags$strong(" How to import into PsyToolkit:"),
        tags$ol(
          style = "margin-top: 10px; margin-bottom: 10px; padding-left: 20px;",
          tags$li("In PsyToolkit, go to ", tags$strong("Create"), " menu and click ", 
                  tags$strong("Create new experiment")),
          tags$li("Choose ", tags$strong("Method 2: From a PsyToolkit experiment file (zip format)")),
          tags$li("Select the downloaded ZIP file"),
          tags$li("Optional: enter a name for your experiment (or keep the default from the ZIP filename)"),
          tags$li("Click ", tags$strong("Create experiment from an uploaded PsyToolkit experiment file"), 
                  " to upload your files and create the experiment"),
          tags$li("Click ", tags$strong("Compile"), " to build the experiment")
        ),
        tags$hr(style = "margin: 10px 0;"),
        tags$strong("Optional: Running from a survey"),
        tags$p(style = "margin-top: 5px; margin-bottom: 5px;",
               "To run this experiment from a PsyToolkit survey, add the following code to your survey ",
               "(after initial questions, before final questions):"),
        tags$pre(
          style = "background-color: #f5f5f5; padding: 8px; border-radius: 4px; font-size: 12px;",
          sprintf("l: expe\nt: experiment\n- psytoolkit_experiment_%s", sanitized_name)
        ),
        tags$small(
          class = "text-muted",
          "Note: If you changed the experiment name at step 4, update the last line accordingly. ",
          "The label 'expe' can also be customized (useful when running multiple experiments from the same survey)."
        )
      )
    })
    
    # -------------------------------------------------------------------------
    # Validation
    # -------------------------------------------------------------------------
    observe({
      messages <- list()
      is_valid <- TRUE
      
      # Check audio files
      if (nrow(audio_data$files) == 0) {
        messages$audio <- list(
          type = "error",
          text = "No audio stimuli defined. Please add MP3 files or import a list."
        )
        is_valid <- FALSE
      } else {
        missing_mp3 <- audio_data$files %>%
          filter(!has_file) %>%
          nrow()
        
        if (missing_mp3 > 0) {
          messages$audio_warning <- list(
            type = "warning",
            text = sprintf(
              "%d MP3 file(s) not uploaded. You will need to upload them to PsyToolkit.",
              missing_mp3
            )
          )
        }
      }
      
      # Check response type
      if (!forced_choice_data$enabled && !likert_data$enabled) {
        messages$response <- list(
          type = "error",
          text = "No response type enabled. Enable Forced Choice and/or Likert Scale."
        )
        is_valid <- FALSE
      }
      
      # Check instructions
      display_instructions <- instructions_data$display_instructions %||% TRUE
      
      if (display_instructions) {
        # Instructions are enabled, check if content is provided
        has_content <- FALSE
        
        if (instructions_data$use_editor %||% TRUE) {
          has_content <- !is.null(instructions_data$editor_html) && 
                         nchar(trimws(instructions_data$editor_html %||% "")) > 0 &&
                         instructions_data$editor_html != "<p><br></p>"
        } else {
          has_content <- !is.null(instructions_data$uploaded_image)
        }
        
        if (!has_content) {
          messages$instructions <- list(
            type = "error",
            text = paste0(
              "Instructions display is enabled but no content provided. ",
              "Go to the Instructions tab to either add instructions content, ",
              "or uncheck 'Display instructions before running task' to generate without instructions."
            )
          )
          is_valid <- FALSE
        }
      }
      
      internal$validation_messages <- messages
      internal$is_valid <- is_valid
    })
    
    # -------------------------------------------------------------------------
    # Render validation UI
    # -------------------------------------------------------------------------
    output$validation_ui <- renderUI({
      messages <- internal$validation_messages
      
      if (length(messages) == 0) {
        return(div(
          class = "alert alert-success",
          icon("check-circle"),
          " Configuration is valid. Ready to generate files."
        ))
      }
      
      tagList(
        lapply(messages, function(msg) {
          class <- switch(
            msg$type,
            "error" = "alert alert-danger",
            "warning" = "alert alert-warning",
            "alert alert-info"
          )
          icon_name <- switch(
            msg$type,
            "error" = "times-circle",
            "warning" = "exclamation-triangle",
            "info-circle"
          )
          
          div(class = class, icon(icon_name), " ", msg$text)
        })
      )
    })
    
    # -------------------------------------------------------------------------
    # Generate PSY code
    # -------------------------------------------------------------------------
    psy_code <- reactive({
      generate_psy_code(
        audio_data = audio_data,
        forced_choice_data = forced_choice_data,
        likert_data = likert_data,
        instructions_data = instructions_data,
        settings_data = settings_data
      )
    })
    
    # -------------------------------------------------------------------------
    # Preview PSY code
    # -------------------------------------------------------------------------
    output$psy_code_preview <- renderText({
      psy_code()
    })
    
    # -------------------------------------------------------------------------
    # Preview instructions - use HTML for formatted preview
    # -------------------------------------------------------------------------
    output$preview_instructions <- renderUI({
      if (instructions_data$use_editor %||% TRUE) {
        html_content <- instructions_data$editor_html
        bg_color <- instructions_data$bg_color %||% "#FFFFE0"
        text_color <- instructions_data$default_text_color %||% "#000000"
        
        if (is.null(html_content) || nchar(trimws(html_content)) == 0 || 
            html_content == "<p><br></p>") {
          html_content <- "<p style='color: #888;'>(No instructions text)</p>"
        }
        
        # Scale down font sizes in HTML for preview (50% of original)
        # Quill uses: 12px (Small), 16px (Normal), 24px (Large), 32px (Huge)
        # Preview uses: 6px, 8px, 12px, 16px
        scaled_html <- html_content
        scaled_html <- gsub('font-size: 12px', 'font-size: 6px', scaled_html)
        scaled_html <- gsub('font-size: 16px', 'font-size: 8px', scaled_html)
        scaled_html <- gsub('font-size: 24px', 'font-size: 12px', scaled_html)
        scaled_html <- gsub('font-size: 32px', 'font-size: 16px', scaled_html)
        
        div(
          style = sprintf(
            "width: 400px; height: 300px; background-color: %s; color: %s; 
             padding: 20px; overflow: hidden; border: 1px solid #ccc;
             display: flex; align-items: center; justify-content: center;
             font-family: Arial, sans-serif; font-size: 8px; position: relative;",
            bg_color, text_color
          ),
          div(
            style = "max-width: 100%; width: 100%;",
            tags$style(HTML("
              .ql-font-serif { font-family: Georgia, serif !important; }
              .ql-font-monospace { font-family: 'Courier New', monospace !important; }
              p.ql-align-center, .ql-align-center { text-align: center !important; }
              p.ql-align-right, .ql-align-right { text-align: right !important; }
              p.ql-align-justify, .ql-align-justify { text-align: justify !important; }
            ")),
            HTML(scaled_html)
          )
        )
        
      } else if (!is.null(instructions_data$uploaded_image)) {
        tags$img(
          src = base64enc::dataURI(
            file = instructions_data$uploaded_image$datapath,
            mime = "image/png"
          ),
          style = "max-width: 400px; max-height: 300px; border: 1px solid #ccc;"
        )
      } else {
        div(
          style = "width: 400px; height: 300px; background: #333; color: #888; 
                   display: flex; align-items: center; justify-content: center;",
          "(No instructions)"
        )
      }
    })
    
    # -------------------------------------------------------------------------
    # Preview buttons UI
    # -------------------------------------------------------------------------
    output$preview_buttons_ui <- renderUI({
      if (!forced_choice_data$enabled) {
        return(tags$p(class = "text-muted", "Forced choice is disabled."))
      }
      
      n <- forced_choice_data$num_choices
      
      fluidRow(
        lapply(1:min(n, 6), function(i) {
          column(
            width = 2,
            plotOutput(ns(paste0("btn_preview_", i)), height = "80px")
          )
        })
      )
    })
    
    # Generate button previews
    observe({
      if (!forced_choice_data$enabled) return()
      
      n <- forced_choice_data$num_choices
      
      for (i in 1:min(n, 6)) {
        local({
          idx <- i
          output[[paste0("btn_preview_", idx)]] <- renderPlot({
            label <- forced_choice_data$labels[[idx]] %||% paste("Choice", idx)
            text_col <- forced_choice_data$text_colors[[idx]] %||% settings_data$text_color
            bg_col <- forced_choice_data$bg_colors[[idx]] %||% settings_data$bg_color
            
            create_button_image(
              label = label,
              text_color = text_col,
              bg_color = bg_col,
              width = 100,
              height = 60
            )
          }, width = 100, height = 60, res = 72)
        })
      }
    })
    
    # -------------------------------------------------------------------------
    # Preview Likert UI
    # -------------------------------------------------------------------------
    output$preview_likert_ui <- renderUI({
      if (!likert_data$enabled) {
        return(tags$p(class = "text-muted", "Likert scales are disabled."))
      }
      
      scales <- likert_data$scales
      num_scales <- likert_data$num_scales %||% 1
      shared_style <- likert_data$shared_item_style %||% TRUE
      item_size <- likert_data$item_size %||% 30
      global_item_color <- settings_data$likert_item_color %||% "#666666"
      
      if (shared_style) {
        # Single shared item
        shape <- likert_data$global_item_shape %||% "disc"
        item_color <- if (length(scales) > 0 && !is.null(scales[[1]]$item_color)) {
          scales[[1]]$item_color
        } else {
          global_item_color
        }
        
        tagList(
          tags$p("Selection item (shared):"),
          plotOutput(ns("likert_item_preview_1"), height = "60px", width = "60px"),
          tags$small(class = "text-muted", sprintf("Shape: %s, Size: %dpx, Color: %s", shape, item_size, item_color)),
          br(), br(),
          tags$p("Labels preview in Live Preview tab.")
        )
      } else {
        # Multiple items - one per scale
        tagList(
          tags$p("Selection items (per scale):"),
          lapply(1:num_scales, function(i) {
            scale <- scales[[i]]
            shape <- scale$item_shape %||% "disc"
            item_color <- scale$item_color %||% global_item_color
            
            div(
              style = "display: inline-block; margin-right: 15px; text-align: center;",
              tags$small(sprintf("Scale %d", i)),
              br(),
              plotOutput(ns(paste0("likert_item_preview_", i)), height = "60px", width = "60px"),
              tags$small(class = "text-muted", shape)
            )
          }),
          br(),
          tags$p("Labels preview in Live Preview tab.")
        )
      }
    })
    
    # Dynamic render for each scale's item preview
    observe({
      num_scales <- likert_data$num_scales %||% 1
      scales <- likert_data$scales
      shared_style <- likert_data$shared_item_style %||% TRUE
      item_size <- likert_data$item_size %||% 30
      global_item_color <- settings_data$likert_item_color %||% "#666666"
      
      if (shared_style) {
        # Render single shared item
        output$likert_item_preview_1 <- renderPlot({
          shape <- likert_data$global_item_shape %||% "disc"
          item_color <- if (length(scales) > 0 && !is.null(scales[[1]]$item_color)) {
            scales[[1]]$item_color
          } else {
            global_item_color
          }
          shape_code <- get_shape_geom(shape)
          
          ggplot() +
            geom_point(aes(x = 0, y = 0), shape = shape_code, size = item_size / 3,
                       color = item_color, fill = item_color) +
            xlim(-1, 1) + ylim(-1, 1) +
            theme_void() +
            theme(plot.background = element_rect(fill = "transparent", color = NA))
        }, width = 60, height = 60, res = 72, bg = "transparent")
      } else {
        # Render individual items for each scale
        for (i in 1:num_scales) {
          local({
            idx <- i
            output[[paste0("likert_item_preview_", idx)]] <- renderPlot({
              scale <- likert_data$scales[[idx]]
              shape <- scale$item_shape %||% "disc"
              item_color <- scale$item_color %||% (settings_data$likert_item_color %||% "#666666")
              shape_code <- get_shape_geom(shape)
              sz <- likert_data$item_size %||% 30
              
              ggplot() +
                geom_point(aes(x = 0, y = 0), shape = shape_code, size = sz / 3,
                           color = item_color, fill = item_color) +
                xlim(-1, 1) + ylim(-1, 1) +
                theme_void() +
                theme(plot.background = element_rect(fill = "transparent", color = NA))
            }, width = 60, height = 60, res = 72, bg = "transparent")
          })
        }
      }
    })
    
    # -------------------------------------------------------------------------
    # Generate all files
    # -------------------------------------------------------------------------
    observeEvent(input$generate_btn, {
      
      withProgress(message = "Generating files...", value = 0, {
        
        # Create temp directory
        temp_dir <- tempfile("psytoolkit_export_")
        dir.create(temp_dir)
        
        files_list <- list()
        
        # Get resolution from settings
        res <- parse_resolution(settings_data$resolution %||% "800x600")
        img_width <- res$width
        img_height <- res$height
        
        # 1. Generate code.psy
        incProgress(0.1, detail = "Generating PSY code...")
        psy_content <- psy_code()
        psy_file <- file.path(temp_dir, "code.psy")
        writeLines(psy_content, psy_file)
        files_list$code <- psy_file
        
        # 1b. Generate stimuli_and_orders.txt if using predefined orders
        use_predefined_orders <- audio_data$use_predefined_orders %||% FALSE
        
        if (use_predefined_orders) {
          incProgress(0.15, detail = "Generating presentation orders file...")
          task_name <- settings_data$task_name %||% "evaluate_audio_stimuli"
          orders_content <- generate_stimuli_orders_file(
            audio_data = audio_data,
            instructions_data = instructions_data,
            settings_data = settings_data,
            task_name = task_name
          )
          orders_file <- file.path(temp_dir, "stimuli_and_orders.txt")
          writeLines(orders_content, orders_file)
          files_list$orders <- orders_file
        }
        
        # 2. Generate instructions image (only if enabled)
        display_instructions <- instructions_data$display_instructions %||% TRUE
        
        if (display_instructions) {
          incProgress(0.2, detail = "Generating instructions...")
          instructions_file <- file.path(temp_dir, "instructions.png")
          
          # Use the save_instructions_image helper function with resolution
          save_instructions_image(instructions_data, instructions_file, 
                                 img_width = img_width, img_height = img_height)
          files_list$instructions <- instructions_file
        }
        
        # 3. Generate forced choice button images
        if (forced_choice_data$enabled) {
          incProgress(0.4, detail = "Generating button images...")
          
          # Calculate scale factor for FC elements
          fc_scale_factor <- img_width / 800
          
          fc_layout <- calculate_forced_choice_layout(
            n_choices = forced_choice_data$num_choices,
            n_rows = forced_choice_data$num_rows,
            img_width = img_width,
            h_spacing = round((forced_choice_data$h_spacing %||% 50) * fc_scale_factor),
            v_spacing = round((forced_choice_data$v_spacing %||% 50) * fc_scale_factor),
            box_height = round((forced_choice_data$box_height %||% 80) * fc_scale_factor),
            y_center = round((forced_choice_data$y_position %||% 0) * fc_scale_factor)
          )
          
          for (i in 1:forced_choice_data$num_choices) {
            use_img <- forced_choice_data$use_images[[i]] %||% FALSE
            
            if (use_img && !is.null(forced_choice_data$images[[i]])) {
              # Copy uploaded image
              btn_file <- file.path(temp_dir, paste0("choice_", i, ".png"))
              file.copy(forced_choice_data$images[[i]]$datapath, btn_file)
            } else {
              # Generate from text
              label <- forced_choice_data$labels[[i]] %||% paste("Choice", i)
              text_col <- forced_choice_data$text_colors[[i]] %||% settings_data$text_color
              bg_col <- forced_choice_data$bg_colors[[i]] %||% settings_data$bg_color
              
              row_data <- fc_layout[i, ]
              
              btn_file <- file.path(temp_dir, paste0("choice_", i, ".png"))
              p <- create_button_image(
                label = label,
                text_color = text_col,
                bg_color = bg_col,
                width = row_data$width,
                height = row_data$height
              )
              ggsave(btn_file, p, width = row_data$width, height = row_data$height,
                     units = "px", dpi = 72)
            }
            files_list[[paste0("choice_", i)]] <- btn_file
          }
        }
        
        # 4. Generate Likert scale images
        if (likert_data$enabled) {
          incProgress(0.6, detail = "Generating Likert images...")
          
          # Calculate scale factor based on resolution (relative to 800x600)
          scale_factor <- img_width / 800
          
          # Selection items - generate one per scale if styles differ, otherwise one shared
          scales <- likert_data$scales
          item_size <- round((likert_data$item_size %||% 30) * scale_factor)
          global_item_color <- settings_data$likert_item_color %||% "#666666"
          
          if (likert_data$shared_item_style %||% TRUE) {
            # Single shared selection item
            item_file <- file.path(temp_dir, "selection_item.png")
            shape <- likert_data$global_item_shape %||% "disc"
            
            # Use first scale's item_color or global
            first_item_color <- if (length(scales) > 0 && !is.null(scales[[1]]$item_color)) {
              scales[[1]]$item_color
            } else {
              global_item_color
            }
            
            if (shape == "custom" && !is.null(likert_data$global_item_image)) {
              file.copy(likert_data$global_item_image$datapath, item_file)
            } else {
              create_likert_item_image(shape, size = item_size, color = first_item_color, 
                                       output_path = item_file)
            }
            files_list$selection_item <- item_file
          } else {
            # Multiple selection items - one per scale
            for (i in 1:likert_data$num_scales) {
              scale <- scales[[i]]
              
              # Determine filename
              if (likert_data$num_scales == 1) {
                item_name <- "selection_item"
              } else {
                item_name <- paste0("selection_item_", i)
              }
              item_file <- file.path(temp_dir, paste0(item_name, ".png"))
              
              # Get scale-specific or global shape
              shape <- scale$item_shape %||% "disc"
              
              # Get scale-specific color or fall back to global
              scale_item_color <- scale$item_color %||% global_item_color
              
              if (shape == "custom" && !is.null(scale$item_image)) {
                file.copy(scale$item_image$datapath, item_file)
              } else {
                create_likert_item_image(shape, size = item_size, color = scale_item_color,
                                         output_path = item_file)
              }
              files_list[[item_name]] <- item_file
            }
          }
          
          # Scale labels
          scales <- likert_data$scales
          num_scales <- likert_data$num_scales %||% 1
          
          # Label dimensions scaled by resolution
          label_width <- round(100 * scale_factor)
          label_height <- round(50 * scale_factor)
          
          if (num_scales > 0 && length(scales) > 0) {
            for (i in 1:num_scales) {
              if (i > length(scales)) break  # Safety check
              scale <- scales[[i]]
              
              # Determine label names based on scale count
              if (num_scales == 1) {
                left_name <- "left"
                right_name <- "right"
              } else {
                left_name <- paste0("left_", i)
                right_name <- paste0("right_", i)
              }
              
              # Left label
              left_file <- file.path(temp_dir, paste0(left_name, ".png"))
              if (isTRUE(scale$left_use_image) && !is.null(scale$left_image)) {
                file.copy(scale$left_image$datapath, left_file)
              } else {
                label_color <- scale$label_color %||% settings_data$text_color %||% "#000000"
                label_bg <- scale$label_bg %||% settings_data$bg_color %||% "#FFFFE0"
                p <- create_button_image(
                  label = scale$left_label %||% "Left",
                  text_color = label_color,
                  bg_color = label_bg,
                  width = label_width,
                  height = label_height
                )
                ggsave(left_file, p, width = label_width, height = label_height, units = "px", dpi = 72)
              }
              files_list[[left_name]] <- left_file
              
              # Right label
              right_file <- file.path(temp_dir, paste0(right_name, ".png"))
              if (isTRUE(scale$right_use_image) && !is.null(scale$right_image)) {
                file.copy(scale$right_image$datapath, right_file)
              } else {
                label_color <- scale$label_color %||% settings_data$text_color %||% "#000000"
                label_bg <- scale$label_bg %||% settings_data$bg_color %||% "#FFFFE0"
                p <- create_button_image(
                  label = scale$right_label %||% "Right",
                  text_color = label_color,
                  bg_color = label_bg,
                  width = label_width,
                  height = label_height
                )
                ggsave(right_file, p, width = label_width, height = label_height, units = "px", dpi = 72)
              }
              files_list[[right_name]] <- right_file
            }
          }
        }
        
        # 5. Generate INFO.txt
        incProgress(0.8, detail = "Creating documentation...")
        info_file <- file.path(temp_dir, "INFO.txt")
        info_content <- generate_info_file(
          audio_data, forced_choice_data, likert_data
        )
        writeLines(info_content, info_file)
        files_list$info <- info_file
        
        # Store generated files
        internal$generated_files <- files_list
        internal$export_ready <- TRUE
        
        incProgress(1.0, detail = "Done!")
      })
      
      showNotification("Files generated successfully!", type = "message")
    })
    
    # -------------------------------------------------------------------------
    # Export status UI with downloadable file links
    # -------------------------------------------------------------------------
    output$export_status_ui <- renderUI({
      if (!internal$export_ready) {
        if (internal$has_content) {
          return(div(
            class = "alert alert-info",
            icon("info-circle"),
            " Click 'Generate All Files' to create the export package."
          ))
        } else {
          return(div(
            class = "alert alert-warning",
            icon("exclamation-triangle"),
            " Add some content to your project before generating files."
          ))
        }
      }
      
      files <- internal$generated_files
      
      div(
        class = "alert alert-success",
        icon("check-circle"),
        sprintf(" %d files generated and ready for download.", length(files)),
        br(), br(),
        tags$div(
          style = "max-height: 200px; overflow-y: auto;",
          tags$table(
            class = "table table-condensed table-hover",
            style = "margin-bottom: 0;",
            tags$tbody(
              lapply(names(files), function(name) {
                file_path <- files[[name]]
                file_name <- basename(file_path)
                file_ext <- tools::file_ext(file_name)
                
                # Icon based on file type
                icon_name <- switch(file_ext,
                  "png" = "image",
                  "mp3" = "music",
                  "psy" = "code",
                  "txt" = "file-alt",
                  "file"
                )
                
                tags$tr(
                  tags$td(style = "width: 30px;", icon(icon_name)),
                  tags$td(file_name),
                  tags$td(
                    style = "width: 100px; text-align: right;",
                    downloadLink(
                      ns(paste0("dl_", name)),
                      "Download",
                      class = "btn btn-xs btn-default"
                    )
                  )
                )
              })
            )
          )
        )
      )
    })
    
    # -------------------------------------------------------------------------
    # Dynamic download handlers for individual files
    # -------------------------------------------------------------------------
    observe({
      files <- internal$generated_files
      
      lapply(names(files), function(name) {
        output[[paste0("dl_", name)]] <- downloadHandler(
          filename = function() {
            basename(files[[name]])
          },
          content = function(file) {
            file.copy(files[[name]], file)
          }
        )
      })
    })
    
    # -------------------------------------------------------------------------
    # Download ZIP
    # -------------------------------------------------------------------------
    output$download_zip <- downloadHandler(
      filename = function() {
        name <- project_name()
        sanitized_name <- sanitize_filename(name)
        paste0("psytoolkit_experiment_", sanitized_name, ".zip")
      },
      content = function(file) {
        req(internal$export_ready)
        
        files <- internal$generated_files
        file_paths <- unlist(files)
        
        # Include MP3 files if checkbox is checked and files are available
        include_mp3 <- input$include_mp3 %||% TRUE
        has_uploaded_mp3 <- nrow(audio_data$files) > 0 && 
                            any(audio_data$files$has_file %||% FALSE)
        
        if (include_mp3 && has_uploaded_mp3) {
          # Get uploaded MP3 files
          mp3_files <- audio_data$files %>%
            filter(has_file == TRUE)
          
          if (nrow(mp3_files) > 0 && "datapath" %in% names(mp3_files)) {
            # Create temp directory for MP3 files with original names
            mp3_temp_dir <- tempfile("mp3_export_")
            dir.create(mp3_temp_dir)
            
            for (i in 1:nrow(mp3_files)) {
              mp3_path <- mp3_files$datapath[i]
              mp3_name <- mp3_files$filename[i]
              
              if (!is.null(mp3_path) && !is.na(mp3_path) && file.exists(mp3_path)) {
                # Copy to temp dir with original filename
                dest_path <- file.path(mp3_temp_dir, mp3_name)
                file.copy(mp3_path, dest_path)
                file_paths <- c(file_paths, dest_path)
              }
            }
          }
        }
        
        # Create ZIP
        zip::zipr(file, file_paths, mode = "cherry-pick")
      },
      contentType = "application/zip"
    )
    
  })
}

# -----------------------------------------------------------------------------
# Helper Functions
# -----------------------------------------------------------------------------

#' Parse resolution string into width and height
#' @param resolution Resolution string like "800x600"
#' @return List with width and height
parse_resolution <- function(resolution) {
  parts <- strsplit(resolution, "x")[[1]]
  if (length(parts) == 2) {
    list(
      width = as.numeric(parts[1]),
      height = as.numeric(parts[2])
    )
  } else {
    list(width = 800, height = 600)  # Default
  }
}

#' Generate PsyToolkit PSY code
#' @return Character string with PSY code
generate_psy_code <- function(audio_data, forced_choice_data, 
                              likert_data, instructions_data, settings_data) {
  
  lines <- character()
  
  # Check if instructions should be displayed
  display_instructions <- instructions_data$display_instructions %||% TRUE
  
  # Check if using predefined presentation orders
  use_predefined_orders <- audio_data$use_predefined_orders %||% FALSE
  
  # ----- Options section -----
  fullscreen <- settings_data$fullscreen %||% FALSE
  resolution <- settings_data$resolution %||% "800x600"
  img_bg_color <- settings_data$img_bg_color %||% "#000000"
  
  # Parse resolution for layout calculations
  res <- parse_resolution(resolution)
  img_width <- res$width
  img_height <- res$height
  
  needs_options <- fullscreen || 
                   resolution != "800x600" || 
                   toupper(img_bg_color) != "#000000"
  
  if (needs_options) {
    lines <- c(lines, "# Global display options")
    lines <- c(lines, "options")
    
    if (fullscreen) {
      lines <- c(lines, "  fullscreen")
    }
    
    if (resolution != "800x600") {
      lines <- c(lines, sprintf("  resolution %s", resolution))
    }
    
    if (toupper(img_bg_color) != "#000000") {
      bg_hex <- gsub("^#", "", img_bg_color)
      lines <- c(lines, sprintf("  background color %s", bg_hex))
    }
    
    lines <- c(lines, "")
  }
  
  # ----- Bitmaps section -----
  lines <- c(lines, "# PNG image files (without .png extension)")
  lines <- c(lines, "bitmaps")
  
  if (display_instructions) {
    lines <- c(lines, "  instructions")
  }
  
  # Forced choice images
  if (forced_choice_data$enabled) {
    lines <- c(lines, "  # Forced choice response buttons")
    for (i in 1:forced_choice_data$num_choices) {
      lines <- c(lines, sprintf("  choice_%d", i))
    }
  }
  
  # Likert images
  if (likert_data$enabled) {
    lines <- c(lines, "  # Likert scale labels and items")
    num_scales <- likert_data$num_scales %||% 1
    if (num_scales == 1) {
      lines <- c(lines, "  left")
      lines <- c(lines, "  right")
    } else {
      for (i in 1:num_scales) {
        lines <- c(lines, sprintf("  left_%d", i))
        lines <- c(lines, sprintf("  right_%d", i))
      }
    }
    
    # Selection items
    if (likert_data$shared_item_style %||% TRUE) {
      lines <- c(lines, "  selection_item")
    } else {
      if (num_scales == 1) {
        lines <- c(lines, "  selection_item")
      } else {
        for (i in 1:num_scales) {
          lines <- c(lines, sprintf("  selection_item_%d", i))
        }
      }
    }
  }
  
  lines <- c(lines, "")
  
  # ----- Sounds section (only for random order mode) -----
  if (!use_predefined_orders) {
    lines <- c(lines, "# MP3 audio files (without .mp3 extension)")
    lines <- c(lines, "# Each line contains the name of an audio file (without .mp3 extension)")
    lines <- c(lines, "sounds")
    
    if (nrow(audio_data$files) > 0) {
      for (filename in audio_data$files$filename) {
        stimulus_name <- tools::file_path_sans_ext(filename)
        lines <- c(lines, sprintf("  %s", stimulus_name))
      }
    }
    
    lines <- c(lines, "")
    
    # ----- Stimuli table -----
    lines <- c(lines, "# Stimulus table with metadata")
    lines <- c(lines, "# Each line starts with metadata in quotes (spaces act as column separators")
    lines <- c(lines, "# in the results file), followed by the stimulus name")
    lines <- c(lines, "table audio_stimuli")
    
    if (nrow(audio_data$metadata) > 0) {
      for (i in 1:nrow(audio_data$metadata)) {
        row <- audio_data$metadata[i, ]
        filename <- row$filename
        stimulus_name <- tools::file_path_sans_ext(filename)
        
        metadata_parts <- c()
        
        if (length(audio_data$metadata_cols) > 0) {
          for (col in audio_data$metadata_cols) {
            if (col %in% names(row)) {
              val <- row[[col]]
              if (!is.na(val)) {
                metadata_parts <- c(metadata_parts, as.character(val))
              }
            }
          }
        }
        
        metadata_parts <- c(metadata_parts, stimulus_name)
        metadata_str <- paste(metadata_parts, collapse = " ")
        
        lines <- c(lines, sprintf('  "%s" %s', metadata_str, stimulus_name))
      }
    }
    
    lines <- c(lines, "")
  }
  
  # ----- Task definition -----
  task_name <- settings_data$task_name %||% "evaluate_audio_stimuli"
  lines <- c(lines, "# Task definition")
  
  if (use_predefined_orders) {
    lines <- c(lines, "# This task plays the stimulus stored in the global variable &target_stimulus")
  } else {
    lines <- c(lines, "# This task presents each stimulus from the audio_stimuli table")
  }
  
  lines <- c(lines, sprintf("task %s", task_name))
  
  if (!use_predefined_orders) {
    lines <- c(lines, "  # Use stimuli from the audio_stimuli table")
    lines <- c(lines, "  table audio_stimuli")
  }
  
  # Sound playback - different syntax for predefined orders
  loop_kw <- if (settings_data$audio_loop %||% FALSE) " loop" else ""
  
  if (use_predefined_orders) {
    lines <- c(lines, "  # Play the audio stimulus stored in global variable &target_stimulus")
    if (settings_data$audio_loop %||% FALSE) {
      lines <- c(lines, "  # The 'loop' keyword makes the sound repeat until response is given")
    }
    lines <- c(lines, sprintf("  sound &target_stimulus%s", loop_kw))
    # Track variables to save for predefined orders mode
    save_vars <- c("&target_stimulus", "%stimulus_name")
  } else {
    lines <- c(lines, "  # Play the audio stimulus (column 2 of the table)")
    if (settings_data$audio_loop %||% FALSE) {
      lines <- c(lines, "  # The 'loop' keyword makes the sound repeat until response is given")
    }
    lines <- c(lines, sprintf("  sound @2%s", loop_kw))
    # Track variables to save
    save_vars <- c("@1", "@2")
  }
  
  # Calculate scale factor for resolution
  scale_factor <- img_width / 800
  
  # ----- Calculate element positions (same logic as preview) -----
  element_positions <- list()
  element_order <- c()
  
  if (forced_choice_data$enabled) {
    n_rows <- forced_choice_data$num_rows %||% 1
    box_height <- forced_choice_data$box_height %||% 80
    v_spacing <- forced_choice_data$v_spacing %||% 50
    fc_height <- n_rows * box_height + (n_rows - 1) * v_spacing
    
    element_positions[["fc"]] <- list(type = "fc", height = fc_height)
    element_order <- c(element_order, "fc")
  }
  
  if (likert_data$enabled) {
    num_scales <- likert_data$num_scales %||% 1
    for (i in 1:num_scales) {
      id <- paste0("likert_", i)
      element_positions[[id]] <- list(type = "likert", index = i, height = 70)
      element_order <- c(element_order, id)
    }
  }
  
  # Calculate Y positions for each element
  if (length(element_order) > 0) {
    n_elements <- length(element_order)
    spacing <- 50
    heights <- sapply(element_order, function(id) element_positions[[id]]$height)
    total_height <- sum(heights) + spacing * (n_elements - 1)
    y_top <- total_height / 2
    
    current_y <- y_top
    for (i in seq_along(element_order)) {
      id <- element_order[i]
      h <- heights[i]
      y_center <- current_y - h / 2
      
      # Apply manual offset if stored
      if (element_positions[[id]]$type == "fc") {
        offset <- forced_choice_data$y_offset %||% 0
      } else {
        scale_idx <- element_positions[[id]]$index
        offset <- likert_data$scales[[scale_idx]]$y_offset %||% 0
      }
      y_center <- y_center + offset
      
      element_positions[[id]]$y_position <- y_center
      current_y <- current_y - h - spacing
    }
  }
  
  # ----- Forced choice section -----
  if (forced_choice_data$enabled) {
    lines <- c(lines, "  # --- Forced choice response ---")
    lines <- c(lines, "  # Display response buttons at specified positions (x y coordinates)")
    lines <- c(lines, "  # Coordinates are relative to screen center (0,0)")
    
    # Get calculated Y position
    fc_y_center <- element_positions[["fc"]]$y_position %||% -100
    
    fc_layout <- calculate_forced_choice_layout(
      n_choices = forced_choice_data$num_choices,
      n_rows = forced_choice_data$num_rows %||% 1,
      img_width = img_width,
      h_spacing = round((forced_choice_data$h_spacing %||% 50) * scale_factor),
      v_spacing = round((forced_choice_data$v_spacing %||% 50) * scale_factor),
      box_height = round((forced_choice_data$box_height %||% 80) * scale_factor),
      y_center = round(fc_y_center * scale_factor)
    )
    
    for (j in 1:forced_choice_data$num_choices) {
      row_data <- fc_layout[j, ]
      
      # Get label or image name for comment
      use_img <- forced_choice_data$use_images[[j]] %||% FALSE
      if (use_img && !is.null(forced_choice_data$images[[j]])) {
        label_text <- forced_choice_data$images[[j]]$name %||% paste("image", j)
      } else {
        label_text <- forced_choice_data$labels[[j]] %||% paste("Choice", j)
      }
      # Truncate long labels and escape special characters
      label_text <- gsub("\n", " ", label_text)
      if (nchar(label_text) > 30) {
        label_text <- paste0(substr(label_text, 1, 27), "...")
      }
      
      comment <- sprintf("  # %d: %s", j, label_text)
      lines <- c(lines, sprintf("  show bitmap choice_%d %d %d %s",
                                j, round(row_data$x), round(-row_data$y), comment))
    }
    
    lines <- c(lines, "  # Initialize response variable")
    lines <- c(lines, "  set $fc_response 0")
    lines <- c(lines, "  # Wait for mouse click on one of the buttons")
    lines <- c(lines, "  # Parameters: button (l=left), correct response (1=arbitrary), timeout in ms")
    
    timeout <- forced_choice_data$timeout %||% 10000
    lines <- c(lines, sprintf("  readmouse l 1 %d", timeout))
    
    lines <- c(lines, "  # Store which button was clicked (1, 2, 3, ... or 0 if none)")
    lines <- c(lines, "  set $fc_response under MOUSE_X MOUSE_Y")
    
    save_vars <- c(save_vars, "$fc_response", "RT", "STATUS")
  }
  
  # ----- Likert scale section -----
  if (likert_data$enabled) {
    num_scales <- likert_data$num_scales %||% 1
    
    for (scale_idx in 1:num_scales) {
      scale <- likert_data$scales[[scale_idx]]
      
      if (num_scales > 1) {
        lines <- c(lines, sprintf("  # --- Likert scale %d ---", scale_idx))
      } else {
        lines <- c(lines, "  # --- Likert scale response ---")
      }
      
      # Get calculated Y position
      likert_id <- paste0("likert_", scale_idx)
      y_pos <- element_positions[[likert_id]]$y_position %||% 100
      scaled_y_pos <- round(y_pos * scale_factor)
      
      lines <- c(lines, "  # Set scale position (x y coordinates relative to screen center)")
      lines <- c(lines, sprintf("  rate option pos 0 %d", -scaled_y_pos))
      
      # Labels
      lines <- c(lines, "  # Set left and right label images")
      if (num_scales == 1) {
        lines <- c(lines, "  rate option labels left right")
      } else {
        lines <- c(lines, sprintf("  rate option labels left_%d right_%d", scale_idx, scale_idx))
      }
      
      # Selection items
      lines <- c(lines, "  # Set the image used for scale points")
      if (likert_data$shared_item_style %||% TRUE) {
        lines <- c(lines, "  rate option items selection_item")
      } else {
        if (num_scales == 1) {
          lines <- c(lines, "  rate option items selection_item")
        } else {
          lines <- c(lines, sprintf("  rate option items selection_item_%d", scale_idx))
        }
      }
      
      # Rate command
      n_points <- scale$num_points %||% 7
      timeout <- scale$timeout %||% 10000
      lines <- c(lines, "  # Collect response: timeout (ms) and number of scale points")
      lines <- c(lines, sprintf("  rate %d %d", timeout, n_points))
      
      # Store results for multiple scales
      if (num_scales > 1) {
        lines <- c(lines, "  # Store scale response in variables (for multiple scales)")
        lines <- c(lines, sprintf("  set $likert%d_response RATE", scale_idx))
        lines <- c(lines, sprintf("  set $likert%d_rt RATE_RT", scale_idx))
        lines <- c(lines, sprintf("  set $likert%d_status RATE_STATUS", scale_idx))
        save_vars <- c(save_vars, 
                       sprintf("$likert%d_response", scale_idx),
                       sprintf("$likert%d_rt", scale_idx),
                       sprintf("$likert%d_status", scale_idx))
      } else {
        save_vars <- c(save_vars, "RATE", "RATE_RT", "RATE_STATUS")
      }
    }
  }
  
  # ----- Save and cleanup -----
  lines <- c(lines, "  # Save data to results file")
  
  if (use_predefined_orders) {
    lines <- c(lines, "  # Columns: stimulus variable, stimulus name, then response data")
    lines <- c(lines, "  # BLOCKORDER: index of the selected presentation order")
  } else {
    lines <- c(lines, "  # Columns: metadata, stimulus name, then response data")
  }
  
  # Add comment explaining save variables
  if (forced_choice_data$enabled && likert_data$enabled) {
    lines <- c(lines, "  # FC: $fc_response (chosen option), RT (reaction time), STATUS (1=responded, 3=timeout)")
    if ((likert_data$num_scales %||% 1) > 1) {
      lines <- c(lines, "  # Likert: $likertN_response, $likertN_rt, $likertN_status for each scale")
    } else {
      lines <- c(lines, "  # Likert: RATE (response 1-N), RATE_RT (reaction time), RATE_STATUS (1=responded, 3=timeout)")
    }
  } else if (forced_choice_data$enabled) {
    lines <- c(lines, "  # $fc_response: chosen option (1, 2, ...), RT: reaction time, STATUS: 1=responded, 3=timeout")
  } else if (likert_data$enabled) {
    if ((likert_data$num_scales %||% 1) > 1) {
      lines <- c(lines, "  # $likertN_response: response (1-N), $likertN_rt: RT, $likertN_status: 1=responded, 3=timeout")
    } else {
      lines <- c(lines, "  # RATE: response (1-N), RATE_RT: reaction time, RATE_STATUS: 1=responded, 3=timeout")
    }
  }
  
  # Add BLOCKORDER for predefined orders mode
  if (use_predefined_orders) {
    save_vars <- c(save_vars, "BLOCKORDER")
  }
  
  lines <- c(lines, sprintf("  save %s", paste(save_vars, collapse = " ")))
  
  lines <- c(lines, "  # Stop audio playback if still playing")
  if (use_predefined_orders) {
    lines <- c(lines, "  silence &target_stimulus")
  } else {
    lines <- c(lines, "  silence @2")
  }
  
  # ----- Block definition (only for random order mode) -----
  if (!use_predefined_orders) {
    block_name <- settings_data$block_name %||% "test"
    n_stimuli <- nrow(audio_data$files)
    
    lines <- c(lines, "")
    lines <- c(lines, "# Experiment block")
    
    if (display_instructions) {
      lines <- c(lines, "# Displays instructions then runs the task for all stimuli in random order")
      lines <- c(lines, sprintf("block %s", block_name))
      lines <- c(lines, "  # Show instructions screen, wait for mouse click to continue")
      lines <- c(lines, "  message instructions mouse")
    } else {
      lines <- c(lines, "# Runs the task for all stimuli in random order")
      lines <- c(lines, sprintf("block %s", block_name))
    }
    
    lines <- c(lines, "  tasklist")
    lines <- c(lines, sprintf("    # Run task %d times (once per stimulus), randomized order", n_stimuli))
    lines <- c(lines, sprintf("    %s %d all_before_repeat", task_name, n_stimuli))
    lines <- c(lines, "  end")
  } else {
    # For predefined orders mode
    
    # Add instructions block if enabled
    if (display_instructions) {
      lines <- c(lines, "")
      lines <- c(lines, "# Instructions block - shown before starting the experiment")
      lines <- c(lines, "block show_instructions")
      lines <- c(lines, "  message instructions mouse")
    }
    
    lines <- c(lines, "")
    lines <- c(lines, "# Include external file with stimulus definitions and presentation orders")
    lines <- c(lines, "# Each listener is randomly assigned one of the predefined orders")
    lines <- c(lines, "include stimuli_and_orders.txt")
  }
  
  paste(lines, collapse = "\n")
}

#' Generate external file for predefined presentation orders
#' @param audio_data Audio data with predefined_orders
#' @param instructions_data Instructions data
#' @param settings_data Settings data
#' @param task_name Name of the task
#' @return Character string with file content
generate_stimuli_orders_file <- function(audio_data, instructions_data, settings_data, task_name) {
  
  lines <- character()
  display_instructions <- instructions_data$display_instructions %||% TRUE
  
  # Get all unique stimuli across all orders
  all_stimuli <- unique(audio_data$files$filename)
  
  # ----- Sounds section -----
  lines <- c(lines, "# MP3 audio files (without .mp3 extension)")
  lines <- c(lines, "sounds")
  
  for (filename in all_stimuli) {
    stimulus_name <- tools::file_path_sans_ext(filename)
    lines <- c(lines, sprintf("  %s", stimulus_name))
  }
  
  # ----- Stimulus blocks (one per stimulus) -----
  for (i in seq_along(all_stimuli)) {
    filename <- all_stimuli[i]
    stimulus_name <- tools::file_path_sans_ext(filename)
    
    lines <- c(lines, "")
    lines <- c(lines, sprintf("block stimulus%d", i))
    lines <- c(lines, sprintf("  set &target_stimulus %s", stimulus_name))
    lines <- c(lines, sprintf('  set %%stimulus_name "%s"', stimulus_name))
  }
  
  lines <- c(lines, "")
  
  # ----- Evaluation block -----
  lines <- c(lines, "# Evaluation block - runs the task for current stimulus")
  lines <- c(lines, "block eval")
  lines <- c(lines, "  tasklist")
  lines <- c(lines, sprintf("    %s 1", task_name))
  lines <- c(lines, "  end")
  lines <- c(lines, "")
  
  # ----- Presentation order blocks using blockorder -----
  predefined_orders <- audio_data$predefined_orders
  n_orders <- length(predefined_orders)
  
  lines <- c(lines, sprintf("# Predefined presentation orders (%d total)", n_orders))
  lines <- c(lines, "# One order is randomly selected for each listener")
  
  for (order_idx in seq_along(predefined_orders)) {
    order_name <- names(predefined_orders)[order_idx]
    order_data <- predefined_orders[[order_name]]
    
    lines <- c(lines, "")
    lines <- c(lines, sprintf("# Presentation order %d (from file: %s)", order_idx, order_name))
    lines <- c(lines, "blockorder")
    
    # Add each stimulus in order with eval after each
    for (i in 1:nrow(order_data)) {
      filename <- order_data$filename[i]
      
      # Find stimulus index
      stim_idx <- which(all_stimuli == filename)
      if (length(stim_idx) > 0) {
        lines <- c(lines, sprintf("  stimulus%d", stim_idx[1]))
        lines <- c(lines, "  eval")
      }
    }
  }
  
  paste(lines, collapse = "\n")
}

#' Calculate Y positions for elements
#' @return Vector of Y positions
calculate_element_y_positions <- function(elements, fc_height = 80, 
                                          likert_height = 60, spacing = 50) {
  n <- length(elements)
  if (n == 0) return(numeric(0))
  
  heights <- sapply(elements, function(e) {
    if (identical(e, "fc")) fc_height else likert_height
  })
  
  total_height <- sum(heights) + spacing * (n - 1)
  y_top <- total_height / 2
  
  positions <- numeric(n)
  current_y <- y_top
  
  for (i in 1:n) {
    positions[i] <- current_y - heights[i] / 2
    current_y <- current_y - heights[i] - spacing
  }
  
  return(positions)
}

#' Create button image
#' @return ggplot object
create_button_image <- function(label, text_color = "#000000", 
                                bg_color = "#FFFFE0",
                                width = 100, height = 60) {
  
  # Calculate font size
  label_lines <- str_split(label, "\n")[[1]]
  max_chars <- max(nchar(label_lines))
  font_size <- min(
    width / (max_chars + 2) * 0.35,
    height / (length(label_lines) + 1) * 0.4
  )
  
  ggplot() +
    annotate("rect", xmin = 0, xmax = width, ymin = 0, ymax = height,
             fill = bg_color, color = NA) +
    annotate("text", x = width / 2, y = height / 2, label = label,
             color = text_color, size = font_size, lineheight = 0.8) +
    xlim(0, width) +
    ylim(0, height) +
    theme_void() +
    theme(plot.background = element_rect(fill = bg_color, color = NA))
}

#' Generate INFO.txt content
#' @return Character string
generate_info_file <- function(audio_data, forced_choice_data, likert_data) {
  lines <- c(
    "PsyToolkit Experiment Package",
    "=============================",
    "",
    sprintf("Generated: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    "Generated by: PsyToolkit Listening Test Designer (Shiny app)",
    "",
    "Contents:",
    "---------",
    "- code.psy: PsyToolkit experiment script",
    "- instructions.png: Instructions screen",
    ""
  )
  
  if (forced_choice_data$enabled) {
    lines <- c(lines, sprintf("- choice_*.png: %d forced choice button images",
                              forced_choice_data$num_choices))
  }
  
  if (likert_data$enabled) {
    lines <- c(lines, sprintf("- left*.png, right*.png: Likert scale labels (%d scale(s))",
                              likert_data$num_scales))
    lines <- c(lines, "- selection_item.png: Likert scale selection marker")
  }
  
  lines <- c(lines, "",
             sprintf("Audio stimuli: %d files", nrow(audio_data$files)),
             "")
  
  # Check for missing MP3s
  missing <- audio_data$files %>% filter(!has_file)
  if (nrow(missing) > 0) {
    lines <- c(lines, 
               "NOTE: The following MP3 files need to be uploaded to PsyToolkit:",
               paste("  -", missing$filename, collapse = "\n"),
               "")
  }
  
  lines <- c(lines,
             "Usage:",
             "------",
             "1. Upload this ZIP file to PsyToolkit (Create new experiment > Method 2)",
             "2. Add any missing MP3 files via 'Upload image or sound files'",
             "3. Click Save, then Compile",
             "",
             "For more information: https://www.psytoolkit.org/")
  
  paste(lines, collapse = "\n")
}

#' Sanitize filename by removing/replacing special characters
#' Handles accented characters by converting to their non-accented equivalents
#' @param name The filename to sanitize
#' @return Sanitized filename safe for use in PsyToolkit
sanitize_filename <- function(name) {
  if (is.null(name) || name == "") return("Untitled_Project")
  
  # Convert accented characters to non-accented equivalents
  # Common French/European accented characters
  accents <- c(
    "à" = "a", "â" = "a", "ä" = "a", "á" = "a", "ã" = "a",
    "À" = "A", "Â" = "A", "Ä" = "A", "Á" = "A", "Ã" = "A",
    "è" = "e", "ê" = "e", "ë" = "e", "é" = "e",
    "È" = "E", "Ê" = "E", "Ë" = "E", "É" = "E",
    "ì" = "i", "î" = "i", "ï" = "i", "í" = "i",
    "Ì" = "I", "Î" = "I", "Ï" = "I", "Í" = "I",
    "ò" = "o", "ô" = "o", "ö" = "o", "ó" = "o", "õ" = "o",
    "Ò" = "O", "Ô" = "O", "Ö" = "O", "Ó" = "O", "Õ" = "O",
    "ù" = "u", "û" = "u", "ü" = "u", "ú" = "u",
    "Ù" = "U", "Û" = "U", "Ü" = "U", "Ú" = "U",
    "ç" = "c", "Ç" = "C",
    "ñ" = "n", "Ñ" = "N",
    "ÿ" = "y", "Ÿ" = "Y",
    "æ" = "ae", "Æ" = "AE",
    "œ" = "oe", "Œ" = "OE",
    "ß" = "ss"
  )
  
  result <- name
  for (i in seq_along(accents)) {
    result <- gsub(names(accents)[i], accents[i], result, fixed = TRUE)
  }
  
  # Replace spaces and special characters with underscores
  result <- gsub("[[:space:]]+", "_", result)
  result <- gsub("[^a-zA-Z0-9_-]", "_", result)
  
  # Remove consecutive underscores
  result <- gsub("_+", "_", result)
  
  # Remove leading/trailing underscores
  result <- gsub("^_+|_+$", "", result)
  
  if (result == "") result <- "Untitled_Project"
  
  return(result)
}
