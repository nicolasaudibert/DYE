# =============================================================================
# Module: Project Import/Export
# =============================================================================
# Handles saving and loading project state including:
# - All configuration settings
# - Audio files
# - Image files (instructions, buttons, Likert items)
# - Packaged as a single ZIP file
# =============================================================================

# -----------------------------------------------------------------------------
# UI Function
# -----------------------------------------------------------------------------
mod_project_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Project controls in a well-styled panel
    div(
      class = "panel panel-default",
      style = "margin-bottom: 15px;",
      
      div(
        class = "panel-heading",
        style = "padding: 8px 12px;",
        fluidRow(
          column(
            4, 
            div(
              style = "display: flex; align-items: center; gap: 10px;",
              tags$strong(icon("folder-open"), " Project:"),
              textInput(
                ns("project_name"),
                label = NULL,
                value = "Untitled Project",
                width = "200px"
              )
            )
          ),
          column(
            8,
            style = "text-align: right;",
            actionButton(
              ns("new_project"),
              "New",
              icon = icon("file"),
              class = "btn-default btn-sm"
            ),
            downloadButton(
              ns("download_trigger"),
              "Save",
              icon = icon("save"),
              class = "btn-primary btn-sm"
            ),
            actionButton(
              ns("load_project_btn"),
              "Load",
              icon = icon("folder-open"),
              class = "btn-default btn-sm"
            )
          )
        )
      ),
      
      # Hidden file input for loading
      div(
        style = "display: none;",
        fileInput(
          ns("load_project_file"),
          label = NULL,
          accept = c(".zip", "application/zip")
        )
      )
    ),
    
    # Confirmation modal for New/Load Project
    shinyjs::hidden(
      div(
        id = ns("confirm_modal"),
        class = "modal fade in",
        style = "display: block; background: rgba(0,0,0,0.5); position: fixed; top: 0; left: 0; right: 0; bottom: 0; z-index: 1050;",
        div(
          class = "modal-dialog",
          style = "margin-top: 100px;",
          div(
            class = "modal-content",
            div(
              class = "modal-header",
              tags$h4(class = "modal-title", "Unsaved Changes")
            ),
            div(
              class = "modal-body",
              p("Do you want to save the current project before proceeding?"),
              p(class = "text-muted", "Any unsaved changes will be lost.")
            ),
            div(
              class = "modal-footer",
              downloadButton(ns("modal_save_first"), "Save First", class = "btn-primary"),
              actionButton(ns("modal_discard"), "Don't Save", class = "btn-warning"),
              actionButton(ns("modal_cancel"), "Cancel", class = "btn-default")
            )
          )
        )
      )
    )
  )
}

# -----------------------------------------------------------------------------
# Server Function
# -----------------------------------------------------------------------------
mod_project_server <- function(id, audio_data, forced_choice_data, likert_data, 
                                instructions_data, settings_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # -------------------------------------------------------------------------
    # Track project state for detecting unsaved changes
    # -------------------------------------------------------------------------
    project_state <- reactiveValues(
      last_saved_hash = NULL,
      pending_action = NULL  # "new" or "load"
    )
    
    # Compute a hash of current project state
    get_project_hash <- function() {
      state <- list(
        project_name = input$project_name,
        fc_enabled = forced_choice_data$enabled,
        fc_num = forced_choice_data$num_choices,
        fc_labels = forced_choice_data$labels,
        likert_enabled = likert_data$enabled,
        likert_num = likert_data$num_scales,
        instructions_html = instructions_data$editor_html,
        settings_text = settings_data$text_color,
        settings_bg = settings_data$bg_color
      )
      digest::digest(state, algo = "md5")
    }
    
    # Check if project has unsaved changes
    has_unsaved_changes <- reactive({
      current_hash <- get_project_hash()
      is.null(project_state$last_saved_hash) || 
        current_hash != project_state$last_saved_hash
    })
    
    # -------------------------------------------------------------------------
    # New Project button
    # -------------------------------------------------------------------------
    observeEvent(input$new_project, {
      if (has_unsaved_changes()) {
        project_state$pending_action <- "new"
        shinyjs::show("confirm_modal")
      } else {
        reset_project()
      }
    })
    
    # -------------------------------------------------------------------------
    # Load Project button - show file dialog or confirmation
    # -------------------------------------------------------------------------
    observeEvent(input$load_project_btn, {
      if (has_unsaved_changes()) {
        project_state$pending_action <- "load"
        shinyjs::show("confirm_modal")
      } else {
        shinyjs::click("load_project_file")
      }
    })
    
    # -------------------------------------------------------------------------
    # Modal actions
    # -------------------------------------------------------------------------
    
    # Cancel
    observeEvent(input$modal_cancel, {
      shinyjs::hide("confirm_modal")
      project_state$pending_action <- NULL
    })
    
    # Discard changes and proceed
    observeEvent(input$modal_discard, {
      shinyjs::hide("confirm_modal")
      action <- project_state$pending_action
      project_state$pending_action <- NULL
      
      if (action == "new") {
        reset_project()
      } else if (action == "load") {
        shinyjs::click("load_project_file")
      }
    })
    
    # Save first download handler
    output$modal_save_first <- downloadHandler(
      filename = function() {
        project_name <- input$project_name
        if (is.null(project_name) || nchar(trimws(project_name)) == 0) {
          project_name <- "Untitled_Project"
        }
        # Sanitize filename - keep only alphanumeric, underscore, hyphen, space
        project_name <- gsub("[^a-zA-Z0-9_ -]", "_", project_name)
        project_name <- gsub(" ", "_", project_name)
        paste0(project_name, ".zip")
      },
      content = function(file) {
        save_project_to_file(file)
        
        # After save, proceed with pending action
        shinyjs::hide("confirm_modal")
        action <- project_state$pending_action
        project_state$pending_action <- NULL
        
        if (action == "new") {
          shinyjs::delay(500, reset_project())
        } else if (action == "load") {
          shinyjs::delay(500, shinyjs::click("load_project_file"))
        }
      },
      contentType = "application/zip"
    )
    
    # -------------------------------------------------------------------------
    # Reset project to defaults
    # -------------------------------------------------------------------------
    reset_project <- function() {
      # Reset project name
      updateTextInput(session, "project_name", value = "Untitled Project")
      
      # Reset audio data
      audio_data$files <- tibble::tibble(
        filename = character(),
        has_file = logical(),
        datapath = character()
      )
      audio_data$metadata <- tibble::tibble()
      audio_data$metadata_cols <- character()
      audio_data$use_predefined_orders <- FALSE
      audio_data$predefined_orders <- list()
      
      # Reset forced choice
      forced_choice_data$enabled <- FALSE
      forced_choice_data$num_choices <- 4
      forced_choice_data$num_rows <- 1
      forced_choice_data$timeout <- 10000
      forced_choice_data$h_spacing <- 50
      forced_choice_data$v_spacing <- 50
      forced_choice_data$box_height <- 80
      forced_choice_data$labels <- list("Choice 1", "Choice 2", "Choice 3", "Choice 4")
      forced_choice_data$text_colors <- list()
      forced_choice_data$bg_colors <- list()
      forced_choice_data$font_sizes <- list()
      forced_choice_data$use_images <- list(FALSE, FALSE, FALSE, FALSE)
      forced_choice_data$images <- list()
      
      # Reset Likert
      likert_data$enabled <- FALSE
      likert_data$num_scales <- 1
      likert_data$auto_spacing <- TRUE
      likert_data$shared_item_style <- TRUE
      likert_data$global_item_shape <- "disc"
      likert_data$item_size <- 30
      likert_data$item_spacing <- 20
      likert_data$label_font_size <- 8
      likert_data$scales <- list(
        list(
          num_points = 7,
          left_label = "Left",
          right_label = "Right",
          left_font_size = 0,
          right_font_size = 0,
          left_use_image = FALSE,
          right_use_image = FALSE,
          y_position = 0,
          timeout = 10000,
          item_shape = "disc",
          label_color = "#000000",
          label_bg = "#FFFFE0"
        )
      )
      
      # Reset instructions
      instructions_data$use_editor <- TRUE
      instructions_data$bg_color <- "#000000"
      instructions_data$default_text_color <- "#FFFFFF"
      instructions_data$editor_html <- ""
      instructions_data$editor_content <- ""
      instructions_data$uploaded_image <- NULL
      
      # Reset settings to defaults (Classic theme)
      settings_data$text_color <- "#000000"
      settings_data$bg_color <- "#FFFFE0"
      settings_data$img_bg_color <- "#000000"
      settings_data$likert_item_color <- "#FFFFE0"
      settings_data$audio_loop <- FALSE
      
      # Update saved hash
      shinyjs::delay(100, {
        project_state$last_saved_hash <- get_project_hash()
      })
      
      showNotification("New project created", type = "message", duration = 3)
    }
    
    # -------------------------------------------------------------------------
    # Save project to file (shared function)
    # -------------------------------------------------------------------------
    save_project_to_file <- function(file) {
      withProgress(message = "Saving project...", value = 0, {
        
        # Create temp directory for project
        temp_dir <- tempfile("project_")
        dir.create(temp_dir)
        
        # Create subdirectories
        audio_dir <- file.path(temp_dir, "audio")
        images_dir <- file.path(temp_dir, "images")
        dir.create(audio_dir)
        dir.create(images_dir)
        
        incProgress(0.1, detail = "Collecting configuration...")
        
        # Collect all configuration data
        project_config <- list(
          version = "1.0",
          created = as.character(Sys.time()),
          project_name = input$project_name %||% "Untitled Project",
          
          # Audio data
          audio = list(
            files = if (nrow(audio_data$files) > 0) {
              list(
                filename = as.list(audio_data$files$filename),
                has_file = as.list(audio_data$files$has_file)
              )
            } else {
              list()
            },
            metadata_cols = as.list(audio_data$metadata_cols %||% character()),
            metadata = if (nrow(audio_data$metadata) > 0) {
              lapply(1:nrow(audio_data$metadata), function(i) {
                as.list(audio_data$metadata[i, ])
              })
            } else {
              list()
            },
            use_predefined_orders = audio_data$use_predefined_orders %||% FALSE,
            predefined_orders = if (length(audio_data$predefined_orders) > 0) {
              lapply(audio_data$predefined_orders, function(df) {
                lapply(1:nrow(df), function(i) as.list(df[i, ]))
              })
            } else {
              list()
            }
          ),
          
          # Forced choice data
          forced_choice = list(
            enabled = forced_choice_data$enabled %||% FALSE,
            num_choices = forced_choice_data$num_choices %||% 4,
            num_rows = forced_choice_data$num_rows %||% 1,
            timeout = forced_choice_data$timeout %||% 10000,
            h_spacing = forced_choice_data$h_spacing %||% 50,
            v_spacing = forced_choice_data$v_spacing %||% 50,
            box_height = forced_choice_data$box_height %||% 80,
            labels = as.list(forced_choice_data$labels %||% list()),
            text_colors = as.list(forced_choice_data$text_colors %||% list()),
            bg_colors = as.list(forced_choice_data$bg_colors %||% list()),
            font_sizes = as.list(forced_choice_data$font_sizes %||% list()),
            use_images = as.list(forced_choice_data$use_images %||% list())
          ),
          
          # Likert data
          likert = list(
            enabled = likert_data$enabled %||% FALSE,
            num_scales = likert_data$num_scales %||% 1,
            auto_spacing = likert_data$auto_spacing %||% TRUE,
            shared_item_style = likert_data$shared_item_style %||% TRUE,
            global_item_shape = likert_data$global_item_shape %||% "disc",
            item_size = likert_data$item_size %||% 30,
            item_spacing = likert_data$item_spacing %||% 20,
            label_font_size = likert_data$label_font_size %||% 8,
            scales = lapply(likert_data$scales %||% list(), function(s) {
              list(
                num_points = s$num_points %||% 7,
                timeout = s$timeout %||% 10000,
                y_position = s$y_position %||% 0,
                left_label = s$left_label %||% "Left",
                right_label = s$right_label %||% "Right",
                left_font_size = s$left_font_size %||% 0,
                right_font_size = s$right_font_size %||% 0,
                left_use_image = s$left_use_image %||% FALSE,
                right_use_image = s$right_use_image %||% FALSE,
                item_shape = s$item_shape %||% "disc",
                label_color = s$label_color %||% "#000000",
                label_bg = s$label_bg %||% "#FFFFE0"
              )
            })
          ),
          
          # Instructions data
          instructions = list(
            use_editor = instructions_data$use_editor %||% TRUE,
            bg_color = instructions_data$bg_color %||% "#000000",
            default_text_color = instructions_data$default_text_color %||% "#FFFFFF",
            editor_html = instructions_data$editor_html %||% "",
            editor_content = instructions_data$editor_content %||% ""
          ),
          
          # Settings data
          settings = list(
            text_color = settings_data$text_color %||% "#000000",
            bg_color = settings_data$bg_color %||% "#FFFFE0",
            img_bg_color = settings_data$img_bg_color %||% "#000000",
            likert_item_color = settings_data$likert_item_color %||% "#FFFFE0",
            audio_loop = settings_data$audio_loop %||% FALSE
          )
        )
        
        incProgress(0.2, detail = "Copying audio files...")
        
        # Copy audio files
        audio_file_refs <- list()
        if (nrow(audio_data$files) > 0 && "datapath" %in% names(audio_data$files)) {
          for (i in 1:nrow(audio_data$files)) {
            row <- audio_data$files[i, ]
            if (!is.null(row$datapath) && !is.na(row$datapath) && file.exists(row$datapath)) {
              dest_name <- paste0("audio_", i, "_", row$filename)
              dest_path <- file.path(audio_dir, dest_name)
              file.copy(row$datapath, dest_path)
              audio_file_refs[[i]] <- dest_name
            } else {
              audio_file_refs[[i]] <- ""  # Placeholder for files not uploaded
            }
          }
          project_config$audio$file_refs <- audio_file_refs
        }
        
        incProgress(0.4, detail = "Copying image files...")
        
        # Copy forced choice images
        fc_image_refs <- list()
        if (!is.null(forced_choice_data$images)) {
          for (i in seq_along(forced_choice_data$images)) {
            img <- forced_choice_data$images[[i]]
            if (!is.null(img) && !is.null(img$datapath) && file.exists(img$datapath)) {
              dest_name <- paste0("fc_", i, "_", img$name)
              dest_path <- file.path(images_dir, dest_name)
              file.copy(img$datapath, dest_path)
              fc_image_refs[[i]] <- dest_name
            }
          }
          project_config$forced_choice$image_refs <- fc_image_refs
        }
        
        # Copy Likert images
        for (i in seq_along(likert_data$scales %||% list())) {
          scale <- likert_data$scales[[i]]
          
          if (!is.null(scale$left_image) && !is.null(scale$left_image$datapath) &&
              file.exists(scale$left_image$datapath)) {
            dest_name <- paste0("likert_", i, "_left_", scale$left_image$name)
            dest_path <- file.path(images_dir, dest_name)
            file.copy(scale$left_image$datapath, dest_path)
            project_config$likert$scales[[i]]$left_image_ref <- dest_name
          }
          
          if (!is.null(scale$right_image) && !is.null(scale$right_image$datapath) &&
              file.exists(scale$right_image$datapath)) {
            dest_name <- paste0("likert_", i, "_right_", scale$right_image$name)
            dest_path <- file.path(images_dir, dest_name)
            file.copy(scale$right_image$datapath, dest_path)
            project_config$likert$scales[[i]]$right_image_ref <- dest_name
          }
          
          if (!is.null(scale$item_image) && !is.null(scale$item_image$datapath) &&
              file.exists(scale$item_image$datapath)) {
            dest_name <- paste0("likert_", i, "_item_", scale$item_image$name)
            dest_path <- file.path(images_dir, dest_name)
            file.copy(scale$item_image$datapath, dest_path)
            project_config$likert$scales[[i]]$item_image_ref <- dest_name
          }
        }
        
        # Copy instructions image
        if (!is.null(instructions_data$uploaded_image) && 
            !is.null(instructions_data$uploaded_image$datapath) &&
            file.exists(instructions_data$uploaded_image$datapath)) {
          dest_name <- paste0("instructions_", instructions_data$uploaded_image$name)
          dest_path <- file.path(images_dir, dest_name)
          file.copy(instructions_data$uploaded_image$datapath, dest_path)
          project_config$instructions$image_ref <- dest_name
        }
        
        incProgress(0.6, detail = "Writing configuration...")
        
        # Write configuration JSON
        config_path <- file.path(temp_dir, "project_config.json")
        jsonlite::write_json(project_config, config_path, pretty = TRUE, auto_unbox = TRUE)
        
        incProgress(0.8, detail = "Creating ZIP archive...")
        
        # Create ZIP file
        old_wd <- getwd()
        setwd(temp_dir)
        zip::zip(file, files = list.files(".", recursive = TRUE))
        setwd(old_wd)
        
        incProgress(1.0, detail = "Done!")
        
        # Clean up temp dir
        unlink(temp_dir, recursive = TRUE)
        
        # Update saved hash
        project_state$last_saved_hash <- get_project_hash()
      })
    }
    
    # -------------------------------------------------------------------------
    # Download handler for saving project
    # -------------------------------------------------------------------------
    output$download_trigger <- downloadHandler(
      filename = function() {
        project_name <- input$project_name
        sanitized_name <- sanitize_project_filename(project_name)
        paste0(sanitized_name, ".zip")
      },
      content = function(file) {
        save_project_to_file(file)
      },
      contentType = "application/zip"
    )
    
    # -------------------------------------------------------------------------
    # Load project
    # -------------------------------------------------------------------------
    observeEvent(input$load_project_file, {
      req(input$load_project_file)
      
      withProgress(message = "Loading project...", value = 0, {
        
        zip_path <- input$load_project_file$datapath
        
        # Create temp directory for extraction
        temp_dir <- tempfile("project_load_")
        dir.create(temp_dir)
        
        incProgress(0.1, detail = "Extracting archive...")
        
        # Extract ZIP
        zip::unzip(zip_path, exdir = temp_dir)
        
        incProgress(0.2, detail = "Reading configuration...")
        
        # Read configuration
        config_path <- file.path(temp_dir, "project_config.json")
        if (!file.exists(config_path)) {
          showNotification("Invalid project file: missing configuration", type = "error")
          unlink(temp_dir, recursive = TRUE)
          return()
        }
        
        config <- jsonlite::read_json(config_path)
        
        incProgress(0.3, detail = "Restoring project name...")
        
        # Restore project name (preserve original with spaces/special chars)
        if (!is.null(config$project_name)) {
          updateTextInput(session, "project_name", value = config$project_name)
        }
        
        incProgress(0.4, detail = "Restoring settings...")
        
        # Restore settings
        if (!is.null(config$settings)) {
          settings_data$text_color <- config$settings$text_color
          settings_data$bg_color <- config$settings$bg_color
          settings_data$img_bg_color <- config$settings$img_bg_color
          settings_data$likert_item_color <- config$settings$likert_item_color
          settings_data$audio_loop <- config$settings$audio_loop
        }
        
        incProgress(0.5, detail = "Restoring instructions...")
        
        # Restore instructions
        if (!is.null(config$instructions)) {
          instructions_data$use_editor <- config$instructions$use_editor
          instructions_data$bg_color <- config$instructions$bg_color
          instructions_data$default_text_color <- config$instructions$default_text_color
          instructions_data$editor_html <- config$instructions$editor_html
          instructions_data$editor_content <- config$instructions$editor_content
          
          # Restore uploaded image
          if (!is.null(config$instructions$image_ref)) {
            img_path <- file.path(temp_dir, "images", config$instructions$image_ref)
            if (file.exists(img_path)) {
              new_path <- tempfile(fileext = ".png")
              file.copy(img_path, new_path)
              instructions_data$uploaded_image <- list(
                name = config$instructions$image_ref,
                datapath = new_path
              )
            }
          }
        }
        
        incProgress(0.6, detail = "Restoring forced choice...")
        
        # Restore forced choice
        if (!is.null(config$forced_choice)) {
          forced_choice_data$enabled <- config$forced_choice$enabled
          forced_choice_data$num_choices <- config$forced_choice$num_choices
          forced_choice_data$num_rows <- config$forced_choice$num_rows
          forced_choice_data$timeout <- config$forced_choice$timeout
          forced_choice_data$h_spacing <- config$forced_choice$h_spacing
          forced_choice_data$v_spacing <- config$forced_choice$v_spacing
          forced_choice_data$box_height <- config$forced_choice$box_height
          forced_choice_data$labels <- config$forced_choice$labels
          forced_choice_data$text_colors <- config$forced_choice$text_colors
          forced_choice_data$bg_colors <- config$forced_choice$bg_colors
          forced_choice_data$font_sizes <- config$forced_choice$font_sizes
          forced_choice_data$use_images <- config$forced_choice$use_images
          
          # Restore FC images
          if (!is.null(config$forced_choice$image_refs)) {
            images <- list()
            for (i in seq_along(config$forced_choice$image_refs)) {
              ref <- config$forced_choice$image_refs[[i]]
              if (!is.null(ref)) {
                img_path <- file.path(temp_dir, "images", ref)
                if (file.exists(img_path)) {
                  ext <- tools::file_ext(ref)
                  new_path <- tempfile(fileext = paste0(".", ext))
                  file.copy(img_path, new_path)
                  images[[i]] <- list(name = ref, datapath = new_path)
                }
              }
            }
            forced_choice_data$images <- images
          }
        }
        
        incProgress(0.7, detail = "Restoring Likert scales...")
        
        # Restore Likert data
        if (!is.null(config$likert)) {
          likert_data$enabled <- config$likert$enabled
          likert_data$num_scales <- config$likert$num_scales
          likert_data$auto_spacing <- config$likert$auto_spacing
          likert_data$shared_item_style <- config$likert$shared_item_style
          likert_data$global_item_shape <- config$likert$global_item_shape
          likert_data$item_size <- config$likert$item_size
          likert_data$item_spacing <- config$likert$item_spacing
          likert_data$label_font_size <- config$likert$label_font_size
          
          # Restore scales with images
          scales <- list()
          for (i in seq_along(config$likert$scales)) {
            s <- config$likert$scales[[i]]
            scale <- list(
              num_points = s$num_points,
              timeout = s$timeout,
              y_position = s$y_position,
              left_label = s$left_label,
              right_label = s$right_label,
              left_font_size = s$left_font_size,
              right_font_size = s$right_font_size,
              left_use_image = s$left_use_image,
              right_use_image = s$right_use_image,
              item_shape = s$item_shape,
              label_color = s$label_color,
              label_bg = s$label_bg
            )
            
            # Restore images
            if (!is.null(s$left_image_ref)) {
              img_path <- file.path(temp_dir, "images", s$left_image_ref)
              if (file.exists(img_path)) {
                ext <- tools::file_ext(s$left_image_ref)
                new_path <- tempfile(fileext = paste0(".", ext))
                file.copy(img_path, new_path)
                scale$left_image <- list(name = s$left_image_ref, datapath = new_path)
              }
            }
            
            if (!is.null(s$right_image_ref)) {
              img_path <- file.path(temp_dir, "images", s$right_image_ref)
              if (file.exists(img_path)) {
                ext <- tools::file_ext(s$right_image_ref)
                new_path <- tempfile(fileext = paste0(".", ext))
                file.copy(img_path, new_path)
                scale$right_image <- list(name = s$right_image_ref, datapath = new_path)
              }
            }
            
            if (!is.null(s$item_image_ref)) {
              img_path <- file.path(temp_dir, "images", s$item_image_ref)
              if (file.exists(img_path)) {
                ext <- tools::file_ext(s$item_image_ref)
                new_path <- tempfile(fileext = paste0(".", ext))
                file.copy(img_path, new_path)
                scale$item_image <- list(name = s$item_image_ref, datapath = new_path)
              }
            }
            
            scales[[i]] <- scale
          }
          likert_data$scales <- scales
        }
        
        incProgress(0.8, detail = "Restoring audio files...")
        
        # Restore audio data
        if (!is.null(config$audio)) {
          # Restore files list
          if (!is.null(config$audio$files) && length(config$audio$files$filename) > 0) {
            audio_files <- tibble::tibble(
              filename = unlist(config$audio$files$filename),
              has_file = unlist(config$audio$files$has_file),
              datapath = NA_character_
            )
            
            # Restore actual audio files if they were saved
            if (!is.null(config$audio$file_refs)) {
              for (i in seq_along(config$audio$file_refs)) {
                ref <- config$audio$file_refs[[i]]
                if (!is.null(ref) && nchar(ref) > 0) {
                  audio_path <- file.path(temp_dir, "audio", ref)
                  if (file.exists(audio_path)) {
                    new_path <- tempfile(fileext = ".mp3")
                    file.copy(audio_path, new_path)
                    
                    # Find matching filename and update datapath
                    if (i <= nrow(audio_files)) {
                      audio_files$datapath[i] <- new_path
                      audio_files$has_file[i] <- TRUE
                    }
                  }
                }
              }
            }
            
            audio_data$files <- audio_files
          } else {
            audio_data$files <- tibble::tibble(
              filename = character(),
              has_file = logical(),
              datapath = character()
            )
          }
          
          # Restore metadata columns
          audio_data$metadata_cols <- unlist(config$audio$metadata_cols %||% list())
          
          # Restore metadata
          if (!is.null(config$audio$metadata) && length(config$audio$metadata) > 0) {
            metadata_list <- lapply(config$audio$metadata, function(row) {
              as.data.frame(row, stringsAsFactors = FALSE)
            })
            audio_data$metadata <- dplyr::bind_rows(metadata_list)
          } else {
            audio_data$metadata <- tibble::tibble()
          }
          
          # Restore predefined orders
          audio_data$use_predefined_orders <- config$audio$use_predefined_orders %||% FALSE
          
          if (!is.null(config$audio$predefined_orders) && length(config$audio$predefined_orders) > 0) {
            orders <- list()
            for (order_name in names(config$audio$predefined_orders)) {
              order_rows <- config$audio$predefined_orders[[order_name]]
              order_list <- lapply(order_rows, function(row) {
                as.data.frame(row, stringsAsFactors = FALSE)
              })
              orders[[order_name]] <- dplyr::bind_rows(order_list)
            }
            audio_data$predefined_orders <- orders
          } else {
            audio_data$predefined_orders <- list()
          }
        }
        
        incProgress(1.0, detail = "Done!")
        
        # Clean up
        unlink(temp_dir, recursive = TRUE)
        
        # Update saved hash after loading
        shinyjs::delay(200, {
          project_state$last_saved_hash <- get_project_hash()
        })
        
        showNotification(
          "Project loaded successfully!",
          type = "message",
          duration = 3
        )
      })
    })
    
  })
}

#' Sanitize project filename by removing/replacing special characters
#' Handles accented characters by converting to their non-accented equivalents
#' @param name The filename to sanitize
#' @return Sanitized filename
sanitize_project_filename <- function(name) {
  if (is.null(name) || nchar(trimws(name)) == 0) return("Untitled_Project")
  
  # Convert accented characters to non-accented equivalents
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
