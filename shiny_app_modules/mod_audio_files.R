# =============================================================================
# Module: Audio Files Management
# =============================================================================
# Handles importing audio file lists (Excel/CSV/TXT) and/or direct MP3 uploads,
# manages stimulus metadata, and validates consistency between sources.
# =============================================================================

# -----------------------------------------------------------------------------
# UI Function
# -----------------------------------------------------------------------------
mod_audio_files_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    br(),
    
    # Section: Import from list
    h4("Import Stimulus List"),
    helpText("Single file (Excel/CSV/TXT) for random order, or ZIP archive with multiple files for predefined presentation orders."),
    fileInput(
      ns("list_upload"),
      label = NULL,
      accept = c(".xlsx", ".xls", ".csv", ".tsv", ".txt", ".zip"),
      placeholder = "Excel, CSV, TXT or ZIP file"
    ),
    
    # Info about predefined orders if active
    uiOutput(ns("predefined_orders_info")),
    
    # Dynamic UI for column selection (shown after file upload)
    uiOutput(ns("column_selection_ui")),
    
    hr(),
    
    # Section: Direct MP3 upload
    h4("Upload MP3 Files"),
    fileInput(
      ns("mp3_upload"),
      label = NULL,
      multiple = TRUE,
      accept = c(".mp3", "audio/mpeg"),
      placeholder = "Select MP3 files"
    ),
    
    hr(),
    
    # Section: Current stimulus list
    h4("Stimulus List"),
    
    # Action buttons
    fluidRow(
      column(6, actionButton(ns("remove_selected"), "Remove Selected", 
                             icon = icon("trash"), class = "btn-sm")),
      column(6, actionButton(ns("clear_all"), "Clear All", 
                             icon = icon("times-circle"), class = "btn-sm"))
    ),
    
    br(),
    
    # DataTable for stimulus display
    DT::dataTableOutput(ns("stimulus_table")),
    
    hr(),
    
    # Section: Metadata columns selection
    h4("Metadata Columns"),
    helpText("Select columns to include in PsyToolkit output:"),
    uiOutput(ns("metadata_selection_ui")),
    
    # Warning messages
    uiOutput(ns("warnings_ui"))
  )
}

# -----------------------------------------------------------------------------
# Server Function
# -----------------------------------------------------------------------------
mod_audio_files_server <- function(id, audio_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # -------------------------------------------------------------------------
    # Reactive values for internal state
    # -------------------------------------------------------------------------
    internal <- reactiveValues(
      imported_list = NULL,        
      imported_columns = NULL,     
      filename_column = NULL,      
      metadata_columns = NULL,     
      uploaded_mp3_names = character(),
      warnings = character(),
      # Predefined orders support
      use_predefined_orders = FALSE,
      predefined_orders = list(),      # List of data frames, one per order
      predefined_order_names = character()  # Names of the order files
    )
    
    # -------------------------------------------------------------------------
    # Handle list file upload (single file or ZIP with predefined orders)
    # -------------------------------------------------------------------------
    observeEvent(input$list_upload, {
      req(input$list_upload)
      
      file_path <- input$list_upload$datapath
      file_name <- input$list_upload$name
      file_ext <- tolower(tools::file_ext(file_name))
      
      tryCatch({
        # Check if ZIP file (predefined orders)
        if (file_ext == "zip") {
          # Extract ZIP contents
          temp_dir <- tempfile("zip_extract_")
          dir.create(temp_dir)
          zip::unzip(file_path, exdir = temp_dir)
          
          # List extracted files
          extracted_files <- list.files(temp_dir, full.names = TRUE, recursive = TRUE)
          valid_exts <- c("xlsx", "xls", "csv", "tsv", "txt")
          data_files <- extracted_files[tolower(tools::file_ext(extracted_files)) %in% valid_exts]
          
          if (length(data_files) == 0) {
            showNotification("No valid data files found in ZIP archive.", type = "error")
            return()
          }
          
          # Read all files
          orders_list <- list()
          all_columns <- NULL
          all_stimuli <- list()
          
          for (f in data_files) {
            f_ext <- tolower(tools::file_ext(f))
            data <- switch(
              f_ext,
              "xlsx" = read_excel(f),
              "xls" = read_excel(f),
              "csv" = read_csv(f, show_col_types = FALSE),
              "tsv" = read_tsv(f, show_col_types = FALSE),
              "txt" = {
                first_lines <- readLines(f, n = 5)
                if (any(str_detect(first_lines, "\t"))) {
                  read_tsv(f, show_col_types = FALSE)
                } else if (any(str_detect(first_lines, ";"))) {
                  read_delim(f, delim = ";", show_col_types = FALSE)
                } else if (any(str_detect(first_lines, ","))) {
                  read_csv(f, show_col_types = FALSE)
                } else {
                  tibble(filename = readLines(f)) %>% filter(filename != "")
                }
              }
            )
            
            orders_list[[basename(f)]] <- data
            
            # Check column consistency
            if (is.null(all_columns)) {
              all_columns <- names(data)
            } else if (!identical(sort(names(data)), sort(all_columns))) {
              showNotification(
                sprintf("Warning: Columns differ between files. File '%s' has different columns.", basename(f)),
                type = "warning",
                duration = 5
              )
            }
          }
          
          internal$use_predefined_orders <- TRUE
          internal$predefined_orders <- orders_list
          internal$predefined_order_names <- names(orders_list)
          
          # Use first file for column selection
          internal$imported_list <- orders_list[[1]]
          internal$imported_columns <- names(orders_list[[1]])
          
          showNotification(
            sprintf("Loaded %d predefined presentation orders from ZIP.", length(orders_list)),
            type = "message",
            duration = 3
          )
          
          # Show column selection modal
          showModal(modalDialog(
            title = "Select Columns (Predefined Orders)",
            size = "m",
            
            selectInput(
              ns("filename_col_select"),
              "Column containing stimulus filenames:",
              choices = internal$imported_columns,
              selected = detect_filename_column(internal$imported_columns)
            ),
            
            checkboxInput(
              ns("filenames_have_extension"),
              "Filenames include .mp3 extension",
              value = TRUE
            ),
            
            checkboxGroupInput(
              ns("metadata_cols_select"),
              "Select metadata columns to include:",
              choices = internal$imported_columns
            ),
            
            footer = tagList(
              modalButton("Cancel"),
              actionButton(ns("confirm_columns"), "Confirm", class = "btn-primary")
            )
          ))
          
        } else {
          # Single file - original behavior
          internal$use_predefined_orders <- FALSE
          internal$predefined_orders <- list()
          internal$predefined_order_names <- character()
          
          # Read file based on extension
          data <- switch(
            file_ext,
            "xlsx" = read_excel(file_path),
            "xls" = read_excel(file_path),
            "csv" = read_csv(file_path, show_col_types = FALSE),
            "tsv" = read_tsv(file_path, show_col_types = FALSE),
            "txt" = {
              # Try to detect delimiter
              first_lines <- readLines(file_path, n = 5)
              if (any(str_detect(first_lines, "\t"))) {
                read_tsv(file_path, show_col_types = FALSE)
              } else if (any(str_detect(first_lines, ";"))) {
                read_delim(file_path, delim = ";", show_col_types = FALSE)
              } else if (any(str_detect(first_lines, ","))) {
                read_csv(file_path, show_col_types = FALSE)
              } else {
                # Single column (one filename per line)
                tibble(filename = readLines(file_path)) %>%
                  filter(filename != "")
              }
            }
          )
          
          internal$imported_list <- data
          internal$imported_columns <- names(data)
        
          # Show column selection modal
          showModal(modalDialog(
            title = "Select Columns",
            size = "m",
            
            selectInput(
              ns("filename_col_select"),
              "Column containing stimulus filenames:",
              choices = internal$imported_columns,
              selected = detect_filename_column(internal$imported_columns)
            ),
            
            checkboxInput(
              ns("filenames_have_extension"),
              "Filenames include .mp3 extension",
              value = TRUE
            ),
            
            hr(),
            
            checkboxGroupInput(
              ns("metadata_cols_select"),
              "Metadata columns to include:",
              choices = internal$imported_columns,
              selected = internal$imported_columns
            ),
            
            footer = tagList(
              modalButton("Cancel"),
              actionButton(ns("confirm_import"), "Import", class = "btn-primary")
            )
          ))
        }  # End of else (single file)
        
      }, error = function(e) {
        showNotification(
          paste("Error reading file:", e$message),
          type = "error",
          duration = 5
        )
      })
    })
    
    # -------------------------------------------------------------------------
    # Update metadata selection when filename column changes
    # -------------------------------------------------------------------------
    observeEvent(input$filename_col_select, {
      req(internal$imported_columns)
      
      # Remove filename column from metadata options
      metadata_options <- setdiff(internal$imported_columns, input$filename_col_select)
      
      updateCheckboxGroupInput(
        session,
        "metadata_cols_select",
        choices = metadata_options,
        selected = metadata_options
      )
    })
    
    # -------------------------------------------------------------------------
    # Confirm import from list
    # -------------------------------------------------------------------------
    observeEvent(input$confirm_import, {
      req(internal$imported_list, input$filename_col_select)
      
      removeModal()
      
      # Extract filename column
      filename_col <- input$filename_col_select
      filenames <- internal$imported_list[[filename_col]]
      
      # Add .mp3 extension if not present
      if (!input$filenames_have_extension) {
        filenames <- paste0(filenames, ".mp3")
      }
      
      # Ensure .mp3 extension (case insensitive)
      filenames <- sapply(filenames, function(f) {
        if (!str_detect(tolower(f), "\\.mp3$")) {
          paste0(f, ".mp3")
        } else {
          f
        }
      })
      
      # Get metadata columns
      metadata_cols <- input$metadata_cols_select
      internal$filename_column <- filename_col
      internal$metadata_columns <- metadata_cols
      
      # Process metadata - check for spaces and replace with underscores
      warnings <- character()
      
      if (length(metadata_cols) > 0) {
        metadata <- internal$imported_list %>%
          select(all_of(c(filename_col, metadata_cols)))
        
        # Check for spaces in metadata values
        for (col in metadata_cols) {
          if (any(str_detect(metadata[[col]], " "), na.rm = TRUE)) {
            metadata[[col]] <- str_replace_all(metadata[[col]], " ", "_")
            warnings <- c(warnings, 
              paste0("Spaces in column '", col, "' replaced with underscores."))
          }
        }
        
        # Rename filename column for consistency
        metadata <- metadata %>%
          rename(filename = !!sym(filename_col))
        
        # Add .mp3 extension to filename in metadata if needed
        if (!input$filenames_have_extension) {
          metadata <- metadata %>%
            mutate(filename = paste0(filename, ".mp3"))
        }
        
        audio_data$metadata <- metadata
        audio_data$metadata_cols <- metadata_cols
      } else {
        audio_data$metadata <- tibble(filename = filenames)
        audio_data$metadata_cols <- character()
      }
      
      # Update files list
      current_files <- audio_data$files
      new_files <- tibble(
        filename = filenames,
        has_file = filenames %in% internal$uploaded_mp3_names,
        datapath = NA_character_
      )
      
      # Merge with existing, avoiding duplicates
      if (nrow(current_files) > 0) {
        # Ensure datapath column exists
        if (!"datapath" %in% names(current_files)) {
          current_files$datapath <- NA_character_
        }
        combined <- bind_rows(current_files, new_files) %>%
          distinct(filename, .keep_all = TRUE)
        audio_data$files <- combined
      } else {
        audio_data$files <- new_files
      }
      
      # Clear predefined orders (single file mode)
      audio_data$use_predefined_orders <- FALSE
      audio_data$predefined_orders <- list()
      
      # Store warnings
      internal$warnings <- warnings
      
      showNotification(
        paste("Imported", length(filenames), "stimuli from list."),
        type = "message",
        duration = 3
      )
    })
    
    # -------------------------------------------------------------------------
    # Confirm columns for predefined orders (ZIP mode)
    # -------------------------------------------------------------------------
    observeEvent(input$confirm_columns, {
      req(internal$use_predefined_orders, internal$predefined_orders)
      req(input$filename_col_select)
      
      removeModal()
      
      filename_col <- input$filename_col_select
      metadata_cols <- input$metadata_cols_select
      has_extension <- input$filenames_have_extension
      
      internal$filename_column <- filename_col
      internal$metadata_columns <- metadata_cols
      
      warnings <- character()
      
      # Process all orders
      processed_orders <- list()
      all_stimuli <- character()
      stimuli_per_order <- list()
      
      for (order_name in names(internal$predefined_orders)) {
        order_data <- internal$predefined_orders[[order_name]]
        
        # Extract filenames
        filenames <- order_data[[filename_col]]
        
        # Add .mp3 extension if needed
        if (!has_extension) {
          filenames <- paste0(filenames, ".mp3")
        }
        
        # Ensure .mp3 extension
        filenames <- sapply(filenames, function(f) {
          if (!str_detect(tolower(f), "\\.mp3$")) paste0(f, ".mp3") else f
        })
        
        # Process metadata
        if (length(metadata_cols) > 0) {
          order_metadata <- order_data %>%
            select(all_of(c(filename_col, metadata_cols)))
          
          for (col in metadata_cols) {
            if (any(str_detect(order_metadata[[col]], " "), na.rm = TRUE)) {
              order_metadata[[col]] <- str_replace_all(order_metadata[[col]], " ", "_")
            }
          }
          
          order_metadata <- order_metadata %>%
            rename(filename = !!sym(filename_col))
          
          if (!has_extension) {
            order_metadata <- order_metadata %>%
              mutate(filename = paste0(filename, ".mp3"))
          }
        } else {
          order_metadata <- tibble(filename = filenames)
        }
        
        processed_orders[[order_name]] <- order_metadata
        stimuli_per_order[[order_name]] <- filenames
        all_stimuli <- union(all_stimuli, filenames)
      }
      
      # Check for stimuli consistency across orders
      first_order_stimuli <- sort(stimuli_per_order[[1]])
      inconsistent_orders <- character()
      
      for (order_name in names(stimuli_per_order)) {
        order_stimuli <- sort(stimuli_per_order[[order_name]])
        if (!identical(order_stimuli, first_order_stimuli)) {
          inconsistent_orders <- c(inconsistent_orders, order_name)
        }
      }
      
      if (length(inconsistent_orders) > 0) {
        warnings <- c(warnings, 
          paste0("Warning: Some orders have different stimuli: ", 
                 paste(inconsistent_orders, collapse = ", "),
                 ". This may be intentional for between-subject designs."))
      }
      
      # Update audio_data
      audio_data$use_predefined_orders <- TRUE
      audio_data$predefined_orders <- processed_orders
      audio_data$metadata_cols <- metadata_cols
      
      # Use first order as primary metadata
      audio_data$metadata <- processed_orders[[1]]
      
      # Update files list with all unique stimuli
      current_files <- audio_data$files
      new_files <- tibble(
        filename = all_stimuli,
        has_file = all_stimuli %in% internal$uploaded_mp3_names,
        datapath = NA_character_
      )
      
      if (nrow(current_files) > 0) {
        if (!"datapath" %in% names(current_files)) {
          current_files$datapath <- NA_character_
        }
        combined <- bind_rows(current_files, new_files) %>%
          distinct(filename, .keep_all = TRUE)
        audio_data$files <- combined
      } else {
        audio_data$files <- new_files
      }
      
      internal$warnings <- warnings
      
      showNotification(
        sprintf("Imported %d predefined orders with %d unique stimuli.", 
                length(processed_orders), length(all_stimuli)),
        type = "message",
        duration = 3
      )
    })
    
    # -------------------------------------------------------------------------
    # Handle direct MP3 upload
    # -------------------------------------------------------------------------
    observeEvent(input$mp3_upload, {
      req(input$mp3_upload)
      
      uploaded_files <- input$mp3_upload$name
      uploaded_paths <- input$mp3_upload$datapath
      internal$uploaded_mp3_names <- union(internal$uploaded_mp3_names, uploaded_files)
      
      # Update files list
      current_files <- audio_data$files
      
      new_files <- tibble(
        filename = uploaded_files,
        has_file = TRUE,
        datapath = uploaded_paths
      )
      
      if (nrow(current_files) > 0) {
        # Ensure datapath column exists
        if (!"datapath" %in% names(current_files)) {
          current_files$datapath <- NA_character_
        }
        # Update has_file status for existing entries
        audio_data$files <- current_files %>%
          mutate(has_file = filename %in% internal$uploaded_mp3_names) %>%
          bind_rows(new_files %>% filter(!filename %in% current_files$filename))
        
        # Update datapath for files that were re-uploaded
        for (i in seq_along(uploaded_files)) {
          idx <- which(audio_data$files$filename == uploaded_files[i])
          if (length(idx) > 0) {
            audio_data$files$datapath[idx[1]] <- uploaded_paths[i]
          }
        }
      } else {
        audio_data$files <- new_files
      }
      
      # Also update metadata if it exists
      if (nrow(audio_data$metadata) > 0) {
        # Check consistency: warn if uploaded files not in metadata
        missing_in_metadata <- setdiff(uploaded_files, audio_data$metadata$filename)
        if (length(missing_in_metadata) > 0) {
          # Add to metadata with NA values
          new_metadata_rows <- tibble(filename = missing_in_metadata)
          for (col in audio_data$metadata_cols) {
            new_metadata_rows[[col]] <- NA_character_
          }
          audio_data$metadata <- bind_rows(audio_data$metadata, new_metadata_rows)
          
          internal$warnings <- c(internal$warnings,
            paste0("Uploaded files not in list: ", 
                   paste(missing_in_metadata, collapse = ", ")))
        }
      } else {
        # Create basic metadata from uploads
        audio_data$metadata <- tibble(filename = uploaded_files)
      }
      
      showNotification(
        paste("Uploaded", length(uploaded_files), "MP3 file(s)."),
        type = "message",
        duration = 3
      )
    })
    
    # -------------------------------------------------------------------------
    # Render stimulus table
    # -------------------------------------------------------------------------
    output$stimulus_table <- DT::renderDataTable({
      req(nrow(audio_data$files) > 0)
      
      display_data <- audio_data$files %>%
        mutate(
          status = if_else(has_file, 
                           "\U2705",  # Check mark
                           "\U26A0\UFE0F")   # Warning sign
        ) %>%
        select(filename, status)
      
      DT::datatable(
        display_data,
        rownames = FALSE,
        colnames = c("Filename", "MP3"),
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 'ftp'
        ),
        selection = 'multiple'
      )
    })
    
    # -------------------------------------------------------------------------
    # Remove selected stimuli
    # -------------------------------------------------------------------------
    observeEvent(input$remove_selected, {
      selected_rows <- input$stimulus_table_rows_selected
      req(length(selected_rows) > 0)
      
      files_to_remove <- audio_data$files$filename[selected_rows]
      
      audio_data$files <- audio_data$files %>%
        filter(!filename %in% files_to_remove)
      
      if (nrow(audio_data$metadata) > 0) {
        audio_data$metadata <- audio_data$metadata %>%
          filter(!filename %in% files_to_remove)
      }
      
      internal$uploaded_mp3_names <- setdiff(internal$uploaded_mp3_names, files_to_remove)
    })
    
    # -------------------------------------------------------------------------
    # Clear all stimuli
    # -------------------------------------------------------------------------
    observeEvent(input$clear_all, {
      audio_data$files <- tibble(filename = character(), has_file = logical(), datapath = character())
      audio_data$metadata <- tibble()
      audio_data$metadata_cols <- character()
      audio_data$use_predefined_orders <- FALSE
      audio_data$predefined_orders <- list()
      internal$uploaded_mp3_names <- character()
      internal$warnings <- character()
      internal$use_predefined_orders <- FALSE
      internal$predefined_orders <- list()
      internal$predefined_order_names <- character()
    })
    
    # -------------------------------------------------------------------------
    # Enable/disable buttons based on state
    # -------------------------------------------------------------------------
    observe({
      # Remove selected button
      if (is.null(input$stimulus_table_rows_selected) || 
          length(input$stimulus_table_rows_selected) == 0) {
        shinyjs::disable("remove_selected")
      } else {
        shinyjs::enable("remove_selected")
      }
      
      # Clear all button
      if (nrow(audio_data$files) == 0) {
        shinyjs::disable("clear_all")
      } else {
        shinyjs::enable("clear_all")
      }
    })
    
    # -------------------------------------------------------------------------
    # Render metadata column selection UI
    # -------------------------------------------------------------------------
    output$metadata_selection_ui <- renderUI({
      req(length(audio_data$metadata_cols) > 0)
      
      checkboxGroupInput(
        ns("active_metadata_cols"),
        label = NULL,
        choices = audio_data$metadata_cols,
        selected = audio_data$metadata_cols,
        inline = TRUE
      )
    })
    
    # -------------------------------------------------------------------------
    # Update active metadata columns
    # -------------------------------------------------------------------------
    observeEvent(input$active_metadata_cols, {
      audio_data$metadata_cols <- input$active_metadata_cols
    }, ignoreNULL = FALSE)
    
    # -------------------------------------------------------------------------
    # Render predefined orders info UI
    # -------------------------------------------------------------------------
    output$predefined_orders_info <- renderUI({
      if (!isTRUE(audio_data$use_predefined_orders)) return(NULL)
      
      n_orders <- length(audio_data$predefined_orders)
      n_stimuli <- nrow(audio_data$files)
      
      div(
        class = "alert alert-info",
        style = "margin-top: 10px;",
        icon("random"),
        tags$strong(sprintf(" %d predefined presentation orders loaded", n_orders)),
        tags$br(),
        sprintf("Each listener will be randomly assigned one of the %d orders. ", n_orders),
        sprintf("Total unique stimuli: %d", n_stimuli),
        tags$br(),
        tags$small(
          icon("info-circle"),
          " To switch back to random order mode, import a single stimulus list file."
        )
      )
    })
    
    # -------------------------------------------------------------------------
    # Render warnings UI
    # -------------------------------------------------------------------------
    output$warnings_ui <- renderUI({
      warnings <- internal$warnings
      
      # Check for missing MP3 files
      if (nrow(audio_data$files) > 0) {
        missing_files <- audio_data$files %>%
          filter(!has_file) %>%
          pull(filename)
        
        if (length(missing_files) > 0) {
          warnings <- c(warnings,
            paste0("MP3 files not uploaded: ", length(missing_files), " file(s). ",
                   "You will need to upload them to PsyToolkit before compilation."))
        }
      }
      
      if (length(warnings) > 0) {
        tagList(
          hr(),
          div(
            class = "alert alert-warning",
            icon("exclamation-triangle"),
            tags$ul(
              lapply(warnings, function(w) tags$li(w))
            )
          )
        )
      }
    })
    
  })
}

# -----------------------------------------------------------------------------
# Helper Functions
# -----------------------------------------------------------------------------

#' Detect the most likely filename column
#' @param col_names Vector of column names
#' @return Best guess for filename column
detect_filename_column <- function(col_names) {
  # Common patterns for filename columns
  patterns <- c("filename", "file", "stimulus", "stim", "audio", "sound", "name")
  
  col_lower <- tolower(col_names)
  
  for (pattern in patterns) {
    matches <- str_detect(col_lower, pattern)
    if (any(matches)) {
      return(col_names[which(matches)[1]])
    }
  }
  
  # Default to first column
  return(col_names[1])
}
