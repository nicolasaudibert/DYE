# =============================================================================
# Module: Live Preview
# =============================================================================
# Provides real-time preview of the listening test layout including:
# - Forced choice buttons arrangement
# - Likert scale(s) display
# - Combined view with proper ordering
# - Element order management with Y position display
# =============================================================================

# -----------------------------------------------------------------------------
# UI Function
# -----------------------------------------------------------------------------
mod_preview_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    br(),
    
    fluidRow(
      column(
        4,
        selectInput(
          ns("preview_page"),
          "Preview page:",
          choices = c("Instructions" = "instructions", "Response" = "response"),
          selected = "response"
        )
      ),
      column(
        4,
        checkboxInput(
          ns("show_guides"),
          "Show alignment grid",
          value = FALSE
        )
      ),
      column(
        4,
        conditionalPanel(
          condition = sprintf("input['%s']", ns("show_guides")),
          colourInput(
            ns("guide_color"),
            "Grid color:",
            value = "#888888",
            showColour = "background"
          )
        )
      )
    ),
    
    # Preview area with external coordinate scales
    div(
      style = "border: 2px solid #ccc; padding: 10px; background: #f9f9f9;",
      
      # Status bar
      uiOutput(ns("status_bar")),
      
      br(),
      
      # Container for preview with coordinate axes
      fluidRow(
        # Y axis labels (left side) - only for response page
        column(
          width = 1,
          style = "padding-right: 0; text-align: right;",
          conditionalPanel(
            condition = sprintf("input['%s'] && input['%s'] == 'response'", 
                               ns("show_guides"), ns("preview_page")),
            uiOutput(ns("y_axis_labels"))
          )
        ),
        # Main preview
        column(
          width = 10,
          style = "padding: 0;",
          # Conditional display: HTML for instructions, plot for response
          conditionalPanel(
            condition = sprintf("input['%s'] == 'instructions'", ns("preview_page")),
            uiOutput(ns("instructions_html_preview"))
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'response'", ns("preview_page")),
            uiOutput(ns("preview_plot_container"))
          ),
          # X axis labels (bottom) - only for response page
          conditionalPanel(
            condition = sprintf("input['%s'] && input['%s'] == 'response'", 
                               ns("show_guides"), ns("preview_page")),
            uiOutput(ns("x_axis_labels"))
          )
        ),
        column(width = 1)
      ),
      
      # Coordinate info
      conditionalPanel(
        condition = sprintf("input['%s']", ns("show_guides")),
        div(
          style = "font-size: 11px; color: #666; margin-top: 5px; text-align: center;",
          "Y: -300 (bottom) to +300 (top) | X: -400 (left) to +400 (right) | Center = (0, 0)"
        )
      )
    ),
    
    br(),
    
    # Element order management panel
    div(
      class = "panel panel-default",
      div(
        class = "panel-heading",
        tags$strong("Element Order & Position")
      ),
      div(
        class = "panel-body",
        uiOutput(ns("element_order_ui"))
      )
    ),
    
    # Modal for offset conflict when reordering elements
    shinyjs::hidden(
      div(
        id = ns("offset_conflict_modal"),
        class = "modal fade in",
        style = "display: block; background: rgba(0,0,0,0.5); position: fixed; top: 0; left: 0; right: 0; bottom: 0; z-index: 1050;",
        div(
          class = "modal-dialog",
          style = "margin-top: 100px;",
          div(
            class = "modal-content",
            div(
              class = "modal-header",
              tags$h4(class = "modal-title", icon("exclamation-triangle"), " Offset Conflict")
            ),
            div(
              class = "modal-body",
              p("The elements being reordered have custom Y offsets. Moving them may cause overlap."),
              p("How would you like to handle the offsets?"),
              tags$hr(),
              radioButtons(
                ns("offset_action"),
                label = NULL,
                choices = c(
                  "Reset offsets to zero for swapped elements" = "reset",
                  "Keep offsets with their elements (most likely to cause overlap)" = "keep_elements",
                  "Keep offsets at their positions (swap offsets between reordered elements)" = "keep_positions"
                ),
                selected = "reset"
              )
            ),
            div(
              class = "modal-footer",
              actionButton(ns("offset_confirm"), "Confirm", class = "btn-primary"),
              actionButton(ns("offset_cancel"), "Cancel", class = "btn-default")
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
mod_preview_server <- function(id, audio_data, forced_choice_data, 
                               likert_data, instructions_data, settings_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # -------------------------------------------------------------------------
    # Internal state for element ordering
    # -------------------------------------------------------------------------
    internal <- reactiveValues(
      element_order = list(),  # List of element IDs in order
      y_offsets = list()       # Manual Y offsets per element
    )
    
    # -------------------------------------------------------------------------
    # Resolution reactive
    # -------------------------------------------------------------------------
    resolution <- reactive({
      res_str <- settings_data$resolution %||% "800x600"
      parts <- strsplit(res_str, "x")[[1]]
      if (length(parts) == 2) {
        list(
          width = as.numeric(parts[1]),
          height = as.numeric(parts[2])
        )
      } else {
        list(width = 800, height = 600)
      }
    })
    
    # -------------------------------------------------------------------------
    # Dynamic plot container with resolution-based dimensions
    # -------------------------------------------------------------------------
    output$preview_plot_container <- renderUI({
      res <- resolution()
      plotOutput(
        ns("preview_plot"),
        width = paste0(res$width, "px"),
        height = paste0(res$height, "px")
      )
    })
    
    # -------------------------------------------------------------------------
    # Dynamic axis labels based on resolution
    # -------------------------------------------------------------------------
    output$y_axis_labels <- renderUI({
      res <- resolution()
      y_limit <- res$height / 2
      y_step <- round(y_limit / 3)
      y_values <- seq(y_limit, -y_limit, by = -y_step)
      
      div(
        style = sprintf("height: %dpx; display: flex; flex-direction: column; justify-content: space-between; font-size: 11px; color: #666; padding: 10px 5px;",
                       res$height),
        lapply(y_values, function(y) {
          if (y == 0) {
            tags$span(as.character(y), style = "font-weight: bold;")
          } else {
            tags$span(as.character(as.integer(y)))
          }
        })
      )
    })
    
    output$x_axis_labels <- renderUI({
      res <- resolution()
      x_limit <- res$width / 2
      x_step <- round(x_limit / 4)
      x_values <- seq(-x_limit, x_limit, by = x_step)
      
      div(
        style = sprintf("display: flex; justify-content: space-between; font-size: 11px; color: #666; padding: 5px 0; width: %dpx;",
                       res$width),
        lapply(x_values, function(x) {
          if (x == 0) {
            tags$span(as.character(x), style = "font-weight: bold;")
          } else {
            tags$span(as.character(as.integer(x)))
          }
        })
      )
    })
    
    # -------------------------------------------------------------------------
    # Status bar showing current configuration
    # -------------------------------------------------------------------------
    output$status_bar <- renderUI({
      fc_status <- if (forced_choice_data$enabled) {
        sprintf("Forced choice: %d options (%d rows)", 
                forced_choice_data$num_choices,
                forced_choice_data$num_rows %||% 1)
      } else {
        "Forced choice: disabled"
      }
      
      likert_status <- if (likert_data$enabled) {
        sprintf("Likert: %d scale(s)", likert_data$num_scales)
      } else {
        "Likert: disabled"
      }
      
      audio_status <- sprintf("Audio: %d stimuli", nrow(audio_data$files))
      
      div(
        class = "well well-sm",
        style = "margin-bottom: 10px; padding: 8px;",
        tags$span(class = "label label-info", fc_status),
        " ",
        tags$span(class = "label label-info", likert_status),
        " ",
        tags$span(class = "label label-default", audio_status)
      )
    })
    
    # -------------------------------------------------------------------------
    # Calculate element positions with proper ordering and adaptive sizing
    # -------------------------------------------------------------------------
    element_positions <- reactive({
      elements <- list()
      
      # Add forced choice if enabled
      if (forced_choice_data$enabled) {
        n_rows <- forced_choice_data$num_rows %||% 1
        box_height <- forced_choice_data$box_height %||% 80
        v_spacing <- forced_choice_data$v_spacing %||% 50
        
        # Calculate total height of FC block
        fc_height <- calculate_fc_total_height(n_rows, box_height, v_spacing)
        
        elements[[length(elements) + 1]] <- list(
          id = "fc",
          type = "fc",
          label = "Forced Choice",
          height = fc_height,
          original_height = fc_height
        )
      }
      
      # Add Likert scales if enabled
      if (likert_data$enabled && likert_data$num_scales > 0) {
        for (i in 1:likert_data$num_scales) {
          elements[[length(elements) + 1]] <- list(
            id = paste0("likert_", i),
            type = "likert",
            index = i,
            label = paste("Likert Scale", i),
            height = 70,
            original_height = 70
          )
        }
      }
      
      if (length(elements) == 0) return(list())
      
      # Apply custom order if set
      if (length(internal$element_order) > 0) {
        ordered_ids <- internal$element_order
        new_order <- list()
        for (oid in ordered_ids) {
          for (elem in elements) {
            if (elem$id == oid) {
              new_order[[length(new_order) + 1]] <- elem
              break
            }
          }
        }
        for (elem in elements) {
          if (!elem$id %in% ordered_ids) {
            new_order[[length(new_order) + 1]] <- elem
          }
        }
        elements <- new_order
      }
      
      # Calculate total height needed
      n_elements <- length(elements)
      base_spacing <- 50
      
      heights <- sapply(elements, function(e) e$height)
      total_height_needed <- sum(heights) + base_spacing * (n_elements - 1)
      
      # Available space (with margins)
      available_height <- 500  # 600 - 50 top margin - 50 bottom margin
      
      # If content doesn't fit, scale down
      scale_factor <- 1.0
      spacing <- base_spacing
      
      if (total_height_needed > available_height) {
        # First try reducing spacing
        min_spacing <- 20
        total_with_min_spacing <- sum(heights) + min_spacing * (n_elements - 1)
        
        if (total_with_min_spacing <= available_height) {
          # Just reduce spacing
          spacing <- (available_height - sum(heights)) / max(n_elements - 1, 1)
        } else {
          # Need to scale heights too
          spacing <- min_spacing
          available_for_elements <- available_height - spacing * (n_elements - 1)
          scale_factor <- available_for_elements / sum(heights)
          
          # Apply scale factor to heights
          for (i in seq_along(elements)) {
            elements[[i]]$height <- elements[[i]]$original_height * scale_factor
          }
          heights <- sapply(elements, function(e) e$height)
        }
      }
      
      # Calculate Y positions - distribute from top to bottom
      total_height <- sum(heights) + spacing * (n_elements - 1)
      y_top <- total_height / 2
      
      current_y <- y_top
      
      for (i in 1:n_elements) {
        # Center Y position for this element
        y_center <- current_y - heights[i] / 2
        
        # Apply manual offset if set - use stored value from internal$y_offsets
        elem_id <- elements[[i]]$id
        offset <- internal$y_offsets[[elem_id]] %||% 0
        y_center <- y_center + offset
        
        elements[[i]]$y_position <- y_center
        elements[[i]]$y_min <- y_center - heights[i] / 2
        elements[[i]]$y_max <- y_center + heights[i] / 2
        elements[[i]]$scale_factor <- scale_factor
        elements[[i]]$current_offset <- offset
        
        current_y <- current_y - heights[i] - spacing
      }
      
      # Update element order
      internal$element_order <- sapply(elements, function(e) e$id)
      
      return(elements)
    })
    
    # -------------------------------------------------------------------------
    # Generate preview plot - REACTIVE to all changes
    # -------------------------------------------------------------------------
    output$preview_plot <- renderPlot({
      
      # Check which page to show
      if (input$preview_page == "instructions") {
        # For instructions, we'll render a simple ggplot placeholder
        # The actual HTML preview will be shown via uiOutput
        return(NULL)
      }
      
      # Response page preview
      guide_color <- input$guide_color %||% "#888888"
      
      # Get resolution dimensions
      res <- resolution()
      x_limit <- res$width / 2
      y_limit <- res$height / 2
      
      # Create base plot with dynamic limits
      p <- ggplot() +
        xlim(-x_limit, x_limit) +
        ylim(-y_limit, y_limit) +
        theme_void() +
        theme(
          plot.background = element_rect(
            fill = settings_data$img_bg_color %||% "#000000",
            color = NA
          )
        )
      
      # Add grid guides if requested
      if (input$show_guides) {
        # Calculate grid spacing based on resolution
        x_step <- round(x_limit / 4)  # ~4 divisions per half
        y_step <- round(y_limit / 3)  # ~3 divisions per half
        
        # Vertical lines
        for (x in seq(-x_limit, x_limit, by = x_step)) {
          lwd <- if (x == 0) 0.8 else 0.3
          p <- p + annotate("segment", x = x, xend = x, y = -y_limit, yend = y_limit,
                           color = guide_color, linewidth = lwd, alpha = 0.6)
        }
        # Horizontal lines
        for (y in seq(-y_limit, y_limit, by = y_step)) {
          lwd <- if (y == 0) 0.8 else 0.3
          p <- p + annotate("segment", x = -x_limit, xend = x_limit, y = y, yend = y,
                           color = guide_color, linewidth = lwd, alpha = 0.6)
        }
      }
      
      # Get calculated element positions
      elements <- element_positions()
      
      if (length(elements) == 0) {
        p <- p + annotate("text", x = 0, y = 0, 
                         label = "Enable Forced Choice or Likert Scale\nto see preview",
                         color = "#888888", size = 6, lineheight = 1.2)
        return(p)
      }
      
      # Draw each element
      for (elem in elements) {
        if (elem$type == "fc") {
          p <- draw_forced_choice(p, forced_choice_data, settings_data, elem$y_position, 
                                  scale_factor = elem$scale_factor %||% 1.0,
                                  img_width = res$width)
        } else if (elem$type == "likert") {
          p <- draw_likert_scale(p, likert_data, settings_data, elem$index, elem$y_position,
                                 img_width = res$width)
        }
      }
      
      return(p)
      
    }, width = function() { resolution()$width }, 
       height = function() { resolution()$height }, 
       res = 72)
    
    # -------------------------------------------------------------------------
    # HTML preview for instructions (shown when preview_page == "instructions")
    # -------------------------------------------------------------------------
    output$instructions_html_preview <- renderUI({
      req(input$preview_page == "instructions")
      
      html_content <- instructions_data$editor_html
      bg_color <- instructions_data$bg_color %||% "#000000"
      text_color <- instructions_data$default_text_color %||% "#FFFFFF"
      
      if (is.null(html_content) || nchar(trimws(html_content)) == 0 || 
          html_content == "<p><br></p>") {
        html_content <- sprintf("<p style='color: #888;'>(Enter instructions in the Instructions tab)</p>")
      }
      
      # Get resolution
      res <- resolution()
      
      # Check if using uploaded image
      if (!(instructions_data$use_editor %||% TRUE) && !is.null(instructions_data$uploaded_image)) {
        return(
          div(
            style = sprintf("width: %dpx; height: %dpx; border: 2px solid #ccc; display: flex; align-items: center; justify-content: center;",
                           res$width, res$height),
            tags$img(
              src = base64enc::dataURI(
                file = instructions_data$uploaded_image$datapath,
                mime = "image/png"
              ),
              style = "max-width: 100%; max-height: 100%;"
            )
          )
        )
      }
      
      div(
        style = sprintf(
          "width: %dpx; height: %dpx; background-color: %s; color: %s; 
           padding: 40px; overflow: auto; border: 2px solid #ccc;
           display: flex; align-items: center; justify-content: center;
           font-family: Arial, sans-serif; font-size: 16px;",
          res$width, res$height, bg_color, text_color
        ),
        div(
          style = "max-width: 100%; width: 100%;",
          # CSS for Quill classes - inline font-size styles will override base
          tags$style(HTML("
            .ql-font-serif { font-family: Georgia, serif !important; }
            .ql-font-monospace { font-family: 'Courier New', monospace !important; }
            .ql-align-center { text-align: center !important; display: block; }
            .ql-align-right { text-align: right !important; display: block; }
            .ql-align-justify { text-align: justify !important; display: block; }
            p { margin: 0.5em 0; }
            p.ql-align-center { text-align: center !important; }
            p.ql-align-right { text-align: right !important; }
            p.ql-align-justify { text-align: justify !important; }
          ")),
          HTML(html_content)
        ),
        div(
          style = "position: absolute; bottom: 5px; right: 10px; font-size: 10px; color: #666;",
          icon("info-circle"),
          " Preview approximation - final image may differ"
        )
      )
    })
    
    # -------------------------------------------------------------------------
    # Element order management UI
    # -------------------------------------------------------------------------
    output$element_order_ui <- renderUI({
      elements <- element_positions()
      
      if (length(elements) == 0) {
        return(tags$p(class = "text-muted", "No elements to display. Enable Forced Choice or Likert Scale."))
      }
      
      # Check if scaling was applied
      scale_factor <- if (length(elements) > 0) elements[[1]]$scale_factor %||% 1.0 else 1.0
      
      tagList(
        # Show warning if elements were scaled down
        if (scale_factor < 1.0) {
          div(
            class = "alert alert-warning",
            style = "padding: 8px; margin-bottom: 10px;",
            icon("exclamation-triangle"),
            sprintf(" Elements scaled to %.0f%% to fit. Consider reducing the number of elements or their sizes.", 
                   scale_factor * 100)
          )
        },
        
        tags$table(
          class = "table table-condensed table-bordered",
          style = "font-size: 12px;",
          tags$thead(
            tags$tr(
              tags$th("Order", style = "width: 80px;"),
              tags$th("Element"),
              tags$th("Y center", style = "width: 70px;"),
              tags$th("Y range", style = "width: 100px;"),
              tags$th("Offset", style = "width: 100px;")
            )
          ),
          tags$tbody(
            lapply(seq_along(elements), function(i) {
              elem <- elements[[i]]
              tags$tr(
                tags$td(
                  # Move up/down buttons
                  if (i > 1) {
                    actionButton(
                      ns(paste0("move_up_", elem$id)),
                      icon("arrow-up"),
                      class = "btn-xs",
                      onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})",
                                       ns("move_up"), elem$id)
                    )
                  },
                  if (i < length(elements)) {
                    actionButton(
                      ns(paste0("move_down_", elem$id)),
                      icon("arrow-down"),
                      class = "btn-xs",
                      onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})",
                                       ns("move_down"), elem$id)
                    )
                  }
                ),
                tags$td(
                  tags$span(
                    class = if (elem$type == "fc") "label label-primary" else "label label-success",
                    elem$label
                  )
                ),
                tags$td(round(elem$y_position)),
                tags$td(sprintf("[%d, %d]", round(elem$y_min), round(elem$y_max))),
                tags$td(
                  numericInput(
                    ns(paste0("offset_", elem$id)),
                    label = NULL,
                    value = elem$current_offset %||% 0,
                    min = -200,
                    max = 200,
                    step = 10,
                    width = "80px"
                  )
                )
              )
            })
          )
        )
      )
    })
    
    # -------------------------------------------------------------------------
    # Handle move up/down events with offset conflict detection
    # -------------------------------------------------------------------------
    
    # Store pending move action
    pending_move <- reactiveValues(
      direction = NULL,  # "up" or "down"
      elem_id = NULL,
      other_id = NULL,
      idx = NULL
    )
    
    # Check if elements have non-zero offsets
    has_offset <- function(elem_id) {
      offset <- internal$y_offsets[[elem_id]]
      !is.null(offset) && offset != 0
    }
    
    observeEvent(input$move_up, {
      elem_id <- input$move_up
      current_order <- internal$element_order
      idx <- which(current_order == elem_id)
      
      if (length(idx) > 0 && idx > 1) {
        other_id <- current_order[idx - 1]
        
        # Check if either element has an offset
        if (has_offset(elem_id) || has_offset(other_id)) {
          # Store pending action and show modal
          pending_move$direction <- "up"
          pending_move$elem_id <- elem_id
          pending_move$other_id <- other_id
          pending_move$idx <- idx
          shinyjs::show("offset_conflict_modal")
        } else {
          # No conflict, proceed directly
          current_order[c(idx - 1, idx)] <- current_order[c(idx, idx - 1)]
          internal$element_order <- current_order
        }
      }
    })
    
    observeEvent(input$move_down, {
      elem_id <- input$move_down
      current_order <- internal$element_order
      idx <- which(current_order == elem_id)
      
      if (length(idx) > 0 && idx < length(current_order)) {
        other_id <- current_order[idx + 1]
        
        # Check if either element has an offset
        if (has_offset(elem_id) || has_offset(other_id)) {
          # Store pending action and show modal
          pending_move$direction <- "down"
          pending_move$elem_id <- elem_id
          pending_move$other_id <- other_id
          pending_move$idx <- idx
          shinyjs::show("offset_conflict_modal")
        } else {
          # No conflict, proceed directly
          current_order[c(idx, idx + 1)] <- current_order[c(idx + 1, idx)]
          internal$element_order <- current_order
        }
      }
    })
    
    # Cancel button
    observeEvent(input$offset_cancel, {
      shinyjs::hide("offset_conflict_modal")
      pending_move$direction <- NULL
      pending_move$elem_id <- NULL
      pending_move$other_id <- NULL
      pending_move$idx <- NULL
    })
    
    # Confirm button
    observeEvent(input$offset_confirm, {
      shinyjs::hide("offset_conflict_modal")
      
      req(pending_move$direction)
      
      action <- input$offset_action
      elem_id <- pending_move$elem_id
      other_id <- pending_move$other_id
      idx <- pending_move$idx
      current_order <- internal$element_order
      
      # Get current offsets
      elem_offset <- internal$y_offsets[[elem_id]] %||% 0
      other_offset <- internal$y_offsets[[other_id]] %||% 0
      
      if (action == "reset") {
        # Reset offsets for both elements
        internal$y_offsets[[elem_id]] <- 0
        internal$y_offsets[[other_id]] <- 0
      } else if (action == "keep_positions") {
        # Swap offsets (keep at positions)
        internal$y_offsets[[elem_id]] <- other_offset
        internal$y_offsets[[other_id]] <- elem_offset
      }
      # For "keep_elements", offsets stay with their elements (no change needed)
      
      # Perform the swap
      if (pending_move$direction == "up") {
        current_order[c(idx - 1, idx)] <- current_order[c(idx, idx - 1)]
      } else {
        current_order[c(idx, idx + 1)] <- current_order[c(idx + 1, idx)]
      }
      internal$element_order <- current_order
      
      # Clear pending action
      pending_move$direction <- NULL
      pending_move$elem_id <- NULL
      pending_move$other_id <- NULL
      pending_move$idx <- NULL
    })
    
    # -------------------------------------------------------------------------
    # Debounced offset handling to prevent infinite loops
    # -------------------------------------------------------------------------
    
    # Create debounced versions of offset inputs
    offset_fc_debounced <- debounce(reactive({ input$offset_fc }), 500)
    offset_likert_1_debounced <- debounce(reactive({ input$offset_likert_1 }), 500)
    offset_likert_2_debounced <- debounce(reactive({ input$offset_likert_2 }), 500)
    offset_likert_3_debounced <- debounce(reactive({ input$offset_likert_3 }), 500)
    offset_likert_4_debounced <- debounce(reactive({ input$offset_likert_4 }), 500)
    offset_likert_5_debounced <- debounce(reactive({ input$offset_likert_5 }), 500)
    
    # Observer for FC offset
    observeEvent(offset_fc_debounced(), {
      val <- offset_fc_debounced()
      if (!is.null(val) && (is.null(internal$y_offsets[["fc"]]) || internal$y_offsets[["fc"]] != val)) {
        internal$y_offsets[["fc"]] <- val
      }
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    
    # Observers for Likert offsets
    observeEvent(offset_likert_1_debounced(), {
      val <- offset_likert_1_debounced()
      if (!is.null(val) && (is.null(internal$y_offsets[["likert_1"]]) || internal$y_offsets[["likert_1"]] != val)) {
        internal$y_offsets[["likert_1"]] <- val
      }
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    
    observeEvent(offset_likert_2_debounced(), {
      val <- offset_likert_2_debounced()
      if (!is.null(val) && (is.null(internal$y_offsets[["likert_2"]]) || internal$y_offsets[["likert_2"]] != val)) {
        internal$y_offsets[["likert_2"]] <- val
      }
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    
    observeEvent(offset_likert_3_debounced(), {
      val <- offset_likert_3_debounced()
      if (!is.null(val) && (is.null(internal$y_offsets[["likert_3"]]) || internal$y_offsets[["likert_3"]] != val)) {
        internal$y_offsets[["likert_3"]] <- val
      }
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    
    observeEvent(offset_likert_4_debounced(), {
      val <- offset_likert_4_debounced()
      if (!is.null(val) && (is.null(internal$y_offsets[["likert_4"]]) || internal$y_offsets[["likert_4"]] != val)) {
        internal$y_offsets[["likert_4"]] <- val
      }
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    
    observeEvent(offset_likert_5_debounced(), {
      val <- offset_likert_5_debounced()
      if (!is.null(val) && (is.null(internal$y_offsets[["likert_5"]]) || internal$y_offsets[["likert_5"]] != val)) {
        internal$y_offsets[["likert_5"]] <- val
      }
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    
  })
}

# -----------------------------------------------------------------------------
# Helper Functions
# -----------------------------------------------------------------------------

#' Render instructions preview
render_instructions_preview <- function(instructions_data, settings_data) {
  if (instructions_data$use_editor %||% TRUE) {
    # Get content - prefer plain text for preview, HTML for export
    text <- instructions_data$editor_content
    if (is.null(text) || nchar(trimws(text)) == 0) {
      text <- "(Enter instructions text in the Instructions tab)"
    }
    
    # Use the colors from instructions module
    font_color <- instructions_data$default_text_color %||% "#FFFFFF"
    bg_color <- instructions_data$bg_color %||% "#000000"
    
    p <- ggplot() +
      annotate("text", x = 0, y = 0, label = text,
               size = 8, color = font_color,
               lineheight = 1.2) +
      xlim(-400, 400) +
      ylim(-300, 300) +
      theme_void() +
      theme(plot.background = element_rect(fill = bg_color, color = NA))
    
    return(p)
    
  } else if (!is.null(instructions_data$uploaded_image)) {
    tryCatch({
      img <- png::readPNG(instructions_data$uploaded_image$datapath)
      grid::grid.newpage()
      grid::grid.raster(img)
      return(ggplot() + theme_void())
    }, error = function(e) {
      return(ggplot() +
               annotate("text", x = 0, y = 0, label = "Error loading image",
                       color = "red", size = 5) +
               theme_void())
    })
  } else {
    return(ggplot() +
             annotate("text", x = 0, y = 0, 
                     label = "(No instructions defined yet)",
                     color = "#888888", size = 5) +
             xlim(-400, 400) + ylim(-300, 300) +
             theme_void() +
             theme(plot.background = element_rect(
               fill = instructions_data$bg_color %||% "#000000", color = NA)))
  }
}

#' Draw forced choice buttons on plot
draw_forced_choice <- function(p, forced_choice_data, settings_data, y_center, 
                               scale_factor = 1.0, img_width = 800) {
  
  # Apply scale factor to dimensions
  box_height <- (forced_choice_data$box_height %||% 80) * scale_factor
  v_spacing <- (forced_choice_data$v_spacing %||% 50) * scale_factor
  
  fc_layout <- calculate_forced_choice_layout(
    n_choices = forced_choice_data$num_choices,
    n_rows = forced_choice_data$num_rows %||% 1,
    img_width = img_width,
    h_spacing = forced_choice_data$h_spacing %||% 50,
    v_spacing = v_spacing,
    box_height = box_height,
    y_center = y_center
  )
  
  labels <- forced_choice_data$labels
  text_colors <- forced_choice_data$text_colors
  bg_colors <- forced_choice_data$bg_colors
  use_images <- forced_choice_data$use_images
  font_sizes <- forced_choice_data$font_sizes
  
  for (i in 1:nrow(fc_layout)) {
    row_data <- fc_layout[i, ]
    
    label <- if (length(labels) >= i && !is.null(labels[[i]])) labels[[i]] else paste("Choice", i)
    text_col <- if (length(text_colors) >= i && !is.null(text_colors[[i]])) {
      text_colors[[i]]
    } else {
      settings_data$text_color %||% "#000000"
    }
    bg_col <- if (length(bg_colors) >= i && !is.null(bg_colors[[i]])) {
      bg_colors[[i]]
    } else {
      settings_data$bg_color %||% "#FFFFE0"
    }
    use_img <- if (length(use_images) >= i) use_images[[i]] %||% FALSE else FALSE
    
    # Get font size (0 = auto)
    manual_fs <- if (length(font_sizes) >= i) font_sizes[[i]] %||% 0 else 0
    
    # Draw rectangle
    p <- p +
      annotate("rect",
               xmin = row_data$xmin, xmax = row_data$xmax,
               ymin = row_data$ymin, ymax = row_data$ymax,
               fill = bg_col, color = NA)
    
    # Draw text or placeholder
    if (!use_img) {
      label_lines <- str_split(label, "\n")[[1]]
      max_chars <- max(nchar(label_lines), 1)
      
      # Calculate auto font size
      auto_fs <- min(
        row_data$width / (max_chars + 2) * 0.4,
        row_data$height / (length(label_lines) + 1) * 0.3
      )
      
      # Use manual if > 0, otherwise auto
      font_size <- if (manual_fs > 0) manual_fs else auto_fs
      
      p <- p +
        annotate("text", x = row_data$x, y = row_data$y, label = label,
                 color = text_col, size = max(font_size, 2), lineheight = 0.8)
    } else {
      p <- p +
        annotate("text", x = row_data$x, y = row_data$y, label = "[IMG]",
                 color = text_col, size = 3, fontface = "italic")
    }
  }
  
  return(p)
}

#' Draw Likert scale on plot
draw_likert_scale <- function(p, likert_data, settings_data, scale_index, y_pos, 
                              img_width = 800) {
  
  scales <- likert_data$scales
  if (length(scales) < scale_index) return(p)
  
  scale <- scales[[scale_index]]
  n_points <- scale$num_points %||% 7
  
  # Get item style
  if (likert_data$shared_item_style %||% TRUE) {
    item_shape <- likert_data$global_item_shape %||% "disc"
  } else {
    item_shape <- scale$item_shape %||% "disc"
  }
  
  # Get item color - use scale-specific if set, otherwise Settings default
  item_color <- scale$item_color
  if (is.null(item_color) || item_color == "") {
    item_color <- settings_data$likert_item_color %||% "#666666"
  }
  
  shape_code <- get_shape_geom(item_shape)
  
  # Scale factor based on resolution
  scale_factor <- img_width / 800
  
  item_size <- (likert_data$item_size %||% 30) * scale_factor
  item_spacing <- (likert_data$item_spacing %||% 20) * scale_factor
  
  # Calculate item positions
  total_width <- (n_points - 1) * (item_size + item_spacing)
  x_positions <- seq(-total_width / 2, total_width / 2, length.out = n_points)
  
  # Draw items
  for (x in x_positions) {
    p <- p +
      annotate("point", x = x, y = y_pos, shape = shape_code,
               size = item_size / 5, color = item_color, fill = item_color)
  }
  
  # Draw labels
  label_color <- scale$label_color %||% settings_data$text_color %||% "#000000"
  label_bg <- scale$label_bg %||% settings_data$bg_color %||% "#FFFFE0"
  
  # Get default font size from likert_data, then check for individual overrides
  default_font_size <- (likert_data$label_font_size %||% 4) * scale_factor
  
  left_font_size <- scale$left_font_size
  if (is.null(left_font_size) || left_font_size == 0) {
    left_font_size <- default_font_size
  } else {
    left_font_size <- left_font_size * scale_factor
  }
  
  right_font_size <- scale$right_font_size
  if (is.null(right_font_size) || right_font_size == 0) {
    right_font_size <- default_font_size
  } else {
    right_font_size <- right_font_size * scale_factor
  }
  
  label_width <- 100 * scale_factor
  label_height <- 50 * scale_factor
  
  # Left label
  left_label <- scale$left_label %||% "Left"
  left_use_img <- scale$left_use_image %||% FALSE
  left_x <- -total_width / 2 - item_size - label_width / 2 - 15
  
  p <- p +
    annotate("rect",
             xmin = left_x - label_width / 2, xmax = left_x + label_width / 2,
             ymin = y_pos - label_height / 2, ymax = y_pos + label_height / 2,
             fill = label_bg, color = NA)
  
  if (!left_use_img) {
    p <- p +
      annotate("text", x = left_x, y = y_pos, label = left_label,
               color = label_color, size = left_font_size)
  } else {
    p <- p +
      annotate("text", x = left_x, y = y_pos, label = "[IMG]",
               color = label_color, size = 2.5, fontface = "italic")
  }
  
  # Right label
  right_label <- scale$right_label %||% "Right"
  right_use_img <- scale$right_use_image %||% FALSE
  right_x <- total_width / 2 + item_size + label_width / 2 + 15
  
  p <- p +
    annotate("rect",
             xmin = right_x - label_width / 2, xmax = right_x + label_width / 2,
             ymin = y_pos - label_height / 2, ymax = y_pos + label_height / 2,
             fill = label_bg, color = NA)
  
  if (!right_use_img) {
    p <- p +
      annotate("text", x = right_x, y = y_pos, label = right_label,
               color = label_color, size = right_font_size)
  } else {
    p <- p +
      annotate("text", x = right_x, y = y_pos, label = "[IMG]",
               color = label_color, size = 2.5, fontface = "italic")
  }
  
  return(p)
}

#' Calculate total height of forced choice block
calculate_fc_total_height <- function(n_rows, box_height = 80, v_spacing = 50) {
  n_rows * box_height + max(0, (n_rows - 1)) * v_spacing
}
