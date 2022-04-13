# Set the max size for file imports to 50MB
options(shiny.maxRequestSize = 50 * 1024 ^ 2)

# Server-side
server <- function(input, output, session) {
  # Create an object for storing reactive values
  # scenario_names required so that scenario menu updates automatically when a scenario changes
  # scenario_exists required so when renaming a scenario (throws error is name already exists)
  cached <- reactiveValues(
    r_code = c(
      get_code_comment("For this code to work out of the box, the working directory has to be the Shiny app's root directory"),
      get_code_comment("Packages/dependencies to import"),
      "packages = c(",
      paste0(get_nbsp(2), get_quot("dplyr", ",")), 
      paste0(get_nbsp(2), get_quot("DT", ",")), 
      paste0(get_nbsp(2), get_quot("ggplot2", ",")), 
      paste0(get_nbsp(2), get_quot("plotly", ",")), 
      paste0(get_nbsp(2), get_quot("readr", ",")), 
      paste0(get_nbsp(2), get_quot("readxl", ",")), 
      paste0(get_nbsp(2), get_quot("scales", ",")), 
      paste0(get_nbsp(2), get_quot("stringr", ",")), 
      paste0(get_nbsp(2), get_quot("tidyr")), 
      ")",
      "",
      get_code_comment("If any packages are not installed, install"),
      "uninstalled_packages = packages[! packages %in% installed.packages()[,&quot;Package&quot;]]",
      "if(length(uninstalled_packages) > 0) install.packages(uninstalled_packages)",
      "",
      get_code_comment("Load packages"),
      "import_packages = lapply(packages, library, character.only = TRUE)",
      "",
      get_code_comment("Import user-defined functions"),
      paste0(
        "source_functions = lapply(list.files(",
        get_quot("functions/", ", "),
        "pattern = ",
        get_quot(".r", "), "),
        "function(x) source(paste0(",
        get_quot("functions/", ", x)))")
      ),
      ""
    ),
    d = NULL,
    scenario_names = get_scenario_names(),
    common_data_tables = NULL,
    scenario_exists = NULL,
    scenario_being_cloned = NULL,
    import_scenario_button_clicks = 0,
    round_decimals_slider = 10,
    visualization_menu_selected = "Table",
    plot_menu_selected = "Bar",
    plot_title = NULL,
    x_variable_menu_selected = NULL,
    x_variable_label = NULL,
    y_variable_menu_selected = NULL,
    y_variable_label = NULL,
    group_by_menu_selected = NULL,
    panel_column_menu_selected = NULL,
    panel_row_menu_selected = NULL,
    hide_visualization_menu = FALSE
  )
  
  output$r_code <- renderText({
    return(
      paste0(
        "<pre class='r'>",
        paste0(cached$r_code, collapse = "<br>"),
        "</pre>"
      )
    )
  })
  
  # Unconditional horizontal rule
  output$unconditional_hr <- renderUI({
    # Render horizontal rule
    tags$hr(tags$span(style = "display: block; height: 1rem;"))
  })
  
  # Conditional horizontal rule
  output$conditional_hr <- renderUI({
    # Dependencies
    req(
      input$scenario_menu, 
      input$scenario_data_table_menu,
      cached$scenario_names
    )
    
    # Render horizontal rule
    tags$hr(tags$span(style = "display: block; height: 1rem;"))
  })
  
  # Scenario menu
  output$scenario_menu <- renderUI({
    # Render scenario menu
    return(
      get_drop_down(
        input_id = "scenario_menu",
        label = "Scenarios",
        choices = cached$scenario_names
      )
    )
  })
  
  # Monitor scenario menu for selections
  observeEvent(input$scenario_menu, {
    # Dependencies
    req(input$scenario_data_table_menu)
    
    # Import and cache data table(s) 
    cached$d <- get_data_table(
      data_path = paste0("scenarios/", match_scenario_name_to_file_name(input$scenario_menu)),
      sheet_name = input$scenario_data_table_menu,
      rounding_digits = cached$round_decimals_slider
    )
    
    # Cache R code
    cached$r_code <- c(
      cached$r_code,
      get_code_comment(
        paste0(
          "Import data table (",
          input$scenario_data_table_menu,
          ")"
        )
      ),
      "d = get_data_table(",
      paste0(get_nbsp(2), "data_path = c("),
      paste0(
        get_nbsp(4),
        get_quot(paste0("scenarios/", match_scenario_name_to_file_name(input$scenario_menu))),
        collapse = ",<br>"
      ),
      get_nbsp(2, "),"),
      paste0(
        get_nbsp(2, "sheet_name = "),
        get_quot(input$scenario_data_table_menu, ",")
      ),
      paste0(
        get_nbsp(2, "rounding_digits = "),
        cached$round_decimals_slider
      ),
      ")",
      ""
    )
  })
  
  # Monitor import scenario button for clicks
  observeEvent(input$import_scenario_button, {
    # Increment clicks counter
    cached$import_scenario_button_clicks <- cached$import_scenario_button_clicks + 1
    
    # Render file input
    output$import_scenario <- renderUI({
      # Render if the click counter is an odd number, else make it disappear
      if(cached$import_scenario_button_clicks %% 2 > 0) {
        return(get_file_input())
      } else {
        # Remove the error message if it exists
        output$import_scenario_error_message <- renderUI(NULL)
        
        # Return a NULL value
        return(NULL)
      }
    })
  })
  
  # Monitor whether a scenario file has been uploaded
  observeEvent(input$import_scenario$datapath, {
    # If new scenario uploaded, import
    # Get file extension of uploaded file
    file_extension <- get_file_extension(input$import_scenario$datapath)
    
    # If invalid file was uploaded, render error message, else upload file
    if(! file_extension %in% gsub("[.]", "", allowed_file_extensions)) {
      # Render error message
      output$import_scenario_error_message <- renderUI({
        # Return error message
        return(
          div(
            "Please upload a file with an .xls(x) extension.",
            class = "pohem-viewer-error-message",
            style = "margin-top: -1rem; margin-bottom: 2rem;"
          )
        )
      })
    } else {
      # Remove the error message if it exists
      output$import_scenario_error_message <- renderUI(NULL)
      
      # Create unique file name
      file_name <- paste0(
        remove_file_extension(input$import_scenario$name),
        " - ",
        get_current_timestamp()
      )
      
      # Store uploaded file
      copy_file <- file.copy(
        from = input$import_scenario$datapath,
        to = paste0(
          "scenarios/",
          file_name,
          ".",
          get_file_extension(input$import_scenario$name)
        )
      )
      
      # Cached R code
      cached$r_code <- c(
        cached$r_code,
        get_code_comment("Store uploaded file"),
        "copy_file = file.copy(",
        get_nbsp(
          2, 
          paste0(
            "from = ",
            get_quot(input$import_scenario$datapath, ",")
          )
        ),
        get_nbsp(
          2, 
          paste0(
            "to = ",
            get_quot(
              paste0(
                "scenarios/",
                file_name,
                ".",
                get_file_extension(input$import_scenario$name)
              )
            )
          )
        ),
        ")",
        ""
      )
      
      # Update scenario menu
      cached$scenario_names <- get_scenario_names()
      
      # Pause for 2 seconds
      Sys.sleep(2)
      
      # Make the upload element disappear
      cached$import_scenario_button_clicks <- cached$import_scenario_button_clicks + 1
    }
  })
  
  # Rename scenario modal button
  output$rename_scenario_modal_button <- renderUI({
    # Dependencies
    req(cached$scenario_names)
    
    # Render button
    return(
      get_button(
        input_id = "rename_scenario_modal_button",
        label = "Rename",
        class = "pohem-viewer-settings-button"
      )
    )
  })
  
  # Monitor rename scenario modal button for clicks
  observeEvent(input$rename_scenario_modal_button, {
    # Render modal
    showModal(
      modalDialog(
        title = "Rename scenario",
        # Render scenario menu
        output$scenario_menu2 <- renderUI({
          return(
            get_drop_down(
              input_id = "scenario_menu2",
              label = "Scenario to rename",
              choices = cached$scenario_names,
              selected = input$scenario_menu[1],
              actions_box = FALSE,
              max_options = 1
            )
          )
        }),
        # Render new scenario name input
        output$new_scenario_name_text_input <- renderUI({
          return(
            get_text_input(
              input_id = "new_scenario_name_text_input",
              label = "New scenario name"
            )
          )
        }),
        # Render error message
        output$rename_scenario_error_message <- renderUI({
          # If scenario can't be renamed (because it already exists)
          if(isTRUE(cached$scenario_exists)) {
            # Return error message
            return(
              div(
                "The scenario name already exists. Please try again.",
                class = "pohem-viewer-error-message"
              )
            )
          }
        }),
        size = "s",
        easyClose = FALSE,
        footer = tagList(
          actionButton(
            inputId = "rename_scenario_button", 
            label = "Rename", 
            class = "pohem-viewer-modal-footer-button"
          ),
          modalButton(label = "Close")
        )
      )
    )
  })
  
  # Monitor rename scenario button for clicks
  observeEvent(input$rename_scenario_button, {
    # Dependencies
    req(input$scenario_menu2, input$new_scenario_name_text_input)
    
    # Get file extension of old scenario file and add to new scenario name
    new_file_name <- paste0(
      input$new_scenario_name_text_input, 
      ".", 
      get_file_extension(match_scenario_name_to_file_name(input$scenario_menu2))
    )
    
    # If the new file name matches a current file name, throw an error
    if(new_file_name %in% list.files("scenarios", pattern = allowed_file_extensions)) {
      cached$scenario_exists <- TRUE
    } else {
      # Cache R code
      cached$r_code <- c(
        cached$r_code,
        get_code_comment("Rename scenario file"),
        "rename_file = file.rename(",
        get_nbsp(
          2, 
          paste0(
            "from = ",
            get_quot(paste0("scenarios/", match_scenario_name_to_file_name(input$scenario_menu2)), ",")
          )
        ),
        get_nbsp(
          2,
          paste0(
            "to = ",
            get_quot(paste0("scenarios/", new_file_name))
          )
        ),
        ")",
        ""
      )
      
      # Rename scenario file
      rename_file <- file.rename(
        from = paste0("scenarios/", match_scenario_name_to_file_name(input$scenario_menu2)),
        to = paste0("scenarios/", new_file_name)
      )
      
      # Reset cached scenario_exists object
      cached$scenario_exists <- FALSE
      
      # Update scenario menu
      cached$scenario_names <- get_scenario_names()
      
      # Remove modal
      removeModal()
    }
  })
  
  # Clone scenario modal button
  output$clone_scenario_modal_button <- renderUI({
    # Dependencies
    req(cached$scenario_names)
    
    # Render button
    return(
      get_button(
        input_id = "clone_scenario_modal_button",
        label = "Clone",
        class = "pohem-viewer-settings-button"
      )
    )
  })
  
  # Monitor clone scenario modal button for clicks
  observeEvent(input$clone_scenario_modal_button, {
    # Render modal
    showModal(
      modalDialog(
        title = "Clone scenario",
        # Render scenario menu
        output$scenario_menu3 <- renderUI({
          return(
            get_drop_down(
              input_id = "scenario_menu3",
              label = "Scenario to clone",
              choices = cached$scenario_names,
              selected = input$scenario_menu[1],
              actions_box = FALSE,
              max_options = 1
            )
          )
        }),
        size = "s",
        easyClose = FALSE,
        footer = tagList(
          actionButton(
            inputId = "clone_scenario_button", 
            label = "Clone", 
            class = "pohem-viewer-modal-footer-button"
          ),
          modalButton(label = "Close")
        )
      )
    )
  })
  
  # Monitor clone scenario button for clicks
  observeEvent(input$clone_scenario_button, {
    # Dependencies
    req(input$scenario_menu3)
    
    # Create file name for cloned scenario
    file_name <- paste0(
      input$scenario_menu3,
      " - ",
      get_current_timestamp()
    )
    
    # Clone scenario
    clone_file <- file.copy(
      from = paste0("scenarios/", match_scenario_name_to_file_name(input$scenario_menu3)),
      to = paste0(
        "scenarios/",
        file_name,
        ".",
        get_file_extension(match_scenario_name_to_file_name(input$scenario_menu3))
      )
    )
    
    # Cache R code
    cached$r_code <- c(
      cached$r_code,
      get_code_comment("Clone scenario"),
      "clone_file = file.copy(",
      paste0(
        get_nbsp(2, "from = "),
        get_quot(paste0("scenarios/", match_scenario_name_to_file_name(input$scenario_menu3)), ",<br>"),
        get_nbsp(2, "to = "),
        get_quot(
          paste0(
            "scenarios/",
            file_name,
            ".",
            get_file_extension(match_scenario_name_to_file_name(input$scenario_menu3))
          )
        )
      ),
      ")",
      ""
    )
    
    # Update scenario menu
    cached$scenario_names <- get_scenario_names()
    
    # Remove modal
    removeModal()
  })
  
  # Delete scenario modal button
  output$delete_scenario_modal_button <- renderUI({
    # Dependencies
    req(cached$scenario_names)
    
    # Render button
    return(
      get_button(
        input_id = "delete_scenario_modal_button",
        label = "Delete",
        class = "pohem-viewer-settings-button"
      )
    )
  })
  
  # Monitor delete scenario modal button for clicks
  observeEvent(input$delete_scenario_modal_button, {
    # Render modal
    showModal(
      modalDialog(
        title = "Delete scenarios",
        # Render scenario menu
        output$scenario_menu4 <- renderUI({
          return(
            get_drop_down(
              input_id = "scenario_menu4",
              label = "Scenarios to delete",
              choices = cached$scenario_names,
              selected = input$scenario_menu,
              actions_box = FALSE
            )
          )
        }),
        size = "s",
        easyClose = FALSE,
        footer = tagList(
          actionButton(
            inputId = "delete_scenario_button", 
            label = "Delete", 
            class = "pohem-viewer-modal-footer-button"
          ),
          modalButton(label = "Close")
        )
      )
    )
  })
  
  # Monitor delete scenario button for clicks
  observeEvent(input$delete_scenario_button, {
    # Require scenario menu
    req(input$scenario_menu4)
    
    # Cache R code
    cached$r_code <- c(
      cached$r_code,
      get_code_comment("Delete scenario(s)"),
      "delete_scenario = sapply(", 
      get_nbsp(2, "c("),
      paste0(get_nbsp(4, get_quot(input$scenario_menu4)), collapse = ",<br>"),
      get_nbsp(2, "),"),
      get_nbsp(2, "function(x) {"),
      get_nbsp(4, "output = file.remove("),
      get_nbsp(
        6, 
        paste0(
          "paste0(",
          get_quot("scenarios/", ", match_scenario_name_to_file_name(x))")
        )
      ),
      get_nbsp(4, ")"),
      get_nbsp(2, "}"),
      ")",
      ""
    )
    
    # Delete scenario(s)
    delete_scenario <- sapply(input$scenario_menu4, function(x) {
      output <- file.remove(
        paste0("scenarios/", match_scenario_name_to_file_name(x))
      )
    })
    
    # Update scenario menu
    cached$scenario_names <- get_scenario_names()
    
    # Remove modal
    removeModal()
  })
  
  # Scenario data table menu
  output$scenario_data_table_menu <- renderUI({
    # Dependencies
    req(input$scenario_menu, cached$scenario_names)
    
    # Get file names that correspond to scenario names
    scenario_files <- match_scenario_name_to_file_name(input$scenario_menu)
    
    # Get data tables
    cached$common_data_tables <- get_scenario_data_table_names(
      scenario_files = paste0("scenarios/", scenario_files)
    )
    
    # Render menu only if all scenario names have a corresponding file name
    # Mismatch occurs after a scenario has been renamed
    if(length(scenario_files) != length(input$scenario_menu)) {
      return(NULL)
      # No common data tables
    } else if(length(cached$common_data_tables) == 0) {
      return(NULL)
    } else {
      return(
        get_drop_down(
          input_id = "scenario_data_table_menu",
          label = "Data tables",
          choices = unique(cached$common_data_tables),
          selected = unique(cached$common_data_tables)[1],
          actions_box = FALSE,
          max_options = 1
        )
      )
    }
  })
  
  # Render error message
  output$scenario_data_table_error_message <- renderUI({
    # If no common data tables
    if(! is.null(cached$common_data_tables) & length(cached$common_data_tables) == 0) {
      # Return error message
      return(
        div(
          "The selected scenarios have no common data tables. Please try again.",
          class = "pohem-viewer-error-message",
          style = "margin-top: 0.5rem; margin-bottom: 1.5rem;"
        )
      )
    } else {
      return(NULL)
    }
  })
  
  # Monitor scenario data table menu for selections
  observeEvent(input$scenario_data_table_menu, {
    # Import and cache the data table 
    cached$d <- get_data_table(
      data_path = paste0("scenarios/", match_scenario_name_to_file_name(input$scenario_menu)),
      sheet_name = input$scenario_data_table_menu,
      rounding_digits = cached$round_decimals_slider
    )
    
    # Cache R code
    cached$r_code <- c(
      cached$r_code,
      get_code_comment(
        paste0(
          "Import data table (",
          input$scenario_data_table_menu,
          ")"
        )
      ),
      "d = get_data_table(",
      paste0(get_nbsp(2), "data_path = c("),
      paste0(
        get_nbsp(4),
        get_quot(paste0("scenarios/", match_scenario_name_to_file_name(input$scenario_menu))),
        collapse = ",<br>"
      ),
      get_nbsp(2, "),"),
      paste0(
        get_nbsp(2, "sheet_name = "),
        get_quot(input$scenario_data_table_menu, ",")
      ),
      paste0(
        get_nbsp(2, "rounding_digits = "),
        cached$round_decimals_slider
      ),
      ")",
      ""
    )
    
    # Reset plot objects that have been cached
    cached$plot_title <- cached$x_variable_label <- cached$y_variable_label <- NULL
  })
  
  # Rename columns modal button
  output$rename_columns_modal_button <- renderUI({
    # Dependencies
    req(
      input$scenario_menu, 
      input$scenario_data_table_menu,
      cached$scenario_names,
      cached$common_data_tables
    )
    
    # Render button
    return(
      get_button(
        input_id = "rename_columns_modal_button",
        label = "Rename columns",
        class = "pohem-viewer-settings-button"
      )
    )
  })
  
  # Column names menu
  column_name_menu <- reactive({
    # Dependencies
    req(cached$d)
    
    # Iterate through each data table column and render filter menu
    lapply(
      names(cached$d),
      function(x) {
        # Render text input
        return(
          get_text_input(
            input_id = paste0("rename_", x),
            label = x,
            value = x
          )
        )
      }
    )
  })
  
  # Monitor rename columns modal button for clicks
  observeEvent(input$rename_columns_modal_button, {
    # Require data table 
    req(cached$d)
    
    # Render modal
    showModal(
      modalDialog(
        title = "Rename columns",
        # Render column name menu
        output$text_ui <- renderUI(column_name_menu()),
        size = "l",
        easyClose = FALSE,
        footer = tagList(
          actionButton(
            inputId = "rename_columns_button", 
            label = "Rename columns", 
            class = "pohem-viewer-modal-footer-button"
          ),
          modalButton(label = "Close")
        )
      )
    )
  })
  
  # Monitor rename columns button for clicks
  observeEvent(input$rename_columns_button, {
    # Dependencies
    req(cached$d)
    
    # If data table contains columns, rename columns
    if(length(names(cached$d)) > 0) {
      # Iterate through column names
      rename_columns <- sapply(names(cached$d), function(x) {
        # Build code
        code <- paste0(
          "names(cached$d)[which(names(cached$d) == '",
          x,
          "')] <- ",
          "input$rename_",
          x
        )
        
        # Cache R code
        cached$r_code <- c(
          cached$r_code,
          get_code_comment("Rename column"),
          paste0(
            sub(
              " <-.*",
              "",
              gsub(
                "cached[$]d", 
                "d", 
                gsub(
                  "[']",
                  "&quot;",
                  code
                )
              )
            ),
            " = ",
            get_quot(eval(parse(text = paste0("input$rename_", x))))
          ),
          ""
        )
        
        # Execute code
        eval(parse(text = code))
      })
    }
    
    # Remove modal
    removeModal()
  })
  
  # Filter columns modal button
  output$filter_columns_modal_button <- renderUI({
    # Dependencies
    req(
      input$scenario_menu, 
      input$scenario_data_table_menu,
      cached$scenario_names,
      cached$common_data_tables
    )
    
    # Render button
    return(
      get_button(
        input_id = "filter_columns_modal_button",
        label = "Filter columns",
        class = "pohem-viewer-settings-button"
      )
    )
  })
  
  # Monitor filter columns modal button for clicks
  observeEvent(input$filter_columns_modal_button, {
    # Dependencies
    req(cached$d)
    
    # Render modal
    showModal(
      modalDialog(
        title = "Filter columns",
        # Render column filter menu
        output$column_menu <- renderUI({
          # Render menu
          return(
            get_drop_down(
              input_id = "column_menu",
              label = "Columns",
              choices = names(cached$d),
              selected = names(cached$d)
            )
          )
        }),
        size = "m",
        easyClose = FALSE,
        footer = tagList(
          actionButton(
            inputId = "filter_columns_button", 
            label = "Filter columns", 
            class = "pohem-viewer-modal-footer-button"
          ),
          modalButton(label = "Close")
        )
      )
    )
  })
  
  # Monitor filter columns button for clicks
  observeEvent(input$filter_columns_button, {
    # Dependencies
    req(input$column_menu)
    
    # If there are column names in the data table, get the entire data table
    if(sum(! input$column_menu %in% names(cached$d)) > 0) {
      cached$d <- get_data_table(
        data_path = paste0("scenarios/", match_scenario_name_to_file_name(input$scenario_menu)),
        sheet_name = input$scenario_data_table_menu,
        rounding_digits = cached$round_decimals_slider
      )
      
      # Cache R code
      cached$r_code <- c(
        cached$r_code,
        get_code_comment(
          paste0(
            "Import data table (",
            input$scenario_data_table_menu,
            ")"
          )
        ),
        "d = get_data_table(",
        paste0(get_nbsp(2), "data_path = c("),
        paste0(
          get_nbsp(4),
          get_quot(paste0("scenarios/", match_scenario_name_to_file_name(input$scenario_menu))),
          collapse = ",<br>"
        ),
        get_nbsp(2, "),"),
        paste0(
          get_nbsp(2, "sheet_name = "),
          get_quot(input$scenario_data_table_menu, ",")
        ),
        paste0(
          get_nbsp(2, "rounding_digits = "),
          cached$round_decimals_slider
        ),
        ")",
        ""
      )
    }
    
    # Subset data table
    cached$d <- cached$d[input$column_menu]
    
    # Cache R code
    cached$r_code <- c(
      cached$r_code,
      get_code_comment("Subset data table"),
      "d = d[",
      get_nbsp(2, "c("),
      paste0(get_nbsp(4), get_quot(input$column_menu), collapse = ",<br>"),
      get_nbsp(2, ")"),
      "]",
      ""
    )
    
    # Remove modal
    removeModal()
  })
  
  # Filter rows modal button
  output$filter_rows_modal_button <- renderUI({
    # Dependencies
    req(
      input$scenario_menu, 
      input$scenario_data_table_menu,
      cached$scenario_names,
      cached$common_data_tables
    )
    
    # Render button
    return(
      get_button(
        input_id = "filter_rows_modal_button",
        label = "Filter rows",
        class = "pohem-viewer-settings-button"
      )
    )
  })
  
  # Row menu
  row_menu <- reactive({
    # Dependencies
    req(cached$d)
    
    # Iterate through each data table column and render filter menu
    lapply(
      names(cached$d),
      function(x) {
        # Sort unique values
        values <- sort(unique(as.vector(unname(unlist(cached$d[x])))))
        
        # If numeric, render a slider, else render a drop-down menu
        if(is.numeric(values)) {
          return(
            get_slider(
              input_id = paste0("filter_", x),
              label = x,
              min = min(values, na.rm = TRUE),
              max = max(values, na.rm = TRUE),
              value = c(min(values, na.rm = TRUE), max(values, na.rm = TRUE)),
              separator = ifelse(str_detect(tolower(x), "year"), "", ",")
            )
          )
        } else {
          return(
            get_drop_down(
              input_id = paste0("filter_", x),
              label = x,
              choices = values,
              selected = values
            )
          )
        }
      }
    )
  })
  
  # Monitor filter rows modal button for clicks
  observeEvent(input$filter_rows_modal_button, {
    # Require data table with rows
    req(nrow(cached$d) > 0)
    
    # Render modal
    showModal(
      modalDialog(
        title = "Filter rows",
        # Render row filter menu
        output$text_ui <- renderUI(row_menu()),
        size = "l",
        easyClose = FALSE,
        footer = tagList(
          actionButton(
            inputId = "filter_rows_button", 
            label = "Filter rows", 
            class = "pohem-viewer-modal-footer-button"
          ),
          modalButton(label = "Close")
        )
      )
    )
  })
  
  # Monitor filter rows button for clicks
  observeEvent(input$filter_rows_button, {
    # Dependencies
    req(cached$d)
    
    # If data table contains columns, filter rows
    if(length(names(cached$d)) > 0) {
      # Iterate through row filters
      filter_variables <- sapply(names(cached$d), function(x) {
        # If column being filtered is numeric, apply min/max filter, else apply %in% filter
        if(is.numeric(unname(unlist(cached$d[x])))) {
          # Get vector name and min/max inputs
          vector <- paste0("cached$d$", x)
          input1 <- paste0("input$filter_", x, "[1]")
          input2 <- paste0("input$filter_", x, "[2]")
          
          # Build filter code
          code <- paste0(
            "cached$d <- cached$d[",
            vector,
            " >= ",
            input1,
            " & ",
            vector,
            " <= ",
            input2,
            ",]"
          )
          
          # Cache R code
          cached$r_code <- c(
            cached$r_code,
            get_code_comment(paste0("Filter rows (", x, ")")),
            "d = d[",
            get_nbsp(2, paste0("d$", x, " >= ", eval(parse(text = input1)), " & ")),
            get_nbsp(2, paste0("d$", x, " <= ", eval(parse(text = input2)))),
            ",]",
            ""
          )
          
          # Execute code
          eval(parse(text = code))
        } else {
          # Get vector name and character input
          vector <- paste0("cached$d$", x)
          input1 <- paste0("input$filter_", x)
          
          # Build filter code
          code <- paste0(
            "cached$d <- cached$d[which(",
            vector,
            " %in% ",
            input1,
            "),]"
          )
          
          # Cache R code
          cached$r_code <- c(
            cached$r_code,
            get_code_comment(paste0("Filter rows (", x, ")")),
            "d = d[",
            get_nbsp(2, "which("),
            get_nbsp(4, paste0("d$", x, " %in% c(")),
            paste0(get_nbsp(6, get_quot(eval(parse(text = input1)))), collapse = ",<br>"),
            get_nbsp(4, ")"),
            get_nbsp(2, ")"),
            ",]",
            ""
          )
          
          # Execute code
          eval(parse(text = code))
        }
      })
    }
    
    # Remove modal
    removeModal()
  })
  
  # Pivot long modal button
  output$pivot_long_modal_button <- renderUI({
    # Dependencies
    req(
      input$scenario_menu, 
      input$scenario_data_table_menu,
      cached$scenario_names,
      cached$common_data_tables
    )
    
    # Render button
    return(
      get_button(
        input_id = "pivot_long_modal_button",
        label = "Pivot long",
        class = "pohem-viewer-settings-button"
      )
    )
  })
  
  # Monitor pivot long modal button for clicks
  observeEvent(input$pivot_long_modal_button, {
    # Require data table with rows
    req(nrow(cached$d) > 0)
    
    # Render modal
    showModal(
      modalDialog(
        title = "Pivot long",
        # Render column menu
        output$column_menu <- renderUI({
          # Render text input
          return(
            get_drop_down(
              input_id = "column_menu",
              label = "Columns to pivot long",
              choices = names(cached$d)
            )
          )
        }),
        output$names_to_input <- renderUI({
          # Render menu
          return(
            get_text_input(
              input_id = "names_to_input",
              label = "Column name where pivoted column names will be stored"
            )
          )
        }),
        output$values_to_input <- renderUI({
          # Render text input
          return(
            get_text_input(
              input_id = "values_to_input",
              label = "Column name where pivoted column values will be stored"
            )
          )
        }),
        size = "m",
        easyClose = FALSE,
        footer = tagList(
          actionButton(
            inputId = "pivot_long_button", 
            label = "Pivot long", 
            class = "pohem-viewer-modal-footer-button"
          ),
          modalButton(label = "Close")
        )
      )
    )
  })
  
  # Monitor pivot long button for clicks
  observeEvent(input$pivot_long_button, {
    # Dependencies
    req(input$column_menu, input$names_to_input, input$values_to_input)
    
    # Pivot if no inputs match existing column names or are empty or are identical
    if(sum(c(input$names_to_input, input$values_to_input) %in% names(cached$d)) == 0) {
      if(trimws(input$names_to_input) != "" & trimws(input$values_to_input) != "" & trimws(input$names_to_input) != trimws(input$values_to_input)) {
        # Pivot data table
        cached$d <- cached$d %>%
          pivot_longer(
            cols = input$column_menu,
            names_to = input$names_to_input,
            values_to = input$values_to_input
          )
        
        # Cache R code
        cached$r_code <- c(
          cached$r_code,
          get_code_comment(
            paste0(
              "Pivot data table (", 
              input$scenario_data_table_menu, 
              ") longer"
            )
          ),
          "d = d %>%",
          get_nbsp(2, "pivot_longer("),
          get_nbsp(4, "cols = c("),
          paste0(
            get_nbsp(6, get_quot(input$column_menu)),
            collapse = ",<br>"
          ),
          get_nbsp(4, "),"),
          get_nbsp(
            4, 
            paste0(
              "names_to = ",
              get_quot(input$names_to_input, ",")
            )
          ),
          get_nbsp(
            4, 
            paste0(
              "values_to = ",
              get_quot(input$values_to_input)
            )
          ),
          get_nbsp(2, ")"),
          ""
        )
        
        # Remove modal
        removeModal()
      }
    }
  })
  
  # Pivot wide modal button
  output$pivot_wide_modal_button <- renderUI({
    # Dependencies
    req(
      input$scenario_menu, 
      input$scenario_data_table_menu,
      cached$scenario_names,
      cached$common_data_tables
    )
    
    # Render button
    return(
      get_button(
        input_id = "pivot_wide_modal_button",
        label = "Pivot wide",
        class = "pohem-viewer-settings-button"
      )
    )
  })
  
  # Monitor pivot wide modal button for clicks
  observeEvent(input$pivot_wide_modal_button, {
    # Require data table with rows
    req(nrow(cached$d) > 0)
    
    # Render modal
    showModal(
      modalDialog(
        title = "Pivot wide",
        # Render names_from menu
        output$names_from_menu <- renderUI({
          # Render menu
          return(
            get_drop_down(
              input_id = "names_from_menu",
              label = "Column(s) from which column names will be taken",
              choices = names(cached$d)
            )
          )
        }),
        # Render values_from menu
        output$values_from_menu <- renderUI({
          # Render menu
          return(
            get_drop_down(
              input_id = "values_from_menu",
              label = "Column(s) from which column values will be taken",
              choices = names(cached$d)
            )
          )
        }),
        size = "m",
        easyClose = FALSE,
        footer = tagList(
          actionButton(
            inputId = "pivot_wide_button", 
            label = "Pivot wide", 
            class = "pohem-viewer-modal-footer-button"
          ),
          modalButton(label = "Close")
        )
      )
    )
  })
  
  # Monitor pivot long button for clicks
  observeEvent(input$pivot_wide_button, {
    # Dependencies
    req(input$names_from_menu, input$values_from_menu)
    
    # Pivot if no inputs are identical
    if(sum(input$names_from_menu == input$values_from_menu) == 0) {
      # Pivot data table
      cached$d <- cached$d %>%
        pivot_wider(
          names_from = input$names_from_menu,
          values_from = input$values_from_menu
        )
      
      # Cache R code
      cached$r_code <- c(
        cached$r_code,
        get_code_comment(
          paste0(
            "Pivot data table (", 
            input$scenario_data_table_menu, 
            ") wider"
          )
        ),
        "d = d %>%",
        get_nbsp(2, "pivot_wider("),
        get_nbsp(4, "names_from = c("),
        paste0(
          get_nbsp(6, get_quot(input$names_from_menu)),
          collapse = ",<br>"
        ),
        get_nbsp(4, "),"),
        get_nbsp(4, "values_from = c("),
        paste0(
          get_nbsp(6, get_quot(input$values_from_menu)),
          collapse = ",<br>"
        ),
        get_nbsp(4, ")"),
        get_nbsp(2, ")"),
        ""
      )
      
      # Remove modal
      removeModal()
    }
  })
  
  # Complete cases button
  output$complete_cases_button <- renderUI({
    # Dependencies
    req(
      input$scenario_menu, 
      input$scenario_data_table_menu,
      cached$scenario_names,
      cached$common_data_tables
    )
    
    # Render button
    return(
      get_button(
        input_id = "complete_cases_button",
        label = "Complete cases",
        class = "pohem-viewer-settings-button"
      )
    )
  })
  
  # Monitor complete cases button for clicks
  observeEvent(input$complete_cases_button, {
    # Render if data table exists
    req(names(cached$d))
    
    # Apply list-wise deletion to the data table
    cached$d <- cached$d[complete.cases(cached$d),]
    
    # Cache R code
    cached$r_code <- c(
      cached$r_code,
      get_code_comment("Apply list-wise deletion to the data table"),
      "d = d[complete.cases(d),]",
      ""
    )
  })
  
  # Reset data button
  output$reset_data_button <- renderUI({
    # Dependencies
    req(
      input$scenario_menu, 
      input$scenario_data_table_menu,
      cached$scenario_names,
      cached$common_data_tables
    )
    
    # Render button
    return(
      get_button(
        input_id = "reset_data_button",
        label = "Reset",
        class = "pohem-viewer-settings-button"
      )
    )
  })
  
  # Monitor reset button for clicks
  observeEvent(input$reset_data_button, {
    # Dependencies
    req(
      input$scenario_menu, 
      input$scenario_data_table_menu,
      cached$scenario_names
    )
    
    # Get the entire data table
    cached$d <- get_data_table(
      data_path = paste0("scenarios/", match_scenario_name_to_file_name(input$scenario_menu)),
      sheet_name = input$scenario_data_table_menu,
      rounding_digits = cached$round_decimals_slider
    )
    
    # Cache R code
    cached$r_code <- c(
      cached$r_code,
      get_code_comment(
        paste0(
          "Import data table (",
          input$scenario_data_table_menu,
          ")"
        )
      ),
      "d = get_data_table(",
      paste0(get_nbsp(2), "data_path = c("),
      paste0(
        get_nbsp(4),
        get_quot(paste0("scenarios/", match_scenario_name_to_file_name(input$scenario_menu))),
        collapse = ",<br>"
      ),
      get_nbsp(2, "),"),
      paste0(
        get_nbsp(2, "sheet_name = "),
        get_quot(input$scenario_data_table_menu, ",")
      ),
      paste0(
        get_nbsp(2, "rounding_digits = "),
        cached$round_decimals_slider
      ),
      ")",
      ""
    )
  })
  
  # Visualization menu
  output$visualization_menu <- renderUI({
    # Dependencies
    req(
      input$scenario_menu, 
      input$scenario_data_table_menu,
      cached$scenario_names,
      cached$common_data_tables
    )
    
    # Hide this menu if the plot controls toggle is off
    if(cached$hide_visualization_menu) {
      return(NULL)
    }
    
    # Render menu
    return(
      get_radio_buttons(
        input_id = "visualization_menu",
        label = "Visualizations",
        choices = c("Table", "Plot"),
        selected = cached$visualization_menu_selected
      )
    )
  })
  
  # Monitor visualization menu for clicks
  observeEvent(input$visualization_menu, {
    # Update cached visualization menu selection
    cached$visualization_menu_selected <- input$visualization_menu
  })
  
  # Plot controls label
  output$plot_controls_label <- renderUI({
    # Dependencies
    req(input$visualization_menu == "Plot")
    
    # Return label
    return(tags$label("Controls"))
  })
  
  # Plot controls toggle
  output$plot_controls_toggle <- renderUI({
    # Dependencies
    req(input$visualization_menu == "Plot")
    
    # Render toggle
    return(
      get_toggle(
        input_id = "plot_controls_toggle",
        label = "Display",
        value = TRUE
      )
    )
  })
  
  # Monitor the plot controls toggle for clicks
  observeEvent(input$plot_controls_toggle, {
    # If the toggle is on, hide the visualization menu, else, hide
    if(isFALSE(input$plot_controls_toggle)) {
      cached$hide_visualization_menu <- TRUE
    } else {
      cached$hide_visualization_menu <- FALSE
    }
  })
  
  # Import scenario button
  output$import_scenario_button <- renderUI({
    # Render button
    return(
      get_button(
        input_id = "import_scenario_button",
        label = "Import",
        class = "pohem-viewer-settings-button"
      )
    )
  })
  
  # Plot menu
  output$plot_menu <- renderUI({
    # Dependencies
    req(
      input$scenario_menu, 
      input$scenario_data_table_menu,
      cached$scenario_names,
      cached$common_data_tables,
      input$visualization_menu == "Plot",
      input$plot_controls_toggle
    )
    
    # Render menu
    return(
      get_drop_down(
        input_id = "plot_menu",
        label = "Plot type",
        choices = c("Bar", "Line", "Scatter"),
        selected = cached$plot_menu_selected,
        actions_box = FALSE,
        max_options = 1
      )
    )
  })
  
  # Plot title
  output$plot_title <- renderUI({
    # Dependencies
    req(
      cached$common_data_tables,
      input$visualization_menu == "Plot",
      input$plot_controls_toggle
    )
    
    # Render input
    return(
      get_text_input(
        input_id = "plot_title",
        label = "Plot title",
        value = cached$plot_title
      )
    )
  })
  
  # Monitor plot title for input
  observeEvent(input$plot_title, {
    # Cache the title
    cached$plot_title <- input$plot_title
  })
  
  # X variable
  output$x_variable_menu <- renderUI({
    # Dependencies
    req(
      input$scenario_menu, 
      input$scenario_data_table_menu, 
      cached$common_data_tables,
      input$visualization_menu == "Plot",
      input$plot_controls_toggle
    )
    
    # Render menu
    return(
      get_drop_down(
        input_id = "x_variable_menu",
        label = "X variable",
        choices = names(cached$d),
        selected = cached$x_variable_menu_selected,
        actions_box = FALSE,
        max_options = 1
      )
    )
  })
  
  # Monitor the x variable menu for clicks
  observeEvent(input$x_variable_menu, {
    # Cache the selection as the label
    cached$x_variable_label <- input$x_variable_menu
  })
  
  # X variable label
  output$x_variable_label <- renderUI({
    # Dependencies
    req(
      cached$common_data_tables,
      input$visualization_menu == "Plot",
      input$x_variable_menu,
      input$plot_controls_toggle
    )
    
    # Render input
    return(
      get_text_input(
        input_id = "x_variable_label",
        label = "X variable label",
        value = ifelse(
          is.null(cached$x_variable_label), 
          input$x_variable_menu, 
          cached$x_variable_label
        )
      )
    )
  })
  
  # Monitor x variable label for input
  observeEvent(input$x_variable_label, {
    cached$x_variable_label <- input$x_variable_label
  })
  
  # Y variable 
  output$y_variable_menu <- renderUI({
    # Dependencies
    req(
      input$scenario_menu, 
      input$scenario_data_table_menu, 
      cached$common_data_tables,
      input$visualization_menu == "Plot",
      input$plot_controls_toggle
    )
    
    # Render menu
    return(
      get_drop_down(
        input_id = "y_variable_menu",
        label = "Y variable",
        choices = names(cached$d),
        selected = cached$y_variable_menu_selected,
        actions_box = FALSE,
        max_options = 1
      )
    )
  })
  
  # Monitor the y variable menu for clicks
  observeEvent(input$y_variable_menu, {
    # Cache the selection as the label
    cached$y_variable_label <- input$y_variable_menu
  })
  
  # Y variable label
  output$y_variable_label <- renderUI({
    # Dependencies
    req(
      cached$common_data_tables,
      input$visualization_menu == "Plot",
      input$y_variable_menu,
      input$plot_controls_toggle
    )
    
    # Render input
    return(
      get_text_input(
        input_id = "y_variable_label",
        label = "Y variable label",
        value = ifelse(
          is.null(cached$y_variable_label), 
          input$y_variable_menu, 
          cached$y_variable_label
        )
      )
    )
  })
  
  # Monitor y variable label for input
  observeEvent(input$y_variable_label, {
    cached$y_variable_label <- input$y_variable_label
  })
  
  # Group by menu
  output$group_by_menu <- renderUI({
    # Dependencies
    req(
      cached$common_data_tables,
      input$visualization_menu == "Plot",
      input$x_variable_menu,
      input$y_variable_menu,
      input$plot_controls_toggle
    )
    
    # Render menu
    return(
      get_drop_down(
        input_id = "group_by_menu",
        label = "Group by",
        choices = names(cached$d),
        selected = cached$group_by_menu_selected,
        actions_box = FALSE,
        max_options = 1
      )
    )
  })
  
  # Panel column menu
  output$panel_column_menu <- renderUI({
    # Dependencies
    req(
      cached$common_data_tables,
      input$visualization_menu == "Plot",
      input$x_variable_menu,
      input$y_variable_menu,
      input$plot_controls_toggle
    )
    
    # Render menu
    return(
      get_drop_down(
        input_id = "panel_column_menu",
        label = "Panel columns",
        choices = names(cached$d),
        selected = cached$panel_column_menu_selected,
        actions_box = FALSE,
        max_options = 1
      )
    )
  })
  
  # Panel row menu
  output$panel_row_menu <- renderUI({
    # Dependencies
    req(
      cached$common_data_tables,
      input$visualization_menu == "Plot",
      input$x_variable_menu,
      input$y_variable_menu,
      input$plot_controls_toggle
    )
    
    # Render menu
    return(
      get_drop_down(
        input_id = "panel_row_menu",
        label = "Panel rows",
        choices = names(cached$d),
        selected = cached$panel_row_menu_selected,
        actions_box = FALSE,
        max_options = 1
      )
    )
  })
  
  # Scenario settings button
  output$general_settings_modal_button <- renderUI({
    # Render button
    return(
      get_button(
        input_id = "general_settings_modal_button",
        label = "",
        icon = icon("cog", lib = "glyphicon"),
        class = "settings-button"
      )
    )
  })
  
  # Monitor general settings button for clicks
  observeEvent(input$general_settings_modal_button, {
    showModal(
      modalDialog(
        title = "General settings",
        # Render round decimals slider
        output$round_decimals_slider <- renderUI(
          return(
            get_slider(
              input_id = "round_decimals_slider",
              label = "Round decimals",
              min = 0,
              max = 10,
              value = cached$round_decimals_slider
            )
          )
        ),
        size = "s",
        easyClose = FALSE,
        footer = tagList(
          actionButton(
            inputId = "general_settings_button", 
            label = "Update", 
            class = "pohem-viewer-modal-footer-button"
          ),
          modalButton(label = "Close")
        )
      )
    )
  })
  
  # Monitor general settings button
  observeEvent(input$general_settings_button, {
    # Require round decimals slider
    req(input$round_decimals_slider)
    
    # Update cached version of the round decimals slider
    cached$round_decimals_slider <- input$round_decimals_slider
    
    # Update all numeric vectors in cached$d
    cached$d <- bind_cols(
      lapply(cached$d, function(x) {
        if(is.numeric(x)) {
          return(round(x, input$round_decimals_slider))
        } else {
          return(x)
        }
      })
    )
    
    # If a data table exists
    if(nrow(cached$d) > 0) {
      # Cache R code
      cached$r_code <- c(
        cached$r_code,
        get_code_comment(paste0("Round all numeric vectors to ", input$round_decimals_slider, " digits")),
        "d = bind_cols(",
        paste0(get_nbsp(2), "lapply(d, function(x) {"),
        paste0(get_nbsp(4), "if(is.numeric(x)) {"),
        paste0(get_nbsp(6), paste0("return(round(x, ", input$round_decimals_slider, "))")),
        paste0(get_nbsp(4), "} else {"),
        paste0(get_nbsp(6), "return(x)"),
        paste0(get_nbsp(4), "}"),
        paste0(get_nbsp(2), "}"),
        ")",
        ""
      )
    }
    
    # Remove modal
    removeModal()
  })
  
  # Create a list of table inputs to listen for
  listen_for_table_inputs <- reactive({
    list(
      input$scenario_menu, 
      input$scenario_data_table_menu,
      input$visualization_menu,
      cached$d
    )
  })
  
  # Cache R code for data table
  observeEvent(
    listen_for_table_inputs(), 
    {
      # Dependencies
      req(
        input$scenario_menu, 
        input$scenario_data_table_menu,
        input$visualization_menu == "Table"
      )
      
      # If common data tables exist
      if(length(cached$common_data_tables) > 0) {
        # Cache R code
        cached$r_code <- c(
          cached$r_code,
          get_code_comment(
            paste0(
              "Render data table (",
              input$scenario_data_table_menu,
              ")"
            )
          ),
          "datatable(",
          get_nbsp(2, "data = d,"),
          paste0(
            get_nbsp(2, "filter = "),
            get_quot("top", ",")
          ),
          paste0(
            get_nbsp(2, "extensions = c("),
            get_quot("Buttons", ", "),
            get_quot("Scroller", ",")
          ),
          get_nbsp(2, "options = list("),
          get_nbsp(4, "searchHighlight = TRUE,"),
          get_nbsp(4, "pageLength = 25,"),
          paste0(
            get_nbsp(4, "dom = "),
            get_quot("Biftip", ",")
          ),
          paste0(
            get_nbsp(4, "buttons = c("),
            get_quot("colvis", "),")
          ),
          get_nbsp(4, "deferRender = TRUE,"),
          get_nbsp(4, "searchDelay = 100,"),
          get_nbsp(4, "initComplete = JS("),
          get_nbsp(6, get_quot("function(settings, json) {", ",")),
          get_nbsp(6, get_quot("$(this.api().table().header()).css({'background-color': '#fff', 'color': '#111'});", ",")),
          get_nbsp(6, get_quot("}")),
          get_nbsp(4, ")"),
          get_nbsp(2, ")"),
          ")",
          ""
        )
      }
    }
  )
  
  # Render data table
  output$data_table <- renderDT(
    {
      # Dependencies  
      req(
        input$scenario_menu, 
        input$scenario_data_table_menu,
        cached$common_data_tables,
        input$visualization_menu
      )
      
      # Pass cached data table
      if(input$visualization_menu != "Table") {
        return(NULL)
      } else {
        return(cached$d)
      }
    },
    filter = "top",
    extensions = c("Buttons", "Scroller"),
    options = list(
      searchHighlight = TRUE,
      pageLength = 25,
      dom = "Biftip",
      buttons = c("colvis"),
      deferRender = TRUE,
      searchDelay = 100,
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#fff', 'color': '#111'});",
        "}"
      )
    ),
    rownames = FALSE,
    escape = FALSE,
    style = "bootstrap"
  )
  
  # Create a list of plot inputs to listen for
  listen_for_plot_inputs <- reactive({
    list(
      input$visualization_menu,
      input$plot_menu,
      input$x_variable_menu,
      input$x_variable_label,
      input$y_variable_menu,
      input$y_variable_label,
      input$group_by_menu,
      input$panel_column_menu,
      input$panel_row_menu,
      input$plot_title,
      cached$d
    )
  }) %>% 
    debounce(500)
  
  # Cache R code for plot
  observeEvent(
    listen_for_plot_inputs(), 
    {
      # Dependencies
      req(
        input$scenario_menu, 
        input$scenario_data_table_menu,
        input$visualization_menu == "Plot",
        input$plot_menu,
        input$x_variable_menu,
        input$y_variable_menu
      )
      
      # Cache R code
      cached$r_code <- c(
        cached$r_code,
        get_code_comment(paste0("Render ", tolower(input$plot_menu), " plot")),
        paste0("get_", tolower(input$plot_menu), "_plot("),
        get_nbsp(2, "df = d,"),
        get_nbsp(
          2, 
          paste0(
            "x = ",
            get_quot(input$x_variable_menu, ",")
          )
        ),
        get_nbsp(
          2, 
          paste0(
            "x_variable_label = ",
            get_quot(input$x_variable_label, ",")
          )
        ),
        get_nbsp(
          2, 
          paste0(
            "y = ",
            get_quot(input$y_variable_menu, ",")
          )
        ),
        get_nbsp(
          2, 
          paste0(
            "y_variable_label = ",
            get_quot(input$y_variable_label, ",")
          )
        ),
        get_nbsp(
          2, 
          paste0(
            "group = ",
            ifelse(
              is.null(input$group_by_menu),
              "NULL,",
              get_quot(input$group_by_menu, ",")
            )
          )
        ),
        get_nbsp(
          2, 
          paste0(
            "panel_column = ",
            ifelse(
              is.null(input$panel_column_menu),
              "NULL,",
              get_quot(input$panel_column_menu, ",")
            )
          )
        ),
        get_nbsp(
          2, 
          paste0(
            "panel_row = ",
            ifelse(
              is.null(input$panel_row_menu),
              "NULL,",
              get_quot(input$panel_row_menu, ",")
            )
          )
        ),
        get_nbsp(
          2, 
          paste0(
            "plot_title = ",
            get_quot(input$plot_title)
          )
        ),
        ")",
        ""
      )
    }
  ) 
  
  # Render plot
  output$plot <- renderPlotly({
    # Dependencies  
    req(
      input$scenario_menu, 
      input$scenario_data_table_menu, 
      cached$common_data_tables,
      input$visualization_menu == "Plot",
      input$plot_menu,
      input$x_variable_menu %in% names(cached$d),
      input$y_variable_menu %in% names(cached$d)
    )
    
    # Update cached plot-related variables
    cached$x_variable_menu_selected <- input$x_variable_menu
    cached$y_variable_menu_selected <- input$y_variable_menu
    cached$group_by_menu_selected <- input$group_by_menu
    cached$panel_column_menu_selected <- input$panel_column_menu
    cached$panel_row_menu_selected <- input$panel_row_menu
    
    # Render plot
    if(input$plot_menu == "Bar") {
      # Get plot object and stats
      p <- get_bar_plot(
        df = cached$d,
        x = input$x_variable_menu,
        x_variable_label = cached$x_variable_label,
        y = input$y_variable_menu,
        y_variable_label = cached$y_variable_label,
        group = cached$group_by_menu_selected,
        panel_column = cached$panel_column_menu_selected,
        panel_row = cached$panel_row_menu_selected,
        plot_title = cached$plot_title
      )
      
      # Render
      return(
        ggplotly(
          p$plot,
          height = 300 * p$rows
        )
      )
    } else if(input$plot_menu == "Line") {
      # Get plot object and stats
      p <- get_line_plot(
        df = cached$d,
        x = input$x_variable_menu,
        x_variable_label = cached$x_variable_label,
        y = input$y_variable_menu,
        y_variable_label = cached$y_variable_label,
        group = cached$group_by_menu_selected,
        panel_column = cached$panel_column_menu_selected,
        panel_row = cached$panel_row_menu_selected,
        plot_title = cached$plot_title
      )
      
      # Render
      return(
        ggplotly(
          p$plot,
          height = 300 * p$rows
        )
      )
    } else {
      # Get plot object and stats
      p <- get_scatter_plot(
        df = cached$d,
        x = input$x_variable_menu,
        x_variable_label = cached$x_variable_label,
        y = input$y_variable_menu,
        y_variable_label = cached$y_variable_label,
        group = cached$group_by_menu_selected,
        panel_column = cached$panel_column_menu_selected,
        panel_row = cached$panel_row_menu_selected,
        plot_title = cached$plot_title
      )
      
      # Render
      return(
        ggplotly(
          p$plot,
          height = 300 * p$rows
        )
      )
    }
  }) %>% 
    debounce(500)
}