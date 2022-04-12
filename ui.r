ui <- fluidPage(
  # App title
  titlePanel(""),
  
  # CSS styling
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css/style.css")
  ),
  
  # Sidebar
  sidebarPanel(
    # Scenarios
    uiOutput("scenario_menu"),
    uiOutput("import_scenario_button"),
    uiOutput("rename_scenario_modal_button"),
    uiOutput("clone_scenario_modal_button"),
    uiOutput("delete_scenario_modal_button"),
    uiOutput("import_scenario"),
    uiOutput("import_scenario_error_message"),
    uiOutput("conditional_hr"),
    # Data tables
    uiOutput("scenario_data_table_menu"),
    uiOutput("scenario_data_table_error_message"),
    uiOutput("rename_columns_modal_button"),
    uiOutput("filter_columns_modal_button"),
    uiOutput("filter_rows_modal_button"),
    uiOutput("pivot_long_modal_button"),
    uiOutput("pivot_wide_modal_button"),
    uiOutput("complete_cases_button"),
    uiOutput("reset_data_button"),
    # Other settings
    uiOutput("unconditional_hr"),
    tags$label("General settings"),
    uiOutput("general_settings_modal_button"),
    width = 3
  ),
  
  # Main page
  mainPanel(
    tabsetPanel(
      id = "tabs",
      tabPanel(
        "Excel viewer", br(),
        fluidRow(
          column(
            width = 3,
            uiOutput("visualization_menu"),
            uiOutput("plot_controls_label"),
            uiOutput("plot_controls_toggle")
          ),
          column(
            width = 3,
            uiOutput("plot_menu"),
            uiOutput("plot_title"),
            uiOutput("group_by_menu")
          ),
          column(
            width = 3,
            uiOutput("x_variable_menu"),
            uiOutput("x_variable_label"),
            uiOutput("panel_column_menu")
          ),
          column(
            width = 3,
            uiOutput("y_variable_menu"),
            uiOutput("y_variable_label"),
            uiOutput("panel_row_menu")
          )
        ), br(),
        DTOutput("data_table") %>% withSpinner(color = "#5bc0de", type = 8, color.background = "#ffffff"),
        plotlyOutput("plot", width = "100%", height = "auto") %>% withSpinner(color = "#5bc0de", type = 8, color.background = "#ffffff")
      ),
      tabPanel(
        "R console", br(),
        htmlOutput("r_code")
      )
    ),
    width = 9
  )
)