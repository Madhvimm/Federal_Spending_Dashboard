# Install and load required libraries
#if (!require("devtools")) install.packages("devtools")
library(devtools)
#devtools::install_github("pdil/usmap")
library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(leaflet)
library(leafdown)
library(shiny)
library(tigris)
library(sf)
library(plotly)
library(colorspace)
library(DT)
library(scales) # for the comma function


# Download and process state shapefile data
states_geo_data <- states(cb = TRUE)  # 'cb' stands for cartographic boundary

# Convert to Spatial Data Frame 
states_geo_data <- st_as_sf(states_geo_data)

# Transform to WGS84 CRS (Coordinate Reference System)
states_geo_data <- st_transform(states_geo_data, crs = 4326)


# Load the data
original_data <- read.csv("filtered_latest_spendingdata.csv")

# Aggregate Data by Agency
agency_data <- original_data %>%
  group_by(agency_identifier_name) %>%
  summarise(agency_total_spending = sum(transaction_obligated_amount, na.rm = TRUE))

# State-level aggregation on the original data
state_data <- original_data %>%
  group_by(recipient_state) %>%
  summarise(total_spending = sum(transaction_obligated_amount, na.rm = TRUE)) %>%
  rename(state = recipient_state)

# Convert 'total_spending' to numeric 
state_data$total_spending <- as.numeric(state_data$total_spending)

# Shiny UI
ui <- fluidPage(
  
  # Custom title panel with clickable logo on the right
  div(class = "title-div", 
      h1("Federal Spending Analysis", class = "title-heading", style = "flex-grow: 1;"), # Heading with flexible growth
      tags$a(href = "https://www.usaspending.gov/", 
             tags$img(src = "usa_spending_logo.png", height = "30px", style = "float: right;"),
             target = "_blank" # Opens the link in a new tab
      ),
      style = "display: flex; align-items: center; justify-content: space-between;"
  ),
  
  # Horizontal line
  tags$hr(style = "border-top: 1px solid #ccc;"),
  
  
  # Create a tabset that includes two tabs
  tabsetPanel(
    # First tab with introductory text
    tabPanel("Introduction",
             fluidRow(
               column(12,
                      tags$head(
                        tags$style(HTML("
                        .instruction-list > li {
                        border-bottom: 1px solid #ddd; 
                        padding-bottom: 12px; 
                        margin-bottom: 12px;
                        }
                        /* Apply Georgia font to the text */
                        body {
                        font-family: 'Georgia', 'serif';
                        font-size: 16px; /* Adjust the font size as needed */}"
                                        )
                                   )
                        ),
                      tags$h2(tags$b("Welcome to the Federal Spending Analysis Dashboard!")),
                      tags$br(),  # Adds a line break
                      tags$p("This interactive dashboard is designed to offer a comprehensive 
                             view of federal spending across the United States, tailored to be accessible and informative for a diverse audience. Whether you're 
                             a policy analyst, researcher, educator, or a curious citizen, this tool aims to demystify the complexities of government spending."),
                      tags$br(),  # Adds a line break
                      tags$h3(tags$b("Understanding Federal Spending:")),
                      tags$p("Federal spending reflects the priorities and strategies of the government, impacting various sectors such as healthcare, 
                             education, defense, and infrastructure. By dissecting this spending, we can gain insights into economic trends and policy directions."),
                      tags$br(),  # Adds a line break
                      tags$h3(tags$b("Data Source and Integrity:")),
                      tags$p("The data fueling this dashboard is sourced directly from ", 
                             tags$a(href = "https://api.usaspending.gov/", "USA Spending", target = "_blank"), 
                             ", the official open data source of federal spending information. We access this data using their API, which 
                             ensures real-time and accurate financial information."),
                      tags$br(),  # Adds a line break
                      tags$h3(tags$b("Approach:")),
                      tags$p("The visualizations are designed to be intuitive, allowing users to explore data at both macro and micro levels. As you 
                             interact with the dashboard, you will find that selecting a state or agency dynamically updates all components, 
                             including the map, bar chart, and the detailed data table, providing a cohesive and comprehensive view of federal spending."),
                      tags$br(),  # Adds a line break
                      tags$p(tags$b("In this dashboard, you will find:")),
                      tags$br(),  # Adds a line break
                      tags$ul(class = "instruction-list",
                              tags$li("An interactive geographical map displaying spending data by state."),
                              tags$li("A dynamic bar chart detailing spending by federal agencies."),
                              tags$li("A detailed table providing granular data views based on your selections."),
                              tags$li("User-friendly filters for a customized viewing experience."),
                              tags$li("Tooltips and hover-over interactions for deeper insights.")),
                      tags$br(),  # Adds a line break
                      tags$h3(tags$b("Navigating the Dashboard:")),
                      tags$br(),  # Adds a line break
                      tags$ul(class = "instruction-list",
                              tags$li("Explore state-specific spending patterns using the map"),
                              tags$li("Click on a state or an agency to dynamically update the bar chart and the detailed data table."),
                              tags$li("Utilize the 'Reset' buttons to clear selections for a broader view"),
                              tags$li("Hover over elements in the visualizations for more detailed information")
                      ),
                      tags$p("Each interaction is designed to give you a comprehensive understanding of federal spending, whether you 
                             are browsing through the high-level state summaries or delving into the specifics of agency expenditures."),
                      tags$br(),  # Adds a line break
                      tags$h4("Thank you for visiting our Federal Spending Analysis Dashboard. Hope this tool empowers you with a clearer 
                              understanding of federal spending patterns."),
                      tags$br(),# Adds a line break
                      # Instructions or additional text
                      tags$p(tags$b(tags$i("For more information, and code, visit the GitHub repository:")),
                             style = "text-align: center; margin-top: 20px;"), # Centered text
                      
                      # GitHub logo with link, centered
                      div(style = "text-align: center;", # Center the contents of the div
                          tags$a(href = "https://github.com/Madhvimm/Federal_Spending_Dashboard", 
                                 tags$img(src = "github-mark.png", height = "40px"),
                                 target = "_blank" # Opens the link in a new tab
                          )
                      ),
                      tags$br(),# Adds a line break
                      tags$br(),# Adds a line break
                      tags$br()# Adds a line break
                      )
               )
             ),
    
    # Second tab with the analysis components
    tabPanel(
      "Analysis",
      tags$br(),# Adds a line break
      wellPanel(
        uiOutput("dynamicMessageBox"), # Dynamic message box with custom styling
        style = "background-color: #BBBCBC;
        color: #041E42;
        padding: 15px;
        border-radius: 4px;
        font-size: 18px;font-family: 'Georgia', 'serif';"  # Font family and size
      ),
      hr(), # Line separator
      fluidRow(
        column(6, actionButton("resetStates", "Reset Map", class = "btn-primary",
                               style = "font-family: 'Georgia', 'serif';")), # Font for reset button
        column(6, actionButton("resetAgency", "Reset Chart", class = "btn-primary",
                               style = "font-family: 'Georgia', 'serif';")) # Font for reset button
      ),
      hr(), # Line separator
      fluidRow(
        column(6, uiOutput("selectedStateTitle")),
        column(6, uiOutput("selectedAgencyTitle"))
      ),
      fluidRow(
        column(6, leafletOutput("spendingMap")),
        column(6, plotlyOutput("agencyBarChart"))
      ),
      hr(), # Line separator
      #TABLE
      fluidRow(
        DT::dataTableOutput("detailsTable")
      )
      
    )
    )
  )
    



# Shiny Server

server <- function(input, output, session) {
  
  # Reactive value to store the selected states as a vector
  selected_states <- reactiveVal(vector())
  
  # Merge state data with geographic data outside of renderLeaflet
  merged_state_data <- merge(states_geo_data, state_data, by.x = "NAME", by.y = "state")
  
  # Reactive value to store the selected agency
  selected_agency <- reactiveVal()
  
  # Reactive expression for the color palette
  color_palette <- reactive({
    # Define a custom color palette for mapping
    custom_palette <- c("#ffffcc", "#c7e9b4", "#7fcdbb", "#1d91c0","#41b6c4", "#225ea8", "#0c2c84")
    
    # Determine the correct spending values based on the selected agency
    spending_values <- if (!is.null(selected_agency())) {
      reactive_state_data()$agency_total_spending
    } else {
      merged_state_data$total_spending
    }
    
    # Calculate the spending range for color mapping
    spending_range <- range(spending_values, na.rm = TRUE)
    
    # Create a color palette based on the spending range
    colorNumeric(palette = custom_palette, domain = spending_range)
  })
  
  # Render the map
  output$spendingMap <- renderLeaflet({
    # Create a Leaflet map using merged state data
    leaflet(merged_state_data) %>%
      # Add map tiles from CartoDB Positron provider
      addProviderTiles(providers$CartoDB.Positron) %>%
      # Add polygons representing states with specified properties
      addPolygons(
        fillColor = ~color_palette()(total_spending),
        fillOpacity = 0.9,
        color = "white",
        weight = 1,
        layerId = ~NAME,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "gold",
          fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        label = ~paste(NAME, ": $", formatC(total_spending / 1e6, format = "f", digits = 2), "M"),
        labelOptions = labelOptions(
          direction = "auto",
          noHide = FALSE,  # Set to FALSE for hover effect
          textOnly = TRUE
        )
      )
  })
  
  # Observe clicks on the map and update the selected states
  observeEvent(input$spendingMap_shape_click, {
    clicked_state <- input$spendingMap_shape_click$id
    current_states <- selected_states()
    if (clicked_state %in% current_states) {
      # Remove the clicked state if it's already selected
      selected_states(current_states[!current_states == clicked_state])
    } else {
      # Add the clicked state to the selection if it's not already selected
      selected_states(c(current_states, clicked_state))
    }
  })
  
  # Render the bar chart based on selected states or all states by default
  output$agencyBarChart <- renderPlotly({
    # Check if any states are selected
    selected <- selected_states()
    selected_agency_name <- selected_agency()
    
    # Determine the data to be used for the bar chart
    filtered_data <- if (length(selected) > 0) {
      # Filter data based on selected states
      original_data %>%
        filter(recipient_state %in% selected) %>%
        group_by(agency_identifier_name) %>%
        summarise(agency_total_spending = sum(transaction_obligated_amount, na.rm = TRUE))
    } else {
      # Use data for all states if no specific state is selected
      agency_data
    }
    
    # Create the bar chart with conditional coloring
    p <- plot_ly(
      data = filtered_data,
      x = ~reorder(agency_identifier_name, agency_total_spending),
      y = ~agency_total_spending,
      type = 'bar',
      marker = list(
        color = if (is.null(selected_agency_name) || selected_agency_name == "") {
          '#225ea8'  # Default color when no agency is selected
        } else {
          ifelse(filtered_data$agency_identifier_name == selected_agency_name, '#1d91c0', 'lightgrey')  # Highlight the selected agency
        }
      ),
      source = "agencyBarChart"  # Source ID for event handling
    ) %>%
      layout(
        yaxis = list(type = "log", title = "Agency Total Spending", titlefont = list(family = "Georgia, serif")),
        xaxis = list(title = "Agency Name", titlefont = list(family = "Georgia, serif")),
        font = list(family = "Georgia, serif")
      )
    
    # Register the click event on the plot
    event_register(p, "plotly_click")
    
    # Return the plot object
    p
  })
  
  # Reactive expression for state data based on selected agency
  reactive_state_data <- reactive({
    if (!is.null(selected_agency())) {
      # Filter original data for the selected agency and summarise
      agency_states <- original_data %>%
        filter(agency_identifier_name == selected_agency()) %>%
        group_by(recipient_state) %>%
        summarise(agency_total_spending = sum(transaction_obligated_amount, na.rm = TRUE))
      
      # Merge with geographic data
      merge(states_geo_data, agency_states, by.x = "NAME", by.y = "recipient_state")
    } else {
      merged_state_data  # Default to showing all state data
    }
  })
  
  # Observe clicks on the bar chart and update the selected agency
  observeEvent(event_data("plotly_click", source = "agencyBarChart"), {
    click_data <- event_data("plotly_click", source = "agencyBarChart")
    selected_agency(click_data$x)
  }, ignoreNULL = FALSE)
  
  # Update the map based on the selected states and agency
  observe({
    data_for_polygons <- if (!is.null(selected_agency())) {
      reactive_state_data()
    } else {
      merged_state_data
    }
    
    if (!is.null(data_for_polygons) && nrow(data_for_polygons) > 0 &&
        "agency_total_spending" %in% names(data_for_polygons) &&
        "NAME" %in% names(data_for_polygons)) {
      
      spending_column <- if (!is.null(selected_agency())) {
        "agency_total_spending"
      } else {
        "total_spending"
      }
      
      # Clear existing map shapes and add new polygons
      leafletProxy("spendingMap", session) %>%
        clearShapes() %>%
        addPolygons(
          data = data_for_polygons,
          fillColor = ~color_palette()(get(spending_column)),
          color = ifelse(data_for_polygons$NAME %in% selected_states(), "darkred", "white"),
          weight = 2,
          fillOpacity = 0.9,
          highlightOptions = highlightOptions(
            weight = 3,
            color = "gold",
            fillOpacity = 0.9,
            bringToFront = TRUE
          ),
          label = ~paste(NAME, ": $", formatC(get(spending_column) / 1e6, format = "f", digits = 2), "M"),
          labelOptions = labelOptions(
            direction = "auto",
            noHide = FALSE,
            textOnly = TRUE
          ),
          layerId = ~NAME
        )
    }
  })
  
  
  #### Reset Button Agency
  
  # Observe clicks on the reset button and reset the selected agency
  observeEvent(input$resetAgency, {
    selected_agency(NULL)
  })
  
  # Agency Title
  output$selectedAgencyTitle <- renderUI({
    selected_agency_name <- selected_agency()
    if (!is.null(selected_agency_name) && selected_agency_name != "") {
      tags$h3(style = "font-family: 'Georgia', 'serif';", 
              "Selected Agency: ", selected_agency_name)
    } else {
      tags$h3(style = "font-family: 'Georgia', 'serif';", "All Agencies are Selected")
    }
  })
  
  ####Output table based on selected states or agencies:
  output$detailsTable <- DT::renderDataTable({
    # Determine the dataset based on selections
    selected_state_names <- selected_states()
    selected_agency_name <- selected_agency()
    
    # Filter the data based on selected states and agency
    filtered_data <- original_data
    if (length(selected_state_names) > 0) {
      filtered_data <- filtered_data %>% filter(recipient_state %in% selected_state_names)
    }
    if (!is.null(selected_agency_name) && selected_agency_name != "") {
      filtered_data <- filtered_data %>% filter(agency_identifier_name == selected_agency_name)
    }
    
    # Select relevant columns and format transaction_obligated_amount
    display_data <- filtered_data %>%
      select(submission_period,
             agency_identifier_name,
             budget_function,
             budget_subfunction,
             program_activity_name,
             transaction_obligated_amount,
             award_type,
             recipient_name,
             recipient_state,
             recipient_county,
             recipient_city) %>%
      mutate(transaction_obligated_amount = if_else(
        transaction_obligated_amount == floor(transaction_obligated_amount),
        scales::comma(transaction_obligated_amount),
        scales::comma(transaction_obligated_amount, accuracy = 0.01)
      ))
    
    # Render the data table
    DT::datatable(display_data, options = list(pageLength = 5), escape = FALSE)
  })
  
  #### Reset Button State
  
  # Observe clicks on the reset button for states and reset the selected states
  observeEvent(input$resetStates, {
    selected_states(vector())
  })
  
  # Display the selected states above the bar chart
  output$selectedStateTitle <- renderUI({
    selected <- selected_states()
    if (length(selected) > 0) {
      tags$h3(style = "font-family: 'Georgia, serif;", 
              "Selected States: ", paste(selected, collapse = ", "))
    } else {
      tags$h3(style = "font-family: 'Georgia, serif;", "All States are Selected")
    }
  })
  
  #Update the Message Box
  output$dynamicMessageBox <- renderUI({
    selected_state_names <- selected_states()
    selected_agency_name <- selected_agency()
    
    # Compute total funding and project count based on current selections
    current_data <- if (!is.null(selected_agency_name) && selected_agency_name != "") {
      # Filter by selected agency and handle all states scenario
      if (length(selected_state_names) > 0) {
        original_data %>%
          filter(agency_identifier_name == selected_agency_name, recipient_state %in% selected_state_names)
      } else {
        original_data %>%
          filter(agency_identifier_name == selected_agency_name)
      }
    } else if (length(selected_state_names) > 0) {
      # Filter by selected states
      original_data %>%
        filter(recipient_state %in% selected_state_names)
    } else {
      # Use all data if no specific selection
      original_data
    }
    
    #Calculate total funding and number of projects
    total_funding <- sum(current_data$transaction_obligated_amount, na.rm = TRUE)
    total_projects <- nrow(current_data)
    
    # Create the message content
    message_content <- paste0(
      "Selected State: ", if (length(selected_state_names) > 0) paste(selected_state_names, collapse = ", ") else "All States",
      "<br>Agency: ", if (!is.null(selected_agency_name) && selected_agency_name != "") selected_agency_name else "All Agencies",
      "<br>Total Funding: $", formatC(total_funding / 1e6, format = "f", digits = 2), " Million",
      "<br>Total Projects: ", total_projects
    )
    
    # Return the formatted message as HTML
    HTML(message_content)
  })
}

# Run the Shiny app
shinyApp(ui, server)
