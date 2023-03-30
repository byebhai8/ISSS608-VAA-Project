#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# ---------------------------------------------------------------------------------------
# Start of R Shiny App (Loading Libraries, Cleansing Datasets)
# ---------------------------------------------------------------------------------------

# Load necessary libraries for the R Shiny App
pacman::p_load(shiny, shinydashboard, shinythemes, shinydashboardPlus, shinyWidgets, shinybusy,
  readxl, lubridate, ggHoriPlot, scales, viridis, timetk, modeltime, kernlab, reactable, tidymodels, plotly, tidyverse)

# Load the dataset
vaByMarketRaw <- read_excel("outputFile -International Visitor Arrivals.xlsx", sheet = "T1", skip = 10)
vaByDemoRaw <- read_excel("outputFile -International Visitor Arrivals.xlsx", sheet = "T2", skip = 10)
vaByStayRaw <- read_excel("outputFile -International Visitor Arrivals.xlsx", sheet = "T3", skip = 9)

# 1) Cleansing of Tourism Markets file

# This file:
# - contains the international visitor arrivals by inbound tourism markets (monthly)
# - excludes arrivals of Malaysians by land
# - Feb 1991 has a sharp decline due to Gulf crisis
# - data for Germany prior to 1991 refers to West Germany only
# - all numbers are counts

# Cleansing steps:

# The following was done to cleanup the vaByMarketRaw dataframe:
# - remove the bottom few rows as they were unnecessary for our visualizations
# - create a new column to assign the value of Region to the respective Countries
# - rename fields and rearrange the columns
# - filter out rows that are not needed anymore
# - pivot date (month-year) and the number of visitors to reduce the number of columns

# Remove bottom rows that are not needed
vaByMarket <- slice(vaByMarketRaw, 2:(62))

# Rename columns to be more appropriate
colnames(vaByMarket)[1] <- "Data"

# Tag countries with a region
# Tag countries with a region
vaByMarket$Region <- case_when(
  vaByMarket$Data %in% c("Brunei Darussalam", "Indonesia", "Malaysia", "Myanmar", "Philippines", "Thailand", "Vietnam", "Other Markets In Southeast Asia") ~ "Southeast Asia",
  vaByMarket$Data %in% c("China", "Hong Kong SAR", "Taiwan", "Other Markets In Greater China") ~ "Greater China",
  vaByMarket$Data %in% c("Japan", "South Korea", "Other Markets In North Asia") ~ "North Asia",
  vaByMarket$Data %in% c("Bangladesh", "India", "Pakistan", "Sri Lanka", "Other Markets In South Asia") ~ "South Asia",
  vaByMarket$Data %in% c("Iran", "Israel", "Kuwait", "Saudi Arabia", "United Arab Emirates", "Other Markets In West Asia") ~ "West Asia",
  vaByMarket$Data %in% c("Canada", "USA", "Other Markets In Americas") ~ "Americas",
  vaByMarket$Data %in% c("Belgium & Luxembourg", "Denmark", "Finland", "France", "Germany", "Italy", "Netherlands", "Norway", "Rep Of Ireland", "Russian Federation", "Spain", "Sweden", "Switzerland", "United Kingdom", "Other Markets In Europe") ~ "Europe",
  vaByMarket$Data %in% c("Australia", "New Zealand", "Other Markets In Oceania") ~ "Oceania",
  vaByMarket$Data %in% c("Egypt", "Mauritius", "South Africa (Rep Of)", "Other Markets In Africa") ~ "Africa",
  vaByMarket$Data %in% c("Others") ~ "Others",
  TRUE ~ "NA"
)

# Reorder columns
vaByMarket <- vaByMarket %>%
  select(542, 1, 2:541)

# Rename columns to be more appropriate
colnames(vaByMarket)[2] <- "Country"

# Issue: certain columns are in character datatype when they should be a numerical datatype
# Solution: map (relevant) columns that are in character datatype to a numerical datatype
# sapply(vaByMarket, class)

# Filter out NA to remove unnecessary rows
# Pivot fields to make dataset long instead of wide
# Covert Year-Month field into a Date field
# Store Year as a new field
vaByMarket <- vaByMarket %>%
  mutate_at(vars(-one_of("Region", "Country")), as.numeric) %>%
  filter(vaByMarket$Region != "NA") %>%
  pivot_longer(cols = ! c("Region", "Country"), names_to = "Period", values_to = "Visitors") %>%
  mutate(Period = as.Date(paste(Period, "01"), "%Y %B %d")) %>%
  mutate(Year = year(Period))

# Prepare data values that will be displayed in the UI

# Find and store the min and max Year values
minYear <- min(vaByMarket$Year)
maxYear <- max(vaByMarket$Year)

# Find and store list of Regions & Countries
listRegions <- unique(vaByMarket$Region)
listRegions <- sort(listRegions)
listCountry <- unique(vaByMarket$Country)
listCountry <- sort(listCountry)

# 2) Cleansing of Visitors' Demographics file

# This file:
# - contains the international visitor arrivals by gender and age groups (monthly for the last 6 months of dec)
# - excludes arrivals of Malaysians by land
# - all numbers are counts

# Cleansing steps:

# The following was done to cleanup the vaByDemoRaw dataframe:
# - remove the bottom few rows as they were unnecessary for our visualizations
# - rename fields and rearrange the columns
# - split the dataframe into two, one to contain info on gender and one to contain info on age
# - next, we will pivot date (month-year) and the number of visitors to reduce the number of columns

vaByDemo <- slice(vaByDemoRaw, 2:11)
vaByDemoGender <- slice(vaByDemo, 1:2)
vaByDemoAge <- slice(vaByDemo, 3:11)

# Cleansing Gender dataframe:
colnames(vaByDemoGender)[1] <- "Gender"
vaByDemoGender <- vaByDemoGender %>%
  pivot_longer(cols = !c("Gender"), names_to = "Period", values_to = "Visitors") %>%
  mutate(Period = as.Date(paste(Period, "01"), "%Y %B %d")) %>%
  mutate(Year = year(Period)) %>%
  mutate(Month = month(Period))

# Cleansing Age dataframe:
colnames(vaByDemoAge)[1] <- "AgeGroup"
vaByDemoAge <- vaByDemoAge %>%
  pivot_longer(cols = !c("AgeGroup"), names_to = "Period", values_to = "Visitors") %>%
  mutate(Period = as.Date(paste(Period, "01"), "%Y %B %d")) %>%
  mutate(Year = year(Period)) %>%
  mutate(Month = month(Period))

# Prepare data values that will be displayed in the UI

# To find the age range of most visitors:
visitorsAge <- vaByDemoAge %>%
  group_by(AgeGroup) %>%
  summarise(Visitors = sum(Visitors))
visitorsAge <- visitorsAge[order(visitorsAge$Visitors, decreasing = TRUE), ]
mostAge <- head(visitorsAge$AgeGroup, 1)

# 3) Cleansing of Visitors' Length of Stay file

# This file:
# - contains the international visitor arrivals by length of stay (monthly for the last 6 months of dec)
# - excludes arrivals of Malaysians by land
# - all numbers are counts

# Cleansing steps:

# The following was done to cleanup the vaByStayRaw dataframe:
# - remove the bottom few rows as they were unnecessary for our visualizations
# - rename fields and rearrange the columns
# - next, we will pivot date (month-year) and the number of visitors to reduce the number of columns
# - we will also replace all mention of "(Number)" as we do not need this text explicitly stated in our column

vaByStay <- slice(vaByStayRaw, 2:15)
colnames(vaByStay)[1] <- "Duration"
vaByStay <- vaByStay %>%
  pivot_longer(cols = !c("Duration"), names_to = "Period", values_to = "Visitors") %>%
  mutate(Period = as.Date(paste(Period, "01"), "%Y %B %d")) %>%
  mutate(Year = year(Period)) %>%
  mutate(Month = month(Period)) %>%
  mutate(Duration = str_replace_all(Duration, " \\(Number\\)", ""))

# To find the avg days most visitors stayed:
visitorsStayed <- vaByStay %>%
  group_by(Duration) %>%
  summarise(Visitors = sum(Visitors))
visitorsStayed <- visitorsStayed[order(visitorsStayed$Visitors, decreasing = TRUE), ]
mostStayed <- head(visitorsStayed$Duration, 1)


# ---------------------------------------------------------------------------------------
# UI Portion of R Shiny App
# ---------------------------------------------------------------------------------------

# Define UI for application
ui <- dashboardPage(
  title = "Visitor Arrivals Analysis",
  skin = "purple",
  dashboardHeader(title = span(tagList(icon("plane-arrival"), span("Visitor Arrivals Analysis", style = "font-size: 16px")))),
  dashboardSidebar(
    tags$style(HTML(".sidebar-menu li a { font-size: 14px; } i { padding-right: 5px; }")),
    sidebarMenu(
      menuItem("Summary", tabName = "tab_summary", icon = icon("house")),
      menuItem("Explore", tabName = "tab_explore", icon = icon("magnifying-glass"),
               menuSubItem("Tourism Markets", tabName = "tab_explore_markets", icon = icon("earth-americas")),
               menuSubItem("Demographics", tabName = "tab_explore_demo", icon = icon("venus-mars")),
               menuSubItem("Length Of Stay", tabName = "tab_explore_los", icon = icon("bed"))
      ),
      menuItem("Forecast", tabName = "tab_forecast", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tags$head(tags$link(rel = "shortcut icon", href = "plane.ico")),
    tags$style(".small-box.bg-yellow { background-color: LightSteelBlue !important; color: #000000 !important; }"),
    tags$style(".shiny-output-error { visibility: hidden; }", ".shiny-output-error:before { visibility: hidden; }"),
    tabItems(
      tabItem("tab_summary",
              fluidRow(
               valueBoxOutput("vboxVisitors", width = 3),
               valueBoxOutput("vboxCountry", width = 3),
               valueBoxOutput("vboxAge", width = 3),
               valueBoxOutput("vboxStay", width = 3)
             ),
             fluidRow(
               accordion(
                 id = "parameters", width = 3,
                 accordionItem(
                   title = span("Filters", style = "font-size: 14px; font-weight: bold"), status = "primary", collapsed = FALSE,
                   sliderInput("sliderYear", label = h5("Year Range"), min = minYear, max = maxYear, value = c(minYear, maxYear), step = 1, sep = ""),
                   pickerInput(
                     "regions", label = h5("Regions"),
                     choices = listRegions,
                     selected = listRegions,
                     multiple = TRUE,
                     options = list(
                       'actions-box' = TRUE,
                       'select-all-text' = "All",
                       'deselect-all-text' = "None",
                       'none-selected-text' = "None",
                       'selected-text-format' = "count > 2",
                       'count-selected-text' = "Selected {0} out of {1} Regions"
                     )
                   )
                 )  
               ),
               accordion(
                 id = "plots", width = 9,
                 accordionItem(
                   title = span("Analysing Visitor Arrivals Across Time", style = "font-size: 14px; font-weight: bold"), status = "primary", collapsed = FALSE,
                   textOutput("overallHeader"),
                   plotlyOutput("overallTimeSeries"),
                   textOutput("validation")
                 )
               )
             )
      ),
      tabItem("tab_explore_markets",
              fluidRow(
                accordion(
                  id = "parametersCountry", width = 3,
                  accordionItem(
                    title = span("Filters", style = "font-size: 14px; font-weight: bold"), status = "primary", collapsed = FALSE,
                    sliderInput("sliderYear3", label = h5("Year Range"), min = minYear, max = maxYear, value = c(2012, 2022), step = 1, sep = ""),
                    pickerInput(
                      "country", label = h5("Country"),
                      choices = listCountry,
                      selected = listCountry[1],
                      multiple = FALSE
                    )
                  )
                ),
                accordion(
                  id = "plotsCountry", width = 9,
                  accordionItem(
                    title = span("Analysing Visitor Arrivals by Country", style = "font-size: 14px; font-weight: bold"), status = "primary", collapsed = FALSE,
                    textOutput("cycleHeader"),
                    plotlyOutput("countryCyclePlot", width = "100%", height = "280px"),
                    textOutput("heatmapHeader"),
                    plotlyOutput("countryHeatmap", width = "100%", height = "280px")
                  )
                )
              )
      ),
      tabItem("tab_explore_demo",
              fluidRow(
                accordion(
                  id = "plotsGender", width = 6,
                  accordionItem(
                    title = span("Analysing Visitor Arrivals by Gender (2022)", style = "font-size: 14px; font-weight: bold"), status = "primary", collapsed = FALSE,
                    textOutput("chartHeader1"),
                    plotlyOutput("genderTimeSeries", width = "100%", height = "185px"),
                    textOutput("chartHeader3"),
                    plotlyOutput("genderHeatmap", width = "100%", height = "185px"),
                    textOutput("chartHeader2"),
                    plotOutput("genderHorizon", width = "100%", height = "185px")
                  )
                ),
                accordion(
                  id = "plotsAge", width = 6,
                  accordionItem(
                    title = span("Analysing Visitor Arrivals by Age Group (2022)", style = "font-size: 14px; font-weight: bold"), status = "primary", collapsed = FALSE,
                    textOutput("chartHeader4"),
                    plotlyOutput("ageTimeSeries", width = "100%", height = "185px"),
                    textOutput("chartHeader6"),
                    plotlyOutput("ageHeatmap", width = "100%", height = "185px"),
                    textOutput("chartHeader5"),
                    plotOutput("ageHorizon", width = "100%", height = "185px"),
                  )
                )
              )
      ),
      tabItem("tab_explore_los",
              fluidRow(
                accordion(
                  id = "plotLOS", width = 12,
                  accordionItem(
                    title = span("Analysing Visitor Arrivals by Length of Stay (2022)", style = "font-size: 14px; font-weight: bold"), status = "primary", collapsed = FALSE,
                    column(
                      width = 6,
                      textOutput("losHeader1"),
                      plotlyOutput("losTimeSeries", width = "100%", height = "280px"),
                      textOutput("losHeader2"),
                      plotOutput("losHorizon", width = "100%", height = "280px")
                    ),
                    column(
                      width = 6,
                      textOutput("losHeader3"),
                      plotlyOutput("losHeatmap", width = "100%", height = "280px"),
                      textOutput("losHeader4"),
                      plotlyOutput("losArea", width = "100%", height = "280px")
                    )
                  )
                )
              )
      ),
      tabItem("tab_forecast",
              fluidRow(
                accordion(
                  id = "parametersForecast", width = 3,
                  accordionItem(
                    title = span("Filters", style = "font-size: 14px; font-weight: bold"), status = "primary", collapsed = FALSE,
                    pickerInput(
                      "regions2", label = h5("Regions"),
                      choices = listRegions,
                      selected = listRegions,
                      multiple = TRUE,
                      options = list(
                        'actions-box' = TRUE,
                        'select-all-text' = "All",
                        'deselect-all-text' = "None",
                        'none-selected-text' = "None",
                        'selected-text-format' = "count > 2",
                        'count-selected-text' = "Selected {0} out of {1} Regions"
                      )
                    ),
                    sliderInput("sliderYear2", label = h5("Year Range"), min = 2012, max = 2022, value = c(2018, 2022), step = 1, sep = ""),
                    sliderInput("sliderForecast", label = h5("Forecast Range"), min = 1, max = 10, value = 3, step = 1, sep = ""),
                    sliderInput("trainTestSplit", label = h5("Train/Test Split Ratio"), min = 0.5, max = 0.9, value = 0.8, step = 0.05),
                    actionButton("performForecasting", label = h5("FORECAST"), width = "100%", style = "background-color: LightSteelBlue;")
                  ),
                  accordionItem(
                    title = span("Model Parameters", style = "font-size: 14px; font-weight: bold"), status = "primary", collapsed = FALSE,
                    selectInput(
                      "model", label = h5("Model"),
                      choices = list("ARIMA" = "arm",
                                     "Prophet" = "prt",
                                     "glmNet" = "glm",
                                     "Random Forest" = "rfm",
                                     "SVM" = "svm",
                                     "XGBoost" = "xgb",
                                     "ARIMA Boost" = "arm_b",
                                     "Prophet Boost" = "prt_b"),
                      selected = "arm"
                    ),
                    conditionalPanel(
                      condition = "input.model == 'arm'",
                      sliderInput("arm_1", label = h5("Order of Non-Seasonal Auto-Regressive Terms"), min = 0, max = 5, value = 3, step = 1),
                      sliderInput("arm_2", label = h5("Order of Non-Seasonal Auto-Regressive Differencing"), min = 0, max = 2, value = 1, step = 0.5),
                      sliderInput("arm_3", label = h5("Order of Non-Seasonal Moving Average Terms"), min = 0, max = 5, value = 3, step = 0.5)
                    ),
                    conditionalPanel(
                      condition = "input.model == 'prt'",
                      sliderInput("prt_1", label = h5("No. Of Potential Changepoints"), min = 1, max = 50, value = 25, step = 1),
                      sliderInput("prt_2", label = h5("Range of Changepoints"), min = 0.1, max = 1, value = 0.5, step = 0.1)
                    ),
                    conditionalPanel(
                      condition = "input.model == 'glm'",
                      sliderInput("glm_1", label = h5("Penalty; Total No. Of Regularization"),min = 0.01, max = 0.1, value = 0.01, step = 0.01),
                      sliderInput("glm_2", label = h5("Mixture: Proportion of L1 Regularization"),min = 0.1, max = 1, value = 0.5, step = 0.1)
                    ),
                    conditionalPanel(
                      condition = "input.model == 'rfm'",
                      sliderInput("rfm_1", label = h5("Min No. Of Data Points"), min = 1, max = 10, value = 1, step = 1),
                      sliderInput("rfm_2", label = h5("No. Of Trees"), min = 0, max = 500, value = 300, step = 50)
                    ),
                    conditionalPanel(
                      condition = "input.model == 'svm'",
                      sliderInput("svm_1", label = h5("Cost Of Predicting Sample"), min = 0.1, max = 1, value = 1, step = 0.1),
                      sliderInput("svm_2", label = h5("Insensitivity Margin"), min = 0.1, max = 1, value = 0.1, step = 0.1)
                    ),
                    conditionalPanel(
                      condition = "input.model == 'xgb'",
                      sliderInput("xgb_1", label = h5("Min No. Of Data Points"), min = 1, max = 10, value = 1, step = 1),
                      sliderInput("xgb_2", label = h5("No. Of Trees"), min = 0, max = 500, value = 300, step = 50),
                      sliderInput("xgb_3", label = h5("Max Depth Of Tree"), min = 5, max = 30, value = 15, step = 1)
                    ),
                    conditionalPanel(
                      condition = "input.model == 'arm_b'",
                      sliderInput("arm_b_1", label = h5("Order of Non-Seasonal Auto-Regressive Terms"), min = 0, max = 5, value = 3, step = 1),
                      sliderInput("arm_b_2", label = h5("Order of Non-Seasonal Auto-Regressive Differencing"), min = 0, max = 2, value = 1, step = 0.5),
                      sliderInput("arm_b_3", label = h5("Order of Non-Seasonal Moving Average Terms"), min = 0, max = 5, value = 3, step = 0.5),
                      sliderInput("arm_b_4", label = h5("Min No. Of Data Points"), min = 1, max = 10, value = 1, step = 1),
                      sliderInput("arm_b_5", label = h5("No. Of Trees"), min = 0, max = 500, value = 300, step = 50),
                      sliderInput("arm_b_6", label = h5("Max Depth Of Tree"), min = 5, max = 30, value = 15, step = 1)
                    ),
                    conditionalPanel(
                      condition = "input.model == 'prt_b'",
                      sliderInput("prt_b_1", label = h5("No. Of Potential Changepoints"), min = 1, max = 50, value = 25, step = 1),
                      sliderInput("prt_b_2", label = h5("Range of Changepoints"),min = 0.1, max = 1, value = 0.5, step = 0.1),
                      sliderInput("prt_b_3", label = h5("Min No. Of Data Points"), min = 1, max = 10, value = 1, step = 1),
                      sliderInput("prt_b_4", label = h5("No. Of Trees"), min = 0, max = 500, value = 300, step = 50),
                      sliderInput("prt_b_5", label = h5("Max Depth Of Tree"), min = 5, max = 30, value = 15, step = 1)
                    )
                  )
                ),
                accordion(
                  id = "plotsForecast", width = 9,
                  accordionItem(
                    title = span("Time Series Forecasting of Visitors Arrivals", style = "font-size: 14px; font-weight: bold"), status = "primary", collapsed = FALSE,
                    tabsetPanel(
                      # tabPanel("Dummy", id = "dummyPanel", class = "border-bottom-0"),
                      tabPanel(
                        "Forecasts", id = "forecastsBase",
                        plotlyOutput("forecastBasePlots", width = "100%",height = "440px"),
                        class = "border-bottom-0"
                      ),
                      tabPanel(
                        "Refitted Forecasts", id = "forecastsRefit",
                        plotlyOutput("forecastRefitPlots", width = "100%", height = "440px"),
                        class = "border-bottom-0"
                      ),
                      tabPanel(
                        "Residuals", id = "forecastsResiduals",
                        plotlyOutput("forecastResidualPlots", width = "100%", height = "440px"),
                        class = "border-bottom-0"
                      )
                    )
                  ),
                  accordionItem(
                    title = span("Accuracy Table of Forecasts", style = "font-size: 14px; font-weight: bold"), status = "primary", collapsed = FALSE,
                    reactableOutput("accuracyTableOutput", width = "100%", height = "360px")
                  )
                )
              )
      )
    )
  )
)


# ---------------------------------------------------------------------------------------
# Server Portion of R Shiny App
# ---------------------------------------------------------------------------------------

# Define server logic for the application
server <- function(input, output, session) {
  
  
  # ---------------------------------------------------------------------------------------
  # Summary Page
  # ---------------------------------------------------------------------------------------
  
  # Dynamically update the value for number of visitors based on the year range that user selects
  output$vboxVisitors <- renderValueBox({
    temp <- vaByMarket[vaByMarket$Year >= input$sliderYear[1] & vaByMarket$Year <= input$sliderYear[2], ]
    temp <- temp %>% filter(temp$Region %in% input$regions)
    totalVisitors = round(sum(temp$Visitors, na.rm = TRUE)/1000000, digits = 0)
    valueBox(value = totalVisitors, span("Total Visitor Arrivals (M)", style = "font-size: 13px;"), icon = icon("person-walking-luggage"), color = "yellow")
  })
  
  # Dynamically update the value for number of countries that visited Singapore based on the year range that user selects
  output$vboxCountry <- renderValueBox({
    temp <- vaByMarket[vaByMarket$Year >= input$sliderYear[1] & vaByMarket$Year <= input$sliderYear[2], ]
    numCountries <- nrow(temp %>% filter(temp$Region %in% input$regions) %>% filter(!is.na(Visitors)) %>% count(Country))
    valueBox(value = numCountries, span("Total Countries visiting SG", style = "font-size: 13px;"), icon = icon("earth-americas"), color = "yellow")
  })
  
  output$vboxAge <- renderValueBox({
    valueBox( "25 to 34", span("Age Range of Most Visitors (2022)", style = "font-size: 13px;"), icon = icon("people-group"), color = "yellow")
  })
  
  output$vboxStay <- renderValueBox({
    valueBox("Under 1 Day", span("Avg. Visitors Stayed (2022)", style = "font-size: 13px;"), icon = icon("calendar-days"), color = "yellow")
  })

  output$overallHeader <- renderText("Time Series Analysis")
  
  output$overallTimeSeries <- renderPlotly({
    temp <- vaByMarket %>% filter(Region %in% input$regions)
    temp <- temp[temp$Year >= input$sliderYear[1] & temp$Year <= input$sliderYear[2], ] %>% group_by(Period) %>% summarise(Visitors = sum(Visitors, na.rm = TRUE))
    temp %>% plot_time_series(Period, Visitors, .interactive = TRUE, .title = NULL)
  })
  
  output$validation <- renderText(
    if(length(input$regions) < 1) {
      "Please select a region to proceed!"
    } else {
      # do nothing
    }
  )
  
  
  # ---------------------------------------------------------------------------------------
  # Explore Page - by Markets
  # ---------------------------------------------------------------------------------------

  output$cycleHeader <- renderText("Cycle Plot Analysis")
  output$heatmapHeader <- renderText("Calendar Heatmap Analysis")
  
  output$countryCyclePlot <- renderPlotly({
    temp <- vaByMarket %>% filter(Country %in% input$country)
    temp <- temp[temp$Year >= input$sliderYear3[1] & temp$Year <= input$sliderYear3[2], ] %>% group_by(Period) %>% summarise(Visitors = sum(Visitors, na.rm = TRUE))
    temp[is.na(temp)] = 0
    countryAvg <- temp %>% mutate(Month = as.character(Period, format = '%b')) %>% group_by(Month) %>% summarise(avgValue = mean(Visitors)) %>% mutate(avgValue = round(avgValue/1000, digits = 0))
    cyclePlot <- temp %>% group_by(Period) %>% summarise(Visitors = sum(Visitors, na.rm = TRUE)) %>% mutate(Month = as.character(Period, format = '%b')) %>% mutate(Visitors = round(Visitors/1000, digits = 0))
    figCyclePlot <- ggplot() + geom_line(data = cyclePlot, aes(x = Period, y = Visitors, group = Month)) + geom_hline(data = countryAvg, aes(yintercept = avgValue), colour = "red", size = 0.3) + labs(x = "Year/Month", y = "No. Of Visitors (k)", title = NULL) + facet_grid(~ factor(Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))) + theme_bw() + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text = element_text(size = 7), axis.title = element_text(size = 8))
    figCyclePlot
  })
  
  output$countryHeatmap <- renderPlotly({
    scaleFUN <- function(x) sprintf("%.0f", x)
    temp <- vaByMarket %>% mutate(Month = month(Period)) %>% filter(Country %in% input$country)
    temp <- temp[temp$Year >= input$sliderYear3[1] & temp$Year <= input$sliderYear3[2], ]
    temp[is.na(temp)] = 0
    figHeatmapPlot <- ggplot(data = temp) + geom_tile(aes(x = Month, y = Year, fill = Visitors)) + labs(title = NULL) + theme_bw() + theme(legend.position = "none", axis.text = element_text(size = 7), axis.title = element_text(size = 8)) + scale_x_continuous(breaks = seq_along(month.name), labels = month.abb) + scale_y_continuous(labels = scaleFUN) + scale_fill_gradient(low = "powderblue", high = "steelblue")
    figHeatmapPlot
  })
  
  
  # ---------------------------------------------------------------------------------------
  # Explore Page - by Demographics
  # ---------------------------------------------------------------------------------------

  output$chartHeader1 <- renderText("Time Series Analysis")
  output$chartHeader2 <- renderText("Horizon Plot Analysis")
  output$chartHeader3 <- renderText("Heatmap Analysis")
  
  output$genderTimeSeries <- renderPlotly({
    vaByDemoGender %>%
      ggplot(aes(x = Period, y = Visitors, color = Gender)) +
      geom_line() +
      labs(x = "Month", y = "No. Of Visitors", title = NULL) +
      theme_bw() +
      theme(axis.text = element_text(size = 7), axis.title = element_text(size = 8), legend.position = "none")
  })
  
  output$genderHeatmap <- renderPlotly({
    vaByDemoGender %>%
      ggplot() +
      geom_tile(aes(x = Month, y = Gender, fill = Visitors)) +
      theme_bw() +
      labs(x = "Month", y = NULL, title = NULL) +
      theme(axis.text = element_text(size = 7), axis.title = element_text(size = 8), legend.position = "none") +
      scale_x_continuous(breaks = seq_along(month.name), labels = month.abb) +
      scale_fill_gradient(low = "powderblue", high = "steelblue")
  })
    
  output$genderHorizon <- renderPlot({
    vaByDemoGender %>%
      group_by(Period, Gender) %>%
      summarise(Visitors = sum(Visitors)) %>%
      ggplot() +
      geom_horizon(aes(x = Period, y =  Visitors), origin = "midpoint", horizonscale = 6) +
      facet_grid(Gender~.) +
      scale_fill_hcl(palette = "RdBu") +
      scale_x_date(expand = c(0,0), date_breaks = "1 month", date_labels = " %b %y") +
      theme_minimal() +
      theme(panel.spacing.y = unit(0, "lines"),
            strip.text.y = element_text(size = 10, angle = 0, hjust = 0),
            legend.position = 'none',
            axis.text.y = element_blank(),
            axis.text.x = element_text(size = 7),
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.ticks.y = element_blank(),
            panel.border = element_blank())
  })
  
  output$chartHeader4 <- renderText("Time Series Analysis")
  output$chartHeader5 <- renderText("Horizon Plot Analysis")
  output$chartHeader6 <- renderText("Heatmap Analysis")
  
  output$ageTimeSeries <- renderPlotly({
    vaByDemoAge %>%
      ggplot(aes(x = Period, y = Visitors, color = AgeGroup)) +
      geom_line() +
      labs(x = "Month", y = "No. Of Visitors", title = NULL) +
      theme_bw() +
      theme(axis.text = element_text(size = 7), axis.title = element_text(size = 8), legend.position = "none")
  })
  
  output$ageHeatmap <- renderPlotly({
    vaByDemoAge %>%
      ggplot() +
      geom_tile(aes(x = Month, y = AgeGroup, fill = Visitors)) +
      theme_bw() +
      labs(x = "Month", y = NULL, title = NULL) +
      theme(axis.text = element_text(size = 7), axis.title = element_text(size = 8), legend.position = "none") +
      scale_x_continuous(breaks = seq_along(month.name), labels = month.abb) +
      scale_fill_gradient(low = "powderblue", high = "steelblue")
  })
  
  output$ageHorizon <- renderPlot({
    vaByDemoAge %>%
      group_by(Period, AgeGroup) %>%
      summarise(Visitors = sum(Visitors)) %>%
      ggplot() +
      geom_horizon(aes(x = Period, y =  Visitors), origin = "midpoint", horizonscale = 6) +
      facet_grid(AgeGroup~.) +
      scale_fill_hcl(palette = "RdBu") +
      scale_x_date(expand = c(0,0), date_breaks = "1 month", date_labels = " %b %y") +
      theme_minimal() +
      theme(panel.spacing.y = unit(0, "lines"),
            strip.text.y = element_text(size = 10, angle = 0, hjust = 0),
            legend.position = 'none',
            axis.text.y = element_blank(),
            axis.text.x = element_text(size = 7),
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.ticks.y = element_blank(),
            panel.border = element_blank())
  })
  
  
  # ---------------------------------------------------------------------------------------
  # Explore Page - by Length of Stay
  # ---------------------------------------------------------------------------------------

  output$losHeader1 <- renderText("Time Series Analysis")
  output$losHeader2 <- renderText("Horizon Plot Analysis")
  
  output$losTimeSeries <- renderPlotly({
    vaByStay %>%
      ggplot(aes(x = Period, y = Visitors, color = Duration)) +
      geom_line() +
      labs(x = "Month", y = "No. Of Visitors", title = NULL) +
      theme_bw() +
      theme(axis.text = element_text(size = 7), axis.title = element_text(size = 8), legend.position = "none")
  })
  
  output$losHorizon <- renderPlot({
    vaByStay %>%
      group_by(Period, Duration) %>%
      summarise(Visitors = sum(Visitors)) %>%
      ggplot() +
      geom_horizon(aes(x = Period, y =  Visitors), origin = "midpoint", horizonscale = 6) +
      facet_grid(Duration~.) +
      scale_fill_hcl(palette = "RdBu") +
      scale_x_date(expand = c(0,0), date_breaks = "1 month", date_labels = " %b %y") +
      theme_minimal() +
      theme(panel.spacing.y = unit(0, "lines"), 
            strip.text.y = element_text(size = 10, angle = 0, hjust = 0),
            legend.position = 'none',
            axis.text.y = element_blank(),
            axis.text.x = element_text(size = 7),
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.ticks.y = element_blank(),
            panel.border = element_blank())
  })
  
  output$losHeader3 <- renderText("Heatmap Analysis")
  output$losHeader4 <- renderText("Area Chart Analysis")
  
  output$losHeatmap <- renderPlotly({
    vaByStay %>%
      ggplot() +
      geom_tile(aes(x = Month, y = Duration, fill = Visitors)) +
      labs(x = "Month", y = NULL, title = NULL) +
      theme_bw() +
      theme(axis.text = element_text(size = 7), axis.title = element_text(size = 8), legend.position = "none") +
      scale_x_continuous(breaks = seq_along(month.name), labels = month.abb) +
      scale_fill_gradient(low = "powderblue", high = "steelblue")
  })
  
  output$losArea <- renderPlotly({
    vaByStay %>% 
      mutate(Visitors = round(Visitors/1000, digits = 0)) %>%
      ggplot(aes(x = Period, y = Visitors, fill = Duration)) +
      geom_area() +
      scale_fill_viridis(discrete = TRUE) +
      labs(x = "Month", y = "No. Of Visitors (K)", title = NULL) +
      theme(legend.position="none") +
      theme_bw() +
      theme(axis.text = element_text(size = 7), axis.title = element_text(size = 8), legend.position="none")
  })
  
  
  # ---------------------------------------------------------------------------------------
  # Forecast Page
  # ---------------------------------------------------------------------------------------
  
  observeEvent(input$performForecasting, {
    
    show_modal_spinner(spin = "trinity-rings", color = "lightblue", text = h5("Please Wait ..."))
    
    temp <- vaByMarket[vaByMarket$Year >= input$sliderYear2[1] & vaByMarket$Year <= input$sliderYear2[2], ]
    
    # Prepare data for forecasting (shown in thousands)
    vaByMarketTable <- temp %>% 
      filter(Region %in% input$regions2) %>%
      group_by(Period) %>% 
      summarise(visitors = round(sum(Visitors, na.rm = TRUE))/1000) %>%
      set_names(c("date", "value"))

    # Split data into training and testing
    yearDiff = input$sliderYear2[2] - input$sliderYear2[1]
    
    splits <- vaByMarketTable %>%
      time_series_split(assess = sprintf("%s years", round(yearDiff * (1 - input$trainTestSplit))), cumulative = TRUE)
    
    # Pre-Processing Recipes
    recipeSpecs <- recipe(value ~ date, training(splits)) %>%
      step_timeseries_signature(date) %>%
      step_dummy(all_nominal())
    
    recipeSpecsParsnip <- recipeSpecs %>%
      update_role(date, new_role = "ID")
    
    # ARIMA model
    modelFitArima <- arima_reg(non_seasonal_ar = input$arm_1, non_seasonal_differences = input$arm_2, non_seasonal_ma = input$arm_3) %>%
      set_engine("auto_arima") %>%
      fit(value ~ date, training(splits))
    
    # Prophet model
    modelFitProphet <- prophet_reg(changepoint_num = input$prt_1, changepoint_range = input$prt_2) %>%
      set_engine("prophet", yearly.seasonality = TRUE) %>%
      fit(value ~ date, training(splits))

    # glmNET model & workflow
    modelSpec_glmnet <- linear_reg(penalty = input$glm_1, mixture = input$glm_2) %>%
      set_engine("glmnet")
     
    workflowFit_glmnet <- workflow() %>%
      add_model(modelSpec_glmnet) %>%
      add_recipe(recipeSpecs %>%
                   step_rm(date)) %>%
      fit(training(splits))

    # Random Forest model
    modelSpec_rf <- rand_forest(min_n = input$rfm_1, trees = input$rfm_2, mode = "regression") %>%
      set_engine("randomForest")

    workflowFit_rf <- workflow() %>%
      add_model(modelSpec_rf) %>%
      add_recipe(recipeSpecs %>% step_rm(date)) %>%
      fit(training(splits))

    # XGBoost model
    workflowFit_xgboost <- workflow() %>%
      add_model(boost_tree(min_n = input$xgb_1, trees = input$xgb_2, tree_depth = input$xgb_3, mode = "regression") %>%
                set_engine("xgboost")) %>%
      add_recipe(recipeSpecsParsnip) %>%
      fit(training(splits))

    # SVM model
    workflowFit_svm <- workflow() %>%
      add_model(svm_rbf(cost = input$svm_1, margin = input$svm_2, mode = "regression") %>% 
                set_engine("kernlab")) %>%
      add_recipe(recipeSpecsParsnip) %>%
      fit(training(splits))

    # ARIMA Boost model
    workflowFit_arimaBoost <- workflow() %>%
      add_model(arima_boost(non_seasonal_ar = input$arm_b_1, non_seasonal_differences = input$arm_b_2, non_seasonal_ma = input$arm_b_3, min_n = input$arm_b_4, trees = input$arm_b_5, tree_depth = input$arm_b_6) %>%
                set_engine("auto_arima_xgboost")) %>%
      add_recipe(recipeSpecs) %>%
      fit(training(splits))

    # Prophet Boost model
    modelSpec_prophetBoost <- prophet_boost(changepoint_num = input$prt_b_1, changepoint_range = input$prt_b_2, min_n = input$prt_b_3, trees = input$prt_b_4, tree_depth = input$prt_b_5) %>%
      set_engine("prophet_xgboost", yearly.seasonality = TRUE)

    workflowFit_prophetBoost <- workflow() %>%
      add_model(modelSpec_prophetBoost) %>%
      add_recipe(recipeSpecs) %>%
      fit(training(splits))
    
    # Model Time Table
    modelTable <- modeltime_table(modelFitArima, modelFitProphet, workflowFit_glmnet, workflowFit_rf, workflowFit_svm, workflowFit_xgboost, workflowFit_arimaBoost, workflowFit_prophetBoost) %>% 
      update_model_description(1, "ARIMA") %>%
      update_model_description(2, "Prophet") %>%
      update_model_description(3, "glmNet") %>%
      update_model_description(4, "Random Forest") %>%
      update_model_description(5, "SVM") %>%
      update_model_description(6, "XGBoost") %>%
      update_model_description(7, "ARIMA Boost") %>%
      update_model_description(8, "Prophet Boost")
    
    # Calibration
    calibrationTable <- modelTable %>%
      modeltime_calibrate(testing(splits))
    
    # Forecasting with Training Set
    forecastTable <- calibrationTable %>%
      modeltime_forecast(actual_data = vaByMarketTable)
    
    forecastPlots <- forecastTable %>%
      filter(.model_desc != "ACTUAL") %>%
      plot_modeltime_forecast(
        .interactive = FALSE,
        .facet_vars = .model_desc,
        .facet_ncol = 4,
        .facet_scales = "fixed"
      ) +
      geom_line(
        data = forecastTable %>%
          filter(.model_desc == "ACTUAL") %>%
          select(.index, .value), size = 0.3
      ) +
      labs(title = "", y = "No. of Visitors", x = "Year") +
      theme_bw() +
      theme(legend.position = "none")

    # Forecasting after Refitting
    refitTable <- calibrationTable %>%
      modeltime_refit(vaByMarketTable) %>%
      modeltime_forecast(h = sprintf("%s years", input$sliderForecast), actual_data = vaByMarketTable) %>%
      mutate(.model_desc = str_replace_all(.model_desc, "UPDATE: ARIMA\\(1,1,0\\)\\(2,0,0\\)\\[12\\]", "ARIMA")) %>%
      mutate(.model_desc = str_replace_all(.model_desc, "W/ XGBOOST ERRORS", "Boost"))
    
    refitPlots <- refitTable %>%
      filter(.model_desc != "ACTUAL") %>%
      plot_modeltime_forecast(
        .interactive = FALSE,
        .facet_vars = .model_desc,
        .facet_ncol = 4,
        .facet_scales = "fixed"
      ) +
      geom_line(
        data = refitTable %>%
          filter(.model_desc == "ACTUAL") %>%
          select(.index, .value), size = 0.3
      ) +
      labs(title = "", y = "No. of Visitors", x = "Year") +
      theme_bw() +
      theme(legend.position = "none")
    
    # Residuals
    residualsTable <- calibrationTable %>%
      modeltime_residuals()
    
    residualsPlots <- residualsTable %>%
      plot_modeltime_residuals(
        .interactive = FALSE,
        .type = "timeplot",
        .facet_vars = .model_desc,
        .facet_ncol = 4,
        .facet_scales = "fixed"
      ) +
      labs(title = "", y = "Residuals", x = "Year") +
      theme_bw() +
      theme(legend.position = "none")
    
    
    # Accuracy Table
    accuracyTable <- calibrationTable %>%
      modeltime_accuracy() %>%
      select(.model_desc, mae, mape, mase, smape, rmse, rsq) %>%
      reactable(
        defaultColDef = colDef(align = "center"),
        columns = list(
          .model_desc = colDef(name = "Model"),
          mae = colDef(name = "Mean Absolute Error", format = colFormat(digits = 2)),
          mape = colDef(name = "Mean Absolute Percentage Error", format = colFormat(digits = 2)),
          mase = colDef(name = "Mean Absolute Scaled Error", format = colFormat(digits = 2)),
          smape = colDef(name = "Symmetric Mean Absolute Percentage Error", format = colFormat(digits = 2)),
          rmse = colDef(name = "Root Mean Squared Error", format = colFormat(digits = 2)),
          rsq = colDef(name = "R-Squared", format = colFormat(digits = 2))
        ),
        highlight = TRUE,
        striped = TRUE,
        bordered = TRUE,
        searchable = FALSE,
        showPageSizeOptions = FALSE
      )
    
    # Output Plots
    output$forecastBasePlots <- renderPlotly({
      forecastPlots
    })
    
    output$forecastRefitPlots <- renderPlotly({
      refitPlots
    })
    
    output$forecastResidualPlots <- renderPlotly({
      residualsPlots
    })
    
    # Output Accuracy Table
    output$accuracyTableOutput <- renderReactable({
      accuracyTable
    })
    
    remove_modal_spinner()
    
  })
  
}


# ---------------------------------------------------------------------------------------
# To Run the R Shiny App
# ---------------------------------------------------------------------------------------

# Run the application 
shinyApp(ui = ui, server = server)
