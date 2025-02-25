library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(plotly)


# Load your data
data_combined <- read_csv("data/data_combined.csv")

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Membership Insights"),
  dashboardSidebar(
    tags$style(HTML("
      .main-sidebar {
        background-color: #2c3e50 !important;
        color: white;
      }
      .sidebar-menu li a {
        color: white !important;
      }
      .sidebar-menu li a:hover {
        background-color: #18bc9c !important;
        color: white !important;
      }
    ")),
    sidebarMenu(
      menuItem("Introduction", tabName = "introduction", icon = icon("info-circle")),
      menuItem("Membership Overview", tabName = "membership_overview", icon = icon("users")),
      menuItem("Pricing Details", tabName = "pricing_details", icon = icon("dollar-sign")),
      menuItem("Magazine Preferences", tabName = "magazine_preferences", icon = icon("book")),
      menuItem("Information Sources", tabName = "information_sources", icon = icon("rss")),
      menuItem("Member Activities", tabName = "member_activities", icon = icon("tasks")),
      menuItem("NPS and Satisfaction", tabName = "nps_satisfaction", icon = icon("smile")),
      menuItem("Potential Benefits Analysis", tabName = "potential_benefits", icon = icon("chart-bar"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        /* Header background color */
        .main-header .navbar {
          background-color: #18bc9c;
        }

        /* Body background color */
        .content-wrapper {
          background-color: #ecf0f1;
        }

        /* Box header background color */
        .box-header {
          background-color: #18bc9c;
          color: white;
        }

        /* Box title font color */
        .box-title {
          font-size: 18px;
          color: white;
        }

        /* ValueBox color */
        .small-box.bg-blue {
          background-color: #3498db !important;
        }
        .small-box.bg-green {
          background-color: #2ecc71 !important;
        }
        .small-box.bg-yellow {
          background-color: #f1c40f !important;
        }
      "))
    ),
    tabItems(
      # Introduction Tab
      tabItem(tabName = "introduction",
              fluidRow(
                box(
                  title = "Welcome",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  h4("Welcome to the Membership Insights Dashboard"),
                  p("Explore survey results related to membership benefits, pricing, magazine preferences, satisfaction, and more.
              Use the tabs above to navigate through different sections.")
                )
              )
      ),
      # Membership Overview Tab
      tabItem(tabName = "membership_overview",
              h4("Membership Overview"),
              fluidRow(
                valueBoxOutput("total_respondents", width = 4),
                valueBoxOutput("average_satisfaction", width = 4),
                valueBoxOutput("promoter_percentage", width = 4)
              ),
              fluidRow(
                box(
                  title = "Membership Type Composition",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("membership_pie_chart", height = "400px")
                )
              ),
              fluidRow(
                tabBox(
                  title = "Membership Benefits",
                  id = "membership_benefits_tab",
                  width = 12,
                  tabPanel("Benefits Enjoyed", plotlyOutput("top_benefits_plot", height = "400px")),
                  tabPanel("Most Important Benefit", plotlyOutput("important_benefit_plot", height = "400px"))
                )
              )
      ),
      # Pricing Details Tab
      # Pricing Details Tab
      tabItem(tabName = "pricing_details",
              h4("Membership Prices (Effective from 1 July 2024)"),
              fluidRow(
                box(
                  title = "Pricing Information",
                  status = "success",
                  solidHeader = TRUE,
                  width = 12,
                  verbatimTextOutput("membership_pricing")
                )
              ),
              fluidRow(
                box(
                  title = "Willingness to Pay by Membership Type",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,  # Adjust to full width or as needed
                  plotlyOutput("willingness_to_pay_by_type_plot", height = "400px")  # New output for the updated plot
                )
              ),
              fluidRow(
                box(
                  title = "Willingness to Pay",
                  status = "info",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("willingness_to_pay_plot", height = "400px")
                ),
                box(
                  title = "Pricing vs. Willingness to Pay Comparison",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("comparison_plot", height = "400px")
                )
              )
      ),

      # Magazine Preferences Tab
      tabItem(tabName = "magazine_preferences",
              h4("Magazine Preferences"),
              fluidRow(
                box(
                  title = "Select Preference Type",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  radioButtons("magazine_type", "Choose magazine preference type:",
                               choices = c("Format Preferences" = "format", "Frequency Preferences" = "frequency"),
                               selected = "format")
                )
              ),
              fluidRow(
                box(
                  title = "Magazine Preference Plot",
                  status = "success",
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("magazine_preferences_plot", height = "400px")
                )
              )
      ),
      # Information Sources Tab
      tabItem(tabName = "information_sources",
              h4("Information Sources Analysis"),
              fluidRow(
                box(
                  title = "Select Information Sources to Display",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  checkboxGroupInput("info_sources_filter", "Select Sources:",
                                     choices = c("Social Media", "E-newsletters", "Newspapers", "Online News Websites or Apps",
                                                 "Magazines", "Blogs", "Television", "On Demand TV",
                                                 "Streaming Services", "Radio", "Podcasts", "Other"),
                                     selected = c("Social Media", "E-newsletters", "Newspapers", "Online News Websites or Apps"))
                )
              ),
              fluidRow(
                box(
                  title = "Top Information Sources Among Members",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("info_sources_plot", height = "400px")
                )
              )
      ),
      # Member Activities Tab
      tabItem(tabName = "member_activities",
              h4("Member Activity Analysis"),
              fluidRow(
                box(
                  title = "Activity Filters",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  checkboxGroupInput("activity_filter", "Select Activities to Display:",
                                     choices = c("Visit HNZPT Place" = "visit_heritage",
                                                 "Visit Other Heritage Org" = "visit_heritage_other_org",
                                                 "Visit Heritage Overseas" = "visit_heritage_overseas",
                                                 "Buy from Shop" = "buy_from_shop",
                                                 "Buy from Cafe" = "buy_from_cafe",
                                                 "Attend Event" = "attend_event",
                                                 "Attend Cultural Event" = "attend_cultural_event",
                                                 "Watch Video" = "watch_video"),
                                     selected = c("visit_heritage", "attend_event"))
                )
              ),
              fluidRow(
                box(
                  title = "Overall Participation Rates",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("overall_participation_plot", height = "400px")
                )
              ),
              fluidRow(
                box(
                  title = "Participation Rates by Membership Type",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("participation_by_type_plot", height = "400px")
                )
              )
      ),
      # NPS and Satisfaction Tab
      tabItem(tabName = "nps_satisfaction",
              h4("Membership Satisfaction and NPS"),
              fluidRow(
                box(
                  title = "Satisfaction Gauge",
                  status = "info",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("satisfaction_indicator", height = "400px")
                ),
                box(
                  title = "NPS Breakdown",
                  status = "info",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("nps_bar_chart", height = "400px")
                )
              ),
              fluidRow(
                box(
                  title = "Filters",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 12,
                  sliderInput("satisfaction_slider", "Satisfaction Score Range:",
                              min = 0, max = 10, value = c(0, 10), step = 1)
                )
              ),
              fluidRow(
                box(
                  title = "Average Satisfaction by Membership Type",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("membership_type_satisfaction", height = "400px")
                )
              )
      ),
      # Potential Benefits Analysis Tab
      tabItem(tabName = "potential_benefits",
              h4("Importance of Future Potential Benefits to Members"),
              fluidRow(
                box(
                  title = "Benefit Filters",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  checkboxGroupInput("benefit_filter", "Select Benefits to Display:",
                                     choices = c("Monthly Membership" = "monthly_membership",
                                                 "Membership Donation" = "membership_donation",
                                                 "Family Content" = "family_content",
                                                 "Member Handbook" = "member_handbook",
                                                 "Volunteering Opportunities" = "volunteering_opportunities",
                                                 "Local Info Events" = "local_info_events"),
                                     selected = c("monthly_membership", "membership_donation", "family_content",
                                                  "member_handbook", "volunteering_opportunities", "local_info_events"))
                )
              ),
              fluidRow(
                box(
                  title = "Potential Benefit Importance",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("benefit_importance_plot", height = "400px")
                )
              )
      )
    )
  )
)


# Server
server <- function(input, output) {
  # Summary Cards
  output$total_respondents <- renderValueBox({
    valueBox(
      format(nrow(data_combined), big.mark = ","),
      "Total Respondents",
      icon = icon("users"),
      color = "blue"
    )
  })

  output$average_satisfaction <- renderValueBox({
    avg_satisfaction <- round(mean(data_combined$Q9, na.rm = TRUE), 1)
    valueBox(
      avg_satisfaction,
      "Average Satisfaction",
      icon = icon("smile"),
      color = "green"
    )
  })

  output$promoter_percentage <- renderValueBox({
    nps_data <- data_combined %>%
      mutate(NPS = case_when(
        Q12 >= 9 ~ "Promoter",
        Q12 >= 7 & Q12 <= 8 ~ "Passive",
        Q12 <= 6 ~ "Detractor"
      ))

    promoter_percentage <- round(sum(nps_data$NPS == "Promoter") / nrow(nps_data) * 100, 1)
    valueBox(
      paste0(promoter_percentage, "%"),
      "Promoters",
      icon = icon("thumbs-up"),
      color = "yellow"
    )
  })

  # Membership Type Composition (Pie Chart using Plotly)
  output$membership_pie_chart <- renderPlotly({
    df_summary <- data_combined %>%
      filter(!is.na(Q7) & Q7 != "not_sure") %>%  # Exclude "Not Sure" category
      count(Q7) %>%
      mutate(Proportion = n / sum(n) * 100)

    plot_ly(df_summary, labels = ~Q7, values = ~Proportion, type = 'pie',
            textinfo = 'label+percent', insidetextorientation = 'radial') %>%
      layout(title = 'Membership Type Composition')
  })

  # Most Important Membership Benefit Plot
  output$important_benefit_plot <- renderPlotly({
    important_benefit <- data_combined %>%
      filter(!is.na(Q2)) %>%
      count(Q2) %>%
      arrange(desc(n))

    # Recode Q2 values for readability
    important_benefit$Q2 <- recode(important_benefit$Q2,
                                   `1` = "Free entry to HNZPT places",
                                   `2` = "Free entry to international heritage places",
                                   `3` = "Protecting heritage",
                                   `4` = "Heritage New Zealand magazine",
                                   `5` = "Monthly e-newsletter",
                                   `6` = "Heritage events",
                                   `7` = "Video information",
                                   `8` = "Discounts on services")

    # Create the Plotly bar chart
    plot_ly(important_benefit,
            x = ~reorder(Q2, -n),
            y = ~n,
            type = 'bar',
            text = ~n,
            textposition = 'auto',
            marker = list(color = '#c5d26c')) %>%
      layout(title = 'Most Important Membership Benefit',
             xaxis = list(title = 'Benefit', tickangle = 45),
             yaxis = list(title = 'Number of Selections'),
             showlegend = FALSE)
  })

  # Membership Benefits Plot
  output$top_benefits_plot <- renderPlotly({
    benefits_usage <- data_combined %>%
      select(Q1_1:Q1_8) %>%
      pivot_longer(cols = everything(), names_to = "Benefit", values_to = "Used") %>%
      filter(!is.na(Used)) %>%
      group_by(Benefit) %>%
      summarise(Count = n()) %>%
      arrange(desc(Count))

    # Recode benefits for better readability
    benefits_usage$Benefit <- recode(benefits_usage$Benefit,
                                     Q1_1 = "Free entry to HNZPT places",
                                     Q1_2 = "Free entry to international heritage places",
                                     Q1_3 = "Protecting heritage",
                                     Q1_4 = "Heritage New Zealand magazine",
                                     Q1_5 = "Monthly e-newsletter",
                                     Q1_6 = "Heritage events",
                                     Q1_7 = "Video information",
                                     Q1_8 = "Discounts on services")

    # Create the Plotly bar chart
    plot_ly(benefits_usage,
            x = ~reorder(Benefit, -Count),
            y = ~Count,
            type = 'bar',
            text = ~Count,
            textposition = 'auto',
            marker = list(color = '#b23c00')) %>%
      layout(title = 'Benefits Enjoyed By Memebers',
             xaxis = list(title = 'Benefit', tickangle = 45),
             yaxis = list(title = 'Number of Members'),
             showlegend = FALSE)
  })


  # Membership Pricing Details
  output$membership_pricing <- renderText({
    pricing_info <- "
    Membership Types and Prices (Effective from 1 July 2024):

    New Zealand residents:
    - Family (two adults at same address, including any school-age children): $79
    - Individual: $69
    - Joint Senior Citizen (retired, 65 years or over, plus one other person at same address): $60
    - Senior Citizen (retired person 65 years or over): $50
    - Student (full time): $50
    - Life: $1050
    - Joint Life (two adults at same address): $1399
    - Organisational: find out more

    Overseas residents (excludes printed magazine):
    - Family (two adults at same address, including any school-age children): NZD $450
    - Individual: NZD $275
    "
    return(pricing_info)
  })

  # Updated server code for the "Willingness to Pay" plot
  output$willingness_to_pay_plot <- renderPlotly({
    # Count the frequency for each price point and calculate the cumulative sum
    df_summary <- data_combined %>%
      count(Q4) %>%
      arrange(Q4) %>%
      mutate(Cumulative_Count = cumsum(n),  # Cumulative count
             Cumulative_Percentage = Cumulative_Count / sum(n) * 100)  # Cumulative percentage

    # Create the plot using Plotly
    plot_ly() %>%
      add_bars(data = df_summary, x = ~Q4, y = ~n, name = 'Frequency',
               text = ~n, textposition = 'outside', marker = list(color = '#A4A0E6')) %>%
      add_lines(data = df_summary, x = ~Q4, y = ~Cumulative_Percentage, name = 'Cumulative Percentage',
                yaxis = 'y2', line = list(color = 'red', width = 2)) %>%
      layout(title = 'Willingness to Pay (Count)',
             xaxis = list(title = 'Price ($)', tickangle = 45),
             yaxis = list(title = 'Number of Respondents'),
             yaxis2 = list(title = 'Cumulative Percentage', overlaying = 'y', side = 'right'),
             legend = list(x = 0.1, y = 1.1),
             hovermode = 'x unified')
  })

  # Server code for the grouped bar plot
  output$comparison_plot <- renderPlotly({
    # Extract the average willingness to pay
    willingness_to_pay <- data_combined %>%
      filter(!is.na(Q7), !is.na(Q4)) %>%
      group_by(Q7) %>%
      summarise(Average_Willingness_to_Pay = mean(Q4, na.rm = TRUE)) %>%
      rename(Category = Q7)

    # Create the pricing data
    pricing_info <- data.frame(
      Category = c("Family", "Individual", "Joint Senior", "Senior", "Student", "Life", "Joint Life"),
      Actual_Price = c(79, 69, 60, 50, 50, 1050, 1399)
    )

    # Merge the two data frames
    comparison_data <- merge(pricing_info, willingness_to_pay, by = "Category", all = TRUE)

    # Reshape the data for the grouped bar plot
    comparison_long <- comparison_data %>%
      pivot_longer(cols = c("Actual_Price", "Average_Willingness_to_Pay"),
                   names_to = "Price_Type", values_to = "Price")

    # Create the grouped bar plot using Plotly
    plot_ly(comparison_long, x = ~Category, y = ~Price, color = ~Price_Type, type = 'bar',
            text = ~paste0(Price_Type, ": $", round(Price, 2)),
            textposition = 'auto') %>%
      layout(title = 'Comparison of Actual Pricing and Willingness to Pay',
             xaxis = list(title = 'Membership Category', tickangle = 45),
             yaxis = list(title = 'Price ($)', zeroline = FALSE),
             barmode = 'group')
  })
  # Updated server code for "Willingness to Pay by Membership Type" plot with plotly
  output$willingness_to_pay_by_type_plot <- renderPlotly({
    # Group data by price (Q4) and membership type (Q7), excluding "Not Sure"
    df_summary <- data_combined %>%
      filter(Q7 != "not_sure") %>%  # Exclude "Not Sure" category
      group_by(Q4, Q7) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      arrange(Q4)

    # Calculate cumulative count for each price point, also excluding "Not Sure"
    df_summary_cumulative <- data_combined %>%
      filter(Q7 != "not_sure") %>%  # Exclude "Not Sure" here as well
      group_by(Q4) %>%
      summarise(CumulativeCount = cumsum(n()), .groups = 'drop')

    # Create the Plotly stacked bar chart with cumulative line
    plot_ly() %>%
      add_bars(data = df_summary,
               x = ~Q4,
               y = ~Count,
               color = ~Q7,
               colors = "Set3",
               name = ~Q7,
               text = ~paste("Membership Type:", Q7, "<br>Count:", Count),
               hoverinfo = "text") %>%
      add_lines(data = df_summary_cumulative,
                x = ~Q4,
                y = ~CumulativeCount,
                name = "Cumulative Count",
                line = list(color = "black"),
                text = ~paste("Cumulative Count:", CumulativeCount),
                hoverinfo = "text") %>%
      layout(title = "Willingness to Pay by Membership Type",
             xaxis = list(title = "Price (Q4)", tickangle = 45),
             yaxis = list(title = "Count of Respondents"),
             yaxis2 = list(title = "Cumulative Count", overlaying = "y", side = "right"),
             barmode = "stack",
             legend = list(title = list(text = "<b>Membership Type</b>")),
             hovermode = "closest")
  })
  # Magazine Preferences
  output$magazine_preferences_plot <- renderPlotly({
    req(input$magazine_type)

    if (input$magazine_type == "format") {
      # Handle format preferences as a bar chart
      magazine_data <- data_combined %>%
        count(Q6A) %>%
        mutate(Format = recode(Q6A, `1` = "Physical", `2` = "Digital")) %>%
        na.omit()

      plot_ly(magazine_data, x = ~Format, y = ~n, type = 'bar',
              text = ~paste("Count:", n), textposition = 'auto',
              marker = list(color = c("#F9ECB3", "#c1f747", "#f4cccc"))) %>%  # Specify colors for each format
        layout(title = 'Magazine Format Preferences',
               xaxis = list(title = 'Magazine Format'),
               yaxis = list(title = 'Count of Respondents'))
    } else {
      # Handle frequency preferences as a line chart (optional)
      counts <- data_combined %>%
        select(Q6B) %>%
        na.omit() %>%
        mutate(Q6B = recode(Q6B,
                            `1` = "Once a year",
                            `2` = "Twice a year",
                            `3` = "Three times a year",
                            `4` = "Quarterly")) %>%
        mutate(Q6B = factor(Q6B, levels = c("Once a year", "Twice a year", "Three times a year", "Quarterly"))) %>%
        count(Q6B)

      plot_ly(counts, x = ~Q6B, y = ~n, type = 'scatter', mode = 'lines+markers',
              text = ~paste("Count:", n), textposition = 'top center',
              marker = list(color = "#9d0e1e")) %>%
        layout(title = 'Preferred Frequency for Physical Magazine',
               xaxis = list(title = 'Frequency'),
               yaxis = list(title = 'Count of Respondents'))
    }
  })

  # NPS Satisfaction Indicator
  output$satisfaction_indicator <- renderPlotly({
    filtered_data <- data_combined %>%
      filter(Q9 >= input$satisfaction_slider[1] & Q9 <= input$satisfaction_slider[2])

    avg_satisfaction <- mean(filtered_data$Q9, na.rm = TRUE)

    fig <- plot_ly(
      type = "indicator",
      mode = "gauge+number",
      value = avg_satisfaction,
      title = list(text = "Average Membership Satisfaction"),
      gauge = list(
        axis = list(range = list(0, 10)),
        steps = list(
          list(range = c(0, 4), color = "#ff0000"),
          list(range = c(4, 7), color = "#ffff00"),
          list(range = c(7, 10), color = "006D2C")
        ),
        bar = list(color = "#345842")
      )
    )

    fig
  })

  # Net Promoter Score Breakdown
  output$nps_bar_chart <- renderPlotly({
    nps_data <- data_combined %>%
      mutate(NPS = case_when(
        Q12 >= 9 ~ "Promoter",
        Q12 >= 7 & Q12 <= 8 ~ "Passive",
        Q12 <= 6 ~ "Detractor"
      )) %>%
      summarise(
        Promoters = sum(NPS == "Promoter") / n() * 100,
        Passives = sum(NPS == "Passive") / n() * 100,
        Detractors = sum(NPS == "Detractor") / n() * 100
      )

    nps_viz_data <- data.frame(
      Category = c("Promoters", "Passives", "Detractors"),
      Percentage = c(nps_data$Promoters, nps_data$Passives, nps_data$Detractors)
    )

    plot_ly(nps_viz_data, x = ~Category, y = ~Percentage, type = 'bar',
            marker = list(color = c("green", "yellow", "red"))) %>%
      layout(title = "NPS Breakdown",
             xaxis = list(title = "Category"),
             yaxis = list(title = "Percentage"))
  })

  # Data transformation for activity analysis
  df_transformed <- reactive({
    data_combined %>%
      mutate(Q7 = case_when(
        Q7 %in% c("life", "joint(life)") ~ "Life",
        Q7 %in% c("senior_citizen", "joint senior") ~ "Senior Citizen",
        TRUE ~ Q7
      )) %>%
      select(Q7, Q10_1:Q10_8) %>%
      mutate(
        visit_heritage = ifelse(!is.na(Q10_1), 1, 0),
        visit_heritage_other_org = ifelse(!is.na(Q10_2), 1, 0),
        visit_heritage_overseas = ifelse(!is.na(Q10_3), 1, 0),
        buy_from_shop = ifelse(!is.na(Q10_4), 1, 0),
        buy_from_cafe = ifelse(!is.na(Q10_5), 1, 0),
        attend_event = ifelse(!is.na(Q10_6), 1, 0),
        attend_cultural_event = ifelse(!is.na(Q10_7), 1, 0),
        watch_video = ifelse(!is.na(Q10_8), 1, 0)
      ) %>%
      select(Q7, all_of(input$activity_filter))
  })

  # Overall participation plot
  output$overall_participation_plot <- renderPlotly({
    activity_participation <- df_transformed() %>%
      summarise(across(-Q7, ~mean(.) * 100)) %>%
      pivot_longer(everything(), names_to = "Activity", values_to = "Participation_Rate")

    plot_ly(activity_participation, x = ~reorder(Activity, Participation_Rate),
            y = ~Participation_Rate, type = 'bar', marker = list(color = '#2a9d8f'),
            text = ~paste0(round(Participation_Rate, 1), "%"), textposition = 'auto') %>%
      layout(title = "Overall Participation Analysis",
             xaxis = list(title = "Activity", tickangle = 45),
             yaxis = list(title = "Participation Rate (%)"),
             showlegend = FALSE)
  })

  # Participation by membership type plot
  output$participation_by_type_plot <- renderPlotly({
    member_participation <- df_transformed() %>%
      group_by(Q7) %>%
      summarise(across(everything(), ~ mean(. == 1) * 100, .names = "{.col}_Participation_Rate")) %>%
      pivot_longer(-Q7, names_to = "Activity", values_to = "Participation_Rate")

    plot_ly(member_participation,
            x = ~Activity,
            y = ~Participation_Rate,
            color = ~Q7,
            type = 'bar',
            barmode = 'group',
            text = ~paste0(round(Participation_Rate, 1), "%"),
            textposition = 'auto') %>%
      layout(title = "Participation Rate by Membership Type",
             xaxis = list(title = "Activity", tickangle = 45),
             yaxis = list(title = "Participation Rate (%)"),
             legend = list(title = list(text = "<b>Membership Type</b>")))
  })


  # Average Satisfaction by Membership Type Plot
  output$membership_type_satisfaction <- renderPlotly({
    # Define a threshold for a low number of responses
    low_response_threshold <- 5

    # Group data by membership type and calculate average satisfaction
    satisfaction_by_type <- data_combined %>%
      filter(!is.na(Q7), !is.na(Q9)) %>%  # Ensure no missing values in Q7 (membership type) and Q9 (satisfaction)
      group_by(Q7) %>%
      summarise(Average_Satisfaction = mean(Q9, na.rm = TRUE),
                Response_Count = n()) %>%  # Also count the number of responses
      arrange(desc(Average_Satisfaction))

    # Define a custom color palette
    colors <- c("#ffc8d2", "#da6b6e", "#b07602", "#d62728", "#84dcda", "#836155", "#eabe67")

    # Create the bar chart using Plotly
    plotly_chart <- plot_ly(satisfaction_by_type,
                            x = ~Q7,
                            y = ~Average_Satisfaction,
                            type = 'bar',
                            text = ~paste("Avg Satisfaction:", round(Average_Satisfaction, 2), "<br>Responses:", Response_Count),
                            textposition = 'auto',
                            marker = list(color = colors)) %>%  # Apply the custom colors
      layout(title = 'Average Satisfaction by Membership Type',
             xaxis = list(title = 'Membership Type'),
             yaxis = list(title = 'Average Satisfaction'))

    # Add a warning annotation if any group has low responses
    low_response_members <- satisfaction_by_type %>%
      filter(Response_Count < low_response_threshold)

    if (nrow(low_response_members) > 0) {
      warning_text <- paste("Note: Membership types with <", low_response_threshold, "responses may not be reliable.")

      plotly_chart <- plotly_chart %>%
        layout(annotations = list(
          list(x = 0.5, y = 1.05, text = warning_text,
               showarrow = FALSE, xref = 'paper', yref = 'paper',
               font = list(size = 12, color = 'red'))
        ))
    }

    # Return the plotly chart
    plotly_chart
  })


  # Data Preparation for Potential Benefits Analysis
  df_summary <- reactive({
    data_combined %>%
      mutate(Q7 = case_when(
        Q7 %in% c("life", "life(joint)") ~ "life",
        Q7 %in% c("senior_citizen", "senior_citizen(Joint)") ~ "senior_citizen",
        TRUE ~ Q7
      )) %>%
      select(Q7, Q3_1:Q3_6) %>%
      mutate(
        monthly_membership = ifelse(!is.na(Q3_1), 1, 0),
        membership_donation = ifelse(!is.na(Q3_2), 1, 0),
        family_content = ifelse(!is.na(Q3_3), 1, 0),
        member_handbook = ifelse(!is.na(Q3_4), 1, 0),
        volunteering_opportunities = ifelse(!is.na(Q3_5), 1, 0),
        local_info_events = ifelse(!is.na(Q3_6), 1, 0)
      ) %>%
      select(-c(Q3_1:Q3_6))
  })

  # Benefit Summary
  benefit_summary <- reactive({
    df_summary() %>%
      select(-Q7) %>%
      summarise(
        monthly_membership = sum(monthly_membership, na.rm = TRUE),
        membership_donation = sum(membership_donation, na.rm = TRUE),
        family_content = sum(family_content, na.rm = TRUE),
        member_handbook = sum(member_handbook, na.rm = TRUE),
        volunteering_opportunities = sum(volunteering_opportunities, na.rm = TRUE),
        local_info_events = sum(local_info_events, na.rm = TRUE)
      ) %>%
      pivot_longer(cols = everything(), names_to = "Benefit", values_to = "Count") %>%
      mutate(Percentage = (Count / sum(Count)) * 100) %>%
      filter(Benefit %in% input$benefit_filter)
  })

  # Interactive Plot for Potential Benefits
  output$benefit_importance_plot <- renderPlotly({
    plot_ly(benefit_summary(), x = ~reorder(Benefit, -Percentage), y = ~Percentage, type = 'bar',
            text = ~paste0(round(Percentage, 1), "%"),
            textposition = 'auto',
            marker = list(color = '#57936e')) %>%
      layout(title = 'Importance of Future Potential Benefits to Members',
             xaxis = list(title = 'Benefit', tickangle = 45),
             yaxis = list(title = 'Percentage of Members'),
             showlegend = FALSE)
  })



  # Interactive Plot for Information Sources
  output$info_sources_plot <- renderPlotly({
    # Filter data based on selected sources
    filtered_info_sources <- data_combined %>%
      select(Q11_1:Q11_12) %>%
      pivot_longer(cols = everything(), names_to = "Source", values_to = "Selected") %>%
      filter(!is.na(Selected)) %>%
      group_by(Source) %>%
      summarise(Count = n()) %>%
      arrange(desc(Count)) %>%
      mutate(Source = recode(Source,
                             Q11_1 = "Social Media",
                             Q11_2 = "E-newsletters",
                             Q11_3 = "Newspapers",
                             Q11_4 = "Online News Websites or Apps",
                             Q11_5 = "Magazines",
                             Q11_6 = "Blogs",
                             Q11_7 = "Television",
                             Q11_8 = "On Demand TV",
                             Q11_9 = "Streaming Services",
                             Q11_10 = "Radio",
                             Q11_11 = "Podcasts",
                             Q11_12 = "Other")) %>%
      filter(Source %in% input$info_sources_filter)

    # Create the plot using Plotly
    plot_ly(filtered_info_sources, x = ~reorder(Source, -Count), y = ~Count, type = 'bar',
            text = ~paste("Count:", Count), textposition = 'auto', marker = list(color = '#abc9b6')) %>%
      layout(title = 'Top Information Sources for Members',
             xaxis = list(title = 'Source', tickangle = 45),
             yaxis = list(title = 'Number of Members'),
             showlegend = FALSE)
  })

}

# Run the application
shinyApp(ui = ui, server = server)
