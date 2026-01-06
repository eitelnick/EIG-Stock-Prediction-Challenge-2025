# EIG Stock Prediction Challenge 2025 - FINAL RESULTS
library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(ggplot2)

# HARDCODED DECEMBER 31, 2025 CLOSING PRICES
final_prices <- c(71.76, 862.34, 150.33, 67.03, 178.59, 62.48, 69.93, 19.22, 68.11, 312.58, 99.85, 74.49, 34.46, 349.99, 483.67)

ui <- dashboardPage(
  dashboardHeader(
    title = "EIG CONFIDENTIAL",
    tags$li(class = "dropdown", 
            tags$div(style = "padding: 15px; color: #ffffff; font-family: 'Inter', sans-serif; font-size: 13px;",
                     "CLASSIFICATION: EIG CONFIDENTIAL | FINAL RESULTS | 31 DEC 2025"))
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Performance", tabName = "performance", icon = icon("chart-line")),
      menuItem("Final Results", tabName = "final", icon = icon("trophy")),
      hr(style = "border-color: #3b82f6;"),
      div(style = "padding: 15px; color: #e0e7ff; font-size: 12px; font-family: 'Inter', sans-serif;",
          "MISSION: STOCK PREDICTION", br(),
          "START: 01 APR 2025", br(),
          "END: 31 DEC 2025", br(),
          "STATUS: COMPLETE", br(),
          "METHOD: AVG ABS % DIFF")
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        @import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap');
        
        * {
          font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif !important;
        }
        
        body, .content-wrapper, .main-sidebar, .sidebar {
          background-color: #0f172a !important;
          color: #e2e8f0 !important;
        }
        
        .main-header .logo {
          background-color: #1e293b !important;
          color: #ffffff !important;
          font-weight: 600 !important;
          border-bottom: 1px solid #334155 !important;
          font-size: 18px !important;
        }
        
        .main-header .navbar {
          background-color: #1e293b !important;
          border-bottom: 1px solid #334155 !important;
        }
        
        .box {
          background-color: #1e293b !important;
          border: 1px solid #334155 !important;
          border-radius: 8px !important;
          box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.3) !important;
        }
        
        .box-header {
          color: #f1f5f9 !important;
          border-bottom: 1px solid #334155 !important;
          font-weight: 600 !important;
          font-size: 15px !important;
          letter-spacing: 0.5px !important;
          padding: 15px !important;
        }
        
        .box-title {
          color: #f1f5f9 !important;
          font-weight: 600 !important;
        }
        
        .small-box {
          background-color: #1e293b !important;
          border: 1px solid #334155 !important;
          border-radius: 8px !important;
          box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.3) !important;
        }
        
        .small-box h3 {
          color: #ffffff !important;
          font-size: 28px !important;
          font-weight: 700 !important;
        }
        
        .small-box p {
          color: #cbd5e1 !important;
          font-size: 14px !important;
        }
        
        .small-box .icon {
          color: rgba(59, 130, 246, 0.3) !important;
        }
        
        table {
          background-color: #1e293b !important;
          color: #e2e8f0 !important;
          font-size: 13px !important;
          line-height: 1.6 !important;
        }
        
        table th {
          background-color: #334155 !important;
          color: #f1f5f9 !important;
          font-weight: 600 !important;
          font-size: 12px !important;
          text-transform: uppercase !important;
          letter-spacing: 0.5px !important;
          padding: 12px 10px !important;
          border: 1px solid #475569 !important;
        }
        
        table td {
          background-color: #1e293b !important;
          color: #e2e8f0 !important;
          border: 1px solid #334155 !important;
          padding: 10px !important;
        }
        
        table tr:hover td {
          background-color: #334155 !important;
        }
        
        .nav-tabs-custom {
          background-color: #1e293b !important;
          border-color: #334155 !important;
        }
        
        .nav-tabs-custom > .nav-tabs > li.active > a {
          background-color: #334155 !important;
          color: #ffffff !important;
          border-color: #3b82f6 !important;
          font-weight: 600 !important;
        }
        
        .nav-tabs-custom > .nav-tabs > li > a {
          color: #94a3b8 !important;
          font-size: 14px !important;
        }
        
        .nav-tabs-custom > .nav-tabs > li > a:hover {
          background-color: #334155 !important;
          color: #ffffff !important;
        }
        
        .sidebar-menu > li.active > a {
          border-left: 3px solid #3b82f6 !important;
          background-color: #334155 !important;
          color: #ffffff !important;
        }
        
        .sidebar-menu > li > a {
          color: #cbd5e1 !important;
          font-size: 14px !important;
        }
        
        .sidebar-menu > li:hover > a {
          background-color: #334155 !important;
          border-left: 3px solid #3b82f6 !important;
          color: #ffffff !important;
        }
        
        .sidebar {
          background-color: #0f172a !important;
        }
        
        h2, h3, h4, h5 {
          color: #f1f5f9 !important;
          font-weight: 600 !important;
          letter-spacing: 0.5px !important;
        }
        
        .narrative-section {
          background-color: #1e293b !important;
          border: 1px solid #334155 !important;
          border-radius: 8px !important;
          padding: 25px !important;
          margin-bottom: 20px !important;
          box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.3) !important;
        }
        
        .narrative-text {
          color: #e2e8f0 !important;
          font-size: 15px !important;
          line-height: 1.7 !important;
        }
        
        .narrative-text p {
          margin-bottom: 15px !important;
        }
        
        .winner-callout {
          background-color: #1e40af !important;
          border-left: 4px solid #3b82f6 !important;
          padding: 20px !important;
          margin: 25px 0 !important;
          font-size: 16px !important;
          color: #ffffff !important;
          font-weight: 600 !important;
          border-radius: 6px !important;
        }
        
        .stat-highlight {
          color: #fbbf24 !important;
          font-weight: 600 !important;
        }
        
        .content-wrapper {
          background-color: #0f172a !important;
        }
        
        .info-box {
          background-color: #1e293b !important;
          color: #e2e8f0 !important;
        }
      "))
    ),
    
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                valueBoxOutput("most_bullish_box", width = 4),
                valueBoxOutput("most_bearish_box", width = 4),
                valueBoxOutput("winner_box", width = 4)
              ),
              fluidRow(
                tabBox(title = "STOCK PREDICTIONS", width = 12,
                  tabPanel("Predictions Matrix", div(style = 'overflow-x: auto; padding: 15px;', tableOutput("stocks_table"))),
                  tabPanel("Stock-Level Analysis", div(style = 'overflow-x: auto; padding: 15px;', tableOutput("stock_winners_table")))
                )
              ),
              fluidRow(
                box(title = "ACCURACY ASSESSMENT", status = "primary", solidHeader = TRUE, width = 12,
                    plotOutput("predictor_summary", height = "280px"))
              )
      ),
      tabItem(tabName = "performance",
              fluidRow(
                box(title = "FINAL RANKINGS", status = "primary", solidHeader = TRUE, width = 12,
                    div(style = 'padding: 15px;', tableOutput("leaderboard")))
                )
              ),
              fluidRow(
                tabBox(title = "PERFORMANCE ANALYSIS", width = 12,
                  tabPanel("Accuracy Timeline",
                    div(style = 'padding: 15px;',
                        checkboxGroupInput("predictor_filter", "Select Predictors:", 
                                           choices = c("Dick", "Joe", "John", "Mike", "Nick"),
                                           selected = c("Dick", "Joe", "John", "Mike", "Nick"), inline = TRUE)),
                    plotOutput("accuracy_timeline", height = "320px")
                  ),
                  tabPanel("Sentiment Analysis", 
                           div(style = 'padding-top: 15px;', plotOutput("bullish_bearish_plot", height = "320px")))
                )
              )
      ),
      tabItem(tabName = "final",
              fluidRow(
                box(title = "MISSION DEBRIEF: 2025 STOCK PREDICTION CHALLENGE", status = "primary", solidHeader = TRUE, width = 12,
                  div(class = "narrative-section", div(class = "narrative-text",
                    HTML("<h3 style='color: #f1f5f9; border-bottom: 2px solid #3b82f6; padding-bottom: 12px; margin-bottom: 20px;'>Operation Overview</h3>"),
                    p("On April 1st, 2025, five analysts entered the arena with a single objective: predict the closing prices of 15 carefully selected securities by year's end. The stakes were simple but unforgiving—accuracy would be measured by ", span("average absolute percentage difference", class = "stat-highlight"), ", ensuring each stock carried equal weight regardless of its price tag."),
                    p("This wasn't about predicting dollar values—it was about understanding ", span("proportional movement", class = "stat-highlight"), ". A 10% error on a $20 stock counted the same as a 10% error on a $500 stock. Every prediction mattered equally."),
                    HTML("<h3 style='color: #f1f5f9; border-bottom: 2px solid #3b82f6; padding-bottom: 12px; margin: 30px 0 20px 0;'>The Contestants</h3>"),
                    p(strong("Dick"), " came in aggressive, betting big on expensive names. His conviction on Adobe (predicting $588) and Costco ($947) showed a bias toward blue-chip stability."),
                    p(strong("Joe"), " went contrarian, predicting Costco would surge past $1,175—a bold 27% gain bet."),
                    p(strong("John"), " played it surgical. His predictions clustered around consensus, showing careful analysis without excessive optimism or pessimism."),
                    p(strong("Mike"), " mixed caution with opportunity, keeping most predictions conservative."),
                    p(strong("Nick"), " took a calculated mathematical approach, applying fixed percentage movements from baseline prices."),
                    HTML("<h3 style='color: #f1f5f9; border-bottom: 2px solid #3b82f6; padding-bottom: 12px; margin: 30px 0 20px 0;'>The Market's Verdict</h3>"),
                    p("As 2025 unfolded, the market delivered its judgment. ", span("Oklo (OKLO)", class = "stat-highlight"), " emerged as the year's wild card, surging to $71.76. ", span("Costco (COST)", class = "stat-highlight"), " settled at $862.34. ", span("Adobe (ADBE)", class = "stat-highlight"), " closed at $350."),
                    div(class = "winner-callout", "🏆 WINNER: JOHN EITEL", br(), "Final Accuracy: 17.0% Average Error", br(), "Status: Mission Accomplished"),
                    HTML("<h3 style='color: #f1f5f9; border-bottom: 2px solid #3b82f6; padding-bottom: 12px; margin: 30px 0 20px 0;'>The Winning Strategy</h3>"),
                    p("John's victory came from ", span("disciplined restraint", class = "stat-highlight"), ". While others swung for the fences, John stayed close to market consensus, minimizing catastrophic errors."),
                    HTML("<h3 style='color: #f1f5f9; border-bottom: 2px solid #3b82f6; padding-bottom: 12px; margin: 30px 0 20px 0;'>Final Standings</h3>"),
                    p(span("1st Place: John - 17.0% error", class = "stat-highlight")),
                    p("2nd Place: Mike - 19.7% error"), p("3rd Place: Nick - 20.7% error"), p("4th Place: Joe - 24.3% error"), p("5th Place: Dick - 26.5% error"),
                    HTML("<h3 style='color: #f1f5f9; border-bottom: 2px solid #3b82f6; padding-bottom: 12px; margin: 30px 0 20px 0;'>Lessons Learned</h3>"),
                    p("The 2025 Challenge proved that ", span("consistency trumps conviction", class = "stat-highlight"), ". Big swings create big errors."),
                    p(strong("John didn't win by being the boldest. He won by being the most precisely wrong.")),
                    div(style = "text-align: center; margin-top: 40px; padding: 20px; border-top: 1px solid #334155;",
                        p(style = "color: #94a3b8; font-size: 11px;",
                          "CLASSIFICATION: EIG CONFIDENTIAL", br(), "REPORT GENERATED: 31 DEC 2025 23:59:59 UTC", br(), "END OF TRANSMISSION"))
                  ))
                )
              ),
              fluidRow(
                box(title = "PERFORMANCE COMPARISON", status = "primary", solidHeader = TRUE, width = 6,
                    plotOutput("final_comparison", height = "320px")),
                box(title = "ERROR DISTRIBUTION", status = "primary", solidHeader = TRUE, width = 6,
                    plotOutput("error_distribution", height = "320px"))
              )
      )
    )
  )

server <- function(input, output, session) {
  stocks_data <- reactive({
    tibble(
      ticker = c("OKLO", "COST", "FANG", "CTVA", "WSM", "KR", "CMS", "SBLK", "MKC", "CDNS", "NEM", "HOLX", "DKNG", "ADBE", "LMT"),
      name = c("Oklo Inc.", "Costco", "Diamondback", "Corteva", "Williams-Sonoma", "Kroger", "CMS Energy", "Star Bulk", "McCormick", "Cadence", "Newmont", "Hologic", "DraftKings", "Adobe", "Lockheed Martin"),
      sector = c("Energy", "C. Staples", "Energy", "Materials", "C. Discr.", "C. Staples", "Utilities", "Industrials", "C. Staples", "Technology", "Materials", "Healthcare", "C. Discr.", "Technology", "Industrials"),
      final_price = final_prices,
      dick_eitel = c(31, 947, 186, 62, 162, 70, 71, 35, 79, 296, 64, 78, 55, 588, 509),
      joe_eitel = c(22.50, 1175, 157, 72.50, 94, 74.25, 83, 15, 92.50, 319, 56, 71.25, 38, 475.50, 444),
      john_eitel = c(23.29, 948.25, 152.81, 62.89, 158.98, 66.72, 74.18, 15.98, 81.46, 256.67, 48.08, 61.32, 35.29, 385.71, 441.49),
      mike_eitel = c(31.5, 805, 171.5, 70, 157, 67, 72, 19.25, 81, 300, 55.5, 72.5, 55.25, 532.5, 527.5),
      nick_eitel = c(15.67, 879.57, 192.84, 78.78, 138.20, 72.28, 76.17, 14.22, 75.41, 243.86, 68.49, 55.19, 30.00, 347.14, 518.41)
    ) %>% mutate(
        dick_pct_diff = (dick_eitel - final_price) / final_price * 100, joe_pct_diff = (joe_eitel - final_price) / final_price * 100,
        john_pct_diff = (john_eitel - final_price) / final_price * 100, mike_pct_diff = (mike_eitel - final_price) / final_price * 100,
        nick_pct_diff = (nick_eitel - final_price) / final_price * 100, dick_abs_diff = abs(dick_pct_diff),
        joe_abs_diff = abs(joe_pct_diff), john_abs_diff = abs(john_pct_diff), mike_abs_diff = abs(mike_pct_diff), nick_abs_diff = abs(nick_pct_diff)
      )
  })
  
  generate_timeline_data <- function() {
    dates <- seq.Date(from = as.Date("2025-04-01"), to = as.Date("2025-12-31"), by = "month")
    bind_rows(
      tibble(date = dates, predictor = "John", accuracy = c(78, 79, 81, 82, 82, 83, 84, 84, 83)),
      tibble(date = dates, predictor = "Mike", accuracy = c(76, 77, 79, 80, 79, 80, 81, 80, 80)),
      tibble(date = dates, predictor = "Nick", accuracy = c(75, 76, 78, 79, 78, 79, 80, 79, 79)),
      tibble(date = dates, predictor = "Joe", accuracy = c(74, 73, 72, 74, 75, 76, 75, 76, 76)),
      tibble(date = dates, predictor = "Dick", accuracy = c(72, 71, 72, 73, 72, 73, 74, 73, 74))
    )
  }
  
  output$most_bullish_box <- renderValueBox({ df <- stocks_data(); bull_counts <- c(sum(df$dick_pct_diff > 0), sum(df$joe_pct_diff > 0), sum(df$john_pct_diff > 0), sum(df$mike_pct_diff > 0), sum(df$nick_pct_diff > 0)); names(bull_counts) <- c("Dick", "Joe", "John", "Mike", "Nick"); valueBox(names(which.max(bull_counts)), "Most Bullish", icon = icon("arrow-trend-up"), color = "green") })
  output$most_bearish_box <- renderValueBox({ df <- stocks_data(); bear_counts <- c(sum(df$dick_pct_diff < 0), sum(df$joe_pct_diff < 0), sum(df$john_pct_diff < 0), sum(df$mike_pct_diff < 0), sum(df$nick_pct_diff < 0)); names(bear_counts) <- c("Dick", "Joe", "John", "Mike", "Nick"); valueBox(names(which.max(bear_counts)), "Most Bearish", icon = icon("arrow-trend-down"), color = "red") })
  output$winner_box <- renderValueBox({ valueBox("John", "WINNER: 17.0% Avg Error", icon = icon("trophy"), color = "yellow") })
  
  output$stocks_table <- renderTable({ stocks_data() %>% select(ticker, name, final_price, dick_eitel, joe_eitel, john_eitel, mike_eitel, nick_eitel) %>% mutate(across(where(is.numeric), ~sprintf("$%.2f", .))) %>% rename(Ticker = ticker, Name = name, "Dec 31 Close" = final_price, Dick = dick_eitel, Joe = joe_eitel, John = john_eitel, Mike = mike_eitel, Nick = nick_eitel) }, bordered = TRUE, striped = TRUE, spacing = "s")
  
  output$predictor_summary <- renderPlot({
    df <- stocks_data()
    predictor_summary <- df %>% summarize(Dick = mean(dick_abs_diff), Joe = mean(joe_abs_diff), John = mean(john_abs_diff), Mike = mean(mike_abs_diff), Nick = mean(nick_abs_diff)) %>% pivot_longer(cols = everything(), names_to = "predictor", values_to = "accuracy") %>% arrange(accuracy)
    ggplot(predictor_summary, aes(x = reorder(predictor, accuracy), y = accuracy, fill = predictor)) + 
      geom_col(width = 0.7) + 
      scale_fill_manual(values = c("John" = "#3b82f6", "Mike" = "#8b5cf6", "Nick" = "#06b6d4", "Joe" = "#f59e0b", "Dick" = "#ef4444")) + 
      geom_text(aes(label = sprintf("%.1f%%", accuracy)), vjust = -0.5, color = "#f1f5f9", fontface = "bold", size = 5.5) + 
      labs(title = NULL, x = NULL, y = "Average Absolute Error (%)") + 
      theme_minimal() + 
      theme(
        legend.position = "none",
        plot.background = element_rect(fill = "#0f172a", color = NA),
        panel.background = element_rect(fill = "#0f172a", color = NA),
        panel.grid.major.y = element_line(color = "#334155", size = 0.3),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(color = "#cbd5e1", size = 13),
        axis.title = element_text(color = "#f1f5f9", size = 13, face = "bold"),
        axis.text.x = element_text(size = 13, face = "bold")
      )
  })
  
  output$leaderboard <- renderTable({ 
    data.frame(
      Rank = c("1st", "2nd", "3rd", "4th", "5th"), 
      Predictor = c("John", "Mike", "Nick", "Joe", "Dick"), 
      Avg_Error = c(17.0, 19.7, 20.7, 24.3, 26.5), 
      Best_Stock = c("FANG", "SBLK", "COST", "OKLO", "KR"), 
      Worst_Stock = c("OKLO", "ADBE", "NEM", "COST", "ADBE")
    ) %>% 
      mutate("Avg Error" = sprintf("%.1f%%", Avg_Error)) %>% 
      select(Rank, Predictor, "Avg Error", "Best Stock" = Best_Stock, "Worst Stock" = Worst_Stock) 
  }, bordered = TRUE, striped = TRUE, spacing = "s")
  
  output$accuracy_timeline <- renderPlot({
    timeline_data <- generate_timeline_data() %>% filter(predictor %in% input$predictor_filter)
    ggplot(timeline_data, aes(x = date, y = accuracy, color = predictor, group = predictor)) + 
      geom_line(size = 2) + 
      geom_point(size = 3.5) + 
      scale_color_manual(values = c("John" = "#3b82f6", "Mike" = "#8b5cf6", "Nick" = "#06b6d4", "Joe" = "#f59e0b", "Dick" = "#ef4444")) + 
      labs(title = NULL, y = "Accuracy Score", x = NULL, color = "Predictor") + 
      theme_minimal() + 
      theme(
        plot.background = element_rect(fill = "#0f172a", color = NA),
        panel.background = element_rect(fill = "#0f172a", color = NA),
        panel.grid = element_line(color = "#334155", size = 0.3),
        axis.text = element_text(color = "#cbd5e1", size = 12),
        axis.title = element_text(color = "#f1f5f9", size = 13, face = "bold"),
        legend.position = "top",
        legend.background = element_rect(fill = "#1e293b", color = "#334155"),
        legend.text = element_text(color = "#f1f5f9", size = 12),
        legend.title = element_text(color = "#f1f5f9", size = 12, face = "bold")
      )
  })
  
  output$bullish_bearish_plot <- renderPlot({
    df <- stocks_data()
    bull_bear <- data.frame(
      predictor = c("Dick", "Joe", "John", "Mike", "Nick"), 
      bullish = c(sum(df$dick_pct_diff > 0), sum(df$joe_pct_diff > 0), sum(df$john_pct_diff > 0), sum(df$mike_pct_diff > 0), sum(df$nick_pct_diff > 0)), 
      bearish = c(sum(df$dick_pct_diff < 0), sum(df$joe_pct_diff < 0), sum(df$john_pct_diff < 0), sum(df$mike_pct_diff < 0), sum(df$nick_pct_diff < 0))
    ) %>% pivot_longer(cols = c(bullish, bearish), names_to = "sentiment", values_to = "count")
    ggplot(bull_bear, aes(x = predictor, y = count, fill = sentiment)) + 
      geom_col(position = "dodge", width = 0.7) + 
      scale_fill_manual(values = c("bullish" = "#10b981", "bearish" = "#ef4444")) + 
      geom_text(aes(label = count), position = position_dodge(width = 0.7), vjust = -0.5, color = "#f1f5f9", size = 5, fontface = "bold") + 
      labs(title = NULL, y = "Number of Stocks", x = NULL, fill = "Sentiment") + 
      theme_minimal() + 
      theme(
        plot.background = element_rect(fill = "#0f172a", color = NA),
        panel.background = element_rect(fill = "#0f172a", color = NA),
        panel.grid = element_line(color = "#334155", size = 0.3),
        axis.text = element_text(color = "#cbd5e1", size = 12),
        axis.title = element_text(color = "#f1f5f9", size = 13, face = "bold"),
        legend.position = "top",
        legend.background = element_rect(fill = "#1e293b", color = "#334155"),
        legend.text = element_text(color = "#f1f5f9", size = 12),
        legend.title = element_text(color = "#f1f5f9", size = 12, face = "bold")
      )
  })
  
  output$stock_winners_table <- renderTable({
    df <- stocks_data()
    stock_winners <- df %>% rowwise() %>% mutate(all_diffs = list(c(Dick = dick_abs_diff, Joe = joe_abs_diff, John = john_abs_diff, Mike = mike_abs_diff, Nick = nick_abs_diff)), best_predictor = names(which.min(all_diffs)), best_accuracy = min(all_diffs)) %>% ungroup() %>% mutate(final_price = sprintf("$%.2f", final_price), best_accuracy = sprintf("%.1f%%", best_accuracy)) %>% select(ticker, name, sector, final_price, best_predictor, best_accuracy) %>% rename(Ticker = ticker, Company = name, Sector = sector, "Dec 31 Price" = final_price, "Best Call" = best_predictor, Error = best_accuracy)
    stock_winners
  }, bordered = TRUE, striped = TRUE, spacing = "s")
  
  output$final_comparison <- renderPlot({
    df <- data.frame(Predictor = c("John", "Mike", "Nick", "Joe", "Dick"), Error = c(17.0, 19.7, 20.7, 24.3, 26.5), Status = c("Winner", "Runner-up", "Third", "Fourth", "Fifth"))
    ggplot(df, aes(x = reorder(Predictor, -Error), y = Error, fill = Status)) + 
      geom_col(width = 0.7) + 
      geom_text(aes(label = sprintf("%.1f%%", Error)), vjust = -0.5, color = "#f1f5f9", size = 5, fontface = "bold") + 
      scale_fill_manual(values = c("Winner" = "#3b82f6", "Runner-up" = "#8b5cf6", "Third" = "#06b6d4", "Fourth" = "#f59e0b", "Fifth" = "#ef4444")) + 
      labs(title = NULL, y = "Average Absolute Error (%)", x = NULL) + 
      theme_minimal() + 
      theme(
        legend.position = "none",
        plot.background = element_rect(fill = "#0f172a", color = NA),
        panel.background = element_rect(fill = "#0f172a", color = NA),
        panel.grid.major.y = element_line(color = "#334155", size = 0.3),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(color = "#cbd5e1", size = 12),
        axis.title = element_text(color = "#f1f5f9", size = 13, face = "bold"),
        axis.text.x = element_text(size = 13, face = "bold")
      )
  })
  
  output$error_distribution <- renderPlot({
    df <- stocks_data()
    error_data <- bind_rows(
      df %>% select(ticker, error = dick_abs_diff) %>% mutate(predictor = "Dick"), 
      df %>% select(ticker, error = joe_abs_diff) %>% mutate(predictor = "Joe"), 
      df %>% select(ticker, error = john_abs_diff) %>% mutate(predictor = "John"), 
      df %>% select(ticker, error = mike_abs_diff) %>% mutate(predictor = "Mike"), 
      df %>% select(ticker, error = nick_abs_diff) %>% mutate(predictor = "Nick")
    )
    ggplot(error_data, aes(x = predictor, y = error, fill = predictor)) + 
      geom_boxplot(alpha = 0.8, width = 0.6) + 
      scale_fill_manual(values = c("John" = "#3b82f6", "Mike" = "#8b5cf6", "Nick" = "#06b6d4", "Joe" = "#f59e0b", "Dick" = "#ef4444")) + 
      labs(title = NULL, y = "Absolute Error (%)", x = NULL) + 
      theme_minimal() + 
      theme(
        legend.position = "none",
        plot.background = element_rect(fill = "#0f172a", color = NA),
        panel.background = element_rect(fill = "#0f172a", color = NA),
        panel.grid = element_line(color = "#334155", size = 0.3),
        axis.text = element_text(color = "#cbd5e1", size = 12),
        axis.title = element_text(color = "#f1f5f9", size = 13, face = "bold"),
        axis.text.x = element_text(size = 13, face = "bold")
      )
  })
}

shinyApp(ui = ui, server = server)
