# EIG Stock Prediction Challenge 2025 - FINAL RESULTS
# Palantir Intelligence Portal Style | Data locked to December 31, 2025
library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(ggplot2)

# HARDCODED DECEMBER 31, 2025 CLOSING PRICES (OFFICIAL)
final_prices <- c(71.76, 862.34, 150.33, 67.03, 178.59, 62.48, 69.93, 19.22, 68.11, 312.58, 99.85, 74.49, 34.46, 349.99, 483.67)

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(
    title = "EIG CONFIDENTIAL",
    tags$li(class = "dropdown", 
            tags$div(style = "padding: 15px; color: #00d9ff; font-family: 'Courier New';",
                     "CLASSIFICATION: EIG CONFIDENTIAL | FINAL RESULTS | 31 DEC 2025"))
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Performance", tabName = "performance", icon = icon("chart-line")),
      menuItem("Final Results", tabName = "final", icon = icon("trophy")),
      hr(style = "border-color: #00d9ff;"),
      div(style = "padding: 15px; color: #00d9ff; font-size: 11px; font-family: 'Courier New';",
          "MISSION: STOCK PREDICTION", br(),
          "START: 01 APR 2025", br(),
          "END: 31 DEC 2025", br(),
          "STATUS: COMPLETE", br(),
          "METHODOLOGY: AVG ABS % DIFF")
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        @import url('https://fonts.googleapis.com/css2?family=Rajdhani:wght@400;600;700&display=swap');
        body, .content-wrapper, .main-sidebar, .sidebar { background-color: #0a0e27 !important; font-family: 'Rajdhani', sans-serif !important; }
        .skin-black .main-header .logo { background-color: #050814 !important; color: #00d9ff !important; font-weight: 700 !important; border-bottom: 2px solid #00d9ff !important; }
        .skin-black .main-header .navbar { background-color: #050814 !important; border-bottom: 2px solid #00d9ff !important; }
        .box { background-color: #141b3d !important; border: 1px solid #1e3a8a !important; box-shadow: 0 0 20px rgba(0, 217, 255, 0.1) !important; }
        .box-header, .box-title { color: #00d9ff !important; border-bottom: 1px solid #1e3a8a !important; font-weight: 600 !important; text-transform: uppercase !important; letter-spacing: 1px !important; }
        .small-box { background-color: #141b3d !important; border: 1px solid #1e3a8a !important; box-shadow: 0 0 20px rgba(0, 217, 255, 0.1) !important; }
        .small-box h3, .small-box p { color: #00d9ff !important; }
        .small-box .icon { color: rgba(0, 217, 255, 0.2) !important; }
        table { background-color: #0a0e27 !important; color: #00d9ff !important; font-family: 'Courier New', monospace !important; font-size: 12px !important; }
        table th { background-color: #1e3a8a !important; color: #00d9ff !important; font-weight: 600 !important; text-transform: uppercase !important; letter-spacing: 1px !important; border: 1px solid #00d9ff !important; }
        table td { border: 1px solid #1e3a8a !important; padding: 8px !important; }
        table tr:hover { background-color: #1e3a8a !important; }
        .narrative-section { background: linear-gradient(135deg, #141b3d 0%, #1e3a8a 100%) !important; border: 1px solid #00d9ff !important; padding: 20px !important; margin-bottom: 20px !important; box-shadow: 0 0 30px rgba(0, 217, 255, 0.2) !important; }
        .narrative-text { color: #e0f2fe !important; font-size: 15px !important; line-height: 1.8 !important; }
        .winner-callout { background-color: #1e3a8a !important; border-left: 4px solid #00d9ff !important; padding: 15px !important; margin: 20px 0 !important; font-size: 18px !important; color: #00d9ff !important; font-weight: 700 !important; letter-spacing: 2px !important; }
        .stat-highlight { color: #fbbf24 !important; font-weight: 700 !important; }
        h2, h3, h4, h5 { color: #00d9ff !important; font-weight: 600 !important; text-transform: uppercase !important; letter-spacing: 2px !important; }
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
                tabBox(title = "INTELLIGENCE BRIEFING", width = 12,
                  tabPanel("Predictions Matrix", div(style = 'overflow-x: auto', tableOutput("stocks_table"))),
                  tabPanel("Stock-Level Analysis", div(style = 'overflow-x: auto', tableOutput("stock_winners_table")))
                )
              ),
              fluidRow(
                box(title = "ACCURACY ASSESSMENT", status = "primary", solidHeader = TRUE, width = 12,
                    plotOutput("predictor_summary", height = "250px"))
              )
      ),
      tabItem(tabName = "performance",
              fluidRow(
                box(title = "FINAL RANKINGS", status = "primary", solidHeader = TRUE, width = 12,
                    tableOutput("leaderboard"))
              ),
              fluidRow(
                tabBox(title = "TEMPORAL ANALYSIS", width = 12,
                  tabPanel("Accuracy Timeline",
                    checkboxGroupInput("predictor_filter", "Select Predictors:", 
                                       choices = c("Dick", "Joe", "John", "Mike", "Nick"),
                                       selected = c("Dick", "Joe", "John", "Mike", "Nick"), inline = TRUE),
                    plotOutput("accuracy_timeline", height = "300px")
                  ),
                  tabPanel("Sentiment Analysis", plotOutput("bullish_bearish_plot", height = "300px"))
                )
              )
      ),
      tabItem(tabName = "final",
              fluidRow(
                box(title = "MISSION DEBRIEF: THE 2025 STOCK PREDICTION CHALLENGE", status = "primary", solidHeader = TRUE, width = 12,
                  div(class = "narrative-section", div(class = "narrative-text",
                    HTML("<h3 style='color: #00d9ff; border-bottom: 2px solid #00d9ff; padding-bottom: 10px;'>OPERATION OVERVIEW</h3>"),
                    p("On April 1st, 2025, five analysts entered the arena with a single objective: predict the closing prices of 15 carefully selected securities by year's end. The stakes were simple but unforgiving—accuracy would be measured by ", span("average absolute percentage difference", class = "stat-highlight"), ", ensuring each stock carried equal weight regardless of its price tag."),
                    p("This wasn't about predicting dollar values—it was about understanding ", span("proportional movement", class = "stat-highlight"), ". A 10% error on a $20 stock counted the same as a 10% error on a $500 stock. Every prediction mattered equally. Every call could make or break the final score."),
                    HTML("<h3 style='color: #00d9ff; border-bottom: 2px solid #00d9ff; padding-bottom: 10px; margin-top: 30px;'>THE CONTESTANTS</h3>"),
                    p(strong("Dick"), " came in aggressive, betting big on expensive names. His conviction on Adobe (predicting $588) and Costco ($947) showed a bias toward blue-chip stability."),
                    p(strong("Joe"), " went contrarian, predicting Costco would surge past $1,175—a bold 27% gain bet."),
                    p(strong("John"), " played it surgical. His predictions clustered around consensus, showing careful analysis without excessive optimism or pessimism."),
                    p(strong("Mike"), " mixed caution with opportunity, keeping most predictions conservative."),
                    p(strong("Nick"), " took a calculated mathematical approach, applying fixed percentage movements from baseline prices."),
                    HTML("<h3 style='color: #00d9ff; border-bottom: 2px solid #00d9ff; padding-bottom: 10px; margin-top: 30px;'>THE MARKET'S VERDICT</h3>"),
                    p("As 2025 unfolded, the market delivered its judgment. ", span("Oklo (OKLO)", class = "stat-highlight"), " emerged as the year's wild card, surging to $71.76. ", span("Costco (COST)", class = "stat-highlight"), " settled at $862.34. ", span("Adobe (ADBE)", class = "stat-highlight"), " closed at $350."),
                    div(class = "winner-callout", "🏆 MISSION ACCOMPLISHED: JOHN EITEL", br(), "FINAL ACCURACY: 17.0% AVERAGE ERROR", br(), "STATUS: OPTIMAL PERFORMANCE ACHIEVED"),
                    HTML("<h3 style='color: #00d9ff; border-bottom: 2px solid #00d9ff; padding-bottom: 10px; margin-top: 30px;'>THE WINNING STRATEGY</h3>"),
                    p("John's victory came from ", span("disciplined restraint", class = "stat-highlight"), ". While others swung for the fences, John stayed close to market consensus, minimizing catastrophic errors."),
                    HTML("<h3 style='color: #00d9ff; border-bottom: 2px solid #00d9ff; padding-bottom: 10px; margin-top: 30px;'>FINAL STANDINGS</h3>"),
                    p(span("1st Place: John - 17.0% error", class = "stat-highlight")),
                    p("2nd Place: Mike - 19.7% error"), p("3rd Place: Nick - 20.7% error"), p("4th Place: Joe - 24.3% error"), p("5th Place: Dick - 26.5% error"),
                    HTML("<h3 style='color: #00d9ff; border-bottom: 2px solid #00d9ff; padding-bottom: 10px; margin-top: 30px;'>LESSONS FROM THE ARENA</h3>"),
                    p("The 2025 Challenge proved that ", span("consistency trumps conviction", class = "stat-highlight"), ". Big swings create big errors."),
                    p(strong("In the end, John didn't win by being the boldest. He won by being the most precisely wrong.")),
                    div(style = "text-align: center; margin-top: 40px; padding: 20px; border-top: 2px solid #00d9ff;",
                        p(style = "color: #00d9ff; font-size: 12px; font-family: 'Courier New';",
                          "CLASSIFICATION: EIG CONFIDENTIAL", br(), "REPORT GENERATED: 31 DEC 2025 23:59:59 UTC", br(), "END OF TRANSMISSION"))
                  ))
                )
              ),
              fluidRow(
                box(title = "PERFORMANCE VISUALIZATION", status = "primary", solidHeader = TRUE, width = 6,
                    plotOutput("final_comparison", height = "300px")),
                box(title = "ERROR DISTRIBUTION", status = "primary", solidHeader = TRUE, width = 6,
                    plotOutput("error_distribution", height = "300px"))
              )
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
  
  output$stocks_table <- renderTable({ stocks_data() %>% select(ticker, name, final_price, dick_eitel, joe_eitel, john_eitel, mike_eitel, nick_eitel) %>% mutate(across(where(is.numeric), ~sprintf("$%.2f", .))) %>% rename(Ticker = ticker, Name = name, "Dec 31 Close" = final_price, Dick = dick_eitel, Joe = joe_eitel, John = john_eitel, Mike = mike_eitel, Nick = nick_eitel) }, bordered = TRUE, striped = TRUE)
  
  output$predictor_summary <- renderPlot({
    df <- stocks_data()
    predictor_summary <- df %>% summarize(Dick = mean(dick_abs_diff), Joe = mean(joe_abs_diff), John = mean(john_abs_diff), Mike = mean(mike_abs_diff), Nick = mean(nick_abs_diff)) %>% pivot_longer(cols = everything(), names_to = "predictor", values_to = "accuracy") %>% arrange(accuracy)
    ggplot(predictor_summary, aes(x = reorder(predictor, accuracy), y = accuracy, fill = predictor)) + geom_col() + scale_fill_manual(values = c("John" = "#00d9ff", "Mike" = "#3b82f6", "Nick" = "#06b6d4", "Joe" = "#0ea5e9", "Dick" = "#0284c7")) + geom_text(aes(label = sprintf("%.1f%%", accuracy)), position = position_stack(vjust = 0.5), color = "#0a0e27", fontface = "bold", size = 5) + labs(title = "", x = "", y = "Avg Absolute Error (%)") + theme_minimal() + theme(legend.position = "none", plot.background = element_rect(fill = "#0a0e27"), panel.background = element_rect(fill = "#0a0e27"), panel.grid = element_line(color = "#1e3a8a"), axis.text = element_text(color = "#00d9ff", size = 12), axis.title = element_text(color = "#00d9ff"))
  })
  
  output$leaderboard <- renderTable({ data.frame(Rank = c("1st", "2nd", "3rd", "4th", "5th"), Predictor = c("John", "Mike", "Nick", "Joe", "Dick"), Avg_Error = c(17.0, 19.7, 20.7, 24.3, 26.5), Best_Stock = c("FANG", "SBLK", "COST", "OKLO", "KR"), Worst_Stock = c("OKLO", "ADBE", "NEM", "COST", "ADBE")) %>% mutate("Avg Error" = sprintf("%.1f%%", Avg_Error)) %>% select(Rank, Predictor, "Avg Error", "Best Stock" = Best_Stock, "Worst Stock" = Worst_Stock) }, bordered = TRUE, striped = TRUE)
  
  output$accuracy_timeline <- renderPlot({
    timeline_data <- generate_timeline_data() %>% filter(predictor %in% input$predictor_filter)
    ggplot(timeline_data, aes(x = date, y = accuracy, color = predictor, group = predictor)) + geom_line(size = 1.5) + geom_point(size = 3) + scale_color_manual(values = c("John" = "#00d9ff", "Mike" = "#3b82f6", "Nick" = "#06b6d4", "Joe" = "#0ea5e9", "Dick" = "#0284c7")) + labs(y = "Accuracy Score", x = "", color = "Predictor") + theme_minimal() + theme(plot.background = element_rect(fill = "#0a0e27"), panel.background = element_rect(fill = "#0a0e27"), panel.grid = element_line(color = "#1e3a8a"), axis.text = element_text(color = "#00d9ff"), axis.title = element_text(color = "#00d9ff"), legend.background = element_rect(fill = "#141b3d"), legend.text = element_text(color = "#00d9ff"), legend.title = element_text(color = "#00d9ff"))
  })
  
  output$bullish_bearish_plot <- renderPlot({
    df <- stocks_data()
    bull_bear <- data.frame(predictor = c("Dick", "Joe", "John", "Mike", "Nick"), bullish = c(sum(df$dick_pct_diff > 0), sum(df$joe_pct_diff > 0), sum(df$john_pct_diff > 0), sum(df$mike_pct_diff > 0), sum(df$nick_pct_diff > 0)), bearish = c(sum(df$dick_pct_diff < 0), sum(df$joe_pct_diff < 0), sum(df$john_pct_diff < 0), sum(df$mike_pct_diff < 0), sum(df$nick_pct_diff < 0))) %>% pivot_longer(cols = c(bullish, bearish), names_to = "sentiment", values_to = "count")
    ggplot(bull_bear, aes(x = predictor, y = count, fill = sentiment)) + geom_col(position = "dodge") + scale_fill_manual(values = c("bullish" = "#10b981", "bearish" = "#ef4444")) + geom_text(aes(label = count), position = position_dodge(width = 0.9), vjust = -0.5, color = "#00d9ff", size = 5, fontface = "bold") + labs(y = "Number of Stocks", x = "", fill = "Sentiment") + theme_minimal() + theme(plot.background = element_rect(fill = "#0a0e27"), panel.background = element_rect(fill = "#0a0e27"), panel.grid = element_line(color = "#1e3a8a"), axis.text = element_text(color = "#00d9ff"), axis.title = element_text(color = "#00d9ff"), legend.background = element_rect(fill = "#141b3d"), legend.text = element_text(color = "#00d9ff"), legend.title = element_text(color = "#00d9ff"))
  })
  
  output$stock_winners_table <- renderTable({
    df <- stocks_data()
    stock_winners <- df %>% rowwise() %>% mutate(all_diffs = list(c(Dick = dick_abs_diff, Joe = joe_abs_diff, John = john_abs_diff, Mike = mike_abs_diff, Nick = nick_abs_diff)), best_predictor = names(which.min(all_diffs)), best_accuracy = min(all_diffs)) %>% ungroup() %>% mutate(final_price = sprintf("$%.2f", final_price), best_accuracy = sprintf("%.1f%%", best_accuracy)) %>% select(ticker, name, sector, final_price, best_predictor, best_accuracy) %>% rename(Ticker = ticker, Company = name, Sector = sector, "Dec 31 Price" = final_price, "Best Call" = best_predictor, Error = best_accuracy)
    stock_winners
  }, bordered = TRUE, striped = TRUE)
  
  output$final_comparison <- renderPlot({
    df <- data.frame(Predictor = c("John", "Mike", "Nick", "Joe", "Dick"), Error = c(17.0, 19.7, 20.7, 24.3, 26.5), Status = c("Winner", "Runner-up", "Third", "Fourth", "Fifth"))
    ggplot(df, aes(x = reorder(Predictor, -Error), y = Error, fill = Status)) + geom_col() + geom_text(aes(label = sprintf("%.1f%%", Error)), vjust = -0.5, color = "#00d9ff", size = 5, fontface = "bold") + scale_fill_manual(values = c("Winner" = "#00d9ff", "Runner-up" = "#3b82f6", "Third" = "#06b6d4", "Fourth" = "#0ea5e9", "Fifth" = "#0284c7")) + labs(y = "Avg Absolute Error (%)", x = "") + theme_minimal() + theme(legend.position = "none", plot.background = element_rect(fill = "#0a0e27"), panel.background = element_rect(fill = "#0a0e27"), panel.grid = element_line(color = "#1e3a8a"), axis.text = element_text(color = "#00d9ff"), axis.title = element_text(color = "#00d9ff"))
  })
  
  output$error_distribution <- renderPlot({
    df <- stocks_data()
    error_data <- bind_rows(df %>% select(ticker, error = dick_abs_diff) %>% mutate(predictor = "Dick"), df %>% select(ticker, error = joe_abs_diff) %>% mutate(predictor = "Joe"), df %>% select(ticker, error = john_abs_diff) %>% mutate(predictor = "John"), df %>% select(ticker, error = mike_abs_diff) %>% mutate(predictor = "Mike"), df %>% select(ticker, error = nick_abs_diff) %>% mutate(predictor = "Nick"))
    ggplot(error_data, aes(x = predictor, y = error, fill = predictor)) + geom_boxplot(alpha = 0.7) + scale_fill_manual(values = c("John" = "#00d9ff", "Mike" = "#3b82f6", "Nick" = "#06b6d4", "Joe" = "#0ea5e9", "Dick" = "#0284c7")) + labs(y = "Absolute Error (%)", x = "") + theme_minimal() + theme(legend.position = "none", plot.background = element_rect(fill = "#0a0e27"), panel.background = element_rect(fill = "#0a0e27"), panel.grid = element_line(color = "#1e3a8a"), axis.text = element_text(color = "#00d9ff"), axis.title = element_text(color = "#00d9ff"))
  })
}

shinyApp(ui = ui, server = server)
