library(shiny)
library(tidyverse)
library(plotly)
library(beeswarm)
library(shinyWidgets) # Required for horizontal buttons
# --- Data Preparation ---
raw_data <- read_csv("df_FINAL.csv", show_col_types = FALSE)

df_processed <- raw_data |> 
  distinct(team_abbr, lineup, minutes, off_pts, deftov, defts, offorb, deforb, off_3pt, ast, offtov) |> 
  group_by(team_abbr) |> 
  summarise(
    avg_off = weighted.mean(off_pts, minutes),
    avg_perim = weighted.mean(deftov, minutes), 
    avg_int = weighted.mean(defts, minutes),
    avg_reb = weighted.mean(offorb + deforb, minutes),
    avg_3pt = weighted.mean(off_3pt, minutes),
    avg_ast = weighted.mean(ast - offtov, minutes), .groups="drop"
  ) |>
  # Scaling everything to Z-scores (Mean = 0, SD = 1)
  mutate(across(starts_with("avg"), ~as.numeric(scale(.x)), .names = "z_{.col}")) |>
  mutate(url_team = paste0("https://a.espncdn.com/i/teamlogos/nba/500/", team_abbr, ".png"))

# Define choices using the Z-score column names
metric_choices <- c(
  "Offense" = "z_avg_off",
  "Perimeter Defense" = "z_avg_perim",
  "Interior Defense" = "z_avg_int",
  "Rebounding" = "z_avg_reb",
  "3PT Shooting" = "z_avg_3pt",
  "Point Guard Play" = "z_avg_ast"
)

# --- UI ---
ui <- fluidPage(
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  titlePanel("NBA Team Performance Distributions"),
  
  fluidRow(
    column(12, align = "center",
           # Horizontal Button Group
           radioGroupButtons(
             inputId = "target_var",
             label = NULL, 
             choices = metric_choices,
             selected = "z_avg_off",
             justified = TRUE, # Makes buttons stretch to fill width
             checkIcon = list(yes = icon("check"))
           )
    )
  ),
  
  fluidRow(
    column(12,
           plotlyOutput("teamPlot", height = "600px")
    )
  )
)

# --- Server ---
server <- function(input, output, session) {
  
  df_reactive <- reactive({
    req(input$target_var)
    
    df_processed |>
      mutate(
        current_z = .data[[input$target_var]],
        # Recalculate swarm Y based on the Z-score distribution
        swarm_y = swarmy(current_z, 0,
                         xsize = 2, 
                         cex = 2, side = 1, priority = "random")$y,
        tooltip_html = paste0(
          "<b>Team:</b> ", team_abbr, "<br>",
          "<b>Z-Score:</b> ", round(current_z, 2), " SD"
        )
      )
  })
  
  output$teamPlot <- renderPlotly({
    df <- df_reactive()
    var_label <- names(metric_choices)[metric_choices == input$target_var]
    
    nba_logos <- lapply(1:nrow(df), function(i) {
      list(
        source = df$url_team[i],
        xref = "x", yref = "y",
        x = df$current_z[i],
        y = df$swarm_y[i],
        sizex = 0.5, sizey = 0.5, # Adjusted for Z-score scale
        xanchor = "center", yanchor = "middle"
      )
    })
    
    plot_ly(df, x = ~current_z, y = ~swarm_y, type = 'scatter', mode = 'markers',
            marker = list(size = 30, opacity = 0), text = ~tooltip_html, hoverinfo = 'text') %>%
      layout(
        title = paste("League Standings:", var_label),
        # Fixed range from -3 to +3 SD for a consistent visual scale
        xaxis = list(title = "Standard Deviations from Average", 
                     range = c(-3.5, 3.5), zeroline = TRUE),
        yaxis = list(visible = FALSE, range = c(-1, max(df$swarm_y) + 1)),
        images = nba_logos
      )
  })
}

shinyApp(ui, server)