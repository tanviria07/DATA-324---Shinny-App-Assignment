
library(shiny)
library(tidyverse)

wc <- read_csv("world_cup_stats.csv", show_col_types = FALSE)


wc <- wc |>
  mutate(
    across(c(Age, Born, MP, Starts, Min, Gls:CrdR), as.integer),
    Position = str_sub(Pos, 1, 2),
    Position = factor(Position,
                      levels = c("GK", "DF", "MF", "FW"),
                      labels = c("Goalkeeper", "Defender", "Midfielder", "Forward"))
  )

teams <- sort(unique(wc$Squad))



stat_choices <- c(
  "Age" = "Age",
  "Matches played" = "MP",
  "Games started" = "Starts",
  "Minutes played" = "Min",
  "Goals" = "Gls",
  "Assists" = "Ast",
  "Goals + assists" = "GA",
  "Non-penalty goals" = "GnPK",
  "Penalty kicks made" = "PK",
  "Penalty kicks attempted" = "PKatt",
  "Yellow cards" = "CrdY",
  "Red cards" = "CrdR",
  "Goals per 90 min" = "Gls90",
  "Assists per 90 min" = "Ast90",
  "Goals + assists per 90 min" = "GA90",
  "Non-penalty goals per 90 min" = "GnPK90",
  "Non-penalty goals + assists per 90 min" = "GAnPK90"
)

# gets the readable name of a stat for axis labels
stat_label <- function(var) {
  names(stat_choices)[stat_choices == var]
}


ui <- fluidPage(
  titlePanel("World Cup 2026: Comparing Two Squads"),

  sidebarLayout(
    sidebarPanel(
      selectInput("team1", "Team 1", choices = teams, selected = "Argentina"),
      selectInput("team2", "Team 2", choices = teams, selected = "France"),

      hr(),

      selectInput("x_var", "X-axis variable", choices = stat_choices, selected = "Min"),
      selectInput("y_var", "Y-axis variable", choices = stat_choices, selected = "GA"),

      hr(),

      checkboxGroupInput("positions", "Positions to show",
                         choices = levels(wc$Position),
                         selected = levels(wc$Position)),

      sliderInput("min_minutes", "Minimum minutes played",
                  min = 0, max = max(wc$Min), value = 0, step = 10),
      helpText("The per 90 stats get weird for players who only came on for a",
               "few minutes (one goal in 3 minutes = 30 goals per 90), so it",
               "helps to raise this when looking at those.")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Scatter plot",
                 plotOutput("scatter", height = "500px", click = "scatter_click"),
                 helpText("Click on a point to see which player(s) are there."),
                 tableOutput("clicked_players")),
        tabPanel("Boxplots",
                 plotOutput("box_x", height = "350px"),
                 plotOutput("box_y", height = "350px"))
      )
    )
  )
)


server <- function(input, output, session) {

  # just the two chosen teams, after the position and minutes filters
  selected_data <- reactive({
    validate(
      need(input$team1 != input$team2, "Pick two different teams to compare."),
      need(length(input$positions) > 0, "Select at least one position.")
    )

    wc |>
      filter(Squad %in% c(input$team1, input$team2),
             Position %in% input$positions,
             Min >= input$min_minutes) |>
      # keeps team 1 as the first color in the legend
      mutate(Squad = factor(Squad, levels = c(input$team1, input$team2)))
  })

  output$scatter <- renderPlot({
    df <- selected_data()
    validate(need(nrow(df) > 0, "No players match these filters."))

    ggplot(df, aes(x = .data[[input$x_var]], y = .data[[input$y_var]],
                   color = Squad, shape = Position)) +
      geom_point(size = 3.5, alpha = 0.7) +
      # the default shape for the 4th group is a thin "+" that's really hard
      # to see, and that's the forwards (the most interesting ones), so I
      # picked the shapes myself
      scale_shape_manual(values = c(Goalkeeper = 15, Defender = 16,
                                    Midfielder = 17, Forward = 18)) +
      labs(x = stat_label(input$x_var),
           y = stat_label(input$y_var),
           color = "Team",
           title = paste(input$team1, "vs.", input$team2)) +
      # team legend on top, position legend below it
      guides(color = guide_legend(order = 1),
             shape = guide_legend(order = 2)) +
      theme_minimal(base_size = 15)
  })

  
  
  output$clicked_players <- renderTable({
    req(input$scatter_click)

    near <- nearPoints(selected_data(), input$scatter_click,
                       xvar = input$x_var, yvar = input$y_var,
                       threshold = 10)
    req(nrow(near) > 0)

    near |>
      select(Player, Squad, Pos, Club, Min, all_of(unique(c(input$x_var, input$y_var))))
  })

  make_boxplot <- function(var) {
    df <- selected_data()
    validate(need(nrow(df) > 0, "No players match these filters."))

    ggplot(df, aes(x = Squad, y = .data[[var]], fill = Squad)) +
      geom_boxplot(alpha = 0.5, outlier.shape = NA) +
      geom_jitter(width = 0.15, height = 0, size = 2, alpha = 0.6) +
      labs(x = NULL, y = stat_label(var)) +
      theme_minimal(base_size = 15) +
      theme(legend.position = "none")
  }

  output$box_x <- renderPlot(make_boxplot(input$x_var))
  output$box_y <- renderPlot(make_boxplot(input$y_var))
}

shinyApp(ui = ui, server = server)
