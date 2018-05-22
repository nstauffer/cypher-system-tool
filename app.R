#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(gridExtra)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Cypher System Interface"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(tabPanel(title = "d20",
                           numericInput(inputId = "difficulty",
                                        label = "Difficulty",
                                        value = 1,
                                        min = 1,
                                        max = 10,
                                        step = 1),
                           radioButtons(inputId = "stat",
                                        label = "Stat",
                                        choices = c("Intellect" = "intellect",
                                                    "Might" = "might",
                                                    "Speed" = "speed"),
                                        selected = character(0),
                                        inline = TRUE),
                           numericInput(inputId = "effort",
                                        label = "Effort",
                                        value = 0,
                                        min = 0,
                                        max = 1,
                                        step = 1),
                           numericInput(inputId = "assets",
                                        label = "Assets",
                                        value = 0,
                                        min = 0,
                                        max = 2,
                                        step = 1),
                           numericInput(inputId = "basecost",
                                        label = "Base Cost",
                                        value = 0,
                                        min = 0,
                                        max = 20,
                                        step = 1)),
                  tabPanel(title = "Other Rolls",
                           selectInput(inputId = "otherdie",
                                        label = "Number of die sides",
                                        choices = c(2, 4, 6, 8, 10, 100)),
                           # textInput(inputId = "rollstring",
                           #           label = "Roll",
                           #           placeholder = "2d6 + 2"),
                           selectInput(inputId = "otherdiestat",
                                       label = "",
                                       choices = c("Intellect" = "intellect",
                                                   "Might" = "might",
                                                   "Speed" = "speed")),
                           radioButtons(inputId = "otherdieaction",
                                        label = "Action",
                                        choices = c("Add" = "add",
                                                    "Subtract" = "subtract",
                                                    "None" = "none")))),

      actionButton(inputId = "rollbutton",
                   label = "ROLL!",
                   icon = icon(name = "fas fa-dice-six",
                               lib = "font-awesome")),
      textOutput("magnitude"),
      textOutput("statcurrenttext"),
      textOutput("projectedwarning"),
      textOutput("error")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel(title = "Results and Stats",
                 textOutput(outputId = "currentresult"),
                 plotOutput("statplot")),
        tabPanel(title = "Character Sheet",
                 textInput(inputId = "charname",
                           label = "Character Name"),
                 numericInput(inputId = "tier",
                              label = "Tier",
                              value = 1,
                              min = 1,
                              max = 6,
                              step = 1),
                 numericInput(inputId = "int",
                              label = "Intellect Pool Capacity",
                              value = 8,
                              min = 0,
                              step = 0),
                 numericInput(inputId = "mgt",
                              label = "Might Pool Capacity",
                              value = 8,
                              min = 0,
                              step = 0),
                 numericInput(inputId = "spd",
                              label = "Speed Pool Capacity",
                              value = 8,
                              min = 0,
                              step = 0),
                 numericInput(inputId = "edge",
                              label = "Edge",
                              value = 1,
                              min = 1,
                              max = 6,
                              step = 1),
                 actionButton(inputId = "charbutton",
                              label = "Submit Changes")),
        tabPanel(title = "Settings",
                 checkboxGroupInput(inputId = "statsbox",
                                    label = "Stats to reset",
                                    choices = c("Intellect" = "intellect",
                                                "Might" = "might",
                                                "Speed" = "speed"),
                                    selected = c("Intellect", "Might", "Speed")),
                 actionButton(inputId = "resetstats",
                              label = "Reset selected stat(s) to max"),
                 actionButton(inputId = "setcurrentstats",
                              label = "Set current value to value below for selected stat(s)"),
                 actionButton(inputId = "addtocurrentstats",
                              label = "Add value below to current value for selected stat(s)"),
                 numericInput(inputId = "manualvalue",
                              label = "Adjustment value",
                              value = 0,
                              step = 1))
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  temp <- reactiveValues(stats = data.frame(stat = c("intellect", "might", "speed"),
                                            max = c(0, 0 ,0),
                                            current = c(NA, NA, NA),
                                            projected = c(NA, NA, NA),
                                            stringsAsFactors = FALSE))
  
  
  #### FUNCTIONS ####
  # Roll a die of an arbitrary number of sides
  # Defaults to a d20
  die <- function(sides = 20){
    if (sides < 1) {
      stop("A die must have at least one side.")
    }
    # A simple call to runif()
    # Because floor(runif()) won't return the max unless min >> max - min, we'll set the max to sides + 1
    # And just in case that somehow does roll sides + 1, we'll reroll as long as the result is > sides
    result <- sides + 1
    while (result > sides){
      result <- floor(runif(n = 1,
                            min = 1,
                            max = sides + 1))
    }
    return(result)
  }
  
  # Make a roll against a target difficulty, taking into account effort and assets
  # Defaults to a d20, difficulty of 1, no effort applied, no assets
  roll <- function(difficulty = 1,
                   effort = 0,
                   assets = 0,
                   sides = 20){
    # Calculate the target value. The floor is 0
    target <- (difficulty - effort - assets) * 3
    if (target < 0){
      target <- 0
    }
    # Roll the die
    result <- die(sides)
    
    # Return the success status, the target value, and the result
    return(list("success" = result >= target,
                "target.value" = target,
                "die.value" = result))
  }
  
  # The first level of effort costs 3 points from the pool, then every subsequent level costs 2
  effort.cost <- function(level){
    if (level < 1){
      return(0)
    } else {
      return(3 + 2 * (level - 1))
    }
  }
  
  #### LOGIC ####
  # When the "Submit Changes" button is pressed, update the values for the character
  observeEvent(eventExpr = {input$charbutton},
               handlerExpr = {
                 temp$stats$max[temp$stats$stat == "intellect"] <- input$int
                 temp$stats$max[temp$stats$stat == "might"] <- input$mgt
                 temp$stats$max[temp$stats$stat == "speed"] <- input$spd
                 
                 # If there's no current value, set it to the max value
                 temp$stats$current[is.na(temp$stats$current) & !is.na(temp$stats$max)] <- temp$stats$max[is.na(temp$stats$current) & !is.na(temp$stats$max)]
                 
                 updateNumericInput(session = session,
                                    inputId = "effort",
                                    max = input$tier)
               })
  
  # When the player hits the roll button, make the roll
  observeEvent(eventExpr = {input$rollbutton},
               handlerExpr = {
                 if (is.null(input$stat)) {
                   output$error <- renderText(paste("Error: No stat selected"))
                 } else if (any(is.na(temp$stats$current))) {
                   output$error <- renderText(paste("Error: Not all character stat pools are set"))
                 } else {
                   output$error <- renderText(NULL)
                   if (temp$stats$projected[temp$stats$stat == input$stat] < 0){
                     message(paste("You don't have that many", input$stat, "points to spend"))
                   } else {
                     # Make the roll
                     temp$recentroll <- roll(difficulty = input$difficulty,
                                             effort = input$effort,
                                             assets = input$assets)
                     # Store the things separately
                     temp$rollsuccess <- temp$recentroll$success
                     temp$rollvalue <- temp$recentroll$die.value
                     temp$targetvalue <- temp$recentroll$target.value
                     # Update the stat pool
                     temp$stats$current[temp$stats$stat == input$stat] <- temp$stats$projected[temp$stats$stat == input$stat]
                     
                     output$currentresult <- renderText(
                       if (is.na(temp$rollsuccess)) {
                         NULL
                       } else if (temp$rollsuccess){
                         paste0("You rolled a ", temp$rollvalue, ", succeeding against a target of ", temp$targetvalue, "!")
                       } else {
                         paste0("You rolled a ", temp$rollvalue, ", a failure ", temp$targetvalue, ".")})
                   }
                   
                   temp$stats$projected[temp$stats$stat == input$stat] <- temp$stats$current[temp$stats$stat == input$stat] - max(c(effort.cost(level = input$effort) + input$basecost - input$edge, 0))
                   temp$stats$projected[temp$stats$stat != input$stat] <- NA
                   
                 }
               })
  
  # When the player adjusts the effort exerted, show the cost as projected value
  observeEvent(eventExpr = {input$effort
    input$assets
    input$stat
    input$charbutton
    temp$stats},
               handlerExpr = {
                 temp$stats$projected[temp$stats$stat == input$stat] <- temp$stats$current[temp$stats$stat == input$stat] - max(c(effort.cost(level = input$effort) + input$basecost - input$edge, 0))
                 temp$stats$projected[temp$stats$stat != input$stat] <- NA
               })
  
  # When the player changes settings
  observeEvent(eventExpr = {input$resetstats},
               handlerExpr = {
                 temp$stats$current[temp$stats$stat %in% input$statsbox] <- temp$stats$max[temp$stats$stat %in% input$statsbox]
               })
  observeEvent(eventExpr = {input$setcurrentstats},
               handlerExpr = {
                 temp$stats$current[temp$stats$stat %in% input$statsbox] <- input$manualvalue
               })
  observeEvent(eventExpr = {input$addtocurrentstats},
               handlerExpr = {
                 temp$stats$current[temp$stats$stat %in% input$statsbox] <- temp$stats$current[temp$stats$stat %in% input$statsbox] + input$manualvalue
               })
  
  #### PRODUCING RENDERED OUTPUTS ####
  # The stat figure
  output$statplot <- renderPlot({
    # Only render once the maxes are set
    if (!all(temp$stats$max)) {
      NULL
    } else {
      stats.tidy <- dplyr::mutate(tidyr::gather(temp$stats, value = value, key = "valuetype", -stat),
                                  stat = toupper(stat),
                                  valuetype = toupper(valuetype))
      
      plots <- lapply(split(stats.tidy, stats.tidy$stat),
                     max.y = max(stats.tidy$value[stats.tidy$valuetype == "MAX"]),
                     palettes = list("INTELLECT" = c("MAX" = "#5E5EB0",
                                                     "CURRENT" = "#767CBE",
                                                     "PROJECTED" = "#909BCB"),
                                     "MIGHT" = c("MAX" = "#69C87E",
                                                 "CURRENT" = "#85D786",
                                                 "PROJECTED" = "#B0E6A3"),
                                     "SPEED" = c("MAX" = "#B99346",
                                                 "CURRENT" = "#C5AC5C",
                                                 "PROJECTED" = "#D1C373")),
                     FUN = function(X, max.y, palettes){
                       plot <- ggplot() +
                         ### COLUMNS
                         ## MAX
                         geom_col(data = dplyr::filter(X, valuetype == "MAX"),
                                  aes(x = stat,
                                      y = value,
                                      fill = c(valuetype)),
                                  width = 0.6) +
                         ## CURRENT
                         geom_col(data = dplyr::filter(X, valuetype == "CURRENT"),
                                  aes(x = stat,
                                      y = value,
                                      fill = valuetype),
                                  width = 0.6) +
                         ## PROJECTED
                         geom_col(data = dplyr::filter(X, valuetype == "PROJECTED"),
                                  aes(x = stat,
                                      y = value,
                                      fill = valuetype),
                                  width = 0.6) +
                         ### HORIZONTAL LINES
                         # Note that for all the line segments, the x values are just hardcoded. SORRY.
                         ## MAX
                         geom_segment(aes(x = c(0.6),
                                          xend = c(1.4),
                                          y = dplyr::filter(X, valuetype == "MAX")$value,
                                          yend = dplyr::filter(X, valuetype == "MAX")$value), 
                                      lty = 3,
                                      lwd = 1,
                                      colour = "black") +
                         ### AESTHETICS
                         scale_fill_manual(values = palettes[[X$stat[1]]]) +
                         scale_y_continuous(breaks = 1:(max.y),
                                            limits = c(0, 1 + max.y),
                                            expand = c(0,0)) +
                         scale_x_discrete(expand = c(0, 0.02)) +
                         labs(y = "POINTS") +
                         theme(legend.position = "none",
                               panel.background = element_blank(),
                               axis.title.x = element_blank(),
                               axis.ticks.x = element_blank()) +
                         theme(axis.text.y = element_blank(),
                               axis.title.y = element_blank(),
                               axis.ticks.y = element_blank()) +
                         annotate("text",
                                  label = paste(X$value[X$valuetype == "MAX"]),
                                  x = 1,
                                  y = X$value[X$valuetype == "MAX"] + 0.1,
                                  hjust = 0.5,
                                  vjust = 0)
                       
                       if (X$value[X$valuetype == "CURRENT"] != X$value[X$valuetype == "MAX"]) {
                         plot <- plot +
                           annotate("text",
                                    label = paste(X$value[X$valuetype == "CURRENT"]),
                                    x = 1,
                                    y = X$value[X$valuetype == "CURRENT"] + 0.1,
                                    hjust = 0.5,
                                    vjust = 0)
                       }
                       
                       if (!is.na(X$value[X$valuetype == "PROJECTED"])) {
                         plot <- plot +
                           annotate("text",
                                    label = paste(X$value[X$valuetype == "PROJECTED"]),
                                    x = 1,
                                    y = X$value[X$valuetype == "PROJECTED"] + 0.1,
                                    hjust = 0.5,
                                    vjust = 0)
                       }
                       
                       
                       return(plot)
                     })
      
      gridExtra::grid.arrange(nrow = 1, ncol = 3, grobs = plots)
    }
  })
  
  # The various text things
  output$magnitude <- renderText({
    if (is.null(temp$rollvalue)) {
      NULL
    } else {
      switch(as.character(temp$rollvalue),
             "1" = {paste("Free GM Intrusion")},
             "17" = {paste("Minor Effect")},
             "18" = {paste("Minor Effect")},
             "19" = {paste("Major Effect")},
             "20" = {paste("Major Effect")})
    }
  })
  
  output$statcurrenttext <- renderText({
    if (any(is.na(c(temp$stats$current, temp$stats$projected, temp$stats$max))) | is.null(input$stat)) {
      NULL
    } else {
      paste0(input$stat, ": ",
             temp$stats$current[temp$stats$stat == input$stat], "/", temp$stats$max[temp$stats$stat == input$stat],
             " \u2192 ",
             temp$stats$projected[temp$stats$stat == input$stat], "/", temp$stats$max[temp$stats$stat == input$stat])
    }
  })
  
  output$projectedwarning <- renderText({
    if (!is.null(input$stat)) {
      if (!is.na(temp$stats$projected[temp$stats$stat == input$stat])) {
        NULL
      } else if (temp$stats$projected[temp$stats$stat == input$stat] < 0){
        (paste("You don't have that many", input$stat, "points to spend"))
      } else {
        NULL
      }
    } else {
      NULL
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

