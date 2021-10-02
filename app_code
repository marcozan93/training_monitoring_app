#Load libraries
library(tidyverse)
library(readxl)
library(lubridate)
library(janitor)
library(hms)

library(ggplot2)
library(ggrepel)
library(patchwork)
library(tidytext)

library(shiny)
library(shinythemes)
library(rintrojs)
library(png)
library(rsconnect)


#USER INTERFACE
ui <- fluidPage(
  titlePanel(tagList(
    img(src = "bath_rugby_logo.png", height = 60, width = 60),
    span("TRAINING MONITORING APP by MZ"))),
  navbarPage(title = NULL, 
    tabPanel("Distance",
  # select position group input
  fluidRow(column(4, selectInput(input = "position2",
              label = "Position",
              choices = data_base_1 %>% ungroup() %>% distinct(., position) %>% pull(position),
              selected = "Hooker")),
           column(4,
           selectInput(input = "competitive_season2",
                       label = "Competitive season",
                       choices = data_base_1 %>% ungroup() %>% distinct(., competitive_season) 
                       %>% pull(competitive_season),
                       selected = "2019-2020"))),
  # select week input
  checkboxGroupInput(inputId = "week2",
                     label = "Week",
                     choices = data_base_1 %>% ungroup() %>% distinct(., week) %>% pull(week),
                     selected = "1",
                     inline = TRUE),
  mainPanel(tabsetPanel(type = "tabs",
                        tabPanel("Tot Distance", plotOutput("tl.plot", width = 1300, heigh = 700)),
                        tabPanel("HSR", plotOutput("hsr.plot", width = 1300, height = 700)),
                        tabPanel("Efforts", plotOutput("efforts.plot", width = 1300, height = 700))),
            tabsetPanel(type = "tabs",
                        tabPanel("Total Distance", tableOutput(outputId = "tl.tbl")),
                        tabPanel("Total HSR Distance", tableOutput(outputId = "hsr.tbl")),
                        tabPanel("Total Efforts", tableOutput(outputId = "efforts.tbl"))))),
  tabPanel("Speed",
  # select player
  selectInput(input = "name2",
              label = "Name",
              choices = data_base %>% distinct(., name) %>% pull(name),
              selected = "AC"),
  dateRangeInput(inputId = "daterange2", 
                 label = "Date range:",
                 start = min(data_base$date),
                 end = max(data_base$date),
                 min = min(data_base$date),
                 max = max(data_base$date)),
  mainPanel(tabsetPanel(type = "tabs",
                        tabPanel("Daily top speed", plotOutput("speed.plot", width = 1300, height = 400)),
                        tabPanel("Weekly top speed", plotOutput("week.speed.plot", width = 1300, height = 400))))),
  tabPanel("Games",
           # select competitive_season
           fluidRow(column(4,
                           selectInput(input = "competitive_season3",
                       label = "Competitive season",
                       choices = data_base_1 %>% ungroup() %>% 
                         distinct(., competitive_season) %>% pull(competitive_season),
                       selected = "2019-2020")),
                    column(8, p(
                      "Graphs represent data collected exclusively for the first
                      half of Gallagher games for players involved in the full first half."
                    ))),
           mainPanel(plotOutput("games.plot", width = 1300, height = 1600))),
  theme = shinytheme("slate")
))

#SERVER
server <- function(input, output){
  # get data for plot
  dat1 <- reactive({
    d <- data_base_1 %>% 
      filter(competitive_season %in% input$competitive_season2,
             position %in% input$position2,
             week %in% input$week2)
    d
  })
  
  # make plot tl.plot
  output$tl.plot <- renderPlot({
    d <- dat1()
    tl.plot <- ggplot(d, aes(x = week, y = total_distance, group = 1)) +
      #ggplot(d, aes(x = week, y = total_distance, group = 1)) +
      geom_col( fill = "darkcyan") +
      geom_text(aes(y = total_distance, 
                    label = round(total_distance, 0)), 
                color = "white", 
                vjust = 1.5) +
      facet_wrap(~name) +
      labs(x = "Training week",
           y = "Total distance (m)")
    
    print(tl.plot)
  }, width = 1300, height = 700)
  
  # get data for hsr plot
  dat3 <- reactive({
    d <- data_base_1 %>% filter(competitive_season %in% input$competitive_season2,
                                position %in% input$position2,
                                week %in% input$week2) %>% 
      select(-total_distance, -vhsr_efforts, -sprint_efforts) %>%
      pivot_longer(c("hsr_distance", "vhsr_distance", "sprint_distance"),
                   names_to = "bands", values_to = "distance") %>%
      group_by(name) %>%
      mutate(y_label = cumsum(distance))
    d
  })
  
  # make plot hsr.plot
  output$hsr.plot <- renderPlot({
    d <- dat3()
    hsr.plot <- ggplot(d, aes(x = week, y = distance, fill = bands, group = 1)) + 
      geom_col() +
      #geom_text(aes(y = y_label, label = round(distance, 0)), 
      #          vjust = 1.5, color = "white") +
      facet_wrap(~name) +
      labs(x = "Training week",
           y = "High, Very-high, Sprint distance (m)")
    
    print(hsr.plot)
  }, width = 1300, height = 700)
  
  # get data for efforts plot
  dat4 <- reactive({
    d <- data_base_1 %>% filter(competitive_season %in% input$competitive_season2,
                                position %in% input$position2,
                                week %in% input$week2) %>% 
      select(-total_distance, -hsr_distance, -vhsr_distance, -sprint_distance) %>%
      pivot_longer(c("vhsr_efforts", "sprint_efforts"),
                   names_to = "bands", values_to = "efforts")
    
    d
  })
  
  # plot efforts
  output$efforts.plot <- renderPlot({
    d <- dat4()
    efforts.plot <-  ggplot(d, aes(x = week, y = efforts, fill = bands)) + 
      geom_col(position = "dodge") +
      geom_text(aes(label = efforts), 
                vjust = 1.5, color = "white") +
      facet_wrap(~name) +
      labs(x = "Training week",
           y = "Very-high & Sprint efforts (n)")
    
    print(efforts.plot)
  }, width = 1300, height = 700)
  
  # get data for total distance table
  dat2 <- reactive({
    d <- data_base_1 %>% 
      filter(competitive_season %in% input$competitive_season2,
             position %in% input$position2,
             week %in% input$week2) %>%
      select(name, week, total_distance) %>%
      mutate(total_distance = round(total_distance, 0)) %>%
      pivot_wider(.,
                  names_from = week,
                  values_from = total_distance)
    
    d
  })
  
  # build the total distance table
  output$tl.tbl <- renderTable(dat2(), align = "l")
  
  # get data for sprint distance table
  dat5 <- reactive({
    d <- data_base_1 %>% 
      filter(competitive_season %in% input$competitive_season2,
             position %in% input$position2,
             week %in% input$week2) %>%
      select(name, week, hsr_distance, vhsr_distance, sprint_distance) %>%
      mutate_if(is.numeric, round) %>% 
      group_by(name, week) %>%
      transmute(total_hsr_distance = sum(hsr_distance, vhsr_distance, sprint_distance)) %>%
      pivot_wider(.,
                  names_from = week,
                  values_from = total_hsr_distance)
    
    d
  })
  
  # build the total distance table
  output$hsr.tbl <- renderTable(dat5(), align = "l")
  
  # get data for sprint distance table
  dat6 <- reactive({
    d <- data_base_1 %>% 
      filter(competitive_season %in% input$competitive_season2,
             position %in% input$position2,
             week %in% input$week2) %>%
      select(name, week, vhsr_efforts, sprint_efforts) %>%
      group_by(name, week) %>%
      transmute(total_efforts = sum(vhsr_efforts, sprint_efforts)) %>%
      pivot_wider(.,
                  names_from = week,
                  values_from = total_efforts)
    
    d
  })
  
  # build the total efforts table
  output$efforts.tbl <- renderTable(dat6(), align = "l")
  
  # get data for speed plot
    dates <- reactive({ data_base %>% filter(name %in% input$name2,
             date >= input$daterange2[1] & date <= input$daterange2[2]) %>%
      group_by(date) %>% summarise_if(is.numeric, max)
  })
  
  # make plot speed.plot
  output$speed.plot <- renderPlot({
    vmax.plot <- ggplot(data = dates(), aes(x = date, y = vmax)) + 
      geom_point(data = dates(), aes(x = date, y = vmax), size = 3, color = "steelblue") +
      geom_line(data = dates(), aes(x = date, y = vmax), color = "steelblue") +
      geom_text_repel(data = dates(), aes(x = date, y = vmax, label = round(vmax,1))) +
      xlab("Date") + ylab("Top Speed (m/s)")
    
    vmax_per.plot <- ggplot(data = dates(), aes(x = date, y = vmax_per)) + 
      geom_point(data = dates(), aes(x = date, y = vmax_per), size = 3, color = "blue") +
      geom_line(data = dates(), aes(x = date, y = vmax_per), color = "blue", linetype = 2) +
      geom_text_repel(data = dates(), aes(x = date, y = vmax_per, label = round(vmax_per,1))) +
      xlab("Date") + ylab("Percentage top speed (%)")
    
    speed.plot <- vmax_per.plot / vmax.plot
    
    print(speed.plot)
  }, width = 1300, height = 400)
  
  # get data for week.speed.plot
  dates <- reactive({ data_base %>% filter(name %in% input$name2,
                                           date >= input$daterange2[1] & date <= input$daterange2[2]) %>%
      group_by(week, date) %>% summarise_if(is.numeric, max)
  })
  
  # make plot week.speed.plot
  output$week.speed.plot <- renderPlot({
    week.vmax.plot <- ggplot(data = dates(), aes(x = as.numeric(week), y = vmax)) + 
      geom_point(data = dates(), aes(x = as.numeric(week), y = vmax), size = 3, color = "steelblue") +
      geom_text_repel(data = dates(), aes(x = as.numeric(week), y = vmax, label = round(vmax,1))) +
      geom_smooth(data = dates(), aes(x = as.numeric(week), y = vmax)) +
      xlab("Week") + ylab("Top speed (m/s)")
    
    week.vmax_per.plot <- ggplot(data = dates(), aes(x = as.numeric(week), y = vmax_per)) + 
      geom_point(data = dates(), aes(x = as.numeric(week), y = vmax_per), 
                 size = 3, shape = "triangle", color = "blue") +
      geom_text_repel(data = dates(), aes(x = as.numeric(week), y = vmax_per, label = round(vmax_per,1))) +
      geom_smooth(data = dates(), aes(x = as.numeric(week), y = vmax_per)) +
      xlab("Week") + ylab("Percentage top speed (%)")
    
    week.speed.plot <- week.vmax_per.plot / week.vmax.plot
    
    print(week.speed.plot)
  }, width = 1300, height = 400)
  
  # get data for games.plot
  games_df_1 <- reactive({
    games <- data_base %>% filter(day_of_training_cycle == "Match (Premiership)", 
                                    session_type == "Full session", 
                                    session_specifics == "Match - Premiership (first half)",
                                    duration > 40
    ) %>%
      group_by(date, position, week, competitive_season, opposition) %>% 
      summarise_if(is.numeric, mean) %>%
      mutate(opposition = as.character(opposition))
    
    # Differentiate Exeter (A) season and Exeter (A) semi-final: 
    for(i in seq(1, nrow(games), 1)) {
      if (games[i,"date"] == "2020-10-10") {
        games[i, "opposition"] <- "Exeter (A) SF"
      } else {}
    }
    
    # Select only columns I am interested in:    
    x1 <- games %>% mutate(opposition = as_factor(opposition)) %>%
      arrange(desc(m_min)) %>% select(1:5, m_min, accel_density_100)
    
    x1$position_f <- factor(x1$position, levels = c("Prop", "Hooker", "Second Row", "Back Row", "Scrum Half", "Fly Half", "Centre", "Wing", "Full Back"))
    
    games_df <- x1 %>% filter(competitive_season %in% input$competitive_season3)
    
    games_df
  })
  
  # make plot games.plot
  output$games.plot <- renderPlot({
    d <- games_df_1()
    m.min.plot <- d %>% ggplot(aes(x = reorder_within(opposition, m_min, position), y = m_min)) + 
      geom_col(aes(fill = position)) + 
      coord_flip() + 
      scale_x_reordered() + 
      facet_wrap(.~position_f, scales = "free_y") +
      theme(legend.position = "none") +
      labs(x = "Premiership Game", 
           y = "Meters per minute",
           title = "GALLAGHER PREMIERSHIP: M.MIN")
    
    
    # Graph for Position & Accel Density
    acc.den.plot <- d %>% ggplot(aes(x = reorder_within(opposition, accel_density_100, position), y = accel_density_100)) + 
      geom_col(aes(fill = position)) + 
      coord_flip() + 
      scale_x_reordered() + 
      facet_wrap(.~position_f, scales = "free_y") +
      theme(legend.position = "none") +
      labs(x = "Premiership Game",
           y = "Avg accel decel (m/s^2)",
           title = "GALLAGHER PREMIERSHIP: AVERAGE ACCELERATION DECELERATION")
    
    games.plot <- m.min.plot / acc.den.plot
    
    print(games.plot)
  }, width = 1300, height = 1600)
  
}

shinyApp(ui, server)
