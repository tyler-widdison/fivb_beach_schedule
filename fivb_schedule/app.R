library(shiny)
library(DT)
library(dplyr)
library(ggplot2)
library(tidyr)
library(shinyWidgets)

load(file = 'tourn_df.rda')
load(file = 'match_df.rda')

# wanted tournaments
tourn <- match_df %>% filter(!TournamentType %in% c('51', '52', '', 'Test')) %>% distinct(TournamentType)

# Define the UI
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(width = 2,
                 hr(),
      pickerInput('type', 'Tournament type', choices = sort(tourn$TournamentType), options = list(`actions-box` = TRUE),multiple = T, 
                  selected = c('GrandSlam',
                               'MajorSeries',
                               'Open',
                               'WorldTour4Star',
                               'WorldTour5Star')),
      pickerInput('gender', 'Gender', choices = sort(unique(match_df$Gender)), multiple = T, 
                  selected = c('M', 
                               'W'))),
      mainPanel(width = 10,
                column(6,
                       plotOutput('teams')),
                column(6,
                       plotOutput('line_plot')),
                       DT::dataTableOutput('data'))
    )
  )



# Define the server code
server <- function(input, output) {
  output$data <- DT::renderDT({
    DT::datatable(df %>% 
                    filter(Gender %in% input$gender & Type %in% input$type & !Type %in% c('51', '52', '')) %>% 
                    select(-No, -Code, -Month, -Title, -EndDateMainDraw) %>% 
                    rename(`Start date` = StartDateQualification,
                           `MD teams` = NbTeamsMainDraw,
                           `Qual teams` = NbTeamsQualification),
                  rownames = F,
                  filter = "top",
                  caption = 'Tournaments played from selected fields',
                  options = list(dom = 'tlp',
                                 scrollX = T,
                                 scrollY="300px",
                                 pageLength = 400))
  })
  
    
  output$teams <- renderPlot({
      dat_team <- df %>%
        filter(Type %in% input$type & Gender %in% input$gender) %>% 
        group_by(Season) %>% 
        summarise(`Main draw` = sum(NbTeamsMainDraw),
                  `Quali` = sum(NbTeamsQualification),
                  diff = Quali-`Main draw`,
                  .groups = 'drop') %>% 
        pivot_longer(c(`Main draw`:Quali)) %>% 
        filter(!is.na(Season))
        
      mdt <- dat_team %>% 
        filter(name == 'Main draw')
      qt <- dat_team %>% 
        filter(name == 'Quali')
      
      dat_team %>% 
        filter(value != 0) %>% 
      ggplot() + 
        #add point range
        geom_segment(data = mdt, aes(x = value, y = Season, yend = qt$Season, xend = qt$value),
                     color = "#aeb6bf", size = 4.5, alpha = .5) + 
        
        geom_point(aes(x = value, y = Season, color = name), size = 4) +
        theme(legend.position="bottom") +
        
        #color points
        scale_color_manual(values = c("#009688","#762a83")) +

                #add facets for more control
        facet_grid(Season ~ ., scales = "free", switch = "y") + 
        ggtitle("FIVB number of qualification and main draw teams")+
        theme_minimal() +   
        theme(panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.ticks.x = element_line(color = "#4a4e4d"),
              strip.text.y.left  = element_text(angle = 0),
              panel.background = element_rect(fill = "white", color = "white"),
              strip.background = element_rect(fill = "white", color = "white"),
              plot.background = element_rect(fill = "white", color = "white"),
              panel.spacing = unit(0, "lines")) + 
        labs(x = '# of teams', 
             color = '')
      
    })
  
  output$line_plot <- renderPlot({
    dat_team <- df %>%
      filter(Type %in% input$type & Gender %in% input$gender) %>% 
      group_by(Season) %>% 
      summarise(`Main draw` = sum(NbTeamsMainDraw),
                `Quali` = sum(NbTeamsQualification),
                diff = Quali-`Main draw`,
                .groups = 'drop') %>% 
      pivot_longer(c(`Main draw`:Quali)) %>% 
      filter(!is.na(Season))
    
    dat_team %>% 
      ggplot(aes(Season, value, color = name)) + 
      scale_color_manual(values = c("#009688","#762a83")) +
      geom_line(size = 1) + 
      geom_point(size = 2, alpha = .4) +
      theme_bw() + 
      labs(x = '',
           y = '# of teams', 
           title = 'Qualifiaction teams apperence FIVB beach',
           caption = "Data: FIVB VIS") + 
      scale_x_continuous(breaks = seq(2000, 2022, 1)) + 
      theme(legend.title = element_blank())
      
  })
}

shinyApp(ui = ui, server = server)
