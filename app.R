library(shiny)
library(tidyverse)
library(shinythemes)

source('getStats.R')

ui <- fluidPage(theme = shinytheme("flatly"),
  navbarPage(
    
    'USPBL',
    tabPanel('Sortable Player Stats',
             fluidRow(
               column(4,
                      wellPanel(
                        selectInput('horp', NULL, c('Hitting', 'Pitching'))
                      )
               ),
               column(4,
                      wellPanel(
                        selectInput('team', NULL, c('All Teams', 'Beavers', 'Diamond Hoppers', 'Unicorns', 'Woolly Mammoths'))
                      )
               ),
               column(4,
                      wellPanel(
                        selectInput('stattype', NULL, c('Basic', 'Advanced', 'Plate Discipline', 'Batted Ball Profile'))
                      )
               )
             ),
             dataTableOutput("fullstats")
    ),
    tabPanel('Sortable Team Stats',
             fluidRow(
               column(6,
                      wellPanel(
                        selectInput('thorp', NULL, c('Hitting', 'Pitching'))
                      )
               ),
               column(6,
                      wellPanel(
                        selectInput('tstattype', NULL, c('Basic', 'Advanced', 'Plate Discipline', 'Batted Ball Profile'))
                      )
               )
             ),
             dataTableOutput("teamstats")
             
    ),
    tabPanel('Pitcher Breakdown',
             navlistPanel(
               'Pitcher Breakdown',
               selectInput('pitname', NULL, activeroster[activeroster$Type == 'Pitcher', 1]),
               tabPanel('Stats',
                        dataTableOutput("pitbasic"),
                        dataTableOutput("pitadv"),
                        dataTableOutput("pitpd"),
                        dataTableOutput("pitbb"),
                        dataTableOutput("pitplat"),
                        dataTableOutput("pitpt")
                    ),
               tabPanel('Game Logs',
                        dataTableOutput("pitlogs")
                    ),
               tabPanel('Pitch Arsenal',
                        dataTableOutput('pitchmetrics'),
                        sidebarLayout(
                          sidebarPanel(
                            selectInput('pitselect', NULL, c(''))
                          ),
                          mainPanel(
                            plotOutput('pitchlogs')
                          )
                        )
                    )
             )
    ),
    tabPanel('Hitter Breakdown',
             navlistPanel(
               'Hitter Breakdown',
               selectInput('hitname', NULL, activeroster[activeroster$Type == 'Hitter', 1]),
               tabPanel('Stats',
                        dataTableOutput("hitbasic"),
                        dataTableOutput("hitadv"),
                        dataTableOutput("hitpd"),
                        dataTableOutput("hitbb"),
                        dataTableOutput("hitplat"),
                        dataTableOutput("hitpt")
               ),
               tabPanel('Game Logs',
                        dataTableOutput("hitlogs")
               ),
               tabPanel('Spray Chart',
                        sidebarLayout(
                          sidebarPanel(
                            selectInput('hitselect', 'Batted Ball Type', c('All', 'Hits', 'Non-Hits'))
                          ),
                          mainPanel(
                            plotOutput('spraychart')
                          )
                        )
               ),
               tabPanel('Heat Map',
                        plotOutput('heatmap'))
             )
    ),
    tabPanel('Split Tool',
             fluidRow(
               column(3,
                      wellPanel(
                        selectInput('hitorpit', NULL, c('Hitting', 'Pitching')),
                        selectInput('playerorteam', NULL, c('By Player', 'By Team'))
                      )
               ),
               column(3,
                      wellPanel(
                        sliderInput('startdate', 'Start Date', value = min(master[,3]),
                                    min = min(master[,3]), max = max(master[,3])),
                        sliderInput('enddate', 'End Date', value = max(master[,3]),
                                    min = min(master[,3]), max = max(master[,3]))
                      )
               ),
               column(3,
                      wellPanel(
                        selectInput('tm', 'Team', c('Any', 'BB', 'DH', 'UU', 'WM')),
                        selectInput('opp', 'Opponent', c('Any', 'BB', 'DH', 'UU', 'WM')),
                        selectInput('ha', 'Home or Away', c('Any', 'Home', 'Away'))
                      )
               ),
               column(3,
                      wellPanel(
                        selectInput('handbat', 'Hand/Bat', c()),
                        selectInput('splitplat', 'Platoon', c()),
                        selectInput('splitpt', 'Pitch Type', c('Any', 'Fastball', 'Breaking Ball', 'Changeup'))
                      )
               )
             ),
             dataTableOutput("splitstats")
    )
  )

)

server <- function(input, output, session) {

  output$fullstats <- renderDataTable({
    
    if (input$horp == 'Hitting') {
      
      if (input$stattype == 'Basic') {
        expr <- basichitting
      }
      
      else if (input$stattype == 'Advanced') {
        expr <- advhitting
      }
      
      else if (input$stattype == 'Plate Discipline') {
        expr <- pdhitting
      }
      
      else if (input$stattype == 'Batted Ball Profile') {
        expr <- bbhitting
      }
      
    }
    
    else if (input$horp == 'Pitching') {
      
      if (input$stattype == 'Basic') {
        expr <- basicpitching
      }
      
      else if (input$stattype == 'Advanced') {
        expr <- advpitching
      }
      
      else if (input$stattype == 'Plate Discipline') {
        expr <- pdpitching
      }
      
      else if (input$stattype == 'Batted Ball Profile') {
        expr <- bbpitching
      }
      
    }
    
    if (input$team == 'Beavers') {
      expr <- expr %>%
        filter(Team == 'BB') %>%
        select(-Team)
    }
    
    else if (input$team == 'Diamond Hoppers') {
      expr <- expr %>%
        filter(Team == 'DH') %>%
        select(-Team)
    }
    
    else if (input$team == 'Unicorns') {
      expr <- expr %>%
        filter(Team == 'UU') %>%
        select(-Team)
    }
    
    else if (input$team == 'Woolly Mammoths') {
      expr <- expr %>%
        filter(Team == 'WM') %>%
        select(-Team)
    }
    
    else if (input$team == 'All Teams') {
      expr <- expr
    }
    
  })
  
  output$teamstats <- renderDataTable({
    
    if (input$thorp == 'Hitting') {
      
      if (input$tstattype == 'Basic') {
        expr <- basicteamhitting
      }
      
      else if (input$tstattype == 'Advanced') {
        expr <- advteamhitting
      }
      
      else if (input$tstattype == 'Plate Discipline') {
        expr <- pdteamhitting
      }
      
      else if (input$tstattype == 'Batted Ball Profile') {
        expr <- bbteamhitting
      }
      
    }
    
    else if (input$thorp == 'Pitching') {
      
      if (input$tstattype == 'Basic') {
        expr <- basicteampitching
      }
      
      else if (input$tstattype == 'Advanced') {
        expr <- advteampitching
      }
      
      else if (input$tstattype == 'Plate Discipline') {
        expr <- pdteampitching
      }
      
      else if (input$tstattype == 'Batted Ball Profile') {
        expr <- bbteampitching
      }
      
    }
    
  })
  
  output$pitbasic <- renderDataTable({
      
      `Basic Pitching` <- c('Stat', 'League Rank')
      
      basicpitching %>%
        rbind(pranks[,1:28]) %>%
        filter(Player == input$pitname) %>%
        cbind(`Basic Pitching`) %>%
        select(-c(1,2)) %>%
        select(27, 1:26) -> expr
    
  },
  options = list(
    lengthChange = FALSE,
    ordering = FALSE,
    paging = FALSE,
    searching = FALSE
  )
  )
  
  output$hitbasic <- renderDataTable({
      
      `Basic Hitting` <- c('Stat', 'League Rank')
      
      basichitting %>%
        rbind(hranks[,1:25]) %>%
        filter(Player == input$hitname) %>%
        cbind(`Basic Hitting`) %>%
        select(-c(1,2)) %>%
        select(24, 1:23) -> expr
    
  },
  options = list(
    lengthChange = FALSE,
    ordering = FALSE,
    paging = FALSE,
    searching = FALSE
  )
  )
  
  output$pitadv <- renderDataTable({
      
      `Advanced Pitching` <- c('Stat', 'League Rank')
      
      advpitching %>%
        rbind(pranks[,c(1,2,29:39)]) %>%
        filter(Player == input$pitname) %>%
        cbind(`Advanced Pitching`) %>%
        select(-c(1,2)) %>%
        select(12, 1:11) -> expr
    
  },
  options = list(
    lengthChange = FALSE,
    ordering = FALSE,
    paging = FALSE,
    searching = FALSE
  )
  )
  
  output$hitadv <- renderDataTable({
      
      `Advanced Hitting` <- c('Stat', 'League Rank')
      
      advhitting %>%
        rbind(hranks[,c(1,2,26:38)]) %>%
        filter(Player == input$hitname) %>%
        cbind(`Advanced Hitting`) %>%
        select(-c(1,2)) %>%
        select(14, 1:13) -> expr
    
  },
  options = list(
    lengthChange = FALSE,
    ordering = FALSE,
    paging = FALSE,
    searching = FALSE
  )
  )
  
  output$pitpd <- renderDataTable({
      
      `Plate Discipline Pitching` <- c('Stat', 'League Rank')
      
      pdpitching %>%
        rbind(pranks[,c(1,2,40:51)]) %>%
        filter(Player == input$pitname) %>%
        cbind(`Plate Discipline Pitching`) %>%
        select(-c(1,2)) %>%
        select(13, 1:12) -> expr
    
  },
  options = list(
    lengthChange = FALSE,
    ordering = FALSE,
    paging = FALSE,
    searching = FALSE
  )
  )
  
  output$hitpd <- renderDataTable({
      
      `Plate Discipline Hitting` <- c('Stat', 'League Rank')
      
      pdhitting %>%
        rbind(hranks[,c(1,2,39:50)]) %>%
        filter(Player == input$hitname) %>%
        cbind(`Plate Discipline Hitting`) %>%
        select(-c(1,2)) %>%
        select(13, 1:12) -> expr
    
  },
  options = list(
    lengthChange = FALSE,
    ordering = FALSE,
    paging = FALSE,
    searching = FALSE
  )
  )
  
  output$pitbb <- renderDataTable({
      
      title <- c('Stat')
      
      title %>%
        cbind(filter(bbpitching, Pitcher == input$pitname)) %>%
        select(-c(Pitcher, Team)) %>%
        rename(c('Batted Ball Profile' = 1)) -> expr
    
  },
  options = list(
    lengthChange = FALSE,
    ordering = FALSE,
    paging = FALSE,
    searching = FALSE
  )
  )
  
  output$hitbb <- renderDataTable({
      
      title <- c('Stat')
      
      title %>%
        cbind(filter(bbhitting, Batter == input$hitname)) %>%
        select(-c(Batter, Team)) %>%
        rename(c('Batted Ball Profile' = 1)) -> expr
    
  },
  options = list(
    lengthChange = FALSE,
    ordering = FALSE,
    paging = FALSE,
    searching = FALSE
  )
  )
  
  output$pitplat <- renderDataTable({
      
      platoonpitching %>%
        filter(Pitcher == input$pitname) %>%
        select(-c(1, 3)) %>%
        mutate(`Batter Hand` = ifelse(`Batter Hand` == 'R', 'VS RHH', 'VS LHH')) %>%
        rename('Platoon Pitching' = 1) -> expr
    
  },
  options = list(
    lengthChange = FALSE,
    ordering = FALSE,
    paging = FALSE,
    searching = FALSE
  )
  )
  
  output$hitplat <- renderDataTable({
      
      platoonhitting %>%
        filter(Batter == input$hitname) %>%
        select(-c(1, 3)) %>%
        mutate(`Pitcher Hand` = ifelse(`Pitcher Hand` == 'R', 'VS RHP', 'VS LHP')) %>%
        rename('Platoon Hitting' = 1) -> expr
    
  },
  options = list(
    lengthChange = FALSE,
    ordering = FALSE,
    paging = FALSE,
    searching = FALSE
  )
  )
  
  output$pitpt <- renderDataTable({
      
      pitchtypepitching %>%
        filter(Pitcher == input$pitname) %>%
        arrange(desc(BF)) %>%
        select(-c(1, 3)) %>%
        rename('Pitch Type Pitching' = 1) -> expr
    
  },
  options = list(
    lengthChange = FALSE,
    ordering = FALSE,
    paging = FALSE,
    searching = FALSE
  )
  )
  
  output$hitpt <- renderDataTable({
    
      pitchtypehitting %>%
        filter(Batter == input$hitname) %>%
        arrange(desc(Seen)) %>%
        select(-c(1, 3)) %>%
        rename('Pitch Type Hitting' = 1) -> expr
    
  },
  options = list(
    lengthChange = FALSE,
    ordering = FALSE,
    paging = FALSE,
    searching = FALSE
  )
  )
  
  output$pitlogs <- renderDataTable({
      
      pitcherlogs %>%
        filter(Player == input$pitname) %>%
        rename(c(
          '2B' = 'X2B',
          '3B' = 'X3B'
        )) %>%
        select(-c(Player, Team)) -> expr
    
  })
  
  output$hitlogs <- renderDataTable({
      
      batterlogs %>%
        filter(Player == input$hitname) %>%
        rename(c(
          '2B' = 'X2B',
          '3B' = 'X3B'
        )) %>%
        select(-c(Player, Team)) -> expr
    
  })
  
  output$pitchmetrics <- renderDataTable({
    
    pitchmetricpitching %>%
      filter(Pitcher == input$pitname) %>%
      select(-c(Pitcher, Team)) -> expr
    
  })
  
  observe({
    updateSelectInput(session, "pitselect", choices = c('All Pitches', pitchtypepitching[pitchtypepitching$Pitcher == input$pitname, 2]))
  })
  
  output$pitchlogs <- renderPlot({
    
    filter(master, Pitcher == input$pitname) -> guy
    
    if (input$pitselect != 'All Pitches') {
      filter(guy, Pitch.Type == input$pitselect) -> guy
    }
    
    guycols <- pitchcols[names(pitchcols) %in% guy$Pitch.Type]
    
    ggplot(guy, 
           aes(x = Pitch.Strike.Zone...Offset..ft., Pitch.Strike.Zone...Height..ft., color = Pitch.Type)) +
      geom_point(size = 6) +
      scale_color_manual(values = guycols) +
      geom_rect(xmin = -1.41667,
                xmax = 1.41667,
                ymin = 1.5, 
                ymax = 3.6, 
                color = "black",
                alpha = 0,
                size = 1.25) +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.text.y = element_blank(),
            axis.text.x= element_blank(),
            axis.ticks.x= element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            legend.key.size = unit(2.5, 'cm'),
            legend.text = element_text(size = 25),
            legend.title = element_blank()) +
      xlim(-2.8, 2.8) +
      ylim(0, 6)
    
  }, height = 1000, width = 800)
  
  output$spraychart <- renderPlot({
    
    filter(master, Batter == input$hitname & Pitch.Call != "Foul Ball" & Pitch.Call != "") -> guy
    
    if (input$hitselect == 'Hits') {
      filter(guy, Pitch.Call == 'Single' | Pitch.Call == 'Double' | Pitch.Call == 'Triple' | Pitch.Call == 'Home Run') -> guy
    }
    
    else if (input$hitselect == 'Non-Hits') {
      filter(guy, Pitch.Call == 'Contact Out' | Pitch.Call == 'Error') -> guy
    }
    
    ggplot(guy,
           aes(Hit_X, Hit_Y)) + labs(x = "", y = "") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "white")) +
      theme(legend.title = element_blank(),
            legend.key.size = unit(2.5, 'cm'),
            legend.text = element_text(size = 25),
            plot.title = element_text(face = "bold", size = 16)) +
      theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
      xlim(-220, 220)+ ylim(0, 400) +
      geom_segment(x = 0, y = 0, xend = 226, yend = 226, color = "black", size = 1) +
      geom_segment(x = 226, y = 226, xend = 80, yend = 378, color = "black", size = 1) +
      geom_segment(x = 80, y = 378, xend = -20, yend = 378, color = "black", size = 1) +
      geom_segment(x = -20, y = 378, xend = -110, yend = 340, color = "black", size = 1) +
      geom_segment(x = -110, y = 340, xend = -219, yend = 219, color = "black", size = 1) +
      geom_segment(x = -219, y = 219, xend = 0, yend = 0, color = "black", size = 1) +
      geom_segment(x = -64, y = 64, xend = 0, yend = 128, color = "black", size = 1) +
      geom_segment(x = 0, y = 128, xend = 64, yend = 64, color = "black", size = 1) +
      geom_point(size = 10) +
      geom_point(aes(color=Pitch.Call), size = 8) + coord_equal() +
      scale_color_manual(values = spraychartcols)
    
  }, height = 600, width = 800)
  
  output$heatmap <- renderPlot({
    
    zonePlot$avgs <- c(hitter_avgs[hitter_avgs$Batter == input$hitname, c(11:14, 2:10)]) %>% unlist()
    
    ggplot(zonePlot, aes(xmin = x, xmax = x + w, ymin = y, ymax = y + h)) +
      geom_rect(aes(fill = avgs), colour = "black") +
      scale_fill_gradient2(
        low = '#4A54FE',
        mid = 'white',
        high = '#FE4D4A',
        midpoint = league_ba
      ) +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.text.y = element_blank(),
            axis.text.x= element_blank(),
            axis.ticks.x= element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            legend.position = 'none') +
      xlim(-6, 6) +
      ylim(0, 5)
    
  }, height = 700, width = 1000)
  
  observe({
    updateSliderInput(session, 'enddate', min = input$startdate)
  })
  
  observe({
    updateSliderInput(session, 'startdate', max = input$enddate)
  })
  
  observe({
    
    if (input$hitorpit == 'Hitting') {
      updateSelectInput(session, 'handbat', choices = c('Any', 'AS LHH', 'AS RHH'))
      updateSelectInput(session, 'splitplat', choices = c('Any', 'VS LHP', 'VS RHP'))
    }
    
    else if (input$hitorpit == 'Pitching') {
      updateSelectInput(session, 'handbat', choices = c('Any', 'AS LHP', 'AS RHP'))
      updateSelectInput(session, 'splitplat', choices = c('Any', 'VS LHH', 'VS RHH'))
    }
    
  })
  
  output$splitstats <- renderDataTable({
    
    master %>%
      filter(Date >= input$startdate & Date <= input$enddate) -> filt
    
    if (input$hitorpit == 'Hitting') {
      
      if (input$tm != 'Any') {
        filt %>%
          filter(Batter.Team == input$tm) -> filt
      }
      
      if (input$opp != 'Any') {
        filt %>%
          filter(Pitcher.Team == input$opp) -> filt
      }
          
      if (input$ha != 'Any') {
        
        if (input$ha == 'Home') {
          filt %>%
            filter(Top.Bottom == 'Bottom') -> filt
        }
        
        else if (input$ha == 'Away') {
          filt %>%
            filter(Top.Bottom == 'Top') -> filt
        }
        
      }
      
      if (input$handbat != 'Any') {
        
        if (input$handbat == 'AS LHH') {
          filt %>%
            filter(Batter.Handedness == 'L') -> filt
        }
        
        else if (input$handbat == 'AS RHH') {
          filt %>%
            filter(Batter.Handedness == 'R') -> filt
        }
        
      }
      
      if (input$splitplat != 'Any') {
        
        if (input$splitplat == 'VS LHP') {
          filt %>%
            filter(Pitcher.Handedness == 'L') -> filt
        }
        
        else if (input$splitplat == 'VS RHP') {
          filt %>%
            filter(Pitcher.Handedness == 'R') -> filt
        }
        
      }
      
      if (input$splitpt != 'Any') {
        
        filt %>%
          filter(PitCat == input$splitpt) -> filt
        
      }
      
      if (input$playerorteam == 'By Player') {
        gethsplits(filt) -> expr
      }
      
      else if (input$playerorteam == 'By Team') {
        getthsplits(filt) -> expr
      }
        
    }
    
    else if (input$hitorpit == 'Pitching') {
      
      if (input$tm != 'Any') {
        filt %>%
          filter(Pitcher.Team == input$tm) -> filt
      }
      
      if (input$opp != 'Any') {
        filt %>%
          filter(Batter.Team == input$opp) -> filt
      }
      
      if (input$ha != 'Any') {
        
        if (input$ha == 'Home') {
          filt %>%
            filter(Top.Bottom == 'Top') -> filt
        }
        
        else if (input$ha == 'Away') {
          filt %>%
            filter(Top.Bottom == 'Bottom') -> filt
        }
        
      }
      
      if (input$handbat != 'Any') {
        
        if (input$handbat == 'AS LHP') {
          filt %>%
            filter(Pitcher.Handedness == 'L') -> filt
        }
        
        else if (input$handbat == 'AS RHP') {
          filt %>%
            filter(Pitcher.Handedness == 'R') -> filt
        }
        
      }
      
      if (input$splitplat != 'Any') {
        
        if (input$splitplat == 'VS LHH') {
          filt %>%
            filter(Batter.Handedness == 'L') -> filt
        }
        
        else if (input$splitplat == 'VS RHH') {
          filt %>%
            filter(Batter.Handedness == 'R') -> filt
        }
        
      }
      
      if (input$splitpt != 'Any') {
        
        filt %>%
          filter(PitCat == input$splitpt) -> filt
        
      }
      
      if (input$playerorteam == 'By Player') {
        getpsplits(filt) -> expr
      }
      
      else if (input$playerorteam == 'By Team') {
        gettpsplits(filt) -> expr
      }
      
    }
    
  })
  
}

shinyApp(ui = ui, server = server)
