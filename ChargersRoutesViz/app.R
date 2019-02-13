#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(gganimate)
library(cowplot)
library(dtwclust)
library(plotly)

load("ChargersRoutes.RData")

cols <- c("yes" = "red", "no" = "black")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Visualizing Chargers Routes"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectizeInput("receiver",
                     "Receivers to Highlight:",
                     choices = sort(unique(routes$displayName)),
                     selected = NULL,
                     multiple = T)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("routesPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$routesPlot <- renderPlot({

     
     if(length(input$receiver) == 0){
       routes %>% arrange(frame.id) %>% ggplot(aes(x = y.trans, y = x.trans)) + 
         annotate("rect", xmin = 0, xmax = 53.3, ymin = -10, ymax = -5, fill = "#005a32",alpha = .15, color = "black") +
         annotate("rect", xmin = 0, xmax = 53.3, ymin = -5, ymax = 0, fill = "#74c476",  alpha = .15, color = "black") +
         annotate("rect", xmin = 0, xmax = 53.3, ymin = 0, ymax = 5, fill = "#005a32",   alpha = .15, color = "black") +
         annotate("rect", xmin = 0, xmax = 53.3, ymin = 5, ymax = 10, fill = "#74c476",  alpha = .15, color = "black") +
         annotate("rect", xmin = 0, xmax = 53.3, ymin = 10, ymax = 15, fill = "#005a32", alpha = .15, color = "black") +
         annotate("rect", xmin = 0, xmax = 53.3, ymin = 15, ymax = 20, fill = "#74c476", alpha = .15, color = "black") +
         annotate("rect", xmin = 0, xmax = 53.3, ymin = 20, ymax = 25, fill = "#005a32", alpha = .15, color = "black") +
         annotate("rect", xmin = 0, xmax = 53.3, ymin = 25, ymax = 30, fill = "#74c476", alpha = .15, color = "black") +
         annotate("rect", xmin = 0, xmax = 53.3, ymin = 30, ymax = 35, fill = "#005a32", alpha = .15, color = "black") +
         annotate("rect", xmin = 0, xmax = 53.3, ymin = 35, ymax = 40, fill = "#74c476", alpha = .15, color = "black") +
         annotate("rect", xmin = 0, xmax = 53.3, ymin = 40, ymax = 45, fill = "#005a32", alpha = .15, color = "black") +
         annotate("rect", xmin = 0, xmax = 53.3, ymin = 45, ymax = 50, fill = "#74c476", alpha = .15, color = "black") +
         geom_path(aes(group = paste0(displayName, " ", gameId, " ", playId)), alpha = .3) + 
         scale_y_continuous(breaks = seq(-10,50,by=10)) +
         labs(x = "\nYards from Closest Sideline at Start of Play",
              y = "Yards Downfield from LOS\n",
              title = "Chargers Pass Routes - 2017 Season, Weeks 1-6\n") + 
         guides(color = F, size = F) + 
         theme(line = element_blank(),
               axis.title = element_text(size = 12),
               plot.title = element_text(size = 13, hjust = 0.5),
               plot.subtitle = element_text(size = 13, hjust = 0.5))
     }
     
     else if(length(input$receiver) == 1){
     routes %>% arrange(frame.id) %>% ggplot(aes(x = y.trans, y = x.trans)) + 
         annotate("rect", xmin = 0, xmax = 53.3, ymin = -10, ymax = -5, fill = "#005a32",alpha = .15, color = "black") +
         annotate("rect", xmin = 0, xmax = 53.3, ymin = -5, ymax = 0, fill = "#74c476",  alpha = .15, color = "black") +
         annotate("rect", xmin = 0, xmax = 53.3, ymin = 0, ymax = 5, fill = "#005a32",   alpha = .15, color = "black") +
         annotate("rect", xmin = 0, xmax = 53.3, ymin = 5, ymax = 10, fill = "#74c476",  alpha = .15, color = "black") +
         annotate("rect", xmin = 0, xmax = 53.3, ymin = 10, ymax = 15, fill = "#005a32", alpha = .15, color = "black") +
         annotate("rect", xmin = 0, xmax = 53.3, ymin = 15, ymax = 20, fill = "#74c476", alpha = .15, color = "black") +
         annotate("rect", xmin = 0, xmax = 53.3, ymin = 20, ymax = 25, fill = "#005a32", alpha = .15, color = "black") +
         annotate("rect", xmin = 0, xmax = 53.3, ymin = 25, ymax = 30, fill = "#74c476", alpha = .15, color = "black") +
         annotate("rect", xmin = 0, xmax = 53.3, ymin = 30, ymax = 35, fill = "#005a32", alpha = .15, color = "black") +
         annotate("rect", xmin = 0, xmax = 53.3, ymin = 35, ymax = 40, fill = "#74c476", alpha = .15, color = "black") +
         annotate("rect", xmin = 0, xmax = 53.3, ymin = 40, ymax = 45, fill = "#005a32", alpha = .15, color = "black") +
         annotate("rect", xmin = 0, xmax = 53.3, ymin = 45, ymax = 50, fill = "#74c476", alpha = .15, color = "black") +
         geom_path(aes(group = paste0(displayName, " ", gameId, " ", playId), 
                       color = ifelse(displayName == input$receiver, "yes", "no"), 
                       size = ifelse(displayName == input$receiver, "yes", "no")), alpha = .3) + 
         scale_color_manual(values = cols) + 
         scale_size_manual(values = c("yes" = 2, "no" = 1)) +
         scale_y_continuous(breaks = seq(-10,50,by=10)) +
         labs(x = "\nYards from Closest Sideline at Start of Play",
              y = "Yards Downfield from LOS\n",
              title = "Chargers Pass Routes - 2017 Season, Weeks 1-6\n",
              subtitle = paste0("Highlighting Routes by ", input$receiver)) + 
         guides(color = F, size = F) + 
         theme(line = element_blank(),
               axis.title = element_text(size = 12),
               plot.title = element_text(size = 13, hjust = 0.5),
               plot.subtitle = element_text(size = 13, hjust = 0.5))
     }
     else if(length(input$receiver) > 1){
       routes %>% arrange(frame.id) %>% ggplot(aes(x = y.trans, y = x.trans)) + 
         annotate("rect", xmin = 0, xmax = 53.3, ymin = -10, ymax = -5, fill = "#005a32",alpha = .15, color = "black") +
         annotate("rect", xmin = 0, xmax = 53.3, ymin = -5, ymax = 0, fill = "#74c476",  alpha = .15, color = "black") +
         annotate("rect", xmin = 0, xmax = 53.3, ymin = 0, ymax = 5, fill = "#005a32",   alpha = .15, color = "black") +
         annotate("rect", xmin = 0, xmax = 53.3, ymin = 5, ymax = 10, fill = "#74c476",  alpha = .15, color = "black") +
         annotate("rect", xmin = 0, xmax = 53.3, ymin = 10, ymax = 15, fill = "#005a32", alpha = .15, color = "black") +
         annotate("rect", xmin = 0, xmax = 53.3, ymin = 15, ymax = 20, fill = "#74c476", alpha = .15, color = "black") +
         annotate("rect", xmin = 0, xmax = 53.3, ymin = 20, ymax = 25, fill = "#005a32", alpha = .15, color = "black") +
         annotate("rect", xmin = 0, xmax = 53.3, ymin = 25, ymax = 30, fill = "#74c476", alpha = .15, color = "black") +
         annotate("rect", xmin = 0, xmax = 53.3, ymin = 30, ymax = 35, fill = "#005a32", alpha = .15, color = "black") +
         annotate("rect", xmin = 0, xmax = 53.3, ymin = 35, ymax = 40, fill = "#74c476", alpha = .15, color = "black") +
         annotate("rect", xmin = 0, xmax = 53.3, ymin = 40, ymax = 45, fill = "#005a32", alpha = .15, color = "black") +
         annotate("rect", xmin = 0, xmax = 53.3, ymin = 45, ymax = 50, fill = "#74c476", alpha = .15, color = "black") +
         geom_path(aes(group = paste0(displayName, " ", gameId, " ", playId), 
                       color = ifelse(displayName %in% input$receiver, input$receiver, "Other") 
                       #,size = ifelse(displayName %in% input$receiver, "yes", "no")
                       ), alpha = .3, size = 2) + 
         scale_color_discrete() + 
         #scale_size_manual(values = c("yes" = 2, "no" = 1)) +
         scale_y_continuous(breaks = seq(-10,50,by=10)) +
         labs(x = "\nYards from Closest Sideline at Start of Play",
              y = "Yards Downfield from LOS\n",
              title = "Chargers Pass Routes - 2017 Season, Weeks 1-6\n",
              subtitle = paste0("Highlighting Routes by ", paste(input$receiver, collapse = ", ")),
              color = "Receiver") + 
         guides(size = F) + 
         theme(line = element_blank(),
               axis.title = element_text(size = 12),
               plot.title = element_text(size = 13, hjust = 0.5),
               plot.subtitle = element_text(size = 13, hjust = 0.5))
     }
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

