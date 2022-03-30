# Install
#install.packages("wesanderson")
getOption("repos")
options(repos = c(CRAN = "https://cran.rstudio.org"))
library(wesanderson)
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)
library(shiny)
library(shinydashboard)
#install.packages("ggrepel")
library(ggrepel)
library(ggplot2)
#library(ddply)
library(plyr)
library(reshape)
options(scipen=999)
@setwd("C:/College_work/shine")
#fill = c("steelblue", "#E69F00", "#56B4E9", "#009E73","#F0E442", "#0072B2", "#D55E00", "#CC79A7","#999999", "#E69F00", "steelblue", "#E69F00", "#56B4E9", "#009E73","#F0E442")
qs_uni <- read.csv('qs_uni.csv', na.strings=c("","NA"))
df <- qs_uni

# Hist
student_score <- data.frame(df$country,round(df$score))
str(inter_stud)
student_scores <- aggregate(.~ df.country ,data=student_score,mean)
student_scores[with(student_scores, order(- student_scores$round.df.score.)),]
prefer <- student_scores[c(70,32,79,52,88,89,78,41,74,2,14,50,23,4,5), ]
prefer$round.df.score.= round(prefer$round.df.score.,0)
# prefer

# LineGraph
subset_c <- data.frame(df$year)
subset_c
main <- ddply(subset_c, .(df.year), nrow)
main

# Piechart
mytable1 <- table(df$research_output)
lbls <- paste(names(mytable1), "\n", mytable1, sep="")

# horizontal bar
reg_size <- ggplot(df, aes(y = region, fill= size))+
  geom_bar() + scale_fill_brewer(palette="Dark2")
reg_size


###########################################################################################

ui <- dashboardPage(
  dashboardHeader(title = 'QS Ranking'),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      box(selectInput("region","Uni Type",choices = df$region,
                      selected = (df[1:2,])),
          plotOutput("Histogram"),
          plotOutput("Piechart")) ,
      box(plotOutput("Horizontal"),plotOutput("hist"))
    )
  )
)


###########################################################################################


server <- function(input, output){
  # output$BoxPlot <- renderPlot({
  #   
  #   ggplot(data = qs_uni, aes(x = country, y = input$attributes, colour = input$attributes)) +
  #     geom_jitter()+ geom_boxplot(alpha=0.6, size= 1) + 
  #     ylab("Attributes") + xlab("Country")+
  #     ggtitle("Histogram")+
  #     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  #     theme(legend.position = 'top')
  # })
  
  output$Histogram <- renderPlot ({
    outputgraph2 <- df %>% 
      filter(df$region == input$region) %>%
      ggplot(aes(x = region, fill = type,color=type)) + geom_bar(alpha = 0.5) + 
      theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1)) + 
      theme(legend.position = 'top')+ scale_fill_brewer(palette="Rushmore")
    outputgraph2
  })
  
  output$hist <- renderPlot ({
    ggplot(prefer, aes(reorder(df.country,prefer$round.df.score.), prefer$round.df.score.))  +
      geom_bar(stat = "identity",fill = c("#f2b3e4", "#56B4E9", "#77f73b", "#7ede98","#999999", "#0072B2", "#f2b3e4", "#F0E442","#999999", "#E69F00", "#6419bf", "#56B4E9", "#E69F00", "#0072B2","#2491f0")) +
      labs(
        title = "15 Countries with highest QS average scores",
        x = "Country" ,
        y = "Average QS Score"
      ) + theme_bw()+  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  })
  
  output$Piechart <- renderPlot ({
    pie(mytable1, labels = lbls,
        main="Research Output\n (across the world)",radius = 1,col = rainbow(5))
  })
  
  output$Horizontal <- renderPlot ({
    
    ggplot(df, aes(y = region, fill= size))+
      geom_bar() + scale_fill_brewer(palette="Blues")+labs(title = "Size Distribution for Universities in different regions")
    
  })
}

shinyApp(ui, server)
