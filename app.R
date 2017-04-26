setwd('~/Documents/MSAN/DataViz/HW3/')

library(reshape2)
library(ggvis)
library(ggplot2)
library(GGally)
library(MASS)

raw <- read.csv('Facebook_metrics/dataset_Facebook.csv', sep=';')
names(raw)
drop <- c("Total.Interactions", "Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post",
          "Lifetime.Post.reach.by.people.who.like.your.Page","Lifetime.Post.Impressions.by.people.who.have.liked.your.Page",
          "Lifetime.Post.Consumptions","Lifetime.Post.Consumers")

data <- raw[!(names(raw) %in% drop)]
names(data) <- c('Page.Likes', "Type", "Category", "Post.Month", "Post.Weekday",  "Post.Hour", "Paid", 'Reach', 'Impressions', 'Engagments', "comment", "like",  'share')

# Remove outlier
std_dat <- ((data$like - mean(data$like, na.rm = T))/sd(data$like, na.rm = T))
std_dat[is.na(std_dat)] =0
data <- data[-which(std_dat>10),]

data$Paid <- ifelse(data$Paid == 1, 'Paid', 'Not Paid')

post_type <- c('All Post Types', as.vector(unique(data$Type)))
color_cats <- c("Type", "Category", "Post.Month", "Post.Weekday","Paid")
data[color_cats] <- lapply(data[color_cats], factor)

cont_cats <- names(data[!(names(data) %in% color_cats)])

color_cats <- c('Post.Hour',color_cats)

# For pairs plot
d <- data[names(data) %in% c("Paid", 'Reach', 'Impressions', 'Engagments')]
d <- subset(d, !is.na(Paid))
d <- subset(d, !is.na(Reach))
d <- subset(d, !is.na(Impressions))
pairs_data <- subset(d, !is.na(Engagments))




ui <- shinyUI(navbarPage("Facebook Data",
    tabPanel("Bubble Plot", sidebarLayout(
      sidebarPanel(
        selectInput('post', 'Select Post Type', choices = post_type),
        selectInput('color', 'Select Color Code', choices = color_cats)
      ),  
      mainPanel(ggvisOutput("bubble"))
    )),
    tabPanel("Scatter Plot Matrix", plotOutput("pairs")),
    tabPanel("Parallel Coordinates Plot", sidebarLayout(
      sidebarPanel(
        uiOutput('par_cats')),
        mainPanel(plotOutput("parallel"))
    )
    )
  )
)



server <- function(input, output){
  
  plot_data <- reactive({
    d <- subset(data, !is.na(like))
    d <- subset(d, !is.na(share))
    
    post_type <- input$post
    if(post_type != "All Post Types"){
      d <- subset(d, Type == post_type, drop=T)
    }
    return(d)
  })
  
  bubble_data <- reactive({  
  plot_data %>% 
    ggvis(prop("x", as.name('like')),
        prop("y",as.name('share')),
        fill = as.name(input$color),
        fillOpacity := 0.5, fillOpacity.hover := 1,
        stroke := NA, stroke.hover = ~Paid, strokeWidth := 4, strokeOpacity := 0.7) %>%
      scale_numeric("size", range = c(50, 500), nice = FALSE) %>%
      layer_points(prop("size",as.name('comment'))) %>%
      add_legend("size", properties = legend_props(legend = list(y = 50))) %>%
      ggvis::hide_legend('size') %>% 
    set_options(width = 800, height = 600, renderer = "svg")
  })
  
  bubble_data %>% 
    bind_shiny('bubble', 'bubble_stuff')
  
  

  
  output$pairs <- renderPlot({ 

    ggpairs(pairs_data, aes(colour = Paid, alpha = 0.4))
  })
  
  
  
  
  output$par_cats <- renderUI({
    selectInput('parvars', 'Select Variables to Visualize', multiple=T,
                choices <- cont_cats, selected = cont_cats)
  })
  
  
  output$parallel <- renderPlot({
    validate(
      need(length(input$parvars) > 1, "Please select at least two variables to visualize")
    )
      parallel_colors <- rainbow(nrow(data))
      par(xpd = T, mar = par()$mar + c(0,0,0,7))
      data[,names(data) %in% input$parvars] %>% 
        parcoord(col=parallel_colors, var.label=TRUE)
  })
}



shinyApp(ui = ui, server = server)

  


