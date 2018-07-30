## Libraries
library(shiny)
library(shinythemes)
library(shinycssloaders)
library(tidyverse)
library(maps)

## Set images resource path
addResourcePath("images", "images")

## Cities Lat long
cities_unique <- read_csv("data/cities_unique.csv")

## Get iowa map data
iowa <- map_data("county") %>%
    filter(region == "iowa")

## Get the ragbrai table
route_table <- read_csv("data/route_table.csv") %>%
    rbind(c("XLVI", 432, NA,  "Onawa",  "Denison",  "Jefferson", "Ames", "Newton", "Sigourney", "Iowa City", "Davenport"))

## Create all pairwise transitions
transitions <- as.data.frame(do.call(rbind, unlist(apply(route_table, 1, function(x) {
    pairs <- list(
        4:5,
        5:6,
        6:7,
        7:8,
        8:9,
        9:10,
        10:11
    )
    
    res <- sapply(pairs, function(index) { x[index] }, simplify = FALSE)
    
    return(res)
}), recursive = FALSE)))
names(transitions) <- c("Start", "End")

## Compute probs
trans_matrix <- transitions %>%
    group_by(Start, End) %>%
    summarise(Count = n()) %>%
    spread(key = End, value = Count)
trans_matrix[is.na(trans_matrix)] <- 0
trans_matrix[,-1] <- trans_matrix[,-1] / rowSums(trans_matrix[,-1])

## Get starting states
start_states <- route_table$`Starting Town`
end_states <- route_table$`Ending Town`

## Static iowa map
# Plot the most recent route
iowa_map <- ggplot(data = iowa, aes(x = long, y = lat)) +
    geom_map(map = iowa, aes(map_id = region), fill = "cornsilk", color = "grey80") +
    theme_void(base_size = 16) +
    labs(
        title = "Simulated RAGBRAI Route",
        subtitle = "Through the State of Iowa"
    ) +
    theme(
        plot.title = element_text(hjust = 0.5, face = "bold", colour = "slateblue4", size = 22 ),
        plot.subtitle = element_text(hjust = 0.5, colour = "#656373"),
        plot.background = element_rect(fill = "grey99"),
        plot.margin = margin(0, 4, 0, 4, "cm")
    )

ui <- fluidPage(theme = shinytheme("cerulean"),
   
   includeCSS("css/styles.css"),
                
   titlePanel("RAGBRAI Route"),
   
   sidebarLayout(
      sidebarPanel(
          a(href = "https://oaiti.org", target = "_blank", img(src = "images/oaiti_transparent.png", width = "135")),
          h4("About"),
          HTML("This application uses <a href='https://ragbrai.com/routemaps/past-overnight-towns-grid/' target='_blank'>historic data on RAGBRAI routes</a>. It let's you simulate a route using Markov Probabilities.<br><br>"),
          hr(),
          actionButton("simulate", "Simulate Route")
      ),
      
      mainPanel(
         withSpinner(plotOutput("map"))
      )
   )
)

server <- function(input, output) {
    
   values <- reactiveValues(route = NULL)
   
   observeEvent(input$simulate, {
       ## Build the simulated path
       myvec <- sample(start_states, 1)
       while(TRUE) {
           myvec[length(myvec) + 1] <- sample(names(trans_matrix[-1]), size = 1, prob = as.numeric(unlist(trans_matrix[trans_matrix$Start == myvec[length(myvec)], -1])))
           
           if (myvec[length(myvec)] %in% end_states) break;
       }
       
       values$route <- myvec
   })
   
   output$map <- renderPlot({
       if (is.null(values$route)) return(NULL)
       
       ## Append to tall data
       myroute <- data.frame(City = values$route) %>%
           left_join(cities_unique)
       
       # Plot the most recent route
       iowa_map +
           geom_point(data = myroute, aes(x = lon, y = lat), color = "grey65") +
           geom_path(data = myroute, aes(x = lon, y = lat), color = "grey50", linetype = 2) +
           geom_label(data = myroute, aes(x = lon, y = lat, label = City), size = 4)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)
