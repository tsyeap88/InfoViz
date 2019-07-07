library(shiny)
library(ggplot2)
library(gapminder)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Population Analysis"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        
        #Continent Selector
        selectInput(inputId = "cont",
                    label = "Select a Continent:",
                    choices = c("", "Africa", "Americas", "Asia", "Europe","Oceania")
        ),
        
        #Year Selector
        selectInput(inputId = "year",
                    label = "Select a Year:",
                    choices = c("","1952", "1957", "1962", "1967", "1972", "1977",
                                "1982", "1987", "1992", "1997", "2002", "2007")
        )
      ),
      
      # Show a reactive title and plot
      mainPanel(
        
        # Output: Formatted text for p1 caption
        h4(textOutput("caption")),
        
        #Output: p1 - pop growth by all years for selected continent 
        plotOutput("p1"),
        
        # Output: Formatted text for p2 caption
        h4(textOutput("caption1")),
        
        #Output: p2 - country population by continent, by selected year
        plotOutput("p2")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  # Compute the formula text for p1
  formulaText <- reactive({
    paste("Population growth by all years for", input$cont)
  })
  
  # Return the formula text for printing as a caption
  output$caption <- renderText({
    formulaText()
  })
  
  #Plot population growth by all years for selected continent 
  output$p1 <- renderPlot({
    if (input$cont != ""){
      sub.index <- which(gapminder$continent == input$cont)
      #sub.index <- which(gapminder$continent == "Asia")
      tmp.data <- gapminder[sub.index, ]  #subset the data based on continent
      
      #show population based on year
      pop1 <- aggregate(tmp.data$pop, list(tmp.data$year), sum)
      
      #rename columns
      names(pop1) <- c("year", "pop")
      
      #barplot using ggplot2
      ggplot(data = pop1, aes(x = year, y = pop)) + 
        geom_bar(stat="identity", fill = "steelblue") + 
        xlab("Year") + ylab("Population") +
        scale_x_continuous("year", labels = as.character(pop1$year), 
                           breaks = pop1$year) +
        theme_minimal()
    }
  })
  
  # Compute the formula text fr p2
  formulaText1 <- reactive({
    paste("Population by country in", input$cont, "for year", input$year)
  })
  
  # Return the formula text for printing as a caption ----
  output$caption1 <- renderText({
    formulaText1()
  })
  
  output$p2 <-renderPlot({
    if (input$year != ""){
      sub.index <- which(gapminder$year == as.numeric(input$year))
      #sub.index <- which(gapminder$year == "1962")
      tmp.data <- gapminder[sub.index, ]
      
      if (input$cont != ""){
        sub.index.2 <- which(tmp.data$continent == input$cont)
        tmp.data <- tmp.data[sub.index.2, ] #subset data based on year and continent
      }
      
      #show population based on country
      pop2 <- aggregate(tmp.data$pop, list(tmp.data$country), sum)
      
      #rename columns
      names(pop2) <- c("country", "pop")
      
      #barplot
      ggplot(data = pop2, aes(x=country, y=pop, fill = "red")) + 
        geom_bar(stat="identity") + 
        xlab("Country") + ylab("Population") + 
        theme_minimal() + 
        theme(axis.text.x  = element_text(angle=90),
              legend.position = "none") 
        
    }
  })
      
}

# Run the application 
shinyApp(ui = ui, server = server)

