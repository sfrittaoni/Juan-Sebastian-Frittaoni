#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(polynom)
library(ggplot2)
library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(curl)
gapminder <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-04-24/gapminder_es.csv")

colnames(gapminder)

ui <- fluidPage(theme = shinytheme("paper"),
                
                titlePanel("Datos de miercoles Gapminder shiny app"),
                
                sidebarLayout(
                  sidebarPanel("Juan Sebastian Frittaoni",
                               sliderInput(inputId = "anio", label = "Año", value = 1952,
                                           min = 1952, max = 2007,step = 5),
                               selectInput(inputId = "z", 
                                           label = "Continente", 
                                           choices= c("Asia","Europa","Africa","Americas","Oceania"),
                                           selected = "Asia"),
                               selectInput(inputId = "x", 
                                           label = "Eje X", 
                                           choices= c("Poblacion"="poblacion",
                                                      "Esperanza de vida"="esperanza_de_vida",
                                                      "PBI per capita"= "pib_per_capita"),
                                           selected = "poblacion"),
                               selectInput(inputId = "y", 
                                           label = "Eje Y", 
                                           choices= c("Poblacion"="poblacion",
                                                      "Esperanza de vida"="esperanza_de_vida",
                                                      "PBI per capita"= "pib_per_capita"),
                                           selected = "esperanza_de_vida")),
                  mainPanel(
                    plotOutput(outputId = "scatterplot")
                  )
                ))


server <- function(input, output) {
  
  # Create the scatterplot object the plotOutput function is expecting
  output$scatterplot <- renderPlot({
    gapminder %>% filter(anio==input$anio, continente==input$z) %>%  ggplot(aes_string(x = input$x, y = input$y)) +
      geom_point()+
      xlim(ifelse(input$x=="poblacion",min(gapminder[gapminder$anio==input$anio & gapminder$continente==input$z, "poblacion"]), ifelse(input$x=="pib_per_capita",min(gapminder[gapminder$anio==input$anio & gapminder$continente==input$z, "pib_per_capita"]), min(gapminder[gapminder$anio==input$anio & gapminder$continente==input$z, "esperanza_de_vida"]))),
           ifelse(input$x=="poblacion",max(gapminder[gapminder$anio==input$anio & gapminder$continente==input$z, "poblacion"]), ifelse(input$x=="pib_per_capita",max(gapminder[gapminder$anio==input$anio & gapminder$continente==input$z, "pib_per_capita"]), max(gapminder[gapminder$anio==input$anio & gapminder$continente==input$z, "esperanza_de_vida"]))))+
      ylim(ifelse(input$y=="poblacion",min(gapminder[gapminder$anio==input$anio & gapminder$continente==input$z, "poblacion"]), ifelse(input$y=="pib_per_capita",min(gapminder[gapminder$anio==input$anio & gapminder$continente==input$z, "pib_per_capita"]), min(gapminder[gapminder$anio==input$anio & gapminder$continente==input$z, "esperanza_de_vida"]))),
           ifelse(input$y=="poblacion",max(gapminder[gapminder$anio==input$anio & gapminder$continente==input$z, "poblacion"]), ifelse(input$y=="pib_per_capita",max(gapminder[gapminder$anio==input$anio & gapminder$continente==input$z, "pib_per_capita"]), max(gapminder[gapminder$anio==input$anio & gapminder$continente==input$z, "esperanza_de_vida"]))))+
      xlab(ifelse(input$x=="poblacion","Poblacion", ifelse(input$x=="pib_per_capita", "PBI per capita", "Esperanza de vida")))+
      ylab(ifelse(input$y=="poblacion","Poblacion", ifelse(input$y=="pib_per_capita", "PBI per capita", "Esperanza de vida")))
  })
}
shinyApp(ui, server)

