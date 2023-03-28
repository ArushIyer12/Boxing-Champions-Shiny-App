library(shiny)
library(tidyverse)

champs <- read_csv('data/All_Champs.csv')
lst <- list(unique(champs$Nationality))

ui <- fluidPage(
  
  navbarPage(
    "Interactive Boxing Visualizations",
    id = "main_navbar",
    
    tabPanel(
      "Density Plots",
  
  # App title ----
  titlePanel("Density Plots of Reach and Height of different Boxers"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(position = "left",
                
                sidebarPanel(
                  helpText("Create density plots of reach and height of past and present boxing world champions. "),
                  
                  selectInput("var",
                              label = "Select Variable",
                              choices = list("Reach", "Height"),
                              selected = "Reach"
                  ),
                  
                  selectInput("nat",
                              label = "Select Nationality",
                              choices = list("All", "Mexican", "American", "British", "Puerto Rican", "Japanese", "Russian", "Ukrainian", "Argentine", "Colombian", "Venezuelan", "Thai", "Italian", "Cuban", "South Korean", "Filipino", "Australian"),
                              selected = "All"
                  ),
                  
                  
                  # radio button
                  radioButtons("radio", 
                               h3("Weight Class"),
                               choices = list("All Weights" = "All",
                                              "Heavyweight" = "Heavy", 
                                              "Cruiserweight" = "Cruiser",
                                              "Light Heavyweight" = "Lheavy",
                                              "Super Middleweight" = "SuperMid",
                                              "Middleweight" = "Mid",
                                              "Light Middlweight" = "LMid",
                                              "Welterweight" = "Welt",
                                              "Light Welterweight" = "LWelt",
                                              "Lightweight" = "Light",
                                              "Super Featherweight" = "SuperFeath",
                                              "Featherweight" = "Feath",
                                              "Super Bantamweight" = "SuperBant",
                                              "Bantamweight" = "Bant",
                                              "Super Flyweight" = "SuperFly",
                                              "Flyweight" = "Fly",
                                              "Light Flyweight" = "LFly",
                                              "Minimumweight" = "MinFly"),
                               selected = "All")
                  
                ),
                
                
                # Main panel for displaying outputs ----
                mainPanel(
                  
                  # Output: Density plot ----
                  plotOutput(outputId = "distPlot"),
                  br(),
                  br(),
                  br(),
                  br(),
                  br(),
                  br(),
                  p("Note that if the density plot is blank, that means that no boxers exist under that criteria. For example, there are no Mexican heavyweight champions (Andy Ruiz Jr. was classified as American)")
                  
                )
  )
),
tabPanel(
  "Barplots",
  
  # App title ----
  titlePanel("Counts of boxing champions by nationality and sanctioning body"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(position = "left",
                
                sidebarPanel(
                  helpText("Visualize how many champions each nationality has had over different weightclasses"),
                  
                  selectInput("natty",
                              label = "Select Nationality",
                              choices = list("All", "Mexican", "American", "British", "Puerto Rican", "Japanese", "Russian", "Ukrainian", "Argentine", "Colombian", "Venezuelan", "Thai", "Italian", "Cuban", "South Korean", "Filipino", "Australian"),
                              selected = "All"
                  ),
                  div(style = "font-size: 13px; margin-top: 20px;",
                      HTML("<p><strong>Weight Classes:</strong></p>
           <p>Heavy = Heavyweight</p>
           <p>Bridger = Bridgerweight</p>
           <p>Cruiser = Cruiserweight</p>
           <p>Lheavy = Light Heavyweight</p>
           <p>SuperMid = Super Middleweight</p>
           <p>Mid = Middleweight</p>
           <p>LMid = Light Middleweight</p>
           <p>Welt = Welterweight</p>
           <p>LWelt = Light Welterweight</p>
           <p>Light = Lightweight</p>
           <p>SuperFeath = Super Featherweight</p>
           <p>Feath = Featherweight</p>
           <p>SuperBant = Super Bantamweight</p>
           <p>Bant = Bantamweight</p>
           <p>SuperFly = Super Flyweight</p>
           <p>Fly = Flyweight</p>
           <p>LFly = Light Flyweight</p>
           <p>MinFly = Minimumweight</p>")
                  )),
                
                
                # Main panel for displaying outputs ----
                mainPanel(
                  plotOutput(outputId = "barPlot")
                  
                )
  )
),tabPanel(
  "About",
  
  # App title ----
  titlePanel("Data Info/Background"),
  
  # Text content ----
  fluidRow(
    column(width = 12, 
           p("This dataset consists of all professional boxers that have been recognized by one of the big four sanctioning bodies as a world champion.")
    ),
    column(width = 12, 
           HTML("<b>Data set info</b>: <ul>
                    <li>2390 Entries</li>
                    <li>Has data on Name, Weight, Nationality, Reach, Height, # of title defenses, and reign beginning and end</li>
                    <li>Many missing values, especially in the Reach and Height columns</li>
                  </ul>")
    ),
    column(width = 12, 
           p("I scraped this data from Wikipedia. The articles are linked below. I iteratively scraped nationality, reach, and height information from each boxer's personal wikipedia page. I cleaned the data and manually imputed nationality values using information from boxrec.com.")
    )
  ),
  
  # Sources section ----
  fluidRow(
    column(width = 12, 
           HTML("<b>Sources:</b>"),
           HTML("<ul>
              <li><a href='https://en.wikipedia.org/wiki/List_of_IBF_world_champions'>List of IBF world champions</a></li>
              <li><a href='https://en.wikipedia.org/wiki/List_of_WBC_world_champions'>List of WBC world champions</a></li>
              <li><a href='https://en.wikipedia.org/wiki/List_of_WBA_world_champions'>List of WBA world champions</a></li>
              <li><a href='https://en.wikipedia.org/wiki/List_of_WBO_world_champions'>List of WBO world champions</a></li>
              <li><a href='https://boxrec.com/'>BoxRec</a></li>
            </ul>")
    )
  )
)

)
)

server <- function(input, output) {
  
  
  output$distPlot <- renderPlot({
    
    
    if (input$radio != "All" & input$nat != "All") {
      temp <- champs %>% filter(Weight == input$radio & Nationality == input$nat) 
    }
    else if (input$nat == "All" & input$radio == "All"){
      temp <- champs
    }
    else if (input$nat == "All" & input$radio != "All"){
      temp <- champs %>% filter(Weight == input$radio) 
    }
    else {
      temp <- champs %>% filter(Nationality == input$nat) 
    }
    
    
    var_hist <- switch(input$var, 
                       'Reach' = temp$Reach,
                       'Height' = temp$Height)
    
    limits <- switch(input$var,
                     'Reach' = champs$Reach,
                     'Height' = champs$Height)
    
    data <- switch(input$nat,
                   "All" = list("gray",'black'),
                   "Mexican" = list("#006847",'#CE1125'), 
                   "American" = list("#0A3161", "#B31942"),
                   "British" = list("#012169", "#C8102E"),
                   "Puerto Rican" = list("#3A5EAB", "#E92228"),
                   "Japanese" = list("#BC002D", "#FFFFFF"),
                   "Russian" = list("#1C3578", "#E4181C"),
                   "Ukrainian" = list("#0057B7", "#FFDD00"),
                   "Argentine" = list("#6CACE4", "#FFB81C"),
                   "Colombian" = list("#FFCD00", "#C8102E"),
                   "Venezuelan" = list("#FCE300", "#003DA5"),
                   "Thai" = list("#EF3340", "#00247D"),
                   "Italian" = list("#008C45", "#CD212A"),
                   "Cuban" = list("#DA291C", "#004B87"),
                   "South Korean" = list("#CD2E3A", "black"),
                   "Filipino" = list("#BF0D3E", "#FED141"),
                   "Australian" = list("#012169", "#FFFFFF"))
    
    x_label <- switch(input$var, 
                      'Reach' = 'Reach in Centimeters',
                      'Height' = 'Height in Centimeters')
    
    x = var_hist
    
    mean = mean(x, na.rm = TRUE)
    
    ggplot(temp, aes(x = x))+
      geom_density(fill = data[1], alpha = 0.5)+
      labs(x = x_label)+
      geom_vline(data=temp, aes(xintercept = mean, color=data[2]),
                 linetype="dashed")+
      theme_classic()+
      theme(axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.title.y = element_blank())+
      xlim(min(limits, na.rm = TRUE), max(limits, na.rm = TRUE))
    
  }, height = 500)
  
  output$barPlot <- renderPlot({
    
    if (input$natty != "All") {
      temp <- champs %>% filter(Nationality == input$natty) 
    }
    else {
      temp <- champs
    }
    
    
    data <- switch(input$nat,
                   "All" = list("gray",'black'),
                   "Mexican" = list("#006847",'#CE1125'), 
                   "American" = list("#0A3161", "#B31942"),
                   "British" = list("#012169", "#C8102E"),
                   "Puerto Rican" = list("#3A5EAB", "#E92228"),
                   "Japanese" = list("#BC002D", "#FFFFFF"),
                   "Russian" = list("#1C3578", "#E4181C"),
                   "Ukrainian" = list("#0057B7", "#FFDD00"),
                   "Argentine" = list("#6CACE4", "#FFB81C"),
                   "Colombian" = list("#FFCD00", "#C8102E"),
                   "Venezuelan" = list("#FCE300", "#003DA5"),
                   "Thai" = list("#EF3340", "#00247D"),
                   "Italian" = list("#008C45", "#CD212A"),
                   "Cuban" = list("#DA291C", "#004B87"),
                   "South Korean" = list("#CD2E3A", "black"),
                   "Filipino" = list("#BF0D3E", "#FED141"),
                   "Australian" = list("#012169", "#FFFFFF"))
    
    
    ggplot(temp, aes(x = fct_rev(fct_infreq(Weight)), fill = Recognition)) + 
      geom_bar(alpha = 0.5)+
      coord_flip()+
      theme_classic()+
      scale_fill_manual(values=c("#003f5c", "#58508d", "#bc5090", "#ff6361", "#ffa600"), 
                        name="Recognition",
                        labels=c("IBF", "WBA", "WBA (Regular)", "WBC", "WBO"))+
      theme(
        axis.title.y = element_blank()
      )
    
  }, height = 500)
  
}

shinyApp(ui = ui, server = server)

