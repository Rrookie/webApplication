#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


# global.r

library(tm)
library(wordcloud)
library(memoise)

# The list of valid papers
papers <- list("Vitamin D and calcium for the prevention of fracture" = "CaD", # CaD.txt in the working directory
               "Gender differences in modifiable risk factors for hip fracture" = "risk",
               "Sero-prevalence of 19 infectious pathogens and associated factors among Chinese" = "seroprevalence")

# Using "memoise" to automatically cache the results
getTermMatrix <- memoise(function(paper) {
  # Careful not to let just any name slip in here; a
  # malicious user could manipulate this value.
  if (!(paper %in% papers))
    stop("Unknown paper")
  
  text <- readLines(sprintf("./%s.txt", paper),
                    encoding="UTF-8")
  
  myCorpus = Corpus(VectorSource(text))
  myCorpus = tm_map(myCorpus, content_transformer(tolower))
  myCorpus = tm_map(myCorpus, removePunctuation)
  myCorpus = tm_map(myCorpus, removeNumbers)
  myCorpus = tm_map(myCorpus, removeWords,
                    c(stopwords("SMART"), "thy", "thou", "thee", "the", "and", "but"))
  
  myDTM = TermDocumentMatrix(myCorpus,
                             control = list(minWordLength = 1))
  
  m = as.matrix(myDTM)
  
  sort(rowSums(m), decreasing = TRUE)
})




# Define UI for application that draws a word cloud

ui <- fluidPage(
  # Application title
  titlePanel("Word Cloud"),
  
  sidebarLayout(
    # Sidebar with a slider and selection inputs
    sidebarPanel(
      selectInput("selection", "Choose a paper:",choices = papers),
      actionButton("update", "Change"),
      hr(),
      sliderInput("freq", "Minimum Frequency:", min = 1,  max = 50, value = 15),
      sliderInput("max","Maximum Number of Words:", min = 1,  max = 300,  value = 100)
    ),
    
    # Show Word Cloud
    mainPanel(
      plotOutput("plot")
    )
  )
)



# Define server logic required to draw a word cloud

server <- function(input, output, session) {
  # Define a reactive expression for the document term matrix
  terms <- reactive({
    # Change when the "update" button is pressed...
    input$update
    # ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        getTermMatrix(input$selection)
      })
    })
  })
  
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  
  output$plot <- renderPlot({
    v <- terms()
    wordcloud_rep(names(v), v, scale=c(4,0.5),
                  min.freq = input$freq, max.words=input$max,
                  random.order=FALSE, rot.per=0.35,
                  colors=brewer.pal(8, "Dark2"))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)



runGitHub("webApplication", "Rrookie")


library(rsconnect)
rsconnect::deployApp('C:/Projects/R in epidemiology/first')


