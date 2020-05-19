library(shiny)
library(shinythemes)
library(colourpicker)
library(wordcloud2)
library(tm)


create_wordcloud<-function(data, num_words = 100, background = "white",language="english",
                           empty_words=c("The","And"), Word_colors="random-dark", 
                           Shape="circle"){
  
  # If text is provided, convert it to a dataframe of word frequencies
  if (is.character(data)) {
    corpus <- Corpus(VectorSource(data))
    corpus <- tm_map(corpus, tolower)
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, removeNumbers)
    corpus <- tm_map(corpus, removeWords, stopwords(language))
    corpus <- tm_map(corpus, removeWords,tolower(empty_words))
    tdm <- as.matrix(TermDocumentMatrix(corpus))
    data <- sort(rowSums(tdm), decreasing = TRUE)
    data <- data.frame(word = names(data), freq = as.numeric(data))
  }
  
  # Make sure a proper num_words is provided
  if (!is.numeric(num_words) || num_words < 3) {
    num_words <- 3
  }  
  
  # Grab the top n most common words
  data <- head(data, n = num_words)
  if (nrow(data) == 0) {
    return(NULL)
  }
  
  wordcloud2(data, backgroundColor = background, shape=Shape, color=Word_colors)
}

#===============================================UI========================================================================
ui <- fluidPage(
  titlePanel("Word cloud creator"),
  h5("By Fernando Rodriguez"),
  theme = shinytheme('paper'),
  sidebarLayout(
    sidebarPanel(
      radioButtons("source", "Word source",choices = c("Upload a file" = "file",
                                                       "Use your own words" = "own"
        )
      ),
      radioButtons("language","Select the language",choices=c("English"="english",
                                                              "Spanish"="spanish",
                                                              "Italian"="italian")),
      conditionalPanel(condition = "input.source == 'file'",
                       fileInput("file", "Select a file")),
      conditionalPanel(condition = "input.source == 'own'",
                       textAreaInput("text", "Enter text below", rows = 7)),
      numericInput("num", "Maximum number of words",
                   value = 100, min = 5),
      textAreaInput("remove","Write words to remove bellow (separate with space)"),
      colourInput("col", "Background color", value = "white"),
      radioButtons("cloud_color","Select word colors",choices = c("Dark palette"="random-dark",
                                                                  "Light palette"="random-light",
                                                                  "Black"="black",
                                                                  "White"="white")),
      selectInput("shape","Cloud shape",choices = c("Circle"="circle","Star"="star",
                                                    "Diamond"="diamond","Foward triangle"="triangle-forward",
                                                    "Triangle"="triangle", "Pentagon"="pentagon")),
      # Add a "draw" button to the app
      actionButton(inputId = "draw", label = "Generate cloud")
    ),
    mainPanel(tabsetPanel(
      tabPanel("Word cloud", wordcloud2Output("cloud", height = 700)),
      tabPanel("Instructions",
               h5("First"), h6("Select the data source from which you want to generate the word cloud: It could be a .txt file with all the text by selecting Upload a file, or you can copy and paste your text by selecting Use your own words"),
               h5("Second"),h6("Select the language of your text, the tool will automatically remove the stop words"),
               h4("Click on GENERATE CLOUD at the bottom of the page to see the first word cloud. It can be enhanced by modifying the following parameters"),
               h5("Third (optional)"),h6("Select the maximum words to appear in the word cloud"),
               h5("Fourth (suggested)"), h6("Write the words you will not like to see"),
               h5("Sixth (optional)"), h6("Select a background color from the palette"),
               h5("Seventh (optional)"),h6("Select a color theme for the words"),
               h5("Eighth (optional)"),h6("Select the shape of the cloud"))
    )
    )
  )
)

#==========================================Server=============================================================


server <- function(input, output) {
  data_source <- reactive({
    if (input$source == "own") {
      data <- input$text
    } else if (input$source == "file") {
      data <- input_file()
    } 
    return(data)
  })
  
  input_file <- reactive({
    if (is.null(input$file)) {
      return("")
    }
    readLines(input$file$datapath)
  })
  
  remove_words<-reactive({
    input$draw
    isolate({
      as.vector(unlist(strsplit(input$remove, " ")))
    })
  })
  
  output$cloud <- renderWordcloud2({
    # Add the draw button as a dependency to cause the word cloud to re-render on click
    input$draw
    isolate({
      create_wordcloud(data_source(), num_words = input$num,
                       background = input$col,language = input$language,
                       empty_words=remove_words(),Word_colors=input$cloud_color,
                       Shape=input$shape)
    })
  })
  

}

shinyApp(ui = ui, server = server)