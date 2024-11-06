library(shiny)
library(udpipe)

# Load the French bsd model (ensure it's downloaded and adjust path if necessary)
model <- udpipe_load_model("french-gsd-ud-2.5-191206.udpipe")

# Define UI for the application
ui <- fluidPage(
  titlePanel("French Readability Analyzer with UDPipe"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("text", "Enter French text:", value = "", placeholder = "Type or paste French text here"),
      actionButton("analyze", "Analyze")
    ),
    
    mainPanel(
      h3("Readability Features"),
      tableOutput("results")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  analyze_text <- eventReactive(input$analyze, {
    text <- input$text
    
    # Annotate text using udpipe with the French model
    annotated <- udpipe_annotate(model, x = text)
    annotated_df <- as.data.frame(annotated)
    
    # Calculate readability metrics
    word_count <- nrow(annotated_df[annotated_df$upos == "NOUN" | annotated_df$upos == "VERB" | 
                                      annotated_df$upos == "ADJ" | annotated_df$upos == "ADV", ])
    
    sentence_count <- length(unique(annotated_df$sentence_id))
    
    # Syllable count - count vowels in each token as an approximation
    syllable_count <- sum(sapply(gregexpr("[aeiouyAEIOUY]", annotated_df$token), function(x) max(0, length(x))))
    
    # Average sentence length (words per sentence)
    avg_sentence_length <- ifelse(sentence_count > 0, word_count / sentence_count, 0)
    
    # Average syllables per word
    avg_syllables_per_word <- ifelse(word_count > 0, syllable_count / word_count, 0)
    
    # Compile results into a data frame
    results <- data.frame(
      Metric = c("Word Count", "Sentence Count", "Syllable Count", 
                 "Average Sentence Length", "Average Syllables per Word"),
      Value = c(word_count, sentence_count, syllable_count, 
                round(avg_sentence_length, 2), round(avg_syllables_per_word, 2))
    )
    
    return(results)
  })
  
  output$results <- renderTable({
    analyze_text()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)