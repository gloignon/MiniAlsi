library(shiny)
library(udpipe)
library(stringr)

# Load the French bsd model (ensure it's downloaded and adjust path if necessary)
model <- udpipe_load_model("french-gsd-ud-2.5-191206.udpipe")

# Define UI for the application
ui <- fluidPage(
  titlePanel("French Readability and Cohesion Analyzer with UDPipe"),
  
  sidebarLayout(
    sidebarPanel(
      textAreaInput("text", "Enter French text:", value = "", 
                    placeholder = "Type or paste French text here", 
                    width      = '100%', height = '200px', resize = "both"),
      actionButton("analyze", "Analyze")
    ),
    
    mainPanel(
      h3("Readability and Cohesion Features"),
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
    
    # Calculate readability and cohesion metrics
    
    # Basic Metrics
    word_count <- nrow(annotated_df[annotated_df$upos %in% c("NOUN", "VERB", "ADJ", "ADV"), ])
    sentence_count <- length(unique(annotated_df$sentence_id))
    
    # Syllable count - count vowels in each token as an approximation
    syllable_count <- sum(sapply(gregexpr("[aeiouyAEIOUY]", annotated_df$token), function(x) max(0, length(x))))
    avg_sentence_length <- ifelse(sentence_count > 0, word_count / sentence_count, 0)
    avg_syllables_per_word <- ifelse(word_count > 0, syllable_count / word_count, 0)
    
    # Lexical Cohesion Metrics
    # Sentence-to-Sentence Lexical Cohesion (percentage of words shared between sentences)
    sentence_ids <- unique(annotated_df$sentence_id)
    cohesion_values <- c()
    for (i in 2:length(sentence_ids)) {
      current_sentence <- annotated_df[annotated_df$sentence_id == sentence_ids[i], "lemma"]
      previous_sentence <- annotated_df[annotated_df$sentence_id == sentence_ids[i - 1], "lemma"]
      shared_words <- length(intersect(current_sentence, previous_sentence))
      cohesion_values <- c(cohesion_values, shared_words / length(current_sentence))
    }
    avg_sentence_to_sentence_cohesion <- ifelse(length(cohesion_values) > 0, mean(cohesion_values, na.rm = TRUE), 0)
    
    # Text-to-Sentence Lexical Cohesion (percentage of words in each sentence shared with entire text)
    text_words <- unique(annotated_df$lemma)
    text_sentence_cohesion <- sapply(sentence_ids, function(sid) {
      sentence_words <- annotated_df[annotated_df$sentence_id == sid, "lemma"]
      shared_words <- length(intersect(sentence_words, text_words))
      shared_words / length(sentence_words)
    })
    avg_text_to_sentence_cohesion <- mean(text_sentence_cohesion, na.rm = TRUE)
    
    # Type-Token Ratio (vocabulary diversity)
    type_token_ratio <- length(unique(annotated_df$lemma)) / word_count
    
    # Results Data Frame
    results <- data.frame(
      Metric = c("Word Count", "Sentence Count", "Syllable Count", 
                 "Average Sentence Length", "Average Syllables per Word",
                 "Avg Sentence-to-Sentence Lexical Cohesion", 
                 "Avg Text-to-Sentence Lexical Cohesion", 
                 "Type-Token Ratio (Vocabulary Diversity)"),
      Value = c(word_count, sentence_count, syllable_count, 
                round(avg_sentence_length, 2), round(avg_syllables_per_word, 2),
                round(avg_sentence_to_sentence_cohesion, 2), 
                round(avg_text_to_sentence_cohesion, 2),
                round(type_token_ratio, 2))
    )
    
    return(results)
  })
  
  output$results <- renderTable({
    analyze_text()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)