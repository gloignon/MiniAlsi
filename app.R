library(shiny)
library(udpipe)
library(stringr)
library(ggplot2)
library(reshape2)

# Load the French bsd model (ensure it's downloaded and adjust path if necessary)
model <- udpipe_load_model("french-gsd-ud-2.5-191206.udpipe")

# Define UI for the application
ui <- fluidPage(
  titlePanel("French Readability and Cohesion Analyzer with UDPipe"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("corpus_zip", "Upload ZIP with TXT files (optional)", 
                accept = c(".zip")),
      textAreaInput("text", "Or enter French text directly:", value = "", 
                    placeholder = "Type or paste French text here", 
                    width = '100%', height = '200px', resize = "both"),
      actionButton("analyze", "Analyze"),
      tags$div(id = "progress", style = "display: none; margin-top: 10px;"),
      tags$style("#progress {font-size: 14px; color: #337ab7;}")
    ),
    
    mainPanel(
      h3("Readability and Cohesion Features"),
      conditionalPanel(
        condition = "output.isCorpus == false",
        tableOutput("results")
      ),
      conditionalPanel(
        condition = "output.isCorpus == true",
        plotOutput("corpusPlots")
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Helper function to calculate metrics for a given text
  calculate_metrics <- function(text) {
    annotated <- udpipe_annotate(model, x = text)
    annotated_df <- as.data.frame(annotated)
    
    word_count <- nrow(annotated_df[annotated_df$upos %in% c("NOUN", "VERB", "ADJ", "ADV"), ])
    sentence_count <- length(unique(annotated_df$sentence_id))
    syllable_count <- sum(sapply(gregexpr("[aeiouyAEIOUY]", annotated_df$token), function(x) max(0, length(x))))
    avg_sentence_length <- ifelse(sentence_count > 0, word_count / sentence_count, 0)
    avg_syllables_per_word <- ifelse(word_count > 0, syllable_count / word_count, 0)
    
    sentence_ids <- unique(annotated_df$sentence_id)
    cohesion_values <- c()
    for (i in 2:length(sentence_ids)) {
      current_sentence <- annotated_df[annotated_df$sentence_id == sentence_ids[i], "lemma"]
      previous_sentence <- annotated_df[annotated_df$sentence_id == sentence_ids[i - 1], "lemma"]
      shared_words <- length(intersect(current_sentence, previous_sentence))
      cohesion_values <- c(cohesion_values, shared_words / length(current_sentence))
    }
    avg_sentence_to_sentence_cohesion <- ifelse(length(cohesion_values) > 0, mean(cohesion_values, na.rm = TRUE), 0)
    
    text_words <- unique(annotated_df$lemma)
    text_sentence_cohesion <- sapply(sentence_ids, function(sid) {
      sentence_words <- annotated_df[annotated_df$sentence_id == sid, "lemma"]
      shared_words <- length(intersect(sentence_words, text_words))
      shared_words / length(sentence_words)
    })
    avg_text_to_sentence_cohesion <- mean(text_sentence_cohesion, na.rm = TRUE)
    
    type_token_ratio <- length(unique(annotated_df$lemma)) / word_count
    
    data.frame(
      "Word Count" = word_count,
      "Sentence Count" = sentence_count,
      "Syllable Count" = syllable_count,
      "Average Sentence Length" = round(avg_sentence_length, 2),
      "Average Syllables per Word" = round(avg_syllables_per_word, 2),
      "Sentence-to-Sentence Lexical Cohesion" = round(avg_sentence_to_sentence_cohesion, 2),
      "Text-to-Sentence Lexical Cohesion" = round(avg_text_to_sentence_cohesion, 2),
      "Type-Token Ratio" = round(type_token_ratio, 2)
    )
  }
  
  # Reactive to handle single text or corpus input
  results <- eventReactive(input$analyze, {
    if (is.null(input$corpus_zip)) {
      # Single text mode
      text <- input$text
      if (nchar(text) > 0) {
        list(data = calculate_metrics(text), isCorpus = FALSE)
      } else {
        NULL
      }
    } else {
      # Corpus mode: analyze each file in the uploaded ZIP
      temp_dir <- tempdir()
      unzip(input$corpus_zip$datapath, exdir = temp_dir)
      txt_files <- list.files(temp_dir, pattern = "\\.txt$", full.names = TRUE)
      
      corpus_metrics <- list()
      n_files <- length(txt_files)
      
      # Show progress to the user
      updateProgress <- function(value) {
        session$sendCustomMessage("updateProgress", list(value = value))
      }
      updateProgress(0)
      
      # Calculate metrics for each text file
      for (i in seq_along(txt_files)) {
        text <- readLines(txt_files[i], warn = FALSE)
        corpus_metrics[[i]] <- calculate_metrics(paste(text, collapse = " "))
        
        # Update progress
        updateProgress(i / n_files * 100)
      }
      
      # Combine metrics into a data frame
      corpus_metrics_df <- do.call(rbind, corpus_metrics)
      list(data = corpus_metrics_df, isCorpus = TRUE)
    }
  })
  
  # Display results table for single text mode
  output$results <- renderTable({
    if (!is.null(results()) && !results()$isCorpus) {
      results()$data
    }
  })
  
  # Display box plots for corpus mode, using facets for individual scales
  output$corpusPlots <- renderPlot({
    if (!is.null(results()) && results()$isCorpus) {
      corpus_metrics_df <- results()$data
      melted_df <- melt(corpus_metrics_df)
      
      ggplot(melted_df, aes(x = variable, y = value)) +
        geom_boxplot() +
        facet_wrap(~ variable, scales = "free_y") +
        labs(x = "Metric", y = "Value", title = "Corpus Analysis - Readability and Cohesion Metrics") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
  
  # Boolean for UI conditionals
  output$isCorpus <- reactive({
    !is.null(results()) && results()$isCorpus
  })
  outputOptions(output, "isCorpus", suspendWhenHidden = FALSE)
  
  # JavaScript for updating progress bar
  session$sendCustomMessage("updateProgress", list(value = 0))
  observe({
    session$sendCustomMessage("addProgress", "")
  })
  
}

# JavaScript for updating progress bar
js <- "
Shiny.addCustomMessageHandler('updateProgress', function(message) {
  if (message.value === 0) {
    $('#progress').show().html('Processing... 0%');
  } else if (message.value === 100) {
    $('#progress').html('Processing complete!').delay(1000).fadeOut();
  } else {
    $('#progress').html('Processing... ' + Math.round(message.value) + '%');
  }
});
"
tags$head(tags$script(HTML(js)))

# Run the application 
shinyApp(ui = ui, server = server)