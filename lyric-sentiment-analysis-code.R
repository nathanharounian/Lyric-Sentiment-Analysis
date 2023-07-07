# ===============================================
# Fill in the following fields
# ===============================================
# Title: U2 Lyrics Analysis Application
# Description: A lyrical analysis of U2 albums, analysizing lyric frequency and associated sentiment with various conditioning options for the user to decide.
# Author: Nathan Harounian
# Date: 12/05/2021


# ===============================================
# Packages
# ===============================================
library(tidyverse)
library(tidytext)
library(wordcloud)    # for plotting word clouds
library(randomcoloR)  # random color palettes
library(igraph)       # for computing networks
library(ggraph)       # graphing networks "a la" ggplot


# ===============================================
# Import data
# ===============================================
# for demo purposes of the "template", we use data starwars
# (but you will have to replace this with the data in "u2-lyrics.csv")
dat <- read.csv('u2-lyrics.csv')


# ===============================================
# Define UserInterface "ui" for application
# ===============================================

ui <- fluidPage(
  
  titlePanel("U2 Lyrics Analysis Application"),
  fluidRow(
    # replace with your widgets
    column(3,
           p(em("Type of Analysis")),
           radioButtons(inputId = "type", 
                        label = "All Words or Without Stopwords", 
                        choices = c("with all words",
                                    "without stopwords"),
                        selected = "with all words")
    ),
    
    # replace with your widgets
    column(3,
           p(em("Number of Displays")),
           sliderInput(inputId = "top", 
                       label = "Top _ Most Frequent Words in Lyrics",
                       value = 5,
                       min = 1,
                       max = 20,
                       step = 1)
    ),
    
    # replace with your widgets
    column(3,
           p(em("Album Selection")),
           selectInput(inputId = "album", 
                       label = "Make a selection",
                       choices = c('ALL ALBUMS', 'PER ALBUM', unique(dat$album)),
                       selected = "ALL ALBUMS")
           
    ),
    
    # replace with your widgets
    column(3,
           p(em("Type of Analysis #2")),
           checkboxGroupInput(inputId = "sentiment", 
                              label = "Sentiment Analysis (PICK ONE)", 
                              choices = list("Classic Sentiment Analysis", "Sentiment Trajectory Analysis", "Sentiment Contribution Analysis"),
                              selected = "Classic Sentiment Analysis")
    )
  ),
  hr(),
  
  tabsetPanel(type = "tabs",
              tabPanel("Analysis #1",
                       h3("Word Frequency Analysis"),
                       plotOutput("barplot1"),
                       hr(),
                       verbatimTextOutput('table1')),
              tabPanel("Analysis #2", 
                       h3("Sentiment Analysis"),
                       plotOutput("barplot2"),
                       hr(),
                       verbatimTextOutput('table2'))
  )
)


# ===============================================
# Define Server "server" logic
# ===============================================

server <- function(input, output) {
  
  # you may need to create reactive objects
  # (e.g. data frame to be used in barchart)
  dat_freq <- reactive({
    #variables
    top = input$top
    album = input$album
    
    if (input$type == 'with all words') {
      if (album == 'ALL ALBUMS') {
        #tokenizing all lyrics in the u2 dataframe
        u2_all_tokens = unnest_tokens(tbl = dat, output = word, input = lyrics)
        
        #counting frequencies
        u2_all_freqs = count(u2_all_tokens, word)
        
        #top n words
        u2_top_n = u2_all_freqs %>% arrange(desc(n)) %>% slice_head(n = top)
        return(u2_top_n)
      }
      
      else if (album == 'PER ALBUM') {
        # tokenize the data
        u2_all_tokens = unnest_tokens(tbl = dat, output = word, input = lyrics)
        
        # counting frequencies
        u2_all_freqs = u2_all_tokens %>% count(album, word, sort = T)
        
        # top n words
        u2_top_n = u2_all_freqs %>% arrange(desc(n))
        return(u2_top_n)
      }
      
      else {
        ## for a given album
        
        # filtering data set for specific album
        filtered = filter(dat, album == input$album)
        filtered
        
        #tokenizing all lyrics in the filtered dataframe
        filtered_all_tokens = unnest_tokens(tbl = filtered, output = word, input = lyrics)
        filtered_all_tokens
        
        #counting frequencies
        filtered_all_freqs = count(filtered_all_tokens, word)
        
        #top n words
        filtered_top_n = filtered_all_freqs %>% arrange(desc(n)) %>% slice_head(n = top)
        return(filtered_top_n)
      }
    }
    
    else {
      if (album == 'ALL ALBUMS') {
        #tokenizing all lyrics in the u2 dataframe
        u2_all_tokens = unnest_tokens(tbl = dat, output = word, input = lyrics)
        
        # remove stopwords, and recalculate frequencies
        tidy_u2 = u2_all_tokens %>% anti_join(stop_words, by = 'word') %>% count(word)
        
        #top n words without stopwords
        u2_top_n = tidy_u2 %>% arrange(desc(n)) %>% slice_head(n = top)
        return(u2_top_n)
      }
      
      else if (album == 'PER ALBUM') {
        # tokenize the data
        u2_all_tokens = unnest_tokens(tbl = dat, output = word, input = lyrics)
        
        # remove stopwords
        tidy_u2 = u2_all_tokens %>% anti_join(stop_words, by = 'word')
        
        # counting frequencies
        u2_all_freqs = tidy_u2 %>% count(album, word, sort = T) 
        
        # top n words
        u2_top_n = u2_all_freqs %>% arrange(desc(n))
        return(u2_top_n)
      }
      
      else {
        ## for a given album
        
        # filtering data set for specific album
        filtered = filter(dat, album == input$album)
        
        #tokenizing all lyrics in the u2 dataframe
        filtered_all_tokens = unnest_tokens(tbl = filtered, output = word, input = lyrics)
        
        # remove stopwords, and recalculate frequencies
        tidy_filtered = filtered_all_tokens %>% anti_join(stop_words, by = 'word') %>% count(word)
        
        #top n words without stopwords
        filtered_top_n = tidy_filtered %>% arrange(desc(n)) %>% slice_head(n = top)
        return(filtered_top_n)
      }
    }
  })
  
  dat_sent <- reactive({
    #variables
    top = input$top
    album = input$album
    
    if (input$type == 'with all words') {
      if (album == 'ALL ALBUMS') {
        if (input$sentiment == 'Classic Sentiment Analysis') {
          ### With All words, for ALL Albums, Classic Sentiment Analysis
          
          
          #tokenizing all lyrics
          u2_lyrics = dat %>% unnest_tokens(output = word, input = lyrics)
          
          # count lyric occurences
          u2_freqs = u2_lyrics %>% count(word, sort = T) %>% ungroup()
          
          # sentiments with inner join
          u2_sentiments = u2_freqs %>% 
            inner_join(sentiments, by = 'word') %>% slice_head(n = top)
          return(u2_sentiments)
        }
        
        else if (input$sentiment == 'Sentiment Trajectory Analysis') {
          ### With All Words, for All Albums, Sentiment Trajectory Analysis
          
          
          # Change of Sentiments Throughout Album
          
          u2_tokens = dat %>% mutate(
              linenumber = row_number()
            ) %>% ungroup() %>% unnest_tokens(output = word, input = lyrics)
          
          # find sentiment score for each word
          u2_sentiments = u2_tokens %>% inner_join(sentiments, by = 'word') %>%
            count(word, index = linenumber %/% 1, sentiment) %>%
            spread(sentiment, n, fill = 0) %>%
            mutate(sentiment = positive - negative)
          return(u2_sentiments)
        }
        
        else if (input$sentiment == 'Sentiment Contribution Analysis') {
          ### With All Words, for All Albums, Sentiment Contribution Analysis
          
          #tokenizing lyrics
          u2_tokens = dat %>% mutate(
            linenumber = row_number()
          ) %>% ungroup() %>% unnest_tokens(output = word, input = lyrics)
          
          # bring word counts
          bing_word_counts <- u2_tokens %>%
            inner_join(sentiments, by = "word") %>%
            count(word, sentiment, sort = TRUE) %>%
            ungroup()
          return(bing_word_counts)
        }
      }
      
      else if (album == 'PER ALBUM') {
        if (input$sentiment == 'Classic Sentiment Analysis') {
          ### With all words, Per Album, Classic Sentiment Analysis

          ### NOT POSSIBLE
        }
        
        else if (input$sentiment == 'Sentiment Trajectory Analysis') {
          ### With all words, Per Album, Sentiment Trajectory Analysis
          
          #tokenizing all lyrics
          u2_tokens = u2 %>% group_by(album) %>% mutate(
            linenumber = row_number()
          ) %>% ungroup() %>% unnest_tokens(output = word, input = lyrics)
          
          # find sentiment score for each word
          u2_sentiments = u2_tokens %>% inner_join(sentiments, by = 'word') %>%
            count(album, index = linenumber %/% 1, sentiment) %>%
            spread(sentiment, n, fill = 0) %>%
            mutate(sentiment = positive - negative)
          return(u2_sentiments)
        }
        
        # else if (input$sentiment == 'Sentiment Contribution Analysis) {
        #   ### With All words, Per Album, Sentiment Contribution Analysis
        #   
        #   
        # }
      }
      
      else {
        if (input$sentiment == 'Classic Sentiment Analysis') {
          ### With all words, For a Given Album, Classic Sentiment Analysis
          
          
          ## for a given album
          
          #tokenizing all lyrics
          u2_lyrics = dat %>% unnest_tokens(output = word, input = lyrics)
          
          # count lyric occurences
          u2_freqs = u2_lyrics %>% count(album, word, sort = T) %>% ungroup()
          
          # sentiments with inner join
          u2_sentiments = u2_freqs %>% filter(album == input$album) %>% 
            inner_join(sentiments, by = 'word') %>% slice_head(n = top)
          return(u2_sentiments)
        }
        
        else if (input$sentiment == 'Sentiment Trajectory Analysis') {
          ### With all words, for a Given Album, Sentiment Trajectory Analysis
          u2_tokens = u2 %>% mutate(
            linenumber = row_number()
          ) %>% ungroup() %>% unnest_tokens(output = word, input = lyrics)
          
          # find sentiment score for each word
          u2_sentiments = u2_tokens %>% inner_join(sentiments, by = 'word') %>%
            count(album, word, index = linenumber %/% 1, sentiment) %>%
            spread(sentiment, n, fill = 0) %>%
            mutate(sentiment = positive - negative) %>% filter(album == input$album)
          return(u2_sentiments)
        }
        
        else if (input$sentiment == 'Sentiment Contribution Analysis') {
          ### With all words, for a Given Album, Sentiment Contribution Analysis
          
          #tokenizing lyrics
          u2_tokens = u2 %>% mutate(linenumber = row_number()) %>% 
            ungroup() %>% unnest_tokens(output = word, input = lyrics)
          
          # find sentiment score for each word
          u2_sentiments = u2_tokens %>% inner_join(sentiments, by = 'word') %>%
            count(album, word, sentiment, sort = T) %>%
            ungroup() %>% filter(album == input$album)
          return(u2_sentiments)
        }
      }
    }
    
    else {
      if (album == 'ALL ALBUMS') {
        if (input$sentiment == 'Classic Sentiment Analysis') {
          ### Without Stopwords, for All Albums, Classic Sentiment Analysis
          
          #tokenizing all lyrics
          u2_lyrics = dat %>% unnest_tokens(output = word, input = lyrics) %>% 
            anti_join(stop_words, by = 'word')
          
          # count lyric occurences
          u2_freqs = u2_lyrics %>% count(word, sort = T) %>% ungroup()
          
          # sentiments with inner join
          u2_sentiments = u2_freqs %>% 
            inner_join(sentiments, by = 'word') %>% slice_head(n = top)
          return(u2_sentiments)
        }
        
        else if (input$sentiment == 'Sentiment Trajectory Analysis') {
          ### Without Stopwords, for All Albums, Sentiment Trajectory Analysis
          
          # Change of Sentiments Throughout Album
          u2_tokens = dat %>% mutate(
            linenumber = row_number()
          ) %>% ungroup() %>% unnest_tokens(output = word, input = lyrics) %>%
            anti_join(stop_words, by = 'word') ##### MIGHT NEED TO PUT ANTI_JOIN BEFORE YOU ADD LINE NUMBER
          
          # find sentiment score for each word
          u2_sentiments = u2_tokens %>% inner_join(sentiments, by = 'word') %>%
            count(word, index = linenumber %/% 1, sentiment) %>%
            spread(sentiment, n, fill = 0) %>%
            mutate(sentiment = positive - negative)
          return(u2_sentiments)
        }
        
        else if (input$sentiment == 'Sentiment Contribution Analysis') {
          ### Without Stopwords, for All Albums, Sentiment Contribution Analysis
          
          #tokenizing lyrics
          u2_tokens = dat %>% mutate(
            linenumber = row_number()
          ) %>% ungroup() %>% unnest_tokens(output = word, input = lyrics) %>%
            anti_join(stop_words, by = 'word')
          
          # bring word counts
          bing_word_counts <- u2_tokens %>%
            inner_join(sentiments, by = "word") %>%
            count(word, sentiment, sort = TRUE) %>%
            ungroup()
          return(bing_word_counts)
        }
      }
      
      else if (album == 'PER ALBUM') {
        if (input$sentiment == 'Classic Sentiment Analysis') {
          ### Without Stopwords, Per Album, Classic Sentiment Analysis

          ### NOT POSSIBLE
        }
        
        else if (input$sentiment == "Sentiment Trajectory Analysis") {
          ### Without Stopwords, Per Album, Sentiment Trajectory Analysis
          
          #tokenizing all lyrics
          u2_tokens = u2 %>% group_by(album) %>% mutate(
            linenumber = row_number()
          ) %>% ungroup() %>% unnest_tokens(output = word, input = lyrics) %>%
            anti_join(stop_words, by = 'word') ##### MIGHT NEED TO PUT ANTI_JOIN BEFORE YOU ADD LINE NUMBER
          
          # find sentiment score for each word
          u2_sentiments = u2_tokens %>% inner_join(sentiments, by = 'word') %>%
            count(album, index = linenumber %/% 1, sentiment) %>%
            spread(sentiment, n, fill = 0) %>%
            mutate(sentiment = positive - negative)
          return(u2_sentiments)
        }
        
        # else if (input$sentiment == 'Sentiment Contribution Analysis) {
        #   ### Without Stopwords, Per Album, Sentiment Contribution Analysis
        #   
        #   
        # }
      }
      
      else {
        if (input$sentiment == "Classic Sentiment Analysis") {
          ### Without Stopwords, For a Given Album, Classic Sentiment Analysis
          
          #tokenizing all lyrics
          u2_lyrics = dat %>% unnest_tokens(output = word, input = lyrics) %>% 
            anti_join(stop_words, by = 'word')
          
          # count lyric occurences
          u2_freqs = u2_lyrics %>% count(album, word, sort = T) %>% ungroup()
          
          # sentiments with inner join
          u2_sentiments = u2_freqs %>% filter(album == input$album) %>% 
            inner_join(sentiments, by = 'word') %>% slice_head(n = top)
          return(u2_sentiments)
        }
        
        else if (input$sentiment == 'Sentiment Trajectory Analysis') {
          ### Without Stopwords, for a Given Album, Sentiment Trajectory Analysis
          
          u2_tokens = u2 %>% mutate(
            linenumber = row_number()
          ) %>% ungroup() %>% unnest_tokens(output = word, input = lyrics) %>%
            anti_join(stop_words, by = 'word') ##### MIGHT NEED TO PUT ANTI_JOIN BEFORE YOU ADD LINE NUMBER
          
          # find sentiment score for each word
          u2_sentiments = u2_tokens %>% inner_join(sentiments, by = 'word') %>%
            count(album, word, index = linenumber %/% 1, sentiment) %>%
            spread(sentiment, n, fill = 0) %>%
            mutate(sentiment = positive - negative) %>% filter(album == input$album)
          return(u2_sentiments)
        }
        
        else if (input$sentiment == 'Sentiment Contribution Analysis') {
          ### Without Stopwords, for a Given Album, Sentiment Contribution Analysis
          
          #tokenizing lyrics
          u2_tokens = u2 %>% mutate(linenumber = row_number()) %>% 
            ungroup() %>% unnest_tokens(output = word, input = lyrics) %>% 
            anti_join(stop_words, by = 'word')
          
          # find sentiment score for each word
          u2_sentiments = u2_tokens %>% inner_join(sentiments, by = 'word') %>%
            count(album, word, sentiment, sort = T) %>%
            ungroup() %>% filter(album == input$album)
          return(u2_sentiments)
        }
      }
    }
    
  })
  
  
  # ===============================================
  # Outputs for the first TAB (i.e. barchart)
  # ===============================================
  
  # code for barplot1
  output$barplot1 <- renderPlot({
    # replace the code below with your code!!!
    if (input$type == 'with all words') {
      if (input$album == 'ALL ALBUMS') {
        ggplot(data = dat_freq(),
               aes(x = reorder(word, -n), y = n)) +
          geom_col(fill = 'light blue') + 
          labs(title = paste0('Top ', input$top, ' Frequent Words Among All U2 Albums')) +
          xlab("word") +
          ylab("count") +
          theme_minimal() +
          theme(plot.title = element_text(face = 'bold', hjust = 0.5, size = 20),
                plot.subtitle = element_text(face = 'bold', hjust = 0.5),
                plot.caption = element_text(face = 'bold', size = 10, color = 'darkgrey', hjust = 0.5),
                axis.title.x = element_text(hjust = 0.5),
                axis.title.y = element_text(face = 'bold', hjust = 0.5),
                axis.text.x = element_text(face = 'bold', hjust = 0.5, size = 15, color = 'black'),
                legend.position = 'none')
      }
      
      else if (input$album == 'PER ALBUM') {
        dat_freq() %>%
          arrange(desc(n)) %>%
          group_by(album) %>%
          top_n(input$top) %>%
          ggplot() +
          geom_col(aes(reorder_within(word, n, album), n, fill = album)) +
          scale_x_reordered() +
          facet_wrap(~ album, scales = 'free') +
          xlab(NULL) +
          coord_flip() +
          labs(title = paste0('Top ', input$top, ' Frequent Words Per U2 Album'))  +
          theme_bw() +
          theme(plot.title = element_text(face = 'bold', hjust = 0.5),
                plot.subtitle = element_text(face = 'bold', hjust = 0.5),
                plot.caption = element_text(face = 'bold', size = 10, color = 'darkgrey', hjust = 0.5),
                axis.title.x = element_text(hjust = 0.5),
                axis.title.y = element_text(face = 'bold', hjust = 0.5),
                axis.text.y = element_text(face = 'bold', hjust = 0.5, size = 10, color = 'black'),
                legend.position = 'none')
      }
      
      else {
        ggplot(data = dat_freq(),
               aes(x = reorder(word, -n), y = n)) +
          geom_col(fill = 'light blue') + 
          labs(title = paste0('Top ', input$top, ' Frequent Words In ', "'", input$album, "'")) +
          xlab("word") +
          ylab("count") +
          theme_minimal() +
          theme(plot.title = element_text(face = 'bold', hjust = 0.5, size = 20),
                plot.subtitle = element_text(face = 'bold', hjust = 0.5),
                plot.caption = element_text(face = 'bold', size = 10, color = 'darkgrey', hjust = 0.5),
                axis.title.x = element_text(hjust = 0.5),
                axis.title.y = element_text(face = 'bold', hjust = 0.5),
                axis.text.x = element_text(face = 'bold', hjust = 0.5, size = 15, color = 'black'),
                legend.position = 'none')
      }
    }
    
    else {
      if (input$album == 'ALL ALBUMS') {
        ggplot(data = dat_freq(),
               aes(x = reorder(word, -n), y = n)) +
          geom_col(fill = 'light blue') + 
          labs(title = paste0('Top ', input$top, ' Frequent Words (Without Stopwords) Among All U2 Albums')) +
          xlab("word") +
          ylab("count") +
          theme_minimal() +
          theme(plot.title = element_text(face = 'bold', hjust = 0.5, size = 20),
                plot.subtitle = element_text(face = 'bold', hjust = 0.5),
                plot.caption = element_text(face = 'bold', size = 10, color = 'darkgrey', hjust = 0.5),
                axis.title.x = element_text(hjust = 0.5),
                axis.title.y = element_text(face = 'bold', hjust = 0.5),
                axis.text.x = element_text(face = 'bold', hjust = 0.5, size = 15, color = 'black'),
                legend.position = 'none')
      }
      
      else if (input$album == 'PER ALBUM') {
        dat_freq() %>%
          arrange(desc(n)) %>%
          group_by(album) %>%
          top_n(input$top) %>%
          ggplot() +
          geom_col(aes(reorder_within(word, n, album), n, fill = album)) +
          scale_x_reordered() +
          facet_wrap(~ album, scales = 'free') +
          xlab(NULL) +
          coord_flip() +
          labs(title = paste0('Top ', input$top, ' Frequent Words (Without Stopwords) Per U2 Album')) +
          theme(plot.title = element_text(face = 'bold', hjust = 0.5),
                plot.subtitle = element_text(face = 'bold', hjust = 0.5),
                plot.caption = element_text(face = 'bold', size = 10, color = 'darkgrey', hjust = 0.5),
                axis.title.x = element_text(hjust = 0.5),
                axis.title.y = element_text(face = 'bold', hjust = 0.5),
                axis.text.y = element_text(face = 'bold', hjust = 0.5, size = 10, color = 'black'),
                legend.position = 'none')
      }
      
      else {
        ggplot(data = dat_freq(),
               aes(x = reorder(word, -n), y = n)) +
          geom_col(fill = 'light blue') + 
          labs(title = paste0('Top ', input$top, ' Frequent Words (Without Stopwords) In ', "'", input$album, "'")) +
          xlab("word") +
          ylab("count") +
          theme_bw() +
          theme(plot.title = element_text(face = 'bold', hjust = 0.5, size = 20),
                plot.subtitle = element_text(face = 'bold', hjust = 0.5),
                plot.caption = element_text(face = 'bold', size = 10, color = 'darkgrey', hjust = 0.5),
                axis.title.x = element_text(hjust = 0.5),
                axis.title.y = element_text(face = 'bold', hjust = 0.5),
                axis.text.x = element_text(face = 'bold', hjust = 0.5, size = 15, color = 'black'),
                legend.position = 'none')
      }
    }

  })
  
  # code for numeric summaries of frequencies
  output$table1 <- renderPrint({
    # replace the code below with your code!!!
    if (input$type == 'with all words') {
      if (input$album == 'ALL ALBUMS') {
        paste0("The most frequent word '", dat_freq()[1,1], "'", ' was said ', dat_freq()[1,2]/dat_freq()[2,2], ' more times than the 2nd most frequent word and ',
               dat_freq()[1,2]/dat_freq()[input$top,2], ' more times than the ', input$top, 'th most frequent word among all U2 albums.')
      }
      
      else if (input$album == 'PER ALBUM') {
        paste0("'", dat_freq()[1,2], "'", ' was the most frequent word in 11/17 U2 albums and was most used (', dat_freq()[1,3], ' times) in ', "'", 
               dat_freq()[1,1], "'")

      }
      
      else {
        paste0("The most frequent word '", dat_freq()[1,1], "'", " was said ", dat_freq()[1,2]/dat_freq()[input$top, 2], " more times than '", dat_freq()[input$top, 1], "', which was the least frequent word out of the top ", input$top, " that you selected to see.")

      }
    }
    
    else {
      if (input$album == 'ALL ALBUMS') {
      paste0("The most frequent non-stopword '", dat_freq()[1,1], "'", ' was said ', dat_freq()[1,2]/dat_freq()[2,2], ' more times than the 2nd most frequent non-stopword and ',
            dat_freq()[1,2]/dat_freq()[input$top,2], ' more times than the ', input$top, 'th most frequent non-stopword among all U2 albums.')
      }

      
      else if (input$album == 'PER ALBUM') {
        paste0("'", dat_freq()[1,2], "'", ' was the most frequent non-stopword in 8/17 U2 albums and was most used (', dat_freq()[1,3], ' times) in ', "'", 
               dat_freq()[1,1], "'")

      }
      
      else {
        paste0("The most frequent non-stopword '", dat_freq()[1,1], "'", " was said ", dat_freq()[1,2]/dat_freq()[input$top, 2], " more times than '", dat_freq()[input$top, 1], "', which was the least frequent non-stopword out of the top ", input$top, " that you selected to see.")

      }

    }
    })
  
  
  # ===============================================
  # Outputs for the second TAB (i.e. barchart)
  # ===============================================
  
  # code for barplot2
  output$barplot2 <- renderPlot({
    # replace the code below with your code!!!
    if (input$type == 'with all words') {
      if (input$album == 'ALL ALBUMS') {
        if (input$sentiment == 'Classic Sentiment Analysis') {
          ### With all Words, for All Albums, Classic Sentiment Analysis
          ggplot(dat_sent()) +
            geom_col(aes(x = reorder(word, n), y = n, fill = sentiment)) +
            coord_flip() +
            labs(title = paste0('Top ', input$top, ' Most Common Words with an Associated Sentiment Among All U2 Albums')) +
            ylab("count")  +
            xlab('word') +
            theme_bw() +
            theme(plot.title = element_text(face = 'bold', hjust = 0.5, size = 16),
                  plot.subtitle = element_text(face = 'bold', hjust = 0.5),
                  plot.caption = element_text(face = 'bold', size = 10, color = 'darkgrey', hjust = 0.5),
                  axis.title.x = element_text(face = 'bold', hjust = 0.5),
                  axis.title.y = element_text(face = 'bold', hjust = 0.5),
                  axis.text.y = element_text(face = 'bold', hjust = 0.5, size = 15, color = 'black'),
                  legend.title = element_text(face = 'bold'),
                  legend.text = element_text(face = 'bold'))
        }
        
        else if (input$sentiment == 'Sentiment Trajectory Analysis') {
          ### With all Words, for All Albums, Sentiment Trajectory Analysis
          ggplot(data = dat_sent(),
                 aes(x = index, y = sentiment)) +
            geom_col(show.legend = F, fill = 'orange') +
            theme_bw() +
            labs(title = 'Sentiments Throughout All U2 Albums') +
            ylab('Sentiment Score Difference (Positive - Negative) using Bing Lexicon') +
            xlab(NULL) +
            geom_hline(yintercept = 0, color = 'red') +
            theme_bw() +
            theme(plot.title = element_text(face = 'bold', hjust = 0.5, size = 16),
                  plot.subtitle = element_text(face = 'bold', hjust = 0.5),
                  plot.caption = element_text(face = 'bold', size = 10, color = 'darkgrey', hjust = 0.5),
                  axis.title.x = element_text(face = 'bold', hjust = 0.5),
                  axis.title.y = element_text(face = 'bold', hjust = 0.5),
                  axis.text.y = element_text(face = 'bold', hjust = 0.5, size = 15, color = 'black'),
                  legend.title = element_text(face = 'bold'),
                  legend.text = element_text(face = 'bold'))
        }
        
        else if (input$sentiment == 'Sentiment Contribution Analysis') {
          ### With all Words, for All Albums, Sentiment Contribution Analysis
          
          dat_sent() %>%
            group_by(sentiment) %>%
            top_n(input$top) %>%
            ungroup() %>%
            mutate(word = reorder(word, n)) %>%
            ggplot(aes(word, n, fill = sentiment)) +
            geom_col(show.legend = FALSE) +
            facet_wrap(~ sentiment, scales = "free_y") +
            labs(y = "Contribution to Sentiment",
                 x = NULL,
                 title = "Lyrics That Contribute to Positive and Negative Sentiments Among All U2 Albums") +
            coord_flip() +
            theme_bw() +
            theme(plot.title = element_text(face = 'bold', hjust = 0.5, size = 16),
                  plot.subtitle = element_text(face = 'bold', hjust = 0.5),
                  plot.caption = element_text(face = 'bold', size = 10, color = 'darkgrey', hjust = 0.5),
                  axis.title.x = element_text(face = 'bold', hjust = 0.5, size = 12),
                  axis.title.y = element_text(face = 'bold', hjust = 0.5),
                  axis.text.y = element_text(face = 'bold', hjust = 0.5, size = 15, color = 'black'),
                  legend.title = element_text(face = 'bold'),
                  legend.text = element_text(face = 'bold'))
        }
      }
    
      
      else if (input$album == 'PER ALBUM') {
        if (input$sentiment == 'Classic Sentiment Analysis') {
          ### With All Words, Per Album, Classic Sentiment Analysis

          ### NOT POSSIBLE
        }
        
        else if (input$sentiment == 'Sentiment Trajectory Analysis') {
          ### With all Words, Per Album, Sentiment Trajectory Analysis
          ggplot(data = dat_sent(),
                 aes(x = index, y = sentiment, fill = album)) +
            geom_col(show.legend = F) +
            facet_wrap(~album, scales = 'free_y') +
            theme_bw() +
            labs(title = 'Sentiments Throughout All U2 Songs (Per Album)') +
            ylab('Sentiment Score Difference (Positive - Negative) using Bing Lexicon') +
            xlab(NULL) +
            geom_hline(yintercept = 0, color = 'red') +
            theme_bw() +
            theme(plot.title = element_text(face = 'bold', hjust = 0.5, size = 16),
                  plot.subtitle = element_text(face = 'bold', hjust = 0.5),
                  plot.caption = element_text(face = 'bold', size = 10, color = 'darkgrey', hjust = 0.5),
                  axis.title.x = element_text(face = 'bold', hjust = 0.5),
                  axis.title.y = element_text(face = 'bold', hjust = 0.5),
                  axis.text.y = element_text(face = 'bold', hjust = 0.5, size = 15, color = 'black'),
                  legend.title = element_text(face = 'bold'),
                  legend.text = element_text(face = 'bold'))
        }
        
        # else if (input$sentiment == 'Sentiment Contribution Analysis) {
        #   ### With all Words, Per Album, Sentiment Contribution Analysis
        # 
        # }
      }
        
      
    else {
      if (input$sentiment == "Classic Sentiment Analysis") {
        ### With all Words, For a Given Album, Classic Sentiment Analysis
        ggplot(dat_sent()) +
          geom_col(aes(x = reorder(word, n), y = n, fill = sentiment)) +
          coord_flip() +
          labs(title = paste0('Top ', input$top, ' Most Common Words with an Associated Sentiment in ', "'", input$album, "'")) +
          ylab("count")  +
          xlab('word') +
          theme_bw() +
          theme(plot.title = element_text(face = 'bold', hjust = 0.5, size = 16),
                plot.subtitle = element_text(face = 'bold', hjust = 0.5),
                plot.caption = element_text(face = 'bold', size = 10, color = 'darkgrey', hjust = 0.5),
                axis.title.x = element_text(face = 'bold', hjust = 0.5),
                axis.title.y = element_text(face = 'bold', hjust = 0.5),
                axis.text.y = element_text(face = 'bold', hjust = 0.5, size = 15, color = 'black'),
                legend.title = element_text(face = 'bold'),
                legend.text = element_text(face = 'bold'))
      }
      
      else if (input$sentiment == 'Sentiment Trajectory Analysis') {
        ### With all Words, For a Given Album, Sentiment Trajectory Analysis
        
        ggplot(data = dat_sent(),
               aes(x = index, y = sentiment)) +
          geom_col(show.legend = F, fill = 'dark green') +
          theme_bw() +
          labs(title = paste0('Sentiments Throughout ', "'", input$album, "'")) +
          ylab('Sentiment Score Difference (Positive - Negative) using Bing Lexicon') +
          xlab(NULL) +
          geom_hline(yintercept = 0, color = 'red') +
          theme_bw() +
          theme(plot.title = element_text(face = 'bold', hjust = 0.5, size = 16),
                plot.subtitle = element_text(face = 'bold', hjust = 0.5),
                plot.caption = element_text(face = 'bold', size = 10, color = 'darkgrey', hjust = 0.5),
                axis.title.x = element_text(face = 'bold', hjust = 0.5),
                axis.title.y = element_text(face = 'bold', hjust = 0.5),
                axis.text.y = element_text(face = 'bold', hjust = 0.5, size = 15, color = 'black'),
                legend.title = element_text(face = 'bold'),
                legend.text = element_text(face = 'bold'))
      }
      
      else if (input$sentiment == 'Sentiment Contribution Analysis') {
        ### With all Words, For a Given Album, Sentiment Contribution Analysis
        
        dat_sent() %>%
          group_by(sentiment) %>%
          top_n(input$top) %>%
          ungroup() %>%
          mutate(word = reorder(word, n)) %>%
          ggplot(aes(word, n, fill = sentiment)) +
          geom_col(show.legend = FALSE) +
          facet_wrap(~ sentiment, scales = "free_y")  +
          labs(y = "Contribution to Sentiment",
               x = NULL,
               title = paste0("Lyrics That Contribute to Positive and Negative Sentiments In ", "'", input$album, "'")) +
          coord_flip() +
          theme_bw() +
          theme(plot.title = element_text(face = 'bold', hjust = 0.5, size = 16),
                plot.subtitle = element_text(face = 'bold', hjust = 0.5),
                plot.caption = element_text(face = 'bold', size = 10, color = 'darkgrey', hjust = 0.5),
                axis.title.x = element_text(face = 'bold', hjust = 0.5, size = 12),
                axis.title.y = element_text(face = 'bold', hjust = 0.5),
                axis.text.y = element_text(face = 'bold', hjust = 0.5, size = 15, color = 'black'),
                legend.title = element_text(face = 'bold'),
                legend.text = element_text(face = 'bold'))
      }
    }
  }
    
  else {
    if (input$album == 'ALL ALBUMS') {
      if (input$sentiment == 'Classic Sentiment Analysis') {
        ### Without Stopwords, for All Albums, Classic Sentiment Analysis
        
        ggplot(dat_sent()) +
          geom_col(aes(x = reorder(word, n), y = n, fill = sentiment)) +
          coord_flip() +
          labs(title = paste0('Top ', input$top, ' Most Common Words (Excluding Stopwords) with an Associated Sentiment Among All U2 Albums')) +
          ylab("count")  +
          xlab('word') +
          theme_bw() +
          theme(plot.title = element_text(face = 'bold', hjust = 0.5, size = 16),
                plot.subtitle = element_text(face = 'bold', hjust = 0.5),
                plot.caption = element_text(face = 'bold', size = 10, color = 'darkgrey', hjust = 0.5),
                axis.title.x = element_text(face = 'bold', hjust = 0.5),
                axis.title.y = element_text(face = 'bold', hjust = 0.5),
                axis.text.y = element_text(face = 'bold', hjust = 0.5, size = 15, color = 'black'),
                legend.title = element_text(face = 'bold'),
                legend.text = element_text(face = 'bold'))
      }
      
      else if (input$sentiment == "Sentiment Trajectory Analysis") {
        ### Without Stopwords, for All Albums, Sentiment Trajectory Analysis
        
        ggplot(data = dat_sent(),
               aes(x = index, y = sentiment)) +
          geom_col(show.legend = F, fill = 'orange') +
          theme_bw() +
          labs(title = 'Sentiments Throughout All U2 Albums (Without Stopwords)') +
          ylab('Sentiment Score Difference (Positive - Negative) using Bing Lexicon') +
          xlab(NULL) +
          geom_hline(yintercept = 0, color = 'red') +
          theme_bw() +
          theme(plot.title = element_text(face = 'bold', hjust = 0.5, size = 16),
                plot.subtitle = element_text(face = 'bold', hjust = 0.5),
                plot.caption = element_text(face = 'bold', size = 10, color = 'darkgrey', hjust = 0.5),
                axis.title.x = element_text(face = 'bold', hjust = 0.5),
                axis.title.y = element_text(face = 'bold', hjust = 0.5),
                axis.text.y = element_text(face = 'bold', hjust = 0.5, size = 15, color = 'black'),
                legend.title = element_text(face = 'bold'),
                legend.text = element_text(face = 'bold'))
      }
      
      else if (input$sentiment == 'Sentiment Contribution Analysis') {
        ### Without Stopwords, for All Albums, Sentiment Contribution Analysis
        
        dat_sent() %>%
          group_by(sentiment) %>%
          top_n(input$top) %>%
          ungroup() %>%
          mutate(word = reorder(word, n)) %>%
          ggplot(aes(word, n, fill = sentiment)) +
          geom_col(show.legend = FALSE) +
          facet_wrap(~ sentiment, scales = "free_y") +
          labs(y = "Contribution to Sentiment",
               x = NULL,
               title = "Lyrics That Contribute to Positive and Negative Sentiments Among All U2 Albums (Without Stopwords)") +
          coord_flip() +
          theme_bw() +
          theme(plot.title = element_text(face = 'bold', hjust = 0.5, size = 16),
                plot.subtitle = element_text(face = 'bold', hjust = 0.5),
                plot.caption = element_text(face = 'bold', size = 10, color = 'darkgrey', hjust = 0.5),
                axis.title.x = element_text(face = 'bold', hjust = 0.5, size = 12),
                axis.title.y = element_text(face = 'bold', hjust = 0.5),
                axis.text.y = element_text(face = 'bold', hjust = 0.5, size = 15, color = 'black'),
                legend.title = element_text(face = 'bold'),
                legend.text = element_text(face = 'bold'))
      }
    }
      
    else if (input$album == 'PER ALBUM') {
      if (input$sentiment == 'Classic Sentiment Analysis') {
        ### Without Stopwords, Per Album, Classic Sentiment Analysis

        ### NOT POSSIBLE
      }
      
      else if (input$sentiment == 'Sentiment Trajectory Analysis') {
        ### Without Stopwords, Per Album, Sentimenet Trajectory Analysis
        
        ggplot(data = dat_sent(),
               aes(x = index, y = sentiment, fill = album)) +
          geom_col(show.legend = F) +
          facet_wrap(~album, scales = 'free_y') +
          theme_bw() +
          labs(title = 'Sentiments Throughout All U2 Songs (Per Album) Without Stopwords') +
          ylab('Sentiment Score Difference (Positive - Negative) using Bing Lexicon') +
          xlab(NULL) +
          geom_hline(yintercept = 0, color = 'red') +
          theme_bw() +
          theme(plot.title = element_text(face = 'bold', hjust = 0.5, size = 16),
                plot.subtitle = element_text(face = 'bold', hjust = 0.5),
                plot.caption = element_text(face = 'bold', size = 10, color = 'darkgrey', hjust = 0.5),
                axis.title.x = element_text(face = 'bold', hjust = 0.5),
                axis.title.y = element_text(face = 'bold', hjust = 0.5),
                axis.text.y = element_text(face = 'bold', hjust = 0.5, size = 15, color = 'black'),
                legend.title = element_text(face = 'bold'),
                legend.text = element_text(face = 'bold'))
      }
      
      # else if (input$sentiment == 'Sentiment Contribution Analysis') {
      #   ### Without Stopwords, Per Album, Sentiment Contribution Analysis
      #   
      # }
    }
      
    else {
      if (input$sentiment == 'Classic Sentiment Analysis') {
        ### Without Stopwords, For a Given Album, Classic Sentiment Analysis
        
        ggplot(dat_sent()) +
          geom_col(aes(x = reorder(word, n), y = n, fill = sentiment)) +
          coord_flip() +
          labs(title = paste0('Top ', input$top, ' Most Common Words (Excluding Stopwords) with an Associated Sentiment in ', "'", input$album, "'")) +
          ylab("count")  +
          xlab('word') +
          theme_bw() +
          theme(plot.title = element_text(face = 'bold', hjust = 0.5, size = 16),
                plot.subtitle = element_text(face = 'bold', hjust = 0.5),
                plot.caption = element_text(face = 'bold', size = 10, color = 'darkgrey', hjust = 0.5),
                axis.title.x = element_text(face = 'bold', hjust = 0.5),
                axis.title.y = element_text(face = 'bold', hjust = 0.5),
                axis.text.y = element_text(face = 'bold', hjust = 0.5, size = 15, color = 'black'),
                legend.title = element_text(face = 'bold'),
                legend.text = element_text(face = 'bold'))
      }
      
      else if (input$sentiment == "Sentiment Trajectory Analysis") {
        ### Without Stopwords, For a Given Album, Sentiment Trajectory Analysis
        
        ggplot(data = dat_sent(),
               aes(x = index, y = sentiment)) +
          geom_col(show.legend = F, fill = 'dark green') +
          theme_bw() +
          labs(title = paste0('Sentiments Throughout ', "'", input$album, "'", " Without Stopwords")) +
          ylab('Sentiment Score Difference (Positive - Negative) using Bing Lexicon') +
          xlab(NULL) +
          geom_hline(yintercept = 0, color = 'red') +
          theme_bw() +
          theme(plot.title = element_text(face = 'bold', hjust = 0.5, size = 16),
                plot.subtitle = element_text(face = 'bold', hjust = 0.5),
                plot.caption = element_text(face = 'bold', size = 10, color = 'darkgrey', hjust = 0.5),
                axis.title.x = element_text(face = 'bold', hjust = 0.5),
                axis.title.y = element_text(face = 'bold', hjust = 0.5),
                axis.text.y = element_text(face = 'bold', hjust = 0.5, size = 15, color = 'black'),
                legend.title = element_text(face = 'bold'),
                legend.text = element_text(face = 'bold'))
      }
      
      else if (input$sentiment == "Sentiment Contribution Analysis") {
        ### Without Stopwords, For a Given Album, Sentiment Contribution Analysis
        
        dat_sent() %>%
          group_by(sentiment) %>%
          top_n(input$top) %>%
          ungroup() %>%
          mutate(word = reorder(word, n)) %>%
          ggplot(aes(word, n, fill = sentiment)) +
          geom_col(show.legend = FALSE) +
          facet_wrap(~ sentiment, scales = "free_y")  +
          labs(y = "Contribution to Sentiment",
               x = NULL,
               title = paste0("Lyrics That Contribute to Positive and Negative Sentiments In ", "'", input$album, "'", "Without Stopwords")) +
          coord_flip() +
          theme_bw() +
          theme(plot.title = element_text(face = 'bold', hjust = 0.5, size = 16),
                plot.subtitle = element_text(face = 'bold', hjust = 0.5),
                plot.caption = element_text(face = 'bold', size = 10, color = 'darkgrey', hjust = 0.5),
                axis.title.x = element_text(face = 'bold', hjust = 0.5, size = 12),
                axis.title.y = element_text(face = 'bold', hjust = 0.5),
                axis.text.y = element_text(face = 'bold', hjust = 0.5, size = 15, color = 'black'),
                legend.title = element_text(face = 'bold'),
                legend.text = element_text(face = 'bold'))
      }
    }
  }
    
  })
  
  # code for statistics
   output$table2 <- renderPrint({
     # replace the code below with your code!!!
     if (input$type == 'with all words') {
       if (input$album == 'ALL ALBUMS') {
         if (input$sentiment == 'Classic Sentiment Analysis') {
           ### With all Words, for All Albums, Classic Sentiment Analysis
           paste0('Among the top ', input$top, ' words that contributed to sentiment the most for all U2 albums, ', summarise(dat_sent(), positive = sum(dat_sent()$sentiment == 'positive'), negative = sum(dat_sent()$sentiment == 'negative'))[1,1],
                  ' words had positive sentiment and ', summarise(dat_sent(), positive = sum(dat_sent()$sentiment == 'positive'), negative = sum(dat_sent()$sentiment == 'negative'))[1,2], " words had negative sentiment. Additionally, the word '",
                  arrange(filter(dat_sent(), sentiment == 'positive'), desc(n))[1,1], "' contributed the most to positive sentiment, and the word '", 
                  arrange(filter(dat_sent(), sentiment == 'negative'), desc(n))[1,1], "' contributed the most to negative sentiment.")
         }
         
         else if (input$sentiment == 'Sentiment Trajectory Analysis') {
           ### With all Words, for All Albums, Sentiment Trajectory Analysis
           paste0("Throughout all the U2 albums, there was a mean sentiment score of ", summarise(dat_sent(), mean = mean(sentiment))[1,1], ", a median sentiment score of ", summarise(dat_sent(), median = median(sentiment))[1,1], ", with a minimum sentiment score of ",
                  min(dat_sent()$sentiment), ", a maximum sentiment score of ", max(dat_sent()$sentiment), ", and a standard deviation of ", sd(dat_sent()$sentiment), ". Additionally, the word '", 
                  arrange(dat_sent(), desc(sentiment))[1,1], "' was the word associated with a positive sentiment with the highest sentiment score, and the word '", arrange(dat_sent(), sentiment)[1,1], "' was the word associated with a negative sentiment with the lowest sentiment score.")

         }
         
         else if (input$sentiment == 'Sentiment Contribution Analysis') {
           ### With all Words, for All Albums, Sentiment Contribution Analysis
           paste0("Among the top ", input$top, " words that contributed to each negative and positive sentiment the most for all U2 albums, '" , arrange(filter(dat_sent(), sentiment == 'positive'), desc(n))[1,1], "' contributed the most to positive sentiment and contributed ",
                  arrange(filter(dat_sent(), sentiment == 'positive'), desc(n))[1,3]/arrange(filter(dat_sent(), sentiment == 'positive'), desc(n))[input$top,3], " times more to positive sentiment than the word '", arrange(filter(dat_sent(), sentiment == 'positive'), desc(n))[input$top,1], "'.",
                  " Additionally, the word '", arrange(filter(dat_sent(), sentiment == 'negative'), desc(n))[1,1], "' contributed the most to negative sentiment and contributed ", arrange(filter(dat_sent(), sentiment == 'negative'), desc(n))[1,3]/arrange(filter(dat_sent(), sentiment == 'negative'), desc(n))[input$top,3],
                  " times more to negative sentiment than the word '", arrange(filter(dat_sent(), sentiment == 'negative'), desc(n))[input$top,1], "'.")

         }
       }
       
       
       else if (input$album == 'PER ALBUM') {
         if (input$sentiment == 'Classic Sentiment Analysis') {
           ### With All Words, Per Album, Classic Sentiment Analysis
           print("WARNING: Classic Sentiment Analysis is only available for an Album Selection choice of 'ALL ALBUMS' and individual albums. Please choice a valid option.")

         }
         
         else if (input$sentiment == 'Sentiment Trajectory Analysis') {
           ### With all Words, Per Album, Sentiment Trajectory Analysis
           print("As a propotion of total sentiment among each album, 'October' had the most negative sentiment throughout, and 'Rattle and Hum' had the most positive sentiment throughout.")

         }
         
         else if (input$sentiment == 'Sentiment Contribution Analysis') {
           ### With all Words, Per Album, Sentiment Contribution Analysis
           print("WARNING: Sentiment Contribution Analysis is only available for an Album Selection choice of 'ALL ALBUMS' and individual albums. Please choice a valid option.")

         }
       }
       
       
       else {
         if (input$sentiment == "Classic Sentiment Analysis") {
           ### With all Words, For a Given Album, Classic Sentiment Analysis
           paste0('Among the top ', input$top, " words that contributed to sentiment the most in '", input$album, "', ", summarise(dat_sent(), positive = sum(dat_sent()$sentiment == 'positive'), negative = sum(dat_sent()$sentiment == 'negative'))[1,1],
                  ' words had positive sentiment and ', summarise(dat_sent(), positive = sum(dat_sent()$sentiment == 'positive'), negative = sum(dat_sent()$sentiment == 'negative'))[1,2], " words had negative sentiment. Additionally, the word '",
                  arrange(filter(dat_sent(), sentiment == 'positive'), desc(n))[1,2], "' contributed the most to positive sentiment, and the word '",
                  arrange(filter(dat_sent(), sentiment == 'negative'), desc(n))[1,2], "' contributed the most to negative sentiment.")

         }
         
         else if (input$sentiment == 'Sentiment Trajectory Analysis') {
           ### With all Words, For a Given Album, Sentiment Trajectory Analysis
           paste0("Throughout '", input$album, "' there was a mean sentiment score of ", summarise(dat_sent(), mean = mean(sentiment))[1,1], ", a median sentiment score of ", summarise(dat_sent(), median = median(sentiment))[1,1], ", with a minimum sentiment score of ",
                  min(dat_sent()$sentiment), ", a maximum sentiment score of ", max(dat_sent()$sentiment), ", and a standard deviation of ", sd(dat_sent()$sentiment), ". Additionally, the word '", 
                  arrange(dat_sent(), desc(sentiment))[1,2], "' was the word associated with a positive sentiment with the highest sentiment score, and the word '", arrange(dat_sent(), sentiment)[1,2], "' was the word associated with a negative sentiment with the lowest sentiment score.")

         }
         
         else if (input$sentiment == 'Sentiment Contribution Analysis') {
           ### With all Words, For a Given Album, Sentiment Contribution Analysis
           paste0("Among the top ", input$top, " words that contributed to each negative and positive sentiment in '", input$album, "', " , arrange(filter(dat_sent(), sentiment == 'positive'), desc(n))[1,2], "' contributed the most to positive sentiment and contributed ",
                  arrange(filter(dat_sent(), sentiment == 'positive'), desc(n))[1,4]/arrange(filter(dat_sent(), sentiment == 'positive'), desc(n))[input$top,4], " times more to positive sentiment than the word '", arrange(filter(dat_sent(), sentiment == 'positive'), desc(n))[input$top,2], "'.",
                  " Additionally, the word '", arrange(filter(dat_sent(), sentiment == 'negative'), desc(n))[1,2], "' contributed the most to negative sentiment and contributed ", arrange(filter(dat_sent(), sentiment == 'negative'), desc(n))[1,4]/arrange(filter(dat_sent(), sentiment == 'negative'), desc(n))[input$top,4],
                  " times more to negative sentiment than the word '", arrange(filter(dat_sent(), sentiment == 'negative'), desc(n))[input$top,2], "'.")

         }
       }
     }

     else {
       if (input$album == 'ALL ALBUMS') {
         if (input$sentiment == 'Classic Sentiment Analysis') {
           ### Without Stopwords, for All Albums, Classic Sentiment Analysis
           paste0('Among the top ', input$top, ' non-stopwords that contributed to sentiment the most for all U2 albums, ', summarise(dat_sent(), positive = sum(dat_sent()$sentiment == 'positive'), negative = sum(dat_sent()$sentiment == 'negative'))[1,1],
                  ' non-stopwords had positive sentiment and ', summarise(dat_sent(), positive = sum(dat_sent()$sentiment == 'positive'), negative = sum(dat_sent()$sentiment == 'negative'))[1,2], " non-stopwords had negative sentiment. Additionally, the non-stopword '",
                  arrange(filter(dat_sent(), sentiment == 'positive'), desc(n))[1,1], "' contributed the most to positive sentiment, and the non-stopword '", 
                  arrange(filter(dat_sent(), sentiment == 'negative'), desc(n))[1,1], "' contributed the most to negative sentiment.")
    
         }
         
         else if (input$sentiment == "Sentiment Trajectory Analysis") {
           ### Without Stopwords, for All Albums, Sentiment Trajectory Analysis
           paste0("Throughout all the U2 albums, there was a mean sentiment score of ", summarise(dat_sent(), mean = mean(sentiment))[1,1], ", a median sentiment score of ", summarise(dat_sent(), median = median(sentiment))[1,1], ", with a minimum sentiment score of ",
                  min(dat_sent()$sentiment), ", a maximum sentiment score of ", max(dat_sent()$sentiment), ", and a standard deviation of ", sd(dat_sent()$sentiment), ". Additionally, the non-stopword '", 
                  arrange(dat_sent(), desc(sentiment))[1,1], "' was the non-stopword associated with a positive sentiment with the highest sentiment score, and the non-stopword '", arrange(dat_sent(), sentiment)[1,1], "' was the non-stopword associated with a negative sentiment with the lowest sentiment score.")


         }
         
         else if (input$sentiment == 'Sentiment Contribution Analysis') {
           ### Without Stopwords, for All Albums, Sentiment Contribution Analysis
           paste0("Among the top ", input$top, " non-stopwords that contributed to each negative and positive sentiment the most for all U2 albums, '" , arrange(filter(dat_sent(), sentiment == 'positive'), desc(n))[1,1], "' contributed the most to positive sentiment and contributed ",
                  arrange(filter(dat_sent(), sentiment == 'positive'), desc(n))[1,3]/arrange(filter(dat_sent(), sentiment == 'positive'), desc(n))[input$top,3], " times more to positive sentiment than the non-stopword '", arrange(filter(dat_sent(), sentiment == 'positive'), desc(n))[input$top,1], "'.",
                  " Additionally, the non-stopword '", arrange(filter(dat_sent(), sentiment == 'negative'), desc(n))[1,1], "' contributed the most to negative sentiment and contributed ", arrange(filter(dat_sent(), sentiment == 'negative'), desc(n))[1,3]/arrange(filter(dat_sent(), sentiment == 'negative'), desc(n))[input$top,3],
                  " times more to negative sentiment than the non-stopword '", arrange(filter(dat_sent(), sentiment == 'negative'), desc(n))[input$top,1], "'.")

         }
       }
       
       else if (input$album == 'PER ALBUM') {
         if (input$sentiment == 'Classic Sentiment Analysis') {
           ### Without Stopwords, Per Album, Classic Sentiment Analysis
           print("WARNING: Classic Sentiment Analysis is only available for an Album Selection choice of 'ALL ALBUMS' and individual albums. Please choice a valid option.")

         }
         
         else if (input$sentiment == 'Sentiment Trajectory Analysis') {
           ### Without Stopwords, Per Album, Sentimenet Trajectory Analysis
           print("As a propotion of total sentiment among each album, 'October' had the most negative sentiment throughout, and 'Rattle and Hum' had the most positive sentiment throughout.")

         }
         
         else if (input$sentiment == 'Sentiment Contribution Analysis') {
           ### Without Stopwords, Per Album, Sentiment Contribution Analysis
           print("WARNING: Sentiment Contribution Analysis is only available for an Album Selection choice of 'ALL ALBUMS' and individual albums. Please choice a valid option.")

         }
       }
       
       else {
         if (input$sentiment == 'Classic Sentiment Analysis') {
           ### Without Stopwords, For a Given Album, Classic Sentiment Analysis
           paste0('Among the top ', input$top, " non-stopwords that contributed to sentiment the most in '", input$album, "', ", summarise(dat_sent(), positive = sum(dat_sent()$sentiment == 'positive'), negative = sum(dat_sent()$sentiment == 'negative'))[1,1],
                  ' non-stopwords had positive sentiment and ', summarise(dat_sent(), positive = sum(dat_sent()$sentiment == 'positive'), negative = sum(dat_sent()$sentiment == 'negative'))[1,2], " non-stopwords had negative sentiment. Additionally, the non-stopword '",
                  arrange(filter(dat_sent(), sentiment == 'positive'), desc(n))[1,2], "' contributed the most to positive sentiment, and the non-stopword '",
                  arrange(filter(dat_sent(), sentiment == 'negative'), desc(n))[1,2], "' contributed the most to negative sentiment.")


         }
         
         else if (input$sentiment == "Sentiment Trajectory Analysis") {
           ### Without Stopwords, For a Given Album, Sentiment Trajectory Analysis
           paste0("Throughout '", input$album, "' there was a mean sentiment score of ", summarise(dat_sent(), mean = mean(sentiment))[1,1], ", a median sentiment score of ", summarise(dat_sent(), median = median(sentiment))[1,1], ", with a minimum sentiment score of ",
                  min(dat_sent()$sentiment), ", a maximum sentiment score of ", max(dat_sent()$sentiment), ", and a standard deviation of ", sd(dat_sent()$sentiment), ". Additionally, the non-stopword '", 
                  arrange(dat_sent(), desc(sentiment))[1,2], "' was the non-stopword associated with a positive sentiment with the highest sentiment score, and the non-stopword '", arrange(dat_sent(), sentiment)[1,2], "' was the word associated with a negative sentiment with the lowest sentiment score.")
         }
         
         else if (input$sentiment == "Sentiment Contribution Analysis") {
           ### Without Stopwords, For a Given Album, Sentiment Contribution Analysis
           paste0("Among the top ", input$top, " non-stopwords that contributed to each negative and positive sentiment in '", input$album, "', " , arrange(filter(dat_sent(), sentiment == 'positive'), desc(n))[1,2], "' contributed the most to positive sentiment and contributed ",
                  arrange(filter(dat_sent(), sentiment == 'positive'), desc(n))[1,4]/arrange(filter(dat_sent(), sentiment == 'positive'), desc(n))[input$top,4], " times more to positive sentiment than the non-stopword '", arrange(filter(dat_sent(), sentiment == 'positive'), desc(n))[input$top,2], "'.",
                  " Additionally, the non-stopword '", arrange(filter(dat_sent(), sentiment == 'negative'), desc(n))[1,2], "' contributed the most to negative sentiment and contributed ", arrange(filter(dat_sent(), sentiment == 'negative'), desc(n))[1,4]/arrange(filter(dat_sent(), sentiment == 'negative'), desc(n))[input$top,4],
                  " times more to negative sentiment than the non-stopword '", arrange(filter(dat_sent(), sentiment == 'negative'), desc(n))[input$top,2], "'.")

         }
       }
     }
   })
  
}



# ===============================================
# Run the application
# ===============================================

shinyApp(ui = ui, server = server)

