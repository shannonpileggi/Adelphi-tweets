library(shiny)
library(tidyverse)
library(tidytext)
library(widyr)
library(ggraph)
library(igraph)

tweets <- readRDS(file = "ar_tweets.rds")

tweets <- tweets %>%
    dplyr::mutate(type = fct_relevel(
        case_when(
            is_retweet == F & is_quote == F ~ "original",
            is_retweet == T & is_quote == F ~ "RT no comment",
            is_retweet == F & is_quote == T ~ "RT w/ comment"
        )),
        has_url = case_when(
            is.na(urls_url) == TRUE ~ FALSE,
            TRUE ~ TRUE
        ),
        has_photo = case_when(
            media_type == "photo" ~ TRUE,
            TRUE ~ FALSE
        ),
        num_hashtags = purrr::map_int(hashtags,~sum(!is.na(.x))), 
        atleast_1_like = favorite_count > 0,
        atleast_1_retweet = retweet_count > 0,
        atleast_1_hashtag = num_hashtags > 0,
        month = lubridate::month(created_at),
        year = lubridate::year(created_at),
        month_year = lubridate::ymd(paste(year, month, 1, sep = '-')),
        post_id = row_number()
    ) %>% 
    filter(type == "original")

hashtags <- tweets %>% 
    unnest(hashtags) %>% 
    mutate(hashtags_lc = tolower(hashtags),
           word =  case_when(
               str_detect(hashtags_lc, "bhbia") == TRUE |
                   str_detect(hashtags_lc, "bobi") == TRUE ~ "bhbia",
               # ------------------------------------------------------
               str_detect(hashtags_lc, "mentalhealth") == TRUE ~ "mentalhealth",
               str_detect(hashtags_lc, "pharm") == TRUE ~ "pharma",
               str_detect(hashtags_lc, "marketresearch") == TRUE ~ "mrx",
               str_detect(hashtags_lc, "adelphi") == TRUE ~ "adelphi",
               str_detect(hashtags_lc, "mrs") == TRUE ~ "mrs",
               # ------------------------------------------------------
               str_detect(hashtags_lc, "cancer") == TRUE |
                   str_detect(hashtags_lc, "oncolgy") == TRUE |
                   str_detect(hashtags_lc, "chemo") == TRUE |
                   str_detect(hashtags_lc, "oncology") == TRUE
               ~ "cancer",
               # ------------------------------------------------------
               str_detect(hashtags_lc, "patient") == TRUE ~ "patient",
               # ------------------------------------------------------
               str_detect(hashtags_lc, "nhs") == TRUE & 
                   !str_detect(hashtags_lc, "nhsnuggets") == TRUE~ "nhs",
               # ------------------------------------------------------
               str_detect(hashtags_lc, "behaviouraleconomics") == TRUE 
               ~ "behavioraleconomics",
               # ------------------------------------------------------
               str_detect(hashtags_lc, "downsyndrome") == TRUE | 
                   str_detect(hashtags_lc, "wdsd") == TRUE ~ "downsyndrome",
               # ------------------------------------------------------
               str_detect(hashtags_lc, "wearable") == TRUE ~ "wearable",
               # ------------------------------------------------------
               TRUE ~ hashtags_lc
           )) %>% 
    filter(is.na(hashtags) == F) %>% 
    add_count(word)

text_words <- tweets %>% 
    select(post_id, created_at, favorite_count, retweet_count, text) %>% 
    unnest_tokens(word, text) %>% 
    anti_join(stop_words, by = "word") %>% 
    filter(!(word %in% c("t.co", "http", "https")),
           str_detect(word, "[a-z]")) %>% 
    add_count(word)

## UI LOGIC ===================

ui <- fluidPage(
    
    titlePanel("Network analysis of original @AdelphiResearch tweets"),
    
    sidebarLayout(
        
        sidebarPanel(
            width = 2,
            strong("Adjust these parameters to filter the network analysis."),
            hr(),
            radioButtons(inputId = "outcome", 
                         label = "Analysis of:", 
                         choices = c("hashtags","tweet text"), 
                         selected = "hashtags"),
            radioButtons(inputId = "metric", 
                         label = "Colored by:", 
                         choices = c("likes","retweets"), 
                         selected = "retweets"),
            numericInput(inputId = "freq", 
                         label = "Min occurences:", 
                         value = 5, 
                         min = 5, 
                         max = 75, 
                         step = 10),
            numericInput(inputId = "top", 
                         label = "Top correlations:", 
                         value = 50, 
                         min = 25, 
                         max = 150, 
                         step = 25),
            sliderInput(
                inputId = "years", 
                label = "Years",
                min = lubridate::year(min(tweets$created_at)), 
                max = lubridate::year(Sys.Date()), 
                value = c(lubridate::year(min(tweets$created_at)), lubridate::year(Sys.Date())),
                sep = ""
            )
        ),
        
        mainPanel(
            plotOutput("plot")
        )
    ))


## SERVER LOGIC ===============

server <- function(input, output, session) {
    
    
    # ---> INSERT NETWORK PLOT HERE <---
    output$plot <- renderPlot({
        
        if (input$outcome == "hashtags") data <- hashtags
        if (input$outcome == "text words") data <- text_words
        
        data <- data %>% 
            filter(lubridate::year(created_at) >= input$years[1],
                   lubridate::year(created_at) <= input$years[2])
        
        top_corrs <- data %>% 
            filter(n >= input$freq) %>% 
            select(post_id, word) %>% 
            pairwise_cor(word, post_id, sort = T) %>% 
            head(input$top) 
        
        vertices <- data %>%  
            add_count(word) %>% 
            group_by(word) %>% 
            summarize(geom_mean_likes = exp(mean(log(favorite_count + 1))),
                      geom_mean_retweets = exp(mean(log(retweet_count + 1))),
                      occurrences = n()) %>% 
            arrange(desc(occurrences)) %>% 
            filter(word %in% top_corrs$item1 | 
                       word %in% top_corrs$item2)
        
        set.seed(2018)
        top_corrs %>% 
            graph_from_data_frame(vertices = vertices) %>% 
            ggraph() +
            geom_edge_link() + 
            # geom_node_point(aes(size = occurrences * 1.1)) +
            geom_node_point(aes_string(size = "occurrences",
                                       color = ifelse(
                                           input$metric == "likes", 
                                           "geom_mean_likes",
                                           "geom_mean_retweets"
                                       ))) + 
            geom_node_text(aes(label = name, size = 100), repel = TRUE) + 
            theme_void() +
            labs(color = "Average",
                 size = "Frequency") +
            scale_color_gradient(low='#E1FA72', high='#F46FEE') +
            scale_size_continuous(range = c(3, 8)) +
            theme(legend.title = element_text(size = rel(2)),
                  legend.text = element_text(size = rel(1.5)))
    })
    
}

shinyApp(ui, server)