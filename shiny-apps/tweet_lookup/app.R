library(shiny)
library(tidyverse)

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


## UI LOGIC ===================

ui <- fluidPage(
    
    titlePanel("Original @AdelphiResearch Tweets"),
    
    sidebarLayout(
        
        sidebarPanel(
            width = 2,
            strong("Adjust these parameters to filter tweets."),
            hr(),
            sliderInput(
                "years", 
                label = "Years",
                min = lubridate::year(min(tweets$created_at)), 
                max = lubridate::year(Sys.Date()), 
                value = c(lubridate::year(min(tweets$created_at)), lubridate::year(Sys.Date())),
                sep = ""
            ),
            sliderInput(
                "likes", 
                label = "Likes",
                min = min(tweets$favorite_count), 
                max = max(tweets$favorite_count), 
                value = c(min(tweets$favorite_count), max(tweets$favorite_count)),
                round = TRUE,
                sep = ""
            ),
            sliderInput(
                "retweets", 
                label = "Retweets",
                min = min(tweets$retweet_count), 
                max = max(tweets$retweet_count), 
                value = c(min(tweets$retweet_count), max(tweets$retweet_count)),
                round = TRUE,
                sep = ""
            ),
            sliderInput(
                "hashtags", 
                label = "Hashtags",
                min = min(tweets$num_hashtags), 
                max = max(tweets$num_hashtags), 
                value = c(min(tweets$num_hashtags), max(tweets$num_hashtags)),
                round = TRUE,
                sep = ""
            )
        ),
        
        mainPanel(
            DT::dataTableOutput("table")
        )
    )
)


## SERVER LOGIC ===============

server <- function(input, output, session) {
    
    # filter tweets based on selected parameters
    filtered_tweets <- reactive({
        tweets %>%
            dplyr::filter(
                lubridate::year(created_at) >= input$years[1],
                lubridate::year(created_at) <= input$years[2],
                favorite_count >= input$likes[1],
                favorite_count <= input$likes[2],
                retweet_count >= input$retweets[1],
                retweet_count <= input$retweets[2],
                num_hashtags >= input$hashtags[1],
                num_hashtags <= input$hashtags[2]
            )
    })
    
    # Tweet table
    output$table <- DT::renderDataTable({
        filtered_tweets() %>%
            dplyr::select(created_at, text, favorite_count, retweet_count, num_hashtags) %>%
            DT::datatable(
                rownames = FALSE,
                colnames = c("Timestamp", "Tweet", "Likes", "Retweets", "Hashtags")
            ) %>%
            DT::formatRound(c("favorite_count", "retweet_count", "num_hashtags"), digits = 0)
    })
    
    
}

shinyApp(ui, server)