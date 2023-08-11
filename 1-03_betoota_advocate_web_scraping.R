### Setup and Configuration ----------------------------------------------------
source(here::here('0-00_setup_and_configuration.R'))



# Web Scraping ------------------------------------------------------------

scrape_urls_from_searchpage <- function(page_number) {
  
  html_data <- glue("https://www.betootaadvocate.com/page/",page_number,"/?s") %>% 
    read_html()
  
  # Scrape all article urls from each page
  article_url <- html_data %>% 
    html_nodes(".td_module_16 a") %>% 
    html_attr("href") %>% 
    as_tibble() %>% 
    rename(article_url = value) %>%
    
    # Filter full urls only
    filter(str_detect(article_url, pattern = "https://www.betootaadvocate.com")) %>% 
    
    # Only take every 2nd row
    slice(which(row_number() %% 2 == 1))
  
  article_date <- html_data %>% 
    html_nodes(".td_module_16 .td-module-date") %>% 
    html_text() %>% 
    as_date(format = "%e %B %Y")
  
  return(tibble(
    article_url,
    article_date))
}

# Filter out podcast urls
urls_articles <- tibble(
  page_number = seq(1532, 1535)
) %>% 
  
  mutate(
    result = map(page_number, scrape_urls_from_searchpage)
  ) %>% 
  
  unnest(cols = c(result)) %>% 
  
  filter(!str_detect(article_url, pattern = "https://www.betootaadvocate.com/podcasts"))

# Create function that scrapes article website
scrape_article <- function(url) {
  
  html_data <- url %>% 
    read_html()
  
  article_title <- html_data %>% 
    html_nodes(".td-post-title .entry-title") %>% 
    html_text()
  
  article_category <- html_data %>% 
    html_nodes(".entry-category a") %>% 
    html_text() %>% 
    str_c(collapse = ", ")
  
  article_content <- html_data %>% 
    html_nodes(".tagdiv-type p") %>% 
    html_text() %>% 
    str_c(collapse = " ")
  
  return(tibble(
    article_title,
    article_category,
    article_content
  ))
}

# Create dataset
raw_articles <- urls_articles %>% 
  mutate(
    result = map(article_url, scrape_article)
  ) %>% 
  unnest(cols = c(result))



# Data Cleaning -----------------------------------------------------------

clean_articles <- raw_articles %>% 
  mutate(
    
    # Extract author information
    article_author = article_content %>% 
      word(1, sep = "\\|") %>% 
      str_trim() %>% 
      str_to_title(),
    
    # Extract author role
    article_author_role = article_content %>% 
      str_squish() %>% 
      word(1, sep = "((?i)\b\\|contact\b)(.*?)|(?i)(?<= |^)\\|contact(?= |$)(.*?)|((?i)\bcontact\b)(.*?)|(?i)(?<= |^)contact(?= |$)(.*?)|(?i)(?<= |^)contact(.*?)") %>% 
      str_to_lower() %>% 
      word(2, sep = str_to_lower(article_author)) %>% 
      str_remove_all(pattern = "\\|") %>%
      str_squish() %>%
      str_to_title(),
    
    # Extract and clean article only content by splitting string by "CONTACT"
    article_content = article_content %>% 
      str_squish() %>% 
      word(2, sep = "((?i)\b\\|contact\b)(.*?)|(?i)(?<= |^)\\|contact(?= |$)(.*?)|((?i)\bcontact\b)(.*?)|(?i)(?<= |^)contact(?= |$)(.*?)|(?i)(?<= |^)contact(.*?)") %>% 
      str_trim() %>% 
      str_remove_all(pattern = "\\[dropcap\\]\\[\\/dropcap\\]"),
    
    # Create logical data for article_category
    article_category_infocus = case_when(
      str_detect(article_category, "IN-Focus") ~ TRUE,
      TRUE                                     ~ FALSE
    ),
    
    article_category_thenation = case_when(
      str_detect(article_category, "The Nation") ~ TRUE,
      TRUE                                       ~ FALSE
    ),
    
    article_category_localnews = case_when(
      str_detect(article_category, "Local News") ~ TRUE,
      TRUE                                       ~ FALSE
    ),
    
    article_category_headlines = case_when(
      str_detect(article_category, "Headlines") ~ TRUE,
      TRUE                                      ~ FALSE
    ),
    
    article_category_breakingnews = case_when(
      str_detect(article_category, "Breaking News") ~ TRUE,
      TRUE                                          ~ FALSE
    ),
    
    article_category_headlines = case_when(
      str_detect(article_category, "Headlines") ~ TRUE,
      TRUE                                      ~ FALSE
    ),
    
    article_category_sports = case_when(
      str_detect(article_category, "Sports") ~ TRUE,
      TRUE                                   ~ FALSE
    ),
    
    article_category_politics = case_when(
      str_detect(article_category, "Politics") ~ TRUE,
      TRUE                                     ~ FALSE
    ),
    
    article_category_worldnews = case_when(
      str_detect(article_category, "World News") ~ TRUE,
      TRUE                                       ~ FALSE
    ),
    
    # Additional case_when() situations to handle different article formatting
    article_content = case_when(
      is.na(article_content) ~ article_author,
      is.na(article_content) & is.na(article_author) ~ raw_articles$article_content,
      TRUE                   ~ article_content
    ),
    
    article_author = case_when(
      is.na(article_author_role)                ~ NA_character_,
      str_detect(article_author, pattern = ":") ~ word(article_author, 2, sep = ":[0-9][0-9] "),
      TRUE                                      ~ article_author
    )
    
  ) %>% 
  
  relocate(
    page_number,
    article_title,
    article_date,
    article_author,
    article_author_role,
    article_category,
    article_url,
    article_content
  )



# Data Analysis -----------------------------------------------------------

clean_articles %>%
  mutate(article_day = wday(article_date, label = TRUE, abbr = FALSE)) %>% 
  group_by(article_day, article_date) %>% 
  count()



# Data Saving -------------------------------------------------------------

#write_csv(clean_articles, file = "2022-07-24-betoota-advocate-articles-4pages.csv")
