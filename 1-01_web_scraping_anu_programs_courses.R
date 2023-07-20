### Setup and Configuration ----------------------------------------------------
source(here::here('0-00_setup_and_configuration.R'))



### Initialisation -------------------------------------------------------------

# Initialise tibble
learning_outcomes_wordtotal <- tibble()

# Common words list
common_words <- c("and",
                  "the",
                  "of",
                  "to",
                  "a",
                  "in",
                  "for",
                  "an",
                  "be",
                  "with",
                  "",
                  "their")

# Course code list
course_code_list <- c("CBEA3001",
                      "COMP1730",
                      "ECON1101",
                      "ECON1100",
                      "ECON2016",
                      "ECON2101",
                      "ECON2102",
                      "ECON3101",
                      "ECON3102",
                      "EMET2007",
                      "EMET3004",
                      "EMET3006",
                      "EMET3007",
                      "ENVS1001",
                      "ENVS1003",
                      "ENVS2011",
                      "ENVS2015",
                      "ENVS3008",
                      "MATH1013",
                      "MATH1014",
                      "MATH2305",
                      "MATH3511",
                      "STAT1008",
                      "STAT2008",
                      "STAT3011",
                      "ECON2900",
                      "MATH3133",
                      "STAT3015",
                      "EMSC3019",
                      "ENVS2002")



### Web Scraping ---------------------------------------------------------------

for (course_code in course_code_list) {
  
  # Specifying the url for desired website to be scraped
  url <- glue('https://programsandcourses.anu.edu.au/2022/course/', course_code)
  
  # Reading the HTML code from the website
  webpage <- read_html(url)
  
  # Using CSS selectors to scrape the learning outcomes section
  html <- html_nodes(webpage, 'ol')
  
  # Converting the learning outcomes data to text
  learning_outcomes <- html_text(html)
  
  # Selecting learning outcomes data
  learning_outcomes <- learning_outcomes[1]
  
  
  
  ### Data Cleaning ------------------------------------------------------------
  
  # Remove non-letter characters, lowercase words, split string into tibble as
  # individual words
  learning_outcomes_words <- learning_outcomes %>% 
    
    str_replace_all(pattern = "[^a-zA-Z]", replace = " ") %>%
    
    tolower() %>% 
    
    str_split(pattern = " ") %>%
    unlist() %>% 
    as_tibble_col(column_name = "word")
  
  learning_outcomes_words <- learning_outcomes_words %>% 
    mutate(
      course_code    = course_code,
      course_letter  = str_extract(course_code, "[A-Z]{4}"),
      course_level   = str_c(str_sub(course_code, start = 5, end = 5), "000")
    )
  
  learning_outcomes_wordtotal <- learning_outcomes_wordtotal %>% 
    bind_rows(learning_outcomes_words)
}



### Data Transformation --------------------------------------------------------

# Count number of unique words, remove common words, and calculate cumulative
# wordcount, and rearrange tibble, and filter by ntotal > 7
learning_outcomes_wordcount <- learning_outcomes_wordtotal %>% 
  
  group_by(course_level) %>% 
  count(word, sort = TRUE) %>% 
  ungroup() %>% 
  filter(!(word %in% common_words)) %>% 
  
  left_join({
    learning_outcomes_wordtotal %>% 
      group_by(word) %>% 
      summarise(ntotal = n())
  }, by = "word") %>% 
  left_join({
    learning_outcomes_wordtotal %>%
      group_by(word, course_level) %>%
      summarise(ntotal3000 = n()) %>%
      filter(course_level == "3000")
  }, by = c("word", "course_level")) %>%
  
  arrange(word, desc(course_level)) %>%
  fill(ntotal3000, .direction = "down") %>%
  
  filter(ntotal > 8)


# Bar chart of frequent words with course level fill
learning_outcomes_wordcount %>% 
  
  ggplot(aes(x = reorder(word, ntotal), y = n, fill = course_level)) +
  geom_bar(
    stat  = "identity",
    color = "black"
  ) +
  coord_flip() +
  labs(
    x     = "Word",
    y     = "Count",
    fill  = "Course Level",
    title = glue("Frequent Words in Learning Outcomes")
  ) +
  scale_fill_brewer(type = "seq", palette = "Greens")

learning_outcomes_wordcount <- learning_outcomes_wordtotal %>% 
  
  group_by(course_letter) %>% 
  count(word, sort = TRUE) %>% 
  ungroup() %>% 
  filter(!(word %in% common_words)) %>% 
  
  left_join({
    learning_outcomes_wordtotal %>% 
      group_by(word) %>% 
      summarise(ntotal = n())
  }, by = "word") %>% 
  # left_join({
  #   learning_outcomes_wordtotal %>% 
  #     group_by(word, course_level) %>% 
  #     summarise(ntotal3000 = n()) %>% 
  #     filter(course_level == "3000")
  # }, by = c("word", "course_level")) %>% 
  # 
  # arrange(word, desc(course_level)) %>% 
  # fill(ntotal3000, .direction = "down") %>% 
  
  filter(ntotal > 8)

# Bar chart of frequent words with course letter fill
learning_outcomes_wordcount %>% 
  
  ggplot(aes(x = reorder(word, ntotal), y = n, fill = course_letter)) +
  geom_bar(
    stat  = "identity",
    color = "black"
  ) +
  coord_flip() +
  labs(
    x     = "Word",
    y     = "Count",
    fill  = "Course Letter",
    title = glue("Frequent Words in Learning Outcomes")
  ) +
  scale_fill_brewer(type = "qual", palette = "Pastel2")
