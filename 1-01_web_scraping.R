### Setup and Configuration ----------------------------------------------------
source(here::here('0-00_setup_and_configuration.R'))



### Initialisation -------------------------------------------------------------

# Initialise tibble
learning_outcomes    <- c()

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
  
  # Converting the learning outcomes data [1] to text
  text <- html_text(html)[1]
  
  # Append learning outcome to list
  learning_outcomes <- append(learning_outcomes, text)
  
}

programs_and_courses <- tibble(
  learning_outcome = learning_outcomes,
  course_code = course_code_list
  )
