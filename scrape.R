
library(tidyverse)
library(rvest)


download_dir <- 'scrape'

download_course_htmls <- function() {
  uac_base_url <- 'https://www.uac.edu.au'
  
  courses_base_url <- 'https://www.uac.edu.au/undergraduate/courses/'
  
  uni_urls <- read_html(courses_base_url) %>%
    html_elements('p') %>%
    html_elements('a') %>%
    html_attrs() %>%
    unlist(use.names = FALSE)
  
  uni_urls <- uni_urls[!str_detect(uni_urls, 'apply-direct')]
  uni_urls <- str_c(courses_base_url, uni_urls)
  
  # uni_url <- 'https://www.uac.edu.au/undergraduate/courses/unsw/list.php'
  walk(uni_urls, function(uni_url) {
    
    uni_code <- str_match(uni_url, '/courses/(.+)/list\\.php')[, 2]
    
    uni_html <- read_html(uni_url)
    
    course_urls <- uni_html %>%
      html_elements('p') %>%
      .[3] %>%
      html_elements('a') %>%
      html_attrs() %>%
      unlist(use.names = FALSE)
    
    course_urls <- str_c(uac_base_url, course_urls)
    course_urls <- course_urls[str_detect(course_urls, '[0-9]+.shtml$')]
    
    dir.create(file.path(download_dir, uni_code))
    
    # course_url <- course_urls[1]
    walk(course_urls, function(course_url) {
      
      course_code <- str_match(course_url, '/([0-9]+).shtml$')[, 2]
      save_file <- file.path(download_dir, uni_code, str_c(course_code, '.html'))
      
      if (!file.exists(save_file)) {
        course_html <- read_html(course_url)
        res <- download.file(course_url, save_file)
        stopifnot(res == 0)
        
        Sys.sleep(runif(1, 0, 300)/1e3)
      }
    })
    
  })
}


parse_courses <- function() {
  
  course_files <- file.path(download_dir, list.files(download_dir, recursive = TRUE))
  
  # course_file <- file.path(download_dir, 'unsw', '425300.html')
  course_raw_df <- map_df(course_files, function(course_file) {
    course_html <- read_html(course_file)
    
    get_text <- function(x) {
      if (length(x) > 0 ) {
        res <- x %>%
          .[[1]] %>% 
          html_element('p') %>%
          html_text() %>% 
          str_replace_all(' *(\r\n)+ *', '\n') %>%
          str_trim()
      } else {
        res <- NA_character_
      }
      res
    }
    
    tibble(
      uni_code = str_split_fixed(course_file, '/', 3)[, 2],
      course_code = str_replace(str_split_fixed(course_file, '/', 3)[, 3], '\\.html', ''),
      course_name = course_html %>% html_nodes('.course-title') %>% html_text(),
      course_intro = course_html %>% html_nodes(xpath = '//*[@id="about-intro"]') %>% get_text(),
      area_of_study = course_html %>% html_nodes(xpath = '//*[@id="about-areas-study"]') %>% get_text(),
      career_ops = course_html %>% html_nodes(xpath = '//*[@id="about-career-opps"]') %>% get_text()
    )
  })
  
  groupings_df <- scrape_groupings()
  
  course_meta_df <- scrape_course_meta()
  
  course_df <- course_raw_df %>%
    group_by(course_code) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    left_join(groupings_df, by = 'course_code') %>%
    left_join(course_meta_df %>% set_names(str_c('meta_', colnames(.))), by = c('course_code' = 'meta_code'))
  
  stopifnot(nrow(course_df) == nrow(distinct(course_df, course_code)))
  
  write_csv(course_df, 'courses.csv')
}

scrape_groupings <- function() {
  
  # manual saving complete html in chrome (control S)
  
  files <- file.path('scrape_groupings', list.files('scrape_groupings', '*.html'))
  
  # file <- 'scrape_groupings/Architecture and Building.html'
  groupings_df <- map_df(files, function(file) {
    course_codes <- read_html(file) %>%
      html_nodes('.course-code') %>%
      html_text()
    
    course_field <- str_replace(str_split_fixed(file, '/', 2)[, 2], '\\.html', '')
    tibble(course_field = course_field, course_code = course_codes)
  }) %>%
    distinct()
  
  stopifnot(nrow(groupings_df) == nrow(distinct(groupings_df, course_code)))
  
  groupings_df
}

scrape_course_meta <- function() {
  
  # find this by going to chrome dev tools: observe the XHR request and responses, and then trial and error
  
  url <- 'https://www.uac.edu.au/course-search/undergraduate/ug.json'
  download.file(url, 'ug.json')
  
  tmp <- jsonlite::fromJSON('ug.json')
  course_meta_df <- as_tibble(tmp$courses) %>% select(-o)
  
  course_meta_df <- course_meta_df %>%
    group_by(code) %>%
    filter(row_number() == 1) %>%
    ungroup()
  
  stopifnot(nrow(course_meta_df) == nrow(distinct(course_meta_df, code)))
  
  course_meta_df
}


