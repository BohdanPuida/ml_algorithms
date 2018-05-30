library(rvest)
library(purrr)

URL <- "https://scistarter.com/finder?phrase=&lat=&lng=&activity=At%20the%20beach&topic=&search_filters=&search_audience=&page=1#view-projects"
sci_html <- read_html(URL)

sci_html %>%
  html_nodes("td") %>%
  html_text %>%
  trimws -> 
  page_list

goals <- page_list[seq(from=1, to=30,by=3)] 
task <- page_list[seq(from=2, to=30,by=3)]
location <- page_list[seq(from=3, to=30,by=3)]

sci_html %>%
  html_nodes("div#project-listing") %>% 
  html_nodes("h3") %>%                  
  html_text %>%                       
  trimws ->
  title

scistarter_df <- data.frame(title, goals, task, location)
View(scistarter_df)











