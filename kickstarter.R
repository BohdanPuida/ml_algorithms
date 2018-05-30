library(rvest)
url.parse <- read_html("https://www.kickstarter.com/projects/1154444340/god-in-new-york/description")
 
reward <- url.parse %>%
  html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "mb0", " " ))]') %>% html_text(trim=T)
reward[c(1,3)] -> reward
substring(reward[1], 2) -> reward[1]
paste((unlist(strsplit(reward[1], ','))), collapse = '') -> reward[1]
as.numeric(reward) -> reward

URL <- 'https://www.kickstarter.com/discover/advanced?woe_id=0&staff_picks=1&sort=magic&seed=2518219&page=10'
k <- read_html(URL)


k %>% 
  html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "normal", " " ))]')

