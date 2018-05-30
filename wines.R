library(rvest)
library(purrr)
library(tibble)

url_base <- "http://www.winemag.com/?s=washington merlot&drink_type=wine&page=%d"

map_df(1:39, function(i) {
  cat(".")
  pg <- read_html(sprintf(url_base, i))
    
  tibble(wine = pg %>% html_nodes(".review-listing .title") %>% html_text, 
        excerpt = pg %>% html_nodes("div.excerpt") %>% html_text, 
        rating = pg %>% html_nodes("span.rating") %>% html_text %>% gsub(" Points", "", .), 
        appellation = pg %>% html_nodes("span.appellation") %>% html_text, 
        price = pg %>% html_nodes("span.price") %>% html_text %>% gsub("\\$", "", .))
  }) -> wines
View(wines)





