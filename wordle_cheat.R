# Extract current Wordle solution

library(stringr)

js <- readLines("https://www.nytimes.com/games/wordle/main.bd4cb59c.js")[1]

solutions <- str_extract(js[1], 'Ma=\\[[a-z|\\"|\\,]+') %>% 
  strsplit(",") %>% 
  unlist() %>% 
  str_remove_all('Ma=\\[|[:punct:]')

# New solutions at 13:00 UTC
# Does not seem t owork on Windows due to tz conflict??
current_time <- as.POSIXct(format(Sys.time(), tz = "UTC"))
start_time <- as.POSIXct("2021-06-17 13:00:00", tz = "UTC")

solutions[ceiling(as.numeric(current_time - start_time))]
