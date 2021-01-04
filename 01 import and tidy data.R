library(tidyverse)

raw_text <- read_file("data.txt") # read in the text file
per_chunk <- unlist(strsplit(raw_text, "=========="))  # seperate into chunks

# This function takes a chunk of character information
# and seperates it into lines. 
seperate_into_lines <- function(chunk){
  result <- stringr::str_split(chunk, "\r\n")
  unlist(result)
}

# result <- seperate_into_lines(per_chunk[100])  # testing if this works 
## you should put this into formal test frameworks such as testhat if you
## build a package. 



# Extract title sentance and remove author
# This function presumes that you already extracted the raw data into
# character chunks.
extract_title <- function(linechunk){
  # search for second line
  titleline <- linechunk[2]
  return <- gsub("\\(.*\\)", "", titleline) # it took me some 
  #time to work this regular expression out.
  stringr::str_trim(return, side = "both") # remove whitespace at ends
}
#extract_title(result) # testcase to see if it works for me.


# Extract the author from chunk, this function looks 
# very much like the one above, it uses the same logic.
extract_author <- function(linechunk){
  # search for second line
  titleline <- linechunk[2] # identical
  author <- stringr::str_extract(titleline, "\\(.*\\)") # extract piece
  return <- gsub("\\(|\\)", "", author)  # 
  stringr::str_trim(return, side = "both")
}
# extract_author(result)


# this function extracts all the pieces
# and subsequent functions will deal with the seperate stuff.
extract_type_location_date <- function(linechunk){
  meta_row <- linechunk[3]
  pieces <- stringr::str_split(meta_row, "\\|") # the literal character, 
  # the '|' has a special meaning in regexp.
  unlist(pieces)
}
# extract_type_location_date(result) # test function


# extract page number by selecting first piece,
# trimming off of whitespace
# selecting a number, at least 1 times, followed by end of line.
extract_pagenumber <- function(pieces){
  pieces[1] %>%
    stringr::str_trim( side = "right") %>% # remove right end
    stringr::str_extract("[0-9]{1,}$") %>% 
    as.numeric()
}
# extract_type_location_date(result) %>%
#     extract_pagenumber()

# Extract locations. Just like above.
extract_locations <- function(pieces){
  pieces[2] %>% 
    stringr::str_trim( side = "both") %>% 
    stringr::str_extract("[0-9]{1,}-[0-9]{1,}$")
}
# extract_type_location_date(result) %>% 
#     extract_locations()

# Extract date and convert to standard time, not US centric.
# I use the strptime from the base package here. The time is 
# US-centric, but structured, so we can use the formatting from strptime.
# For example: %B is Full month name in the current locale
# and %I:%M %p means hours, minutes, am/pm. 
extract_date <- function(pieces){
  pieces[3] %>% 
    stringr::str_trim( side = "both") %>% 
    stringr::str_extract("[A-z]{3,} [0-9]{1,2}, [0-9]{4}, [0-9]{2}:[0-9]{2} [A-Z]{2}") %>% 
    strptime(format = "%B %e, %Y, %I:%M %p") 
}

# Extract the highlight part.
extract_highlights <- function(linechunk){
  linechunk[5]
}
# extract_highlights(result)

data <- data_frame(
  author = character(length = length(per_chunk)),
  title = character(length(per_chunk)),
  location = character(length(per_chunk)),
  pagenr = numeric(length(per_chunk)),
  highlight = character(length(per_chunk))
)
# loop through all values 
for( i in seq_along(per_chunk)){
  hold <- per_chunk[i] %>% seperate_into_lines()
  data$author[i] <- hold %>% extract_author()
  data$title[i] <- hold %>% extract_title()
  data$location[i] <- hold %>% extract_type_location_date() %>% extract_locations()
  data$pagenr[i] <- hold %>% extract_type_location_date() %>% extract_pagenumber()
  data$highlight[i] <- hold %>% extract_highlights()
}
data
