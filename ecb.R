library(rvest)
library(magrittr)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)

url <- "https://www.ecb.europa.eu/press/calendars/mgcgc/html/index.en.html"

events <- 
  read_html(url) %>% 
  html_node(".definition-list") %>% 
  html_text() %>% 
  str_split("\\t\\n") %>% 
  .[[1]] %>%
  str_subset("^ $", T) %>% 
  str_subset("^\n $", T) %>% 
  str_subset("^$", T) %>% 
  str_split("\\n\\n\\s+\\n") %>% 
  tibble::enframe() %>% 
  unnest(value) %>% 
  group_by(name) %>% 
  mutate(type = if_else(row_number() == 1, "date", "detail")) %>% 
  ungroup() %>% 
  pivot_wider(names_from = type, values_from = value) %>% 
  mutate(across(detail, ~str_remove_all(., "\\t") %>% str_trim())) %>% 
  mutate(date = lubridate::dmy(date)) %>% 
  select(-name) %>% 
  # delete the press conferences after the gov council meeting
  filter(str_detect(detail, "Press conference following the Governing Council meeting", T))

if (!length(events$date %>% unique()) == nrow(events)) {
  stop("More than one event in the same day; check to see if these can be merged.\nThis is because code is assuming only one event per day to generate the UIDs")
}

make_event <- function(date, summary) {
  #output <- 
    c(
    "BEGIN:VEVENT",
    paste0("DTEND;VALUE=DATE:", gsub("-", "", date+1)),
    paste0("DTSTART;VALUE=DATE:", gsub("-", "", date)),
    paste0("SUMMARY:", summary),
    paste0("UID:ca6af7456b0088abad9a69f9f620f5ac-", date),
    "SEQUENCE:0",
    #paste0("DTSTAMP:", paste0(format(Sys.Date(), "%Y%m%d"), "T", format(Sys.time(), "%H%M%S"), "Z")), #comentado para não criar um commit novo mesmo quando não há alterações
    "END:VEVENT"
  )
}

# start the file
c(
  "BEGIN:VCALENDAR",
  "VERSION:2.0",
  "METHOD:PUBLISH",
  "PRODID:PERSONALCALENDAR",
  "CALSCALE:GREGORIAN"
) %>% 
  cat(file = "ecb_calendar.ics", sep = "\n")

# add the events
events %>% 
  mutate(event = map2(date, detail, make_event)) %>% 
  select(event) %>% 
  unnest(event) %>% 
  pull(event) %>% 
  cat(file = "ecb_calendar.ics", sep="\n", append = TRUE)

# end the file
cat(c("END:VCALENDAR"), file = "ecb_calendar.ics", sep="\n", append = TRUE)
