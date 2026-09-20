# Scrape the ECB Governing Council calendar and write .ics feeds.
# Base R only, except xml2 to fetch and parse the HTML page.
library(xml2)

url <- "https://www.ecb.europa.eu/press/calendars/mgcgc/html/index.en.html"

# grab the text of the calendar's definition-list (CSS ".definition-list")
node <- xml_find_first(
  read_html(url),
  "//*[contains(concat(' ', normalize-space(@class), ' '), ' definition-list ')]"
)
raw <- xml_text(node)

# split into entries and drop the blank separators between them
entries <- strsplit(raw, "\t\n")[[1]]
entries <- entries[nzchar(trimws(entries))]

# each entry is "<date>\n\n   \n<detail>\t   "; split into the two halves
parts  <- strsplit(entries, "\n\n[[:space:]]+\n")
date   <- as.Date(vapply(parts, `[`, character(1), 1L), format = "%d/%m/%Y")
detail <- trimws(gsub("\t", "", vapply(parts, `[`, character(1), 2L)))

events <- data.frame(date = date, detail = detail)

# delete the press conferences after the gov council meeting
events <- events[!grepl("Press conference following the Governing Council meeting", events$detail), ]

# shorten the wording
events$detail <- sub("Governing Council of the ECB: ", "ECB GovC: ", events$detail)
events$detail <- sub("General Council meeting of the ECB", "ECB General Council: meeting", events$detail)

if (length(unique(events$date)) != nrow(events)) {
  stop("More than one event in the same day; check to see if these can be merged.\nThis is because code is assuming only one event per day to generate the UIDs")
}

make_event <- function(date, summary) {
  c(
    "BEGIN:VEVENT",
    paste0("DTEND;VALUE=DATE:", gsub("-", "", date + 1)),
    paste0("DTSTART;VALUE=DATE:", gsub("-", "", date)),
    paste0("SUMMARY:", summary),
    paste0("UID:ca6af7456b0088abad9a69f9f620f5ac-", date),
    "SEQUENCE:0",
    #paste0("DTSTAMP:", paste0(format(Sys.Date(), "%Y%m%d"), "T", format(Sys.time(), "%H%M%S"), "Z")), #comentado para não criar um commit novo mesmo quando não há alterações
    "END:VEVENT"
  )
}

write_file <- function(events, path) {
  lines <- c(
    "BEGIN:VCALENDAR",
    "VERSION:2.0",
    "METHOD:PUBLISH",
    "PRODID:PERSONALCALENDAR",
    "CALSCALE:GREGORIAN",
    unlist(Map(make_event, events$date, events$detail)),
    "END:VCALENDAR"
  )
  writeLines(lines, path)
}

# write two versions: all and only monetary policy
write_file(events, "docs/ecb_calendar.ics")

mon_policy <- events[grepl("monetary", events$detail, ignore.case = TRUE) &
                       !grepl("non-monetary", events$detail, ignore.case = TRUE), ]
write_file(mon_policy, "docs/ecb_calendar_mon_policy.ics")
