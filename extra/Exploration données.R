library(readr)
eruptions <- read_csv("data/eruptions.csv")
View(eruptions)

unique(eruptions$evidence_method_dating)

events <- read_csv("data/events.csv")
View(events)

unique(events$event_type)
