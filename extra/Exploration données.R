library(readr)
eruptions <- read_csv("data/eruptions.csv")
View(eruptions)

unique(eruptions$evidence_method_dating)

events <- read_csv("data/events.csv")
View(events)

unique(events$event_type)


barplot(table(volcan$minor_rock_1,volcan$region))
barplot(table(volcan$vei))
barplot(table(volcan$start_year,volcan$region))

barplot(table(volcan$eruption_category))
table(volcan$eruption_category)
