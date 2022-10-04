##### Oppgave_3_SOK-2008

options(encoding="UTF-8")
library(httr)

# henter rjstat bibliotek for behandling av JSON-stat
library(rjstat)
url <- "https://data.ssb.no/api/v0/no/table/11155/"

# spørring fra konsoll
data <- '{"query": [{"code": "Kjonn","selection": {"filter": "item","values": ["0"]}},{"code": "Alder","selection": {"filter": "item","values": ["20-64","15-24"]}},{"code": "UtdNivaa","selection": {"filter": "item","values": ["TOT","1-2","3-5","6-8"]},{"code": "ContentsCode","selection": {"filter": "item","values": ["ArbLedigProsent"]}},{"code": "Tid","selection": {"filter": "item","values": ["2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020"]}}],"response": {"format": "json-stat2"}}'
d.tmp <- POST(url , body = data, encode = "json", verbose())

# Henter ut innholdet fra d.tmp som tekst deretter bearbeides av fromJSONstat
sbtabell <- fromJSONstat(content(d.tmp, "text"))

# Viser datasettet
sbtabell

# Laster pakker til å lage graf osv

library(tidyverse)

g1 <- sbtabell %>% 
  filter(år == "2020", utdanningsnivå == "Utdanningsnivå i alt")

ggplot(g1, aes(x = alder, y = value, fill = alder, colour = alder)) +
  geom_col(bins = 10, position = "dodge") +
  ggtitle("Arbeidsledighet i prosent blant ungdommer og voksne") +
  theme_bw()
