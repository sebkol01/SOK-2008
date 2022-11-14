# Oppg 1
options(encoding="UTF-8")
library(httr)
library(rjstat)
library("scales") # For å få leselige tall i graf
url <- "https://data.ssb.no/api/v0/no/table/05185/"

data <-'
{
  "query": [
    {
      "code": "Landbakgrunn",
      "selection": {
        "filter": "agg:Verdensdel2",
        "values": [
          "b0",
          "b11",
          "b12",
          "b13",
          "b14",
          "b2",
          "b3",
          "b4",
          "b5",
          "b6",
          "b8",
          "b9"
        ]
      }
    },
    {
      "code": "Tid",
      "selection": {
        "filter": "item",
        "values": [
          "2005",
          "2006",
          "2007",
          "2008",
          "2009",
          "2010",
          "2011",
          "2012",
          "2013",
          "2014",
          "2015",
          "2016",
          "2017",
          "2018",
          "2019",
          "2020",
          "2021",
          "2022"
        ]
      }
    }
  ],
  "response": {
    "format": "json-stat2"
  }
}
'
d.tmp <- POST(url , body = data, encode = "json", verbose())

inn <- fromJSONstat(content(d.tmp, "text"))

inn

# Fra hvilken region har invandringen til Norge økt mest i denne perioden?

library(tidyverse)

oppg1 <- inn %>%
  ggplot(aes(x = år, y = value, col = landbakgrunn))+
  labs(title = "Innvandring til Norge fra ulike regioner", xlab = "år", ylab = "Antall delt på 100")+
  scale_y_continuous("Antall",labels = comma_format(big.mark = ".",
                                           decimal.mark = ","))+
  geom_point()+
  theme_bw()

oppg1

# Regionen som innvandringen til Norge har økt mest denne perioden er EU-land 
# i Øst-Europa. Det er dog ikke den regionen med flest innvandrere, da dette er
# Afrika, som også har hatt en tydelig økning. 

# Oppg 2

url2 <- "https://data.ssb.no/api/v0/no/table/13215/"

data2 <- '
{
  "query": [
    {
      "code": "Kjonn",
      "selection": {
        "filter": "item",
        "values": [
          "0"
        ]
      }
    },
    {
      "code": "Alder",
      "selection": {
        "filter": "item",
        "values": [
          "15-74"
        ]
      }
    },
    {
      "code": "InnvandrKat",
      "selection": {
        "filter": "item",
        "values": [
          "B"
        ]
      }
    },
    {
      "code": "Landbakgrunn",
      "selection": {
        "filter": "item",
        "values": [
          "015a"
        ]
      }
    },
    {
      "code": "NACE2007",
      "selection": {
        "filter": "agg:NACE260InnvGrupp2",
        "values": [
          "SNI-01-03",
          "SNI-05-09",
          "SNI-10-33",
          "SNI-35-39",
          "SNI-41-43",
          "SNI-45-47",
          "SNI-49-53",
          "SNI-49.3",
          "SNI-55",
          "SNI-56",
          "SNI-58-63",
          "SNI-64-66",
          "SNI-68-75",
          "SNI-77-82",
          "SNI-78.2",
          "SNI-81.2",
          "SNI-84",
          "SNI-85",
          "SNI-86-88",
          "SNI-90-99",
          "SNI-00"
        ]
      }
    },
    {
      "code": "Tid",
      "selection": {
        "filter": "item",
        "values": [
          "2021"
        ]
      }
    }
  ],
  "response": {
    "format": "json-stat2"
  }
}
'
d.tmp2 <- POST(url2 , body = data2, encode = "json", verbose())

jobb <- fromJSONstat(content(d.tmp2, "text"))

oppg2 <- ggplot(jobb) + geom_bar(aes(x = value, y = `næring (SN2007)`), stat = "identity")+
  labs(title = "Næringsfordeling for innvandrere fra EU-land i Øst-Europa", ylab = "Næring")+
  scale_x_continuous("Antall",labels = comma_format(big.mark = ".",
                                                    decimal.mark = ","))

oppg2

# Med denne landbakgrunnen jobbet flest innenfor bygge- og anleggsvirksomheter.

# Oppg 3

# Gjøres i word dokument
