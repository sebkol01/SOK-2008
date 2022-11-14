library(PxWebApiData)
library(tidyverse)

syke <- ApiData("https://data.ssb.no/api/v0/no/table/12441/", 
                Kjonn=list('item', c("1", "2")), 
                Sykefraver2=list('item', c("Alt")), 
                Tid=list('item', c("2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019")), 
                NACE2007=FALSE, 
                ContentsCode=TRUE)

arbledig <- ApiData("https://data.ssb.no/api/v0/no/table/05111/", 
        ArbStyrkStatus=list('item', c("2")), 
        Kjonn=list('item', c("1", "2")), 
        ContentsCode=list('item', c("Prosent")), 
        Tid=list('item', c("2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019")), 
        Alder=FALSE)

syke <- as.tibble(syke[[1]])

arbledig <- as.tibble(arbledig[[1]])

BigData <- merge(syke, arbledig, by = c("år", "kjønn"))

BigData <- BigData %>% 
  rename("sykefravær" = value.x, "arbeidsledighet" = value.y)

Menn <- BigData %>% 
  group_by(år) %>% 
  select(år, sykefravær, arbeidsledighet, kjønn) %>% 
  filter(kjønn == "Menn")

mcoeff <- 1.3

ggplot(Menn, mapping=aes(x = år, y = sykefravær, group = 1)) +
  geom_line(col = "blue", label = "sykefravær") +
  geom_line(Menn, mapping=aes(y = arbeidsledighet*mcoeff), col = "red") +
  scale_y_continuous("Sykefraværsprosent, blå", 
                     sec.axis = sec_axis(~./mcoeff, name = "Arbeidsledighetsprosent, rød")) +
  ggtitle("Totalt sykefravær og arbeidsledighet")

kcoeff <- 0.5

Kvinner <- BigData %>% 
  group_by(år) %>% 
  select(år, sykefravær, arbeidsledighet, kjønn) %>% 
  filter(kjønn == "Kvinner")

ggplot(Kvinner, mapping=aes(x = år, y = sykefravær, group = 1)) +
  geom_line(col = "blue") +
  geom_line(Kvinner, mapping=aes(y = arbeidsledighet/kcoeff), col = "red") + 
  scale_y_continuous("Sykefraværsprosent, blå",
                     sec.axis = sec_axis(~.*kcoeff, name = "Arbeidsledighetsprosent, rød")) +
  ggtitle("Totalt sykefravær og arbeidsledighet")
