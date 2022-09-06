library(PxWebApiData)
library(tidyverse)
library(janitor)
library(gglorenz)
library(cowplot)
#Hvilke variabler som finnes i tabellen
variables <- ApiData("https://data.ssb.no/api/v0/en/table/12558/", 
                     returnMetaFrames = TRUE)
names(variables)

#hvilke verdier har ulike variablene
values <- ApiData("https://data.ssb.no/api/v0/en/table/12558/", 
                  returnMetaData = TRUE)
#Kommunekoder
values[[1]]$values
#Inntekt før/etter skatt
values[[2]]$values # 00 = Samlet inntekt, 00S=Inntekt etter skatt
#Desiler
values[[3]]$values
#Statistikkvariabel
values[[4]]$values
#År
values[[5]]$values
data <- ApiData("https://data.ssb.no/api/v0/en/table/12558/",
                Tid =c("2005","2020"), # Velg årene 2005 og 2020
                Desiler=c("01", "02", "03" ,"04", "05", "06" ,"07", "08" ,"09", "10"), #Vi velger alle desiler
                InntektSkatt="00", #Vi velger samlet inntekt
                ContentsCode="VerdiDesil", #Velger den høyeste verdien i desilen
                Region=c("5401","1902")) #Tromsø endret kommunenummer i 2020



test <- data %>% 
  as.tibble(data)

test <- test %>% 
  clean_names()

old <- test %>% 
  filter(x12558_households_by_region_income_before_after_tax_decil_group_contents_and_year$year == "2005")

new <- test %>% 
  filter(x12558_households_by_region_income_before_after_tax_decil_group_contents_and_year$year == "2020")

p1 <- ggplot(old, aes(old$x12558_households_by_region_income_before_after_tax_decil_group_contents_and_year$value)) +
  stat_lorenz(desc = FALSE) +
  geom_abline(linetype = "dashed") +
  labs(y = "",
       x = "",
       title = "Lorenz kurve inntekt 2005") +
  theme_bw()

p2 <- ggplot(new, aes(new$x12558_households_by_region_income_before_after_tax_decil_group_contents_and_year$value)) +
  stat_lorenz(desc = FALSE) +
  geom_abline(linetype = "dashed") +
  labs(y = "",
       x = "",
       title = "Lorenz kurve inntekt 2020") +
  theme_bw()

plot_grid(p1, p2)

#kommentarer i word

