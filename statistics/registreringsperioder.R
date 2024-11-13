#
#
#Registreringsstatistik
#
#

library(tidyverse)
uppsatser <- read_csv(file="/home/shub/assets/diva/diva_studentessays_latest.csv")
forskarpubl <- read_csv(file="/home/shub/assets/diva/diva_researchpubl_sh_latest.csv")

uppsatser <- uppsatser %>% 
  filter(between(Year, 2019, 2023)) %>%
  select(Year, CreatedDate, PublicationDate) %>%
  separate(PublicationDate, into = c("År", "Månad", "Dag"))

write_csv(uppsatser, "uppsatsreg.csv")

uppsatser_pub <- uppsatser %>%
  group_by(Year, Månad)%>%
  summarise(Månad, n())

ggplot(uppsatser_pub) +
  geom_bar(aes(Månad))


forskarpubl <- forskarpubl %>%
  filter(between(Year, 2019, 2023)) %>%
  select(Year, CreatedDate, PublicationDate) %>%
  separate(PublicationDate, into = c("År", "Månad", "Dag"))

forskar_granskning <- forskarpubl %>%
  group_by(Year, Månad)%>%
  summarise(Månad, n())

ggplot(forskar_granskning) +
  geom_bar(aes(Månad))
           
           
