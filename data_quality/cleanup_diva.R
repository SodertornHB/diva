#
#
#
#Städa i DiVA
#170921 GL
#Olika skript som sammanställer poster som behöver åtgärdas i DiVA. 
#Innehåller: Ahead of print, Ej granskningsmärkta, Manuskript i sammanläggningsavhandlingar, Sidor saknas i kapitel
#
#
#

library(tidyverse)
library(stringr)

#Läs in data från DiVA. Vi använder en csvall2-fil.
diva <- read_csv(file = "/home/shub/assets/diva/diva_researchpubl_sh_latest.csv")


# Ahead of print ----------------------------------------------------------
#För att använda Rmarkdown behöver även filinläsning och biblioteken klistras in.

#Uraval av publikationer efter status
ahead_of_print <- diva %>%
  subset(Status == "aheadofprint") %>%
  select(PID, DOI) %>%
  transmute(PID, link = str_c("https://doi.org/", DOI))


# Ej granskningsmärkta ----------------------------------------------------
#Poster som är markerade som ej granskade, trots att de inte ligger i granskningen.
#Ger en csv-fil med PID och titel.
not_reviewed <- diva %>% filter(Reviewed == "false")
not_reviewed <- not_reviewed %>% select(`PID`, Title)
write_csv(not_reviewed, "Ej granskningsmärkta")


# Manuskript i sammanläggningsavhandlingar --------------------------------
#Ger en csv-fil

#Välj första publikationsår för avhandlingarna
thesis_year <- 2012

manuscript <- diva %>%
  filter(PublicationType == "Manuskript (preprint)") %>%
  select(PID, Name, Title, PublicationType, PartOfThesis)
thesis <- diva %>%
  filter(PublicationType == "Doktorsavhandling, sammanläggning", Year >= thesis_year) %>%
  select(Year, NBN)

manus_to_check <- manuscript %>%
  inner_join(thesis, c("PartOfThesis" = "NBN")) %>%
  arrange(desc(Year)) %>%
  write_csv("Manuskript i avhandlingar")


# Sidor saknas i kapitel --------------------------------------------------
#Ger en csv.fil med kapitel som saknar både start- och slutsida.

kapitel <- diva %>%
  filter(PublicationType == "Kapitel i bok, del av antologi") %>%
  filter((is.na(StartPage) & (is.na(EndPage)))) %>%
  select(PID, Title, HostPublication, Year, Publisher, ISBN) %>%
  write_csv("Kapitel utan sidor")
