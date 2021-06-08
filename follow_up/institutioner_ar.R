#
# intitution_ar
# 190116 GL
# Underlaget till årsredovisningen uppdelat på institution.
#
#


library(tidyverse)

source('/home/shub/src/common/lib/sh_parameters.R')
source('/home/shub/src/common/lib/sh_diva_bibliometrics_functions.R')

sh_archive_start("ikl")

# Lägg filen från sammaställningen till årsredoviningen i wd
diva <- read_csv(file="Diva_rådata.csv")
#diva <- read_csv(file="/home/shub/assets/diva/diva_researchpubl_sh_latest.csv")
diva <- diva %>% filter(between(Year, 2014, 2020))

#Gör en körning per institution: nmt, sam, hs, ikl, (polisutbildning, lararutbildning)
inst <- ikl

inst_ar <- diva %>% filter_orgs(inst)

#De publikationer som skall ingå i sammanställningen
divaAR <- inst_ar %>% 
  filter(!(PublicationType == "Samlingsverk (redaktörskap)"|PublicationType == "Proceedings (redaktörskap)" |PublicationType == "Övrigt")) %>%
  filter(ContentType!="Övrig (populärvetenskap, debatt, mm)") %>%
  filter((is.na(Status))|Status=="published"|Status=="inPress") %>%
  filter(is.na(PublicationSubtype)|PublicationSubtype == "publishedPaper" |PublicationSubtype == "meetingAbstract" 
         |PublicationSubtype == "editorialMaterial")

#Slå ihop publikationstyper genom att byta namn på värden
divaAR$PublicationType <- recode(divaAR$PublicationType,
                                 "Artikel i tidskrift" = "Artiklar i tidskrift",
                                 "Artikel, forskningsöversikt" = "Artiklar i tidskrift",
                                 "Artikel, recension" = "Artiklar i tidskrift",
                                 "Kapitel i bok, del av antologi" = "Artiklar i antologi",
                                 "Doktorsavhandling, monografi" = "Doktorsavhandlingar",
                                 "Doktorsavhandling, sammanläggning" = "Doktorsavhandlingar",
                                 "Licentiatavhandling, monografi" = "Licentiatavhandlingar",
                                 "Licentiatavhandling, sammanläggning" = "Licentiatavhandlingar",
                                 "Bok" = "Monografier",
                                 "Konferensbidrag" = "Publicerade konferensbidrag",
                                 "Rapport" = "Rapporter")


#Gör en tabell som skrivs till csv omsorterad och med innhållstyp för artiklar
divaAR$PublicationType <- factor(divaAR$PublicationType, ordered = TRUE, 
                                 levels = c("Artiklar i tidskrift", "Artiklar i antologi", "Monografier", "Publicerade konferensbidrag", 
                                            "Doktorsavhandlingar", "Licentiatavhandlingar", "Rapporter"))

divaAR$ContentType <- factor(divaAR$ContentType, ordered = TRUE,
                             levels = c("Refereegranskat", "Övrigt vetenskapligt"))


articles <- divaAR %>%
  subset(PublicationType=="Artiklar i tidskrift"|PublicationType=="Artiklar i antologi") %>%
  group_by(PublicationType,ContentType) %>% 
  count(Year) %>%
  spread(Year, n)%>%
  unite(PublicationType, PublicationType, ContentType, sep=": ")

other_publication <- divaAR %>%
  subset(!(PublicationType=="Artiklar i tidskrift"|PublicationType=="Artiklar i antologi")) %>%
  group_by(PublicationType) %>% 
  count(Year) %>%
  spread(Year, n)

inst_ar_table <- bind_rows(articles, other_publication)
inst_ar_table <- inst_ar_table[c(1,6,5,4,3,2)]
inst_ar_table[is.na(inst_ar_table)] <- 0L

#Ta bort listorna från filtreringen
divaAR <- select(divaAR, -(list_of_orgs))
inst_ar <- select(inst_ar, -(list_of_orgs))

#Spara i excel innan leverans
write_csv(inst_ar_table, "Inst_underlag_årsredovisning.csv")
sh_archive_resource("Inst_underlag_årsredovisning.csv")
sh_archive_df(divaAR, "Inst_medräknade_publikationer")
sh_archive_df(inst_ar, "Inst_Diva_rådata")
sh_archive_end()

