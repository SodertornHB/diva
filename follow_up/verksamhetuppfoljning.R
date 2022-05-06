#
# Verksamhetsuppföljning institutioner
# 220317 GL
# Uppföljning av publikationer INTE samma urval som ÅR. 
# Går det att få in instnamn genom att hämta värde?
#
#


library(tidyverse)
source('/home/shub/src/common/lib/sh_diva_bibliometrics_functions.R')

diva <- read_csv(file="/home/shub/assets/diva/diva_researchpubl_sh_latest.csv")
diva <- diva %>% filter(between(Year, 2017, 2021))

diva_urval <- diva %>% 
  filter(!(PublicationType == "Övrigt")) %>%
  filter(ContentType!="Övrig (populärvetenskap, debatt, mm)") %>%
  filter((is.na(Status))|Status=="published"|Status=="inPress") %>%
  filter(is.na(PublicationSubtype)|PublicationSubtype == "publishedPaper" |PublicationSubtype == "meetingAbstract" 
         |PublicationSubtype == "editorialMaterial"|PublicationSubtype == "letter")

tabell_urval <- diva_urval %>%
  group_by(PublicationType,ContentType) %>% 
  count(Year) %>%
  spread(Year, n)%>%
  unite(PublicationType, PublicationType, ContentType, sep=": ")

tabell_urval <- tabell_urval[c(1,6,5,4,3,2)]
tabell_urval[is.na(tabell_urval)] <- 0L

ahead <- diva %>%
  filter(ContentType!="Övrig (populärvetenskap, debatt, mm)") %>%
  filter(PublicationType == "Artikel i tidskrift"|PublicationType == "Artikel, forskningsöversikt"
         |PublicationType == "Artikel, recension")%>%
  filter(Status=="aheadofprint")

tabell_ahead <- ahead %>%
  group_by(PublicationType,ContentType) %>% 
  count(Year) %>%
  spread(Year, n)%>%
  unite(PublicationType, PublicationType, ContentType, sep=": ")

tabell_ahead[is.na(tabell_ahead)] <- 0L

write_csv(diva_urval, "sh_publikationer.csv")
write_csv(tabell_urval, "sh_publikationer_tabell.csv")
write_csv(tabell_ahead, "sh_artiklar_ahead.csv")

# Författare --------------------------------------------------------------

diva_author <- read_csv(file="/home/shub/assets/diva/diva_author_sh_latest.csv")
diva_author <- diva_author %>% filter(between(Year, 2017, 2021))

diva_author_urval <- diva_author %>% 
  filter(!(PublicationType == "Övrigt")) %>%
  filter(ContentType!="Övrig (populärvetenskap, debatt, mm)") %>%
  filter((is.na(Status))|Status=="published"|Status=="inPress") %>%
  filter(is.na(PublicationSubtype)|PublicationSubtype == "publishedPaper" |PublicationSubtype == "meetingAbstract" 
         |PublicationSubtype == "editorialMaterial"|PublicationSubtype == "letter")

write_csv(diva_author_urval, "sh_författare.csv")

# ISV ---------------------------------------------------------------------
inst <- diva_urval %>% filter_orgs(sam)

tabell_inst <- inst %>%
  group_by(PublicationType,ContentType) %>% 
  count(Year) %>%
  spread(Year, n)%>%
  unite(PublicationType, PublicationType, ContentType, sep=": ") %>%
  rename(ISV = PublicationType)

tabell_inst <- tabell_inst[c(1,6,5,4,3,2)]
tabell_inst[is.na(tabell_inst)] <- 0L

tabell_inst_ahead <- ahead %>%
  filter_orgs(sam) %>%
  group_by(PublicationType,ContentType) %>% 
  count(Year) %>%
  spread(Year, n)%>%
  unite(PublicationType, PublicationType, ContentType, sep=": ")%>%
  rename("ISV Ahead of print" = PublicationType)

tabell_inst_ahead[is.na(tabell_inst_ahead)] <- 0L

#Spara filer
inst <- select(inst, -(list_of_orgs))
write_csv(inst, "isv_publikationer.csv" )
write_csv(tabell_inst, "isv_publikationer_tabell.csv")
write_csv(tabell_inst_ahead, "isv_artiklar_ahead.csv")

# IKL ---------------------------------------------------------------------
inst <- diva_urval %>% filter_orgs(ikl)

tabell_inst <- inst %>%
  group_by(PublicationType,ContentType) %>% 
  count(Year) %>%
  spread(Year, n)%>%
  unite(PublicationType, PublicationType, ContentType, sep=": ") %>%
  rename(IKL = PublicationType)

tabell_inst <- tabell_inst[c(1,6,5,4,3,2)]
tabell_inst[is.na(tabell_inst)] <- 0L

tabell_inst_ahead <- ahead %>%
  filter_orgs(ikl) %>%
  group_by(PublicationType,ContentType) %>% 
  count(Year) %>%
  spread(Year, n)%>%
  unite(PublicationType, PublicationType, ContentType, sep=": ")%>%
  rename("IKL Ahead of print" = PublicationType)

tabell_inst_ahead[is.na(tabell_inst_ahead)] <- 0L

#Spara filer
inst <- select(inst, -(list_of_orgs))
write_csv(inst, "ikl_publikationer.csv" )
write_csv(tabell_inst, "ikl_publikationer_tabell.csv")
write_csv(tabell_inst_ahead, "ikl_artiklar_ahead.csv")

# IHS ---------------------------------------------------------------------
inst <- diva_urval %>% filter_orgs(hs)

tabell_inst <- inst %>%
  group_by(PublicationType,ContentType) %>% 
  count(Year) %>%
  spread(Year, n)%>%
  unite(PublicationType, PublicationType, ContentType, sep=": ") %>%
  rename(IHS = PublicationType)

tabell_inst <- tabell_inst[c(1,6,5,4,3,2)]
tabell_inst[is.na(tabell_inst)] <- 0L

tabell_inst_ahead <- ahead %>%
  filter_orgs(hs) %>%
  group_by(PublicationType,ContentType) %>% 
  count(Year) %>%
  spread(Year, n)%>%
  unite(PublicationType, PublicationType, ContentType, sep=": ")%>%
  rename("IHS Ahead of print" = PublicationType)

tabell_inst_ahead[is.na(tabell_inst_ahead)] <- 0L

#Spara filer
inst <- select(inst, -(list_of_orgs))
write_csv(inst, "ihs_publikationer.csv" )
write_csv(tabell_inst, "ihs_publikationer_tabell.csv")
write_csv(tabell_inst_ahead, "ihs_artiklar_ahead.csv")


# NMT ---------------------------------------------------------------------

inst <- diva_urval %>% filter_orgs(nmt)

tabell_inst <- inst %>%
  group_by(PublicationType,ContentType) %>% 
  count(Year) %>%
  spread(Year, n)%>%
  unite(PublicationType, PublicationType, ContentType, sep=": ") %>%
  rename(NMT = PublicationType)

tabell_inst <- tabell_inst[c(1,6,5,4,3,2)]
tabell_inst[is.na(tabell_inst)] <- 0L

tabell_inst_ahead <- ahead %>%
  filter_orgs(nmt) %>%
  group_by(PublicationType,ContentType) %>% 
  count(Year) %>%
  spread(Year, n)%>%
  unite(PublicationType, PublicationType, ContentType, sep=": ")%>%
  rename("NMT Ahead of print" = PublicationType)

tabell_inst_ahead[is.na(tabell_inst_ahead)] <- 0L

#Spara filer
inst <- select(inst, -(list_of_orgs))
write_csv(inst, "nmt_publikationer.csv" )
write_csv(tabell_inst, "nmt_publikationer_tabell.csv")
write_csv(tabell_inst_ahead, "nmt_artiklar_ahead.csv")


# IPA -------------------------------------------------------------------

inst <- diva_urval %>% filter_orgs(polis)

tabell_inst <- inst %>%
  group_by(PublicationType,ContentType) %>% 
  count(Year) %>%
  spread(Year, n)%>%
  unite(PublicationType, PublicationType, ContentType, sep=": ") %>%
  rename(IPA = PublicationType)

#Lägg till kolumn 6 för omsortering
tabell_inst <- tabell_inst[c(1,5,4,3,2)]
tabell_inst[is.na(tabell_inst)] <- 0L

tabell_inst_ahead <- ahead %>%
  filter_orgs(polis) %>%
  group_by(PublicationType,ContentType) %>% 
  count(Year) %>%
  spread(Year, n)%>%
  unite(PublicationType, PublicationType, ContentType, sep=": ")%>%
  rename("IPA Ahead of print"= PublicationType)

tabell_inst_ahead[is.na(tabell_inst_ahead)] <- 0L

#Spara filer
inst <- select(inst, -(list_of_orgs))
write_csv(inst, "ipa_publikationer.csv" )
write_csv(tabell_inst, "ipa_publikationer_tabell.csv")
write_csv(tabell_inst_ahead, "ipa_artiklar_ahead.csv")

# Lararinst ---------------------------------------------------------------

inst <- diva_urval %>% filter_orgs(lararinst)

tabell_inst <- inst %>%
  group_by(PublicationType,ContentType) %>% 
  count(Year) %>%
  spread(Year, n)%>%
  unite(PublicationType, PublicationType, ContentType, sep=": ") %>%
  rename(Lararinst = PublicationType)

tabell_inst <- tabell_inst[c(1,6,5,4,3,2)]
tabell_inst[is.na(tabell_inst)] <- 0L

tabell_inst_ahead <- ahead %>%
  filter_orgs(lararinst) %>%
  group_by(PublicationType,ContentType) %>% 
  count(Year) %>%
  spread(Year, n)%>%
  unite(PublicationType, PublicationType, ContentType, sep=": ")%>%
  rename("Lärarinst Ahead of print" = PublicationType)

tabell_inst_ahead[is.na(tabell_inst_ahead)] <- 0L

#Spara filer
inst <- select(inst, -(list_of_orgs))
write_csv(inst, "lararinst_publikationer.csv" )
write_csv(tabell_inst, "lararinst_publikationer_tabell.csv")
write_csv(tabell_inst_ahead, "lararinst_artiklar_ahead.csv")
