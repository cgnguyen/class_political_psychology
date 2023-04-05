# Setup-----------------------------------------------------------------------
## Packete Laden ----
  library(tidyverse)
  library(haven)
  library(labelled)
  library(car) #Aufpassen - es gibt einen Konflikt mit dplyr und purrr
## Daten Einlesen und Anschauen --------------------------------------------

  DATA<-read.csv("./DATA/ESS/ESS1-9e01_1_full.csv")
  
  View(DATA)
  summary(DATA)
  names(DATA)
  
  
#Numerische Daten säubern --------------------------------------------
  #Referenz für Codes im Handbuch - hier "codebook.html"  
  
  ##NA für numerische Variablen --------------------------------------------
  DATA$agea
  
  ###Alter - Base R----
  DATA$age_1 <- car::recode(DATA$agea, "999=NA")
  summary(DATA$age_1)

  ###Alter - Tidyverse ----
  DATA<-
    DATA %>%
      mutate(age = dplyr::na_if(agea, 999))
  
  DATA%>%
    select(agea,age_1)%>%
    summary(.)
  
  
  ###Mehrere Variablen 1: Gen Vertrauen und Across -------------------------------------------- 
  #Hier für alle Vertrauen Variablen - wie sehen diese aus? 
  DATA%>%
    select(ppltrst,pplfair,pplhlp)%>%
    summary()
  
  #Jeder wert für 99 wird zu NA
  DATA<-
    DATA%>%
      mutate(across(c("ppltrst","pplfair","pplhlp"), ~na_if(.,99)))
  
  DATA%>%
    select(ppltrst,pplfair,pplhlp)%>%
    summary()
  
  
  #Oh - wir haben Werte vergessen - Handbuch sagt 77 und 88 sind auch NAs
  DATA<-
    DATA%>%
      mutate(across(c("ppltrst","pplfair","pplhlp"), ~na_if(.,77)),
             across(c("ppltrst","pplfair","pplhlp"), ~na_if(.,88)))
  
  summary(DATA$ppltrst)
  
  ###Mehrere Variablen 2:Inst. Vertrauen und Across+starts_with -----
  names(DATA)
  #viel tippen ist nervig - alle variablen beginnen mit "trst"
  DATA%>%
    dplyr::select(starts_with("trst"))%>%
    summary(.)
  
  #Tidyverse Pure 
  DATA<-
    DATA%>%
      mutate(
        across(starts_with("trst"),~na_if(.,77)),
        across(starts_with("trst"),~na_if(.,88)),
        across(starts_with("trst"),~na_if(.,99)))
        
  #Tidyverse + car Packet
  DATA<-
    DATA%>%
      mutate(
        across(starts_with("trst"), 
               ~car::recode(.,"c(77,88,99)=NA")))
  
  ##Numerische Daten Manipulieren ----
  #### z.B. Einen Vertrauen Mittelwert Index bilden ---- 
  
  #Entscheidung - Missing Values?
  DATA<-
    DATA%>%
      mutate(trust_index_1 = rowMeans(.[c("ppltrst","pplfair","pplhlp")]))

  DATA<-
    DATA%>%
      mutate(trust_index= rowMeans(.[c("ppltrst","pplfair","pplhlp")], na.rm=T))

  DATA %>%
    select(trust_index_1,trust_index)%>%
    summary(.)
  
  
  
#Faktor Variablen Säubern -----
  ##Numerische Variable in Faktoren umwandeln - "Einfach": Geschlecht -------
  summary(DATA$gndr)
  
  
  ###Base R Version ----
  
  DATA$gender<-factor(DATA$gndr, 
                      levels=c(1,2,9), 
                      labels=c("Mann","Frau",NA))  # Auch hier- Entscheidung "Keine Antwort" als NA zu coden 
  summary(DATA$gender)
  
  
  ###Tidyverse Version ----
  DATA<-
    DATA%>%
      mutate(gender = dplyr::recode_factor(gndr, 
                                         `1` ="Mann",
                                         `2` ="Frau",
                                         `9` = NA_character_))
  
  
  ##Numerische Variablen in Faktoren Umwandeln - Kompliziert:Arbeitsmarktstatus
  summary(DATA$mnactic)
    
   DATA<-
      DATA%>%
        mutate(activity = dplyr::recode_factor(mnactic, 
                                          `1` ="Bezahlte Arbeit",
                                          `2` ="Ausbildung, Schule, oder Universität",
                                          `3` ="Arbeitslos - Arbeitssuchend",
                                          `4` ="Arbeitslos - Inaktiv" ,
                                          `5` ="Arbeitsunfähig ",
                                          `6` = "In Rente",
                                          `7` = "Zivil oder Militärdienst",
                                          `8` = 'Hausarbeit oder Kindererziehung',
                                          `9` = "Andere Tätigkeiten",
                                          `66` = NA_character_,
                                          `77` = NA_character_,
                                          `88` = NA_character_,
                                          `99` = NA_character_))
  
  
  summary(DATA$activity)
  
  
  ##Faktoren vereinfachen  ----
  ###Beim Recoden ---------------------------
    DATA<-
      DATA%>%
        mutate(activity_simple = dplyr::recode_factor(mnactic, 
                                          `1` ="Bezahlte Arbeit",
                                          `2` ="Ausbildung, Schule, oder Universität",
                                          `3` ="Nicht in Arbeit",
                                          `4` ="Nicht in Arbeit" ,
                                          `5` ="Nicht in Arbeit",
                                          `6` = "In Rente",
                                          `7` = "Andere Tätigkeiten",
                                          `8` = 'Andere Tätigkeiten',
                                          `9` = "Andere Tätigkeiten",
                                          `66` = NA_character_,
                                          `77` = NA_character_,
                                          `88` = NA_character_,
                                          `99` = NA_character_))
  summary(DATA$activity_simple)
  
  
  ###Bestehende Faktoren recode ----
  library(questionr)
  #Dann addin-level recoding Funktion nutzen ## Recoding DATA$activity into DATA$activity_rec
  
  
  
  ##Referenz Kategorie Festlegen ----
  summary(DATA$gender)
  
  DATA$gender<-relevel(DATA$gender, ref="Frau")
  
  summary(DATA$gender)
  
  
  ##Faktoren Reinfolge ändern ----
  DATA$activity <- factor(DATA$activity,
  levels = c(
    "Bezahlte Arbeit", "In Rente", "Arbeitslos - Arbeitssuchend",
    "Arbeitslos - Inaktiv", "Arbeitsunfähig ", "Ausbildung, Schule, oder Universität",
    "Zivil oder Militärdienst", "Hausarbeit oder Kindererziehung",
    "Andere Tätigkeiten" ))
  
  #Addin ist am Einfachsten  - questionr + level reordering



  

# Andere Variablen Reinigen -----------------------------------------------
  DATA<-
    DATA%>%
      mutate(
        country=as.factor(cntry),
        across(starts_with("stf"), 
          ~car::recode(.,"c(77,88,99)=NA")),
        happy=car::recode(happy,"c(77,88,99)=NA"),
        lr=car::recode(lrscale,"c(77,88,99)=NA"),
        eduyrs=car::recode(eduyrs,"c(77,88,99)=NA"),
        education_simple = dplyr::recode_factor(eisced, 
                                          `0` = "Andere",
                                          `1` ="Niedrig",
                                          `2` ="Niedrig",
                                          `3` ="Mittel",
                                          `4` ="Mittel" ,
                                          `5` ="Mittel",
                                          `6` = "Hoch",
                                          `7` = "Hoch",
                                          `55` = NA_character_,
                                          `77` = NA_character_,
                                          `88` = NA_character_,
                                          `99` = NA_character_)
        )

  
  
    DATA$hincfel<-as.factor(DATA$hincfel)

    ## Recoding DATA$hincfel into DATA$income_feel
      DATA$income_feel <- fct_recode(DATA$hincfel,
        "Comfortable" = "1",
        "Coping" = "2",
        "Difficult" = "3",
        "Very Difficult" = "4",
        NULL = "7",
        NULL = "8",
        NULL = "9"
      )

# Filter Variablen  -------------------------------------------------------
  DATA<-DATA %>%
    select("cntry", "cname", "cedition", "cproddat", "cseqno", "name", 
          "essround", "edition", "idno", "dweight", "pspwght", "pweight", 
          "anweight", "ppltrst", "pplfair", "pplhlp", "polintr", "trstprl", 
          "trstlgl", "trstplc", "trstplt", "trstprt", "trstep", "trstun", 
          "vote", "lrscale", "stflife", "stfeco", "stfgov", "stfdem", "happy", 
          "gndr", "agea", "eisced", "mnactic", "age_1", "age", "trust_index_1", 
          "trust_index", "gender", "activity", "activity_simple", "country", 
          "lr", "education_simple","eduyrs","income_feel")
            
  

  
#Daten speichern ----- 
  #Internationales CSV Format
  write.csv(DATA, file="./DATA/ess.csv")
  
  #Deutsches CSV Format 
  write.csv2(DATA, file="./DATA/ess_de.csv")
  
  #SPSS Format  - haven Packet
  write_sav(DATA, path="./DATA/ess.sav")
  
  #STATA Format - haven Packet
  write_dta(DATA, path="./DATA/ess.dta")
  
  #R Environment Format 
  saveRDS(DATA, file = "./DATA/ess_neu.rds")

  
  
   
