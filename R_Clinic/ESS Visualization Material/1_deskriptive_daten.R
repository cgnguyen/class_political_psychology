# Setup -------------------------------------------------------------------
## Pakete  ----------------------------------------------------------------
  library(tidyverse)
  library(haven)

##Daten Einlesen ---------------------------------------------------------
  #Graphische Oberfläche 
  ESS<-readRDS(file = choose.files())
  
  #über direkte Ansteuerung 
  ESS<-readRDS(file="./Data/ess.rds")

# Daten Anschauen ---------------------------------------------------------
##Ganz einfache Übersicht -------------------------------------------------
  #Variablen 
  names(ESS)
  
  #Zusammenfassung
  summary(ESS)
  
  #Anschauen
  View(ESS)

  
##Spezifische Variablen Anschauen -------------------------------------------

###Numerische Variablen --------------------------------------------------
  class(ESS$trust_index)
  
  #Summary Statistics - Base R
  summary(ESS$trust_index)
  
  #Summary Statistics - Tidyverse
  ESS%>%
    select(trust_index)%>%
    summary()
  
  #Durchschnitt
  mean(ESS$trust_index)

  mean(ESS$trust_index, na.rm=T)
  
  #Mittelwert 
  median(ESS$trust_index, na.rm=T)
  
  #Tidyverse Version
  ESS%>%
    summarize(durchschnitt=mean(trust_index, na.rm=T),
              mittelwert=median(trust_index, na.rm=T))
  
  
###Faktor Variablen --------------------------------------------------------
  #Base R Version
  summary(ESS$activity_simple)

  #Tidyverse Version
  ESS%>%
    select(activity_simple)%>%
    summary(.)
  
  

### Tidyverse und Berechnungen ----------------------------------------------------
  ESS %>%
    group_by(cntry,activity_simple) %>%
    summarize(n=n())%>%
    mutate(freq = n / sum(n))
    
  
  

#Cross-Tabs, Zusammenfassung nach Gruppen ---------------------------------------------------------------
##Gruppenzusammenfassungen ------------------------------------------------
  ESS%>%
    group_by(gender)%>%
    summarize(trust_mean=mean(trust_index,na.rm=T))%>%
    print(n=30)
  
  ESS%>%
    group_by(cntry)%>%
    summarize(trust_mean=mean(trust_index,na.rm=T))%>%
    print(n=30)
  
  
  ESS%>%
    group_by(cntry,gender)%>%
    summarize(trust_mean=mean(trust_index,na.rm=T))%>%
    print(n=30)
  
  ESS%>%
    group_by(cntry,gender)%>%
    summarize(trust_mean=mean(trust_index,na.rm=T))%>%
    filter(!is.na(gender))%>%
    print(n=30)
  

#Anwendungen ------------------------------------------------------
  ##Veränderungen über die Zeit + Veränderung über Zeit in den Ländern?---------
  ESS%>%
    group_by(essround)%>%
    summarize(trust_mean=mean(trust_index,na.rm=T))
  
  ESS%>%
    group_by(essround,country)%>%
    summarize(trust_mean=mean(trust_index,na.rm=T))%>%
    print(n=200)
  
  #Etwas schwer zu lesen - Pivot Wider
   ESS%>%
    group_by(essround,country)%>%
    summarize(trust_mean=mean(trust_index,na.rm=T))%>%
    pivot_wider(names_from =essround, values_from = trust_mean)%>%
    print(n=30)
   
   
   #Als Tabelle Speichern 
   table_trust<-
    ESS%>%
      group_by(essround,country)%>%
      summarize(trust_mean=mean(trust_index,na.rm=T))%>%
      pivot_wider(names_from =essround, values_from = trust_mean)

   write.csv(table_trust, file="trust_table.csv")
   #xtable, rmarkdown, officerR, latex etc. als weiterführende Option 
   #Beispiel knitr und KableExtra für Rmarkdown/ HTML
   library(knitr)
   library(kableExtra)
   
   table_trust %>%
    kbl(caption = "Vertrauen in Europe über die Zeit") %>%
    kable_classic(full_width = F, html_font = "Cambria")
  
  table_trust %>%
   mutate(across(where(is.numeric), round, 2)) %>%
    kbl(caption = "Vertrauen in Europe über die Zeit") %>%
    kable_classic(full_width = F, html_font = "Cambria")

   
  ##Unterschied Mann und Frau in unterschiedlichen Ländern ----------------------  
  ESS%>%
    group_by(cntry,gender)%>%
    summarize(trust_mean=mean(trust_index,na.rm=T))%>%
    filter(!is.na(gender))%>%
    pivot_wider(names_from =gender, values_from = trust_mean)%>%
    mutate(Unterschied=Frau-Mann)%>%
    arrange(Unterschied)%>%
    kbl(caption = "Trust Gap und Gender") %>%
    kable_classic(full_width = F, html_font = "Cambria")

  

  

  
  

  
  