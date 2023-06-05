#Setup-------------
  library(tidyverse)
  library(haven)
  library(panelr)
  library(naniar)
  library(car)
  
  
  D<-read_dta("./Data/dezim_aar_wide_c_100.dta")

#Beispiel Daten Reinigen-------------------
  
 ##Faktor Variable- Geschlecht---------------------
  D$v_13_w1
  
  summary(D$v_13_w1)

  
  D<-
    D%>%
      mutate(gender_resp=as_factor(v_13_w1))
  
  
  summary(D$gender_resp)
  
  #Fehler- Anyonimisert= divers: Recode 
  
  D<-
    D%>%
    mutate(gender_resp=car::recode(gender_resp,
                                   "'Anonymisiert'='divers';
                                      'Keine Angaben'= NA"))
  
  summary(D$gender_resp)

  ##Andere Variablen --------------
  D<-
    D%>%
      mutate(
        bundesland=as_factor(v_161_w1),
        age_group=as_factor(v_12_g_w1),
      )
 
  ##Aufgabe 1-----------------
  #Bereinigit die Variable  Hauptaktivität (F2)
  
  

  ##numerische Variable zu Faktor Variable ---------------------------
  
  
  D<-D%>%
    mutate(asian_resp=as.factor(case_when(v_47_w1 ==1 ~ "Yes", 
                                          v_47_w1 ==0 ~ "No")),
           black_resp=as.factor(case_when(v_48_w2 ==1 ~ "Yes",
                                          TRUE ~ "No")),
           white_resp=as.factor(case_when(v_49_w2 ==1 ~ "Yes")))
  
  
  ##Numerische Variablen Reinigen 
  
  ###Beispiel-  Gruppen Evaluation ---------------
  look_for(D, "F31") # note coding error 94=NA?
  D<-
    D%>%
      mutate(eval_white=car::recode(v_184_w1, "94=NA"),
             eval_asian=car::recode(v_178_w1, "94=NA"),
             eval_black=car::recode(v_180_w1, "94=NA"),
             eval_muslim=car::recode(v_182_w1, "94=NA"))
           
  ###Aufgabe 2- 
  #Links rechts Selbsteinstufung in Welle 1 - Nutzt das Codebuch um den Namen zu finden
  #Die Skala sollte von 0 bis 10  laufen
  
  
  
  
   
  ##Numerische variable Reinigen - Komplex ------------------
  #Big 5
  D<-
    D%>%
    mutate(big5_extro_1=(car::recode(v_63_w1,"98=NA;99=NA;-999:-970=NA")*-1+5),
           big5_agree_1=(car::recode(v_64_w1,"98=NA;99=NA;-999:-970=NA")-1),
           big5_consc_1=(car::recode(v_65_w1,"98=NA;99=NA;-999:-970=NA")*-1+5),
           big5_neuro_1=(car::recode(v_66_w1,"98=NA;99=NA;-999:-970=NA")*-1+5),
           big5_open_1=(car::recode(v_67_w1,"98=NA;99=NA;-999:-970=NA")*-1+5),
           big5_extro_2=(car::recode(v_68_w1,"98=NA;99=NA;-999:-970=NA")-1),
           big5_agree_2=(car::recode(v_69_w1,"98=NA;99=NA;-999:-970=NA")*-1+5),
           big5_consc_2=(car::recode(v_70_w1,"98=NA;99=NA;-999:-970=NA")-1),
           big5_neuro_2=(car::recode(v_71_w1,"98=NA;99=NA;-999:-970=NA")-1),
           big5_open_2=(car::recode(v_72_w1,"98=NA;99=NA;-999:-970=NA")-1))%>%
    mutate(big_5_extro = rowMeans(dplyr::select(.,big5_extro_1,big5_extro_2), na.rm=T),
           big_5_agree= rowMeans(dplyr::select(.,big5_agree_1,big5_agree_2), na.rm=T),
           big_5_consc= rowMeans(dplyr::select(.,big5_consc_1,big5_extro_2), na.rm=T),
           big_5_neuro= rowMeans(dplyr::select(.,big5_neuro_1,big5_neuro_2), na.rm=T),
           big_5_open= rowMeans(dplyr::select(.,big5_open_1,big5_open_2), na.rm=T))
  
  
  
  
#Daten zusammenfassen und visualsieren--------------------------
    #Verteilung von Links-Rechts Selbsteinstufung 
    D%>%
      select(lr)%>%
      summary(.)
  
    #Histogram 
    D%>%
      ggplot()+
        aes(x=lr)+
        geom_bar()
    
    #Durschnitt LR
    D%>%
      summarize(lr_mean=mean(lr,na.rm=T))
    
    
    #Aufgabe 3: Visualsiert die Verteilung von Offenheit in den Daten als Balkendiagramm 
    
    
    #Durschnitt LR nach geschlecht
    D%>%
      group_by(gender_resp)%>%
      summarize(lr_mean=mean(lr,na.rm=T))
    
    #Visualsiert
    D%>%
      group_by(gender_resp)%>%
      summarize(lr_mean=mean(lr,na.rm=T))%>%
      ggplot()+
        aes(x=gender_resp,y=lr_mean)+
        geom_col()
    
    #Visualsiert etwas schöner 
    D%>%
      group_by(gender_resp)%>%
      summarize(lr_mean=mean(lr,na.rm=T))%>%
      ggplot()+
        aes(x=gender_resp,y=lr_mean)+
        geom_col()+
        theme_minimal()+
        ylab("Links-Rechts")+xlab("Geschlecht")+
        ylim(0,10)
    
    
# Zusammenhang zwischen Persönlichkeit und Links-Rechts 
    
    D%>%
      group_by(big_5_open)%>%
      summarize(lr_mean=mean(lr,na.rm=T)) 
    
    
    #Visualsiert 1
    D%>%
      group_by(big_5_open)%>%
      summarize(lr_mean=mean(lr,na.rm=T))%>%
      ggplot()+
        aes(x=big_5_open,y=lr_mean)+
        geom_col()+
        theme_minimal()+
        ylab("Links-Rechts")+xlab("Big 5: Offenheit")+
        ylim(0,10)
    
    #Streudiagramm 1 -warum geht das nicht?
     D%>%
      ggplot()+
      aes(x=big_5_open,y=lr)+
      geom_point()+
      theme_minimal()+
      ylab("Links-Rechts")+xlab("Big 5: Offenheit")+
      ylim(0,10)
    
     #Streudiagramm 2 - Etwas besser
     D%>%
       ggplot()+
       aes(x=big_5_open,y=lr)+
       geom_point(position = "jitter", alpha=0.2)+
       theme_minimal()+
       ylab("Links-Rechts")+xlab("Big 5: Offenheit")+
       ylim(0,10)
     
     
  
  #Aufgabe 4: Visualiert den Zusammenhang zwischen Big 5: Neuroticism und Links Rechts
    
    
    
    
  

  
  
