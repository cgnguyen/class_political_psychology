# Setup -------------------------------------------------------------------
## Packete  ----------------------------------------------------------------
  library(tidyverse)

  #Notiz: 0_data_cleaning.R für Hintergrund zur Datenreinigung und Aufarbeitung

##Daten Einlesen ---------------------------------------------------------
  # #Graphische Oberfläche 
  # ESS<-readRDS(file = choose.files())
  
  #über direkte Ansteuerung 
  ESS<-readRDS(file="./Data/ess.rds")

  
  
  

# Balken-diagramme ---------------------------------------------------------

##Einfach - Durchschnittliches Vertrauen nach Bildungsstand-------------------------------------------
  fig_simple<-
    ESS%>%
      group_by(education_simple)%>%
      summarize(trust_mean=mean(trust_index,na.rm=T))%>%
      filter(!is.na(education_simple))%>%
      filter(education_simple!="Andere")%>%
      ggplot()+
        aes(x=education_simple, y=trust_mean)+
        geom_col()
  
  fig_simple
  
  ###Anpassungen und schöner machen--------------------------------------------
  fig_simple+
    theme_bw()+
    xlab("Bildungsstand")+
    ylab("Durschnittliches Vertrauen")+
    ggtitle("Durschnittliches Vertrauen in Europa")+
    ylim(0,10)
  
  
  
  ###Standardfehler hinzufügen --------------------------------------------------
    library(plotrix)

    fig_se<-
      ESS%>%
        group_by(education_simple)%>%
        summarize(trust_mean=mean(trust_index,na.rm=T),
                  se= std.error(trust_index,na.rm=T))%>%
        filter(!is.na(education_simple))%>%
        filter(education_simple!="Andere")%>%
        ggplot()+
          aes(x=education_simple, y=trust_mean, ymin=trust_mean-1.96*se,ymax=trust_mean+1.96*se)+
          geom_col(color="black",fill="gray")+
          geom_errorbar(width=0.3, color="red")+
          theme_minimal()+
          xlab("Bildungsstand")+
          ylab("Durschnittliches Vertrauen")+
          ggtitle("Durschnittliches Vertrauen in Europa")+
          ylim(0,10)
    
    
  ###Abbildungen speichern ----------------------------
      fig_se
      ggsave(plot=fig_se, filename = "./Figures/trust_se.png")
    
    

### Tidyverse und Abbildung Mehr verbinden  ---------------------------------
  ESS %>%
    group_by(activity_simple) %>%
    summarize(n=n())%>%
    mutate(freq = n / sum(n))%>%
    filter(!is.na(activity_simple))%>%
        ggplot()+
        aes(x=activity_simple,y=freq)+
        geom_col()+
        theme_bw()+
        theme(axis.text.x = element_text(angle = 45, hjust=1))
    
  
    
## Etwas komplexer - Vertrauen, Bildung, Geschlecht  --------------------------
  ###Erste Version - Das funktioniert nicht
      ESS%>%
        group_by(education_simple,gender)%>%
        summarize(trust_mean=mean(trust_index,na.rm=T))%>%
        filter(!is.na(education_simple))%>%
        filter(education_simple!="Andere")%>%
        filter(!is.na(gender))%>%
        ggplot()+
          aes(x=education_simple, y=trust_mean, group=gender)+
          geom_col()
      
      
      #Nebeneinander
      ESS%>%
        group_by(education_simple,gender)%>%
        summarize(trust_mean=mean(trust_index,na.rm=T), 
                  se= std.error(trust_index,na.rm=T))%>%
        filter(!is.na(education_simple))%>%
        filter(education_simple!="Andere")%>%
        filter(!is.na(gender))%>%
        ggplot()+
          aes(x=education_simple, y=trust_mean, 
              fill=gender,
              ymin=trust_mean-1.96*se,ymax=trust_mean+1.96*se)+
          geom_col(position = position_dodge())  +
          geom_errorbar(position=position_dodge(.9), width = .4)+
          theme_minimal()+
          xlab("Bildungsstand")+
          ylab("Durschnittliches Vertrauen")+
          ggtitle("Durschnittliches Vertrauen in Europa nach Geschlecht")+
          ylim(0,10)+
          scale_fill_manual(values=c("darkred","darkblue"))
      
      
##Sehr Komplex: Vertrauen,Bildung, Geschlecht nach Land------------------------
      ESS%>%
        group_by(education_simple,gender,cntry)%>%
        summarize(trust_mean=mean(trust_index,na.rm=T), 
                  se= std.error(trust_index,na.rm=T))%>%
        filter(!is.na(education_simple))%>%
        filter(education_simple!="Andere")%>%
        filter(!is.na(gender))%>%
        ggplot()+
          aes(x=education_simple, y=trust_mean, 
              fill=gender,
              ymin=trust_mean-1.96*se,ymax=trust_mean+1.96*se)+
          geom_col(position = position_dodge())  +
          geom_errorbar(position=position_dodge(.9), width = .4)+
          facet_wrap(.~cntry)+
          theme_minimal()+
          xlab("Bildungsstand")+
          ylab("Durschnittliches Vertrauen")+
          ggtitle("Durschnittliches Vertrauen in Europa nach Geschlecht")+
          ylim(0,10)+
          scale_fill_manual(values=c("darkred","darkblue"))
      
      
#Linendiagramme ----------------------------------------------------------------
    ##Einfach:Vertrauen über Zeit----------------------------------------------
    ###Einfache Abbildung -----------------------------------------------------
     ESS%>%
        group_by(essround)%>%
        summarize(trust_mean=mean(trust_index,na.rm=T))%>%
        ggplot()+
          aes(x=essround, y=trust_mean)+
          geom_line()
      
      
      ###Anpassungen------------------------------------------
      ESS%>%
        group_by(essround)%>%
        summarize(trust_mean=mean(trust_index,na.rm=T))%>%
        ggplot()+
          aes(x=essround, y=trust_mean)+
          geom_line()+
          xlab("Jahr")+
          ylab("Durschnittliches Vertrauen")+
          ggtitle("Durschnittliches Vertrauen in Europa Über die Zeit")+
          theme_bw()+
          scale_x_continuous(breaks=seq(1:9),
                             labels=seq(from=2002, to=2018, by=2))
                                      
      #Warnung: unterschiedliche Länder + Keine Gewichtung 
      
      
    
    ##Vertrauen Über Zeit in unterschiedlichen Ländern-----------------------
    ###Nicht sehr übersichtliche Version -----------------------------------
    ESS%>%
        group_by(essround,cntry)%>%
        summarize(trust_mean=mean(trust_index,na.rm=T))%>%
        ggplot()+
          aes(x=essround, y=trust_mean, color=cntry)+
          geom_line()+
          xlab("Jahr")+
          ylab("Durschnittliches Vertrauen")+
          ggtitle("Durschnittliches Vertrauen in Europa Über die Zeit")+
          theme_bw()+
          scale_x_continuous(breaks=seq(1:9),
                             labels=seq(from=2002, to=2018, by=2))
        
      
    ###Bestimmte Länder + Krise -------------------------------------------
                         
    #Nur einige Länder ausgewählt: DE, ES, UK, DK, FR
    select.vec<-c("DE","ES","GB","DK","FR")
      
    library(ggthemes) #Für etwas bessere Farben + Farbblindheit
      
    ESS%>%
        group_by(essround,cntry)%>%
        summarize(trust_mean=mean(trust_index,na.rm=T))%>%
        filter(cntry %in% select.vec)%>%
        ggplot()+
          aes(x=essround, y=trust_mean, color=cntry)+
          geom_line()+
          xlab("Jahr")+
          ylab("Durschnittliches Vertrauen")+
          ggtitle("Durschnittliches Vertrauen in Europa Über die Zeit")+
          theme_bw()+
          scale_x_continuous(breaks=seq(1:9),
                             labels=seq(from=2002, to=2018, by=2))+
          scale_color_colorblind()+
          geom_vline(aes(xintercept=4), linetype="dashed")
 
    
    
 
    ###Alle Länder im Facet_wrap -------------------------------------
    ESS%>%
        group_by(essround,cntry)%>%
        summarize(trust_mean=mean(trust_index,na.rm=T))%>%
        ggplot()+
          aes(x=essround, y=trust_mean)+
          facet_wrap(~cntry)+
          geom_line()+
          xlab("ESS Runde")+
          ylab("Durschnittliches Vertrauen")+
          ggtitle("Durschnittliches Vertrauen in Europa Über die Zeit")+
          theme_bw()+
          scale_x_continuous(breaks=seq(1:9))+
          geom_vline(aes(xintercept=4), linetype="dashed")
 
    
    

# Streudiagramme ----------------------------------------------------------
  ##Zusammenhang zwischen Allgemeinem Vertrauen und Vertrauen in die Politik----
  ###Durschnittsversion-----------------------------------------
  ESS%>%
        group_by(trstplt)%>%
        summarize(trust_mean=mean(trust_index,na.rm=T))%>%
        filter(!is.na(trstplt))%>%
        ggplot()+
          aes(x=trstplt, y=trust_mean)+
          geom_point()+
          xlab("Vertrauen in Politiker")+
          ylab("Allgemeines Vertrauen")+
          theme_bw()
    
    
    
  ###Zusammenhang auf Länderebene ----------------------------------------------------
  library(ggrepel)
  ESS%>%
    group_by(cntry)%>%
    summarize(trust_mean=mean(trust_index,na.rm=T),
              trust_pol_mean=mean(trstplt,na.rm=T))%>%
      ggplot()+
          aes(x=trust_pol_mean, y=trust_mean, label=cntry)+
          geom_point()+
          geom_label_repel()+
          xlab("Vertrauen in Politiker")+
          ylab("Allgemeines Vertrauen")+
          theme_bw()
    
  
  
  ###"Proper" scatterplot------------------------------------------------------------
  #Funktioniert nicht - aber warum?
   ESS%>%
    ggplot()+
      aes(x=trstplt, y=trust_index)+
      theme_bw()+
      geom_point()+
      xlab("Vertrauen in Politiker")+
      ylab("Allgemeines Vertrauen")+
      theme_bw()
  
  ESS%>%
    sample_n(2000)%>%
    ggplot()+
      aes(x=trstplt, y=trust_index)+
      theme_bw()+
      geom_point(position=position_jitter(), alpha=0.4)+
      xlab("Vertrauen in Politiker")+
      ylab("Allgemeines Vertrauen")+
      theme_bw()+
      geom_smooth(method="lm")

  
  

# Bonus: Karten -----------------------------------------------------------
    library(maps) #maps
    library(viridis)    #For theme
    
    

##Durschnittsvertrauen berechnen ----------------------------------------------
  data.temp<-
    ESS%>%
      group_by(cntry)%>%
      summarize(trust_mean=mean(trust_index,na.rm=T))


##Karte Herunerladen und daten Verbinden----------------------------------------
  world_map <- map_data("world")
  #Read in the country code conversion 
  countrycodes<-read.csv("./DATA/countrycodes.csv")
  names(countrycodes)<-c("region","cntry")
    
  #Merge with data frame for easier integration into map
  data.temp<-inner_join(data.temp,countrycodes)
    
  #Merge data into map
  eu_map <- right_join(data.temp, world_map, by = "region")
  
    
  #Karte 1
  figure_map<-ggplot(eu_map, aes(long, lat, group = group))+
    geom_polygon(aes(fill = trust_mean ), color = "white")
  figure_map
    
  #Karte Zugeschnitten 
  figure_map<-ggplot(eu_map, aes(long, lat, group = group))+
    geom_polygon(aes(fill = trust_mean ), color = "white")+
    coord_fixed(xlim = c(-10, 30),  ylim = c(36, 72), ratio = 1.3)+
      theme(legend.position = "none")+
      scale_fill_continuous(high = "#132B43", low = "#56B1F7")+
      theme_minimal() +
      xlab("")+ylab("")+
      theme(legend.position = "none")
      
      

  
  #Show map
  figure_map
    
    
  
    
  
  
  
  
  
   

  
    
    
    
    

    
    
    
    
    
      
      
      
      
      
  