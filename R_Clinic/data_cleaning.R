#Setup--------------------------
  library(haven)
  library(tidyverse)
  library(labelled)
  library(car)

  #Saved in base directory- not synced 
  DATA<-read_sav("aar1.sav")
  DATA<-read_sav(choose.files())
  
##Check Data--------------------------
  View(DATA)

#Data Cleaning----------------------------------
##Sociodemographics---------------------
  DATA$gender_resp<-as_factor(DATA$v_13)
  DATA$migrant<-as_factor(DATA$c_0013)
  DATA$edu_simple<-as_factor(DATA$v_15)
  DATA$age<-DATA$v_12
  DATA$bundesland<-as_factor(DATA$v_16)
  DATA$lr<-car::recode(DATA$v_102,"98=NA;99=NA")
  DATA$activity<-as_factor(DATA$v_28)
  
  
  ##Corona Evaluations---------------------
  DATA$corona_affected<-car::recode(DATA$v_203,"98=NA;99=NA")
  DATA$corona_fear<-car::recode(DATA$v_207,"98=NA;99=NA")
  DATA$corona_eval<-car::recode(DATA$v_205,"98=NA;99=NA")
  
  ##Main Activity---------------------
  ## Recoding DATA$activity into DATA$activity_simple
    DATA$activity_simple <- recode_factor(DATA$activity,
      "Vollzeit berufstätig" = "Full_Time",
      "Teilzeit berufstätig" = "Part_Time",
      "In Pension/ Rente" = "Retired",
      "Schüler/Student/ in Ausbildung" = "Education",
      "Arbeitslos oder arbeitssuchend" = "Unemployed",
      "Hausmann/Hausfrau" = "At_home",
      "Elternzeit" = "At_home",
      "Arbeitsunfähig" = "Sick_or_disabled",
      "Anderes" = "Other",
      "weiß nicht" = NA_character_,
      "keine Angabe" = NA_character_
    )
  
  
##Own identity---------------------
  temp<-look_for(DATA,"F11")
    
  temp
  
  DATA<-DATA%>%
    mutate(asian_resp=as.factor(case_when(v_47 ==1 ~ "Yes",
                                          TRUE ~ "No")),
           black_resp=as.factor(case_when(v_48 ==1 ~ "Yes",
                                          TRUE ~ "No")),
           white_resp=as.factor(case_when(v_49 ==1 ~ "Yes",
                                          TRUE ~ "No")))
  
  
  

##Disgust Sensitivity---------------------
  #Mean function to speed up mean calcuatlion
  DATA<-
    DATA %>% 
      mutate(disgust_1=v_96,
             disgust_2=v_97,
             disgust_3=v_98,
             disgust_4=v_99,
             disgust_5=v_100,
             disgust_6=v_101) %>%
      mutate(disgust_all = rowMeans(dplyr::select(.,disgust_1,disgust_2,disgust_3,
                                      disgust_4,disgust_5,disgust_6), na.rm=T),
             disgust_disease = rowMeans(dplyr::select(.,disgust_3,disgust_6),na.rm=T),
             disgust_animal =  rowMeans(dplyr::select(.,disgust_2,disgust_5),na.rm=T),
             disgust_general = rowMeans(dplyr::select(.,disgust_1,disgust_4),na.rm=T))
             
    ##Racism Measures--------------------------
    ###Modern Racism Scale-------------------------
    #Note- last item reverse coded
    DATA<-
      DATA %>% 
        mutate(mod_rac_1= car::recode(as.numeric(v_162)," 6= NA;5=NA;98=NA"),
               mod_rac_2= car::recode(as.numeric(v_163)," 6= NA;5=NA;98=NA"),
               mod_rac_3= car::recode(as.numeric(v_164)," 6= NA;5=NA;98=NA"),
               mod_rac_4= car::recode(as.numeric(v_165)," 6= NA;5=NA;98=NA"),
               mod_rac_5= car::recode(as.numeric(v_166)," 6= NA;5=NA;98=NA"),
               mod_rac_6= car::recode(as.numeric(v_167)," 6= NA;5=NA;98=NA;
                                        5=1;4=2;3=3;2=4;1=5",))%>%
        mutate(mod_rac_index=rowMeans(dplyr::select(.,mod_rac_1,
                                                    mod_rac_2,
                                                    mod_rac_3,
                                                    mod_rac_4,
                                                    mod_rac_5,
                                                    mod_rac_6), na.rm=T))

  ###PTV /AFD Support----------------------
    look_for(DATA, "F22")
    DATA$ptv_afd<-DATA$v_123
    
    look_for(DATA, "F21")
     
     DATA<-
       DATA%>%
       mutate(party= as_factor(v_103))%>%
       mutate(afd= as.factor(case_when(party == "AfD"  ~ 1,
                             party == "keine Angabe"  ~ NA_real_,
                             party == "weiß nicht" ~ NA_real_,
                             TRUE ~ 0 )))
     
    ###Group Evaluations------------------------------
     DATA<-
       DATA%>%
        mutate(eval_white=v_184,
               eval_asian=v_178,
               eval_black=v_180,
               eval_muslim=v_182)
              
     
    ##Strength of German identity--------------------------
     DATA$ger_strength<-car::recode(DATA$v_40,"98=NA;99=NA")-1

    ##Evaluations of Life/Situation--------------------
     look_for(DATA,"F8")
     
     DATA<-
       DATA%>%
        mutate(eval_life=v_134,
               eval_econ_self=v_136,
               eval_econ_ger=v_138,
               eval_gov=v_140,
               eval_demo=v_142)

    ##Generalized Trust Variables-------------------
     look_for(DATA,"F27")
     
     DATA<-DATA%>%
       mutate(trust_1=v_145, 
              trust_2=v_148,
              trust_3=v_149)%>%
      mutate(trust_index = rowMeans(dplyr::select(.,trust_1,trust_2,trust_3),na.rm=T))
     
     
    ##Trust in Institutions----------
     look_for(DATA,"F28") 
     
     DATA<-DATA%>%
       mutate(trust_parl=v_151, 
              trust_court=v_153,
              trust_police=v_155,
              trust_politicans=v_157,
              trust_rki=v_159,
              trust_media=v_161)
         
      ##Social Distance Measures----------------------
     look_for(DATA, "F33")
     
     DATA<-
       DATA%>%
        mutate(soc_dist_asian_1=car::recode(v_185,"98=NA;99=NA")-4,
               soc_dist_asian_2=car::recode(v_186,"98=NA;99=NA")-4,
               soc_dist_asian_3=car::recode(v_187,"98=NA;99=NA")-4,
               soc_dist_black_1=car::recode(v_188,"98=NA;99=NA")-4,
               soc_dist_black_2=car::recode(v_189,"98=NA;99=NA")-4,
               soc_dist_black_3=car::recode(v_190,"98=NA;99=NA")-4,
               soc_dist_muslim_1=car::recode(v_191,"98=NA;99=NA")-4,
               soc_dist_muslim_2=car::recode(v_192,"98=NA;99=NA")-4,
               soc_dist_muslim_3=car::recode(v_193,"98=NA;99=NA")-4,
               soc_dist_white_1=car::recode(v_194,"98=NA;99=NA")-4,
               soc_dist_white_2=car::recode(v_195,"98=NA;99=NA")-4,
               soc_dist_white_3=car::recode(v_196,"98=NA;99=NA")-4)%>%
         mutate(soc_dist_asian_index = rowMeans(dplyr::select(.,soc_dist_asian_1,soc_dist_asian_2,soc_dist_asian_3),na.rm=T),
                soc_dist_black_index = rowMeans(dplyr::select(.,soc_dist_black_1,soc_dist_black_2,soc_dist_black_3),na.rm=T),
                soc_dist_muslim_index = rowMeans(dplyr::select(.,soc_dist_muslim_1,soc_dist_muslim_2,soc_dist_muslim_3),na.rm=T),
                soc_dist_white_index = rowMeans(dplyr::select(.,soc_dist_white_1,soc_dist_white_2,soc_dist_white_3),na.rm=T))
     
     
    ####*Big 5####
    #Some Items reverse coded)
  DATA<-
    DATA%>%
    mutate(big5_extro_1=(car::recode(v_63,"98=NA;99=NA;-999:-970=NA")*-1+5),
           big5_agree_1=(car::recode(v_64,"98=NA;99=NA;-999:-970=NA")-1),
           big5_consc_1=(car::recode(v_65,"98=NA;99=NA;-999:-970=NA")*-1+5),
           big5_neuro_1=(car::recode(v_66,"98=NA;99=NA;-999:-970=NA")*-1+5),
           big5_open_1=(car::recode(v_67,"98=NA;99=NA;-999:-970=NA")*-1+5),
           big5_extro_2=(car::recode(v_68,"98=NA;99=NA;-999:-970=NA")-1),
           big5_agree_2=(car::recode(v_69,"98=NA;99=NA;-999:-970=NA")*-1+5),
           big5_consc_2=(car::recode(v_70,"98=NA;99=NA;-999:-970=NA")-1),
           big5_neuro_2=(car::recode(v_71,"98=NA;99=NA;-999:-970=NA")-1),
           big5_open_2=(car::recode(v_72,"98=NA;99=NA;-999:-970=NA")-1))%>%
    mutate(big_5_extro = rowMeans(dplyr::select(.,big5_extro_1,big5_extro_2), na.rm=T),
           big_5_agree= rowMeans(dplyr::select(.,big5_agree_1,big5_agree_2), na.rm=T),
           big_5_consc= rowMeans(dplyr::select(.,big5_consc_1,big5_extro_2), na.rm=T),
           big_5_neuro= rowMeans(dplyr::select(.,big5_neuro_1,big5_neuro_2), na.rm=T),
           big_5_open= rowMeans(dplyr::select(.,big5_open_1,big5_open_2), na.rm=T))
  
  
##Asian Blame-------------------------
  look_for(DATA,"Asiaten")
  
  DATA$asian_blame_1<-car::recode(DATA$v_209,"98=NA;99=NA")-4
  DATA$asian_blame_2<-car::recode(DATA$v_210,"98=NA;99=NA")-4
 
  
  
##Perspective on "Asian"- German relations---------------------
  look_for(DATA,"F46")
  
  DATA$asian_trust<-car::recode(DATA$v_227,"98=NA;99=NA")-3
  DATA$asian_culture<-car::recode(DATA$v_228,"98=NA;99=NA")-3
  DATA$asian_children<-car::recode(DATA$v_229,"98=NA;99=NA")-3
  
  