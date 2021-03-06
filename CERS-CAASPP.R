

library(tidyverse)
library(janitor)
library(readxl)
library(here)
library(ggthemes)
library(googlesheets4)


south.monterey <- read_csv(here("data","South_Monterey_county_joint_union_high_school.csv"))

gonzales <- read_csv(here("data","Gonzales District_and_School_Export_File.csv"))

mpusd <- read_csv(here("data","MPUSD_CAASPP_21-22.csv"))

san.ardo <- read_csv(here("data","San_Ardo_All.csv")) 


king.city <- read_csv(here("data","KCUSD.csv")) 
king.city.21 <- read_csv(here("data","KCUSD_20.21.csv")) 

salinas.city <- read_csv(here("data","SCESD_District_and_School_Export_File.csv"))


pg <- read_csv(here("data","PGUSD_District_and_School_Export_File_071522.csv"))

san.antonio <- read_excel(here("data","SAUSDCAASPP2021-22.xlt" ))
soledad1 <- read_excel(here("data","SUSD CAASPP Results 2022.xlsm"), sheet = 1) %>%
   mutate( GradeLevelWhenAssessed = factor(GradeLevelWhenAssessed, levels = c("KG",1,2,3,4,5,6,7,8,9,10,11,12)))

soledad2 <- read_excel(here("data","SUSD CAASPP Results 2022.xlsm"), sheet = 2)%>%
    mutate( GradeLevelWhenAssessed = factor(GradeLevelWhenAssessed, levels = c("KG",1,2,3,4,5,6,7,8,9,10,11,12)))

soledad3 <- read_excel(here("data","SUSD CAASPP Results 2022.xlsm"), sheet = 3)%>%
    mutate( GradeLevelWhenAssessed = factor(GradeLevelWhenAssessed, levels = c("KG",1,2,3,4,5,6,7,8,9,10,11,12)))

# soledad4 <- read_excel(here("data","SUSD CAASPP Results 2022.xlsm"), sheet = 4)
soledad5 <- read_excel(here("data","SUSD CAASPP Results 2022.xlsm"), sheet = 5) %>%
    mutate( GradeLevelWhenAssessed = factor(GradeLevelWhenAssessed, levels = c("KG",1,2,3,4,5,6,7,8,9,10,11,12)))


soledad <- bind_rows(soledad1,soledad2, soledad3,soledad5) %>%
    mutate( GradeLevelWhenAssessed = recode_factor( GradeLevelWhenAssessed,
                                                    "KG"= "KG",
                                                    "1" = "01",
                                                    "2" = "02",
                                                    "3" = "03",
                                                    "4" = "04",
                                                    "5" = "05",
                                                    "6" = "06",
                                                    "7" = "07",
                                                    "8" = "08",
                                                    "9" = "09")) 




san.lucas <- read_csv(here("data","San_Lucas_District_and_School_Export_File.csv")) 

alisal <- read_csv(here("data","Alisal USD - District_and_School_Export_File.7.13.2022.csv"))



santa.rita <- read_csv(here("data","Santa_Rita_District_and_School_Export_File CAASP assessments 2021-2022.csv"))

### Reference -------

reference <- read_excel(here("data","ScaleScoreREference.xlsx"))

reference2 <- pivot_longer(reference, cols = c(`1`,`2`,`3`,`4`) ) %>%
    mutate(Grade = if_else(str_length(Grade) >= 2, Grade, paste0(0,Grade))) %>%
    rename(Subject = Subject,
           GradeLevelWhenAssessed = Grade,
           ScaleScoreAchievementLevel = name,
           ScaleScoreNext = value)


reference3 <- reference2 %>%
    filter(ScaleScoreAchievementLevel == 2) %>%
    select(-ScaleScoreAchievementLevel) %>%
    rename(MeetStandard = ScaleScoreNext)

reference2 <- reference2 %>%
    left_join(reference3)









 san.antonio %>% 
     filter(AssessmentType == "Summative") %>%
     group_by(AssessmentName) %>%
     tabyl(Subject,ScaleScoreAchievementLevel)


 ### Cleaning -------
 
 clean.df <- function(df) {
     
     df %>% 
         filter(AssessmentType == "Summative") %>%
         mutate(GradeLevelWhenAssessed = if_else(str_length(GradeLevelWhenAssessed) >= 2,
                                                 GradeLevelWhenAssessed, 
                                                 paste0(0,GradeLevelWhenAssessed))) %>%
         mutate(GradeLevelWhenAssessed = factor(GradeLevelWhenAssessed, levels = c(
                                                    "KG",
                                                    "01",
                                                    "02",
                                                    "03",
                                                    "04",
                                                    "05",
                                                    "06",
                                                    "07",
                                                    "08",
                                                    "09",
                                                    "10",
                                                    "11",
                                                    "12"))
                ) %>%
         mutate(EL = ifelse(EnglishLanguageAcquisitionStatus == "EL" | EnglishLanguageAcquisitionStatus == "RFEP", "Yes", NA))
     
 }
 
 
 san.lucas <- clean.df(san.lucas)
 san.antonio <- clean.df(san.antonio)
 alisal <- clean.df(alisal)
 santa.rita <- clean.df(santa.rita)
 pg <- clean.df(pg)
 salinas.city <- clean.df(salinas.city)
 king.city <- clean.df(king.city)
 mpusd <- clean.df(mpusd)
 
 
 leas <- c("santa.rita", "san.lucas", "alisal", "san.antonio", "soledad")
 
 leas <- list(santa.rita, san.lucas, alisal, san.antonio, soledad, pg, salinas.city, king.city, mpusd)
 
 
 
### Graphs --------- 

overall.graph <- function(df) {
    
    df %>% 
        mutate(ScaleScoreAchievementLevel = factor(ScaleScoreAchievementLevel),
        ) %>%
        ggplot( aes( y = Subject, fill = ScaleScoreAchievementLevel)) +
        geom_bar(color = "black") +
        geom_text(    stat = "count",
                      aes(label = ..count..), 
                      position = position_stack(vjust = 0.5), size = 2) +
        theme_hc() +
        scale_fill_brewer() + 
        labs(y = "",
             x = "",
             fill = "Achievement Level",
             title = "Count of Students at each Achievement Level")
}


 san.lucas %>%
     filter(SWD == "Yes") %>%
     graph.wrap()
 
 ggsave("San Lucas SWD.png", width = 12, height = 7)
 
 
overall.graph(king.city)

overall.graph(santa.rita)


graph.wrap <- function(df) {
   
df %>% 
    mutate(ScaleScoreAchievementLevel = factor(ScaleScoreAchievementLevel),
           GradeLevelWhenAssessed = factor(GradeLevelWhenAssessed, levels = c("KG",1,2,3,4,5,6,7,8,11)),
           AssessmentName = case_when(AssessmentName == "Kindergarten ELPAC Summative" ~ "Grade  KG ELPAC Summative",
                                      AssessmentName == "Grade 11 ELA Summative" ~ "Grade11 ELA Summative",
                                      AssessmentName == "Grade 11 Math Summative" ~ "Grade11 Math Summative",
                                      AssessmentName == "Grade 10 ELPAC Summative" ~ "Grade10 ELPAC Summative",
                                      AssessmentName == "Grade 11 ELPAC Summative" ~ "Grade11 ELPAC Summative",
                                      AssessmentName == "Grade 12 ELPAC Summative" ~ "Grade12 ELPAC Summative",
                                    TRUE ~AssessmentName)
    ) %>%
    ggplot( aes( y = AssessmentName, fill = ScaleScoreAchievementLevel)) +
    geom_bar(color = "black") +
    facet_wrap(vars(Subject),
               # vars(GradeLevelWhenAssessed),
               scales = "free") +
    geom_text(    stat = "count",
                  aes(label = ..count..), 
                  position = position_stack(vjust = 0.5), size = 2) +
    # coord_flip() +
    theme_hc() +
    scale_fill_brewer() + 
    labs(y = "",
         x = "",
         fill = "Achievement Level",
         title = "Count of Students at each Achievement Level")

}

graph.grid <- function(df) {
    
    df %>% 
        mutate(ScaleScoreAchievementLevel = factor(ScaleScoreAchievementLevel),
               GradeLevelWhenAssessed2 = factor(GradeLevelWhenAssessed, levels = c("KG",1,2,3,4,5,6,7,8,11)),
               AssessmentName = case_when(AssessmentName == "Kindergarten ELPAC Summative" ~ "Grade  KG ELPAC Summative",
                                          AssessmentName == "Grade 11 ELA Summative" ~ "Grade11 ELA Summative",
                                          AssessmentName == "Grade 11 Math Summative" ~ "Grade11 Math Summative",
                                          AssessmentName == "Grade 10 ELPAC Summative" ~ "Grade10 ELPAC Summative",
                                          AssessmentName == "Grade 11 ELPAC Summative" ~ "Grade11 ELPAC Summative",
                                          AssessmentName == "Grade 12 ELPAC Summative" ~ "Grade12 ELPAC Summative",
                                          TRUE ~AssessmentName)
        ) %>%
        ggplot( aes( y = AssessmentName, fill = ScaleScoreAchievementLevel)) +
        geom_bar(color = "black") +
        facet_grid(vars(Subject),
                   vars(GradeLevelWhenAssessed),
                   scales = "free") +
        geom_text(    stat = "count",
                      aes(label = ..count..), 
                      position = position_stack(vjust = 0.5), size = 2) +
        theme_hc() +
        scale_fill_brewer() + 
        labs(y = "",
             x = "",
             fill = "Achievement Level",
             title = "Count of Students at each Achievement Level")
    
}

save.wrap <- function(df) {
    
    print(df[1,2])
    
    graph.wrap(df)
    
    ggsave(here("output",paste0(df[1,2], " wrap ", Sys.Date(),".png")), width = 12, height = 7)
}

save.grid <- function(df) {
    
    print(df[1,2])
    
    graph.grid(df)
    
    ggsave(here("output",paste0(df[1,2], " grid ", Sys.Date(),".png")), width = 12, height = 7)
}

save.overall <- function(df) {
    
    print(df[1,2])
    
    overall.graph(df)
    
    ggsave(here("output",paste0(df[1,2], " overall ", Sys.Date(),".png")), width = 8, height = 7)
}


graph.wrap(king.city)

graph.grid(salinas.city)

save.overall(king.city)
save.wrap(king.city)
save.grid(king.city)

map(leas, save.wrap)
map(leas, save.grid)
map(leas, save.overall)


###  Passing Percentage -----


passing.perc <- function(df) {
    
    
    ddff <-     deparse(substitute(df)) 
    
hold <- df %>%
    group_by(Subject) %>%
    mutate(Above = ifelse(ScaleScoreAchievementLevel >= 3, TRUE, FALSE),
           perc = mean(Above)*100) %>%
    select(Subject, perc) %>%
    distinct()%>%
    mutate(district = ddff)

sheet_append(ss = sheet,
             sheet = "Percent Met or Exceeded",
             data = hold )




}


passing.perc(santa.rita)

san.lucas %>%
    filter(SWD == "Yes") %>%
    passing.perc()
    
    
    
passing.perc(alisal)

alisal %>%
    filter(EnglishLanguageAcquisitionStatus == "RFEP") %>%
    passing.perc()


passing.perc(soledad)


### Distance from Standard ------


dfs <- function(df) {
    
    df %>% 
        filter(Subject %in% c("ELA","Math")) %>%
        mutate(ScaleScoreAchievementLevel = factor(ScaleScoreAchievementLevel),
        ) %>%
        left_join(reference2) %>%
        group_by(Subject) %>%
        mutate(dist.standard = ScaleScore - MeetStandard,
               mean.dist.stand = mean(dist.standard))  %>%
        select(Subject,mean.dist.stand) %>%
        distinct()
    
}


dfs(santa.rita)

dfs(san.lucas)

dfs(alisal)


 dfs(san.antonio)    
 dfs(soledad)
 
 santa.rita %>%
     #  filter(Filipino == "Yes") %>%
   #  filter(HispanicOrLatinoEthnicity == "Yes") %>%
   #  filter(White == "Yes") %>%
   #  filter(EnglishLanguageAcquisitionStatus == "EL") %>%
     dfs()

 
 ### Student Group Size ------
 
 student.group.size <- function(df) {
     
 df %>%
#         mutate(EL = ifelse(EnglishLanguageAcquisitionStatus == "EL", "Yes", NA)) %>%
         filter(Subject %in% c("ELA", "Math")) %>%
         group_by(Subject) %>%
         summarise( across(c(HispanicOrLatinoEthnicity:Filipino,EL), ~  sum(!is.na(.)))) %>%
         pivot_longer(cols = c(HispanicOrLatinoEthnicity:Filipino,EL)) %>%
     filter(value >= 30)
 }

 

 santa.rita %>%
     filter(Subject %in% c("ELA", "Math")) %>%
    # filter(EnglishLanguageAcquisitionStatus == "EL")
     summarise(EnglishLanguageAcquisitionStatus, sum(str_detect(EnglishLanguageAcquisitionStatus,"EL")))
 
 
 
 
 student.group.size(san.lucas) 

 student.group.size(san.antonio) 
 
 student.group.size(soledad)
 
 student.group.size(king.city)
 
 student.group.size(soledad) # doesn't work because of the wrong files without race data
 
 
 
 ### Records ----
 
 
 
 my.list <- student.group.size(santa.rita) %>%
     select(name) %>%
     distinct() %>%
     as.vector()
 
for (i in my.list) {
    
    ii <-     noquote(i) 
    
    print(ii)
    
    
}
 
 
 
 sheet <- "https://docs.google.com/spreadsheets/d/1iS2Sd37hU7LYzakI2fbiotnzYn60cVp0d0pMB9k6zXk/edit#gid=0"

 dfs2 <- function(df,students) {
     
     ddff <-     deparse(substitute(df)) 
studentsss <-     deparse(substitute(students))
     
    holder <-  df %>% 
         filter(Subject %in% c("ELA","Math")) %>%
         filter({{students}} == "Yes") %>%
         mutate(ScaleScoreAchievementLevel = factor(ScaleScoreAchievementLevel),
         ) %>%
         left_join(reference2) %>%
         group_by(Subject) %>%
         mutate(dist.standard = ScaleScore - MeetStandard,
                mean.dist.stand = mean(dist.standard))  %>%
         select(Subject,mean.dist.stand) %>%
         distinct() %>%
         mutate(district = ddff,
                students = studentsss
         )
    
    sheet_append(ss = sheet,
                 sheet = "Distance from Standard",
                data = holder )
     
 }

 dfs2(santa.rita,White) 
  dfs2(santa.rita,EL) 
 dfs2(santa.rita, HispanicOrLatinoEthnicity)
 
  dfs2(alisal,White) 
  dfs2(alisal,EL) 
     dfs2(alisal, HispanicOrLatinoEthnicity)
  dfs2(alisal,Filipino) 
  
  dfs2(san.antonio,White) 
  dfs2(san.antonio, HispanicOrLatinoEthnicity)
  
  dfs2(san.lucas, HispanicOrLatinoEthnicity)
  dfs2(san.lucas, SWD)
  
  
  dfs2(pg,White) 
  dfs2(pg,EL) 
  dfs2(pg,Asian) 
  dfs2(pg,HispanicOrLatinoEthnicity) 
  
  
  
  dfs2(salinas.city,White) 
  dfs2(salinas.city,EL) 
  dfs2(salinas.city, HispanicOrLatinoEthnicity)
  dfs2(salinas.city,Filipino) 
  
  
  
  dfs2(king.city,White) 
  dfs2(king.city,EL) 
  dfs2(king.city, HispanicOrLatinoEthnicity)
  
  
  
  ####  ELPAC by School -----
  
  
  elpac.school <- function(df) {
      
      df %>% 
          filter( Subject =="ELPAC" ) %>%
          mutate(ScaleScoreAchievementLevel = factor(ScaleScoreAchievementLevel),
                 GradeLevelWhenAssessed2 = factor(GradeLevelWhenAssessed, levels = c("KG",1,2,3,4,5,6,7,8,11)),
                 AssessmentName = case_when(AssessmentName == "Kindergarten ELPAC Summative" ~ "Grade  KG ELPAC Summative",
                                            AssessmentName == "Grade 11 ELA Summative" ~ "Grade11 ELA Summative",
                                            AssessmentName == "Grade 11 Math Summative" ~ "Grade11 Math Summative",
                                            TRUE ~AssessmentName)
          ) %>%
          ggplot( aes( y = GradeLevelWhenAssessed, fill = ScaleScoreAchievementLevel)) +
          geom_bar(color = "black") +
          facet_wrap(vars(SchoolName),
                     # vars(GradeLevelWhenAssessed),
                     #    scales = "free"
          ) +
          geom_text(    stat = "count",
                        aes(label = ..count..), 
                        position = position_stack(vjust = 0.5), size = 2) +
          theme_hc() +
          scale_fill_brewer() + 
          labs(y = "",
               x = "",
               fill = "Achievement Level",
               title = "ELPAC Count of Students at each Achievement Level")
      
  }

  king.city %>% 
      filter(str_detect(DistrictName,"King City")) %>%
  elpac.school()  
  
  ggsave(here("output",paste0("King City", " ELPAC by School ", Sys.Date(),".png")), width = 12, height = 7)
  
  alisal %>% 
      filter(str_detect(DistrictName,"Alisal")) %>%
      elpac.school()  
  
  ggsave(here("output",paste0("Alisal", " ELPAC by School ", Sys.Date(),".png")), width = 12, height = 7)
  
  
  salinas.city %>% 
      filter(str_detect(DistrictName,"Salinas City") ,
             Subject =="ELPAC" ) %>%
      mutate(ScaleScoreAchievementLevel = factor(ScaleScoreAchievementLevel),
             GradeLevelWhenAssessed2 = factor(GradeLevelWhenAssessed, levels = c("KG",1,2,3,4,5,6,7,8,11)),
             AssessmentName = case_when(AssessmentName == "Kindergarten ELPAC Summative" ~ "Grade  KG ELPAC Summative",
                                        AssessmentName == "Grade 11 ELA Summative" ~ "Grade11 ELA Summative",
                                        AssessmentName == "Grade 11 Math Summative" ~ "Grade11 Math Summative",
                                        TRUE ~AssessmentName)
      ) %>%
      ggplot( aes( y = GradeLevelWhenAssessed, fill = ScaleScoreAchievementLevel)) +
      geom_bar(color = "black") +
      # facet_grid(vars(SchoolName),
      #            vars(GradeLevelWhenAssessed),
      #            scales = "free") +
      facet_wrap(vars(SchoolName),
                 # vars(GradeLevelWhenAssessed),
             #    scales = "free"
                 ) +
      geom_text(    stat = "count",
                    aes(label = ..count..),
                    position = position_stack(vjust = 0.5), size = 2) +
      theme_hc() +
      scale_fill_brewer() + 
      labs(y = "",
           x = "",
           fill = "Achievement Level",
           title = "Count of Students at each Achievement Level")
  
  
  ggsave(here("output",paste0("Salinas City", " ELPAC by School ", Sys.Date(),".png")), width = 12, height = 7)
  
  
  gonzales %>% 
      filter(str_detect(DistrictName,"Gon")) %>%
      elpac.school()  
  
  ggsave(here("output",paste0("MPUSD", " ELPAC by School ", Sys.Date(),".png")), width = 12, height = 7)
  

 
 
 #### ELPI ----
 
 
 elpi.levels <- function(df) {
     
      
      df %>% 
     filter(Subject =="ELPAC" ,
             str_detect(DistrictName,"King City")) %>%
     mutate(elpi_level = case_when(
         Subject == "ELPAC" & GradeLevelWhenAssessed == "KG" ~ cut(ScaleScore,
                                            c(1149, 1373, 1397, 1421, 1447, 1473, 1700),
                                            labels=1:6),
         Subject == "ELPAC" & GradeLevelWhenAssessed == "01" ~ cut(ScaleScore,
                                              c(1149, 1410, 1432 , 1454, 1480 , 1506, 1700),
                                              labels=1:6),
         Subject == "ELPAC" & GradeLevelWhenAssessed == "02" ~ cut(ScaleScore,
                                                                   c(1149, 1423, 1446 , 1470, 1500 , 1531, 1700),
                                                                   labels=1:6),
         Subject == "ELPAC" & GradeLevelWhenAssessed == "03" ~ cut(ScaleScore,
                                                                   c(1149, 1447, 1467 , 1487, 1510 , 1534, 1800),
                                                                   labels=1:6),
         Subject == "ELPAC" & GradeLevelWhenAssessed == "04" ~ cut(ScaleScore,
                                                                   c(1149, 1458, 1478 , 1498, 1523  , 1548, 1800),
                                                                   labels=1:6),
         Subject == "ELPAC" & GradeLevelWhenAssessed == "05" ~ cut(ScaleScore,
                                                                   c(1149, 1466, 1489 , 1513, 1536 , 1559, 1800),
                                                                   labels=1:6),
         Subject == "ELPAC" & GradeLevelWhenAssessed == "06" ~ cut(ScaleScore,
                                                                   c(1149, 1474, 1495 , 1516, 1541  , 1566, 1900),
                                                                   labels=1:6),
         Subject == "ELPAC" & GradeLevelWhenAssessed == "07" ~ cut(ScaleScore,
                                                                   c(1149, 1480, 1503 , 1526, 1550  , 1575, 1900),
                                                                   labels=1:6),
         Subject == "ELPAC" & GradeLevelWhenAssessed == "08" ~ cut(ScaleScore,
                                                                   c(1149, 1485,1509  , 1533, 1561  , 1589, 1900),
                                                                   labels=1:6),
         Subject == "ELPAC" & GradeLevelWhenAssessed == "09" ~ cut(ScaleScore,
                                                                   c(1149, 1492, 1518 , 1544, 1574 , 1605, 1950),
                                                                   labels=1:6),
         Subject == "ELPAC" & GradeLevelWhenAssessed == "10" ~ cut(ScaleScore,
                                                                   c(1149, 1492, 1518 , 1544, 1574 , 1605, 1950),
                                                                   labels=1:6),
         Subject == "ELPAC" & GradeLevelWhenAssessed == "11" ~ cut(ScaleScore,
                                                                   c(1149, 1499,  1526  , 1544, 1584 , 1614, 1950),
                                                                   labels=1:6),
         Subject == "ELPAC" & GradeLevelWhenAssessed == "12" ~ cut(ScaleScore,
                                                                   c(1149, 1499, 1526 , 1544, 1584 , 1614, 1950),
                                                                   labels=1:6)

     ))
 }
 
 elpi.change <- function(df.old,df.new, filename) {
     
     temp.elpi.22 <- elpi.levels(df.new) %>%
         select(StudentIdentifier,
                elpi.22 = elpi_level)
     temp.elpi.21 <- elpi.levels(df.old) %>%
         select(StudentIdentifier,
                elpi.21 = elpi_level)
     
     temp.elpi <- full_join(temp.elpi.21,temp.elpi.22) %>%
         na.omit() %>%
         mutate(elpi.change = as.numeric(elpi.22) - as.numeric(elpi.21),
                elpi.pos = case_when( elpi.change > 0 ~ TRUE,
                                      elpi.22 == 6 ~ TRUE,
                                      TRUE ~ FALSE))

     
     
     write_csv(temp.elpi, paste0(filename,".csv"))
     
     mean(temp.elpi$elpi.pos) 
     
 }
 
 
 
 elpi.change(king.city.21, king.city, "King City Test")
 
 
 elpi.change(king.city.21, king.city %>%
                 filter(str_detect(SchoolName,"Chalone")),
             "King City Chalone Test")
 
  
  
  
  
 ###  All of it ------
  
 south.monterey <- clean.df(south.monterey)
  
  overall.graph(south.monterey)
  
  graph.wrap(south.monterey)
  
  graph.grid(south.monterey)
  
  save.overall(south.monterey)
  save.wrap(south.monterey)
  save.grid(south.monterey)
  
  
  passing.perc(south.monterey)
  
  
  student.group.size(south.monterey)
  
  
  # dfs2(gonzales,White) 
   dfs2(south.monterey,EL) 
  # dfs2(gonzales,Asian) 
  # dfs2(gonzales,Filipino) 
  # dfs2(gonzales,BlackOrAfricanAmerican) 
  # dfs2(gonzales,NativeHawaiianOrOtherPacificIslander) 
   dfs2(south.monterey,HispanicOrLatinoEthnicity) 
  
