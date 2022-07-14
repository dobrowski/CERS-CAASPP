

library(tidyverse)
library(janitor)
library(readxl)
library(here)
library(ggthemes)
library(googlesheets4)


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


leas <- c("santa.rita", "san.lucas", "alisal", "san.antonio", "soledad")

leas <- list(santa.rita, san.lucas, alisal, san.antonio, soledad)


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
         mutate(EL = ifelse(EnglishLanguageAcquisitionStatus == "EL", "Yes", NA))
     
 }
 
 
 san.lucas <- clean.df(san.lucas)
 san.antonio <- clean.df(san.antonio)
 alisal <- clean.df(alisal)
 santa.rita <- clean.df(santa.rita)
 
 
 
 
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


overall.graph(san.antonio)


overall.graph(soledad)

overall.graph(alisal)


overall.graph(san.lucas)


overall.graph(santa.rita)


graph.wrap <- function(df) {
   
df %>% 
    mutate(ScaleScoreAchievementLevel = factor(ScaleScoreAchievementLevel),
           GradeLevelWhenAssessed = factor(GradeLevelWhenAssessed, levels = c("KG",1,2,3,4,5,6,7,8,11)),
           AssessmentName = case_when(AssessmentName == "Kindergarten ELPAC Summative" ~ "Grade  KG ELPAC Summative",
                                      AssessmentName == "Grade 11 ELA Summative" ~ "Grade11 ELA Summative",
                                      AssessmentName == "Grade 11 Math Summative" ~ "Grade11 Math Summative",
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
    
    graph.wrap(df)
    
    ggsave(here("output",paste0(df[1,2], " grid ", Sys.Date(),".png")), width = 12, height = 7)
}


graph.wrap(san.antonio)

graph.grid(san.lucas)


save.wrap(alisal)
save.grid(santa.rita)

map(leas, save.wrap)
map(leas, save.grid)


###  Passing Percentage -----


passing.perc <- function(df) {
    
df %>%
    group_by(Subject) %>%
    mutate(Above = ifelse(ScaleScoreAchievementLevel >= 3, TRUE, FALSE),
           perc = mean(Above)) %>%
    select(Subject, perc) %>%
    distinct()
}


passing.perc(santa.rita)

passing.perc(san.lucas)

passing.perc(alisal)

alisal %>%
    filter(EnglishLanguageAcquisitionStatus == "RFEP") %>%
    passing.perc()


passing.perc(san.antonio)


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
 
 student.group.size(alisal)
 
 student.group.size(santa.rita)
 
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
  
  
