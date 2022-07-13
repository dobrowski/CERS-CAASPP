

library(tidyverse)
library(janitor)
library(readxl)
library(here)
library(ggthemes)


san.antonio <- read_excel(here("data","SAUSDCAASPP2021-22.xlt" ))
soledad1 <- read_excel(here("data","SUSD CAASPP Results 2022.xlsm"), sheet = 1) %>%
   mutate( GradeLevelWhenAssessed = factor(GradeLevelWhenAssessed, levels = c("KG",1,2,3,4,5,6,7,8,11)))

soledad2 <- read_excel(here("data","SUSD CAASPP Results 2022.xlsm"), sheet = 2)%>%
    mutate( GradeLevelWhenAssessed = factor(GradeLevelWhenAssessed, levels = c("KG",1,2,3,4,5,6,7,8,11)))

soledad3 <- read_excel(here("data","SUSD CAASPP Results 2022.xlsm"), sheet = 3)%>%
    mutate( GradeLevelWhenAssessed = factor(GradeLevelWhenAssessed, levels = c("KG",1,2,3,4,5,6,7,8,11)))

# soledad4 <- read_excel(here("data","SUSD CAASPP Results 2022.xlsm"), sheet = 4)
soledad5 <- read_excel(here("data","SUSD CAASPP Results 2022.xlsm"), sheet = 5) %>%
    mutate( GradeLevelWhenAssessed = factor(GradeLevelWhenAssessed, levels = c("KG",1,2,3,4,5,6,7,8,11)))


soledad <- bind_rows(soledad1,soledad2, soledad3,soledad5)# %>%
  #  filter(!is.na(AssessmentName))



san.lucas <- read_csv(here("data","San_Lucas_District_and_School_Export_File.csv")) %>% 
    filter(AssessmentType == "Summative")







 san.antonio %>% 
     filter(AssessmentType == "Summative") %>%
     group_by(AssessmentName) %>%
     tabyl(Subject,ScaleScoreAchievementLevel)


 
 
 san.antonio <- san.antonio %>% 
     filter(AssessmentType == "Summative")
     
 
 
 

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


overall.graph(san.lucas)


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

graph.wrap(san.antonio)

graph.grid(san.antonio)

graph.wrap(soledad)

ggsave(here("output","Soledad wrap.png"), width = 12, height = 7)

graph.grid(soledad)


graph.wrap(san.lucas)


passing.perc <- function(df) {
    
df %>%
    group_by(Subject) %>%
    mutate(Above = ifelse(ScaleScoreAchievementLevel >= 3, TRUE, FALSE),
           perc = mean(Above)) %>%
    select(Subject, perc) %>%
    distinct()
}


passing.perc(soledad)
