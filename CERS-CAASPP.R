

library(tidyverse)
library(janitor)
library(readxl)
library(here)
library(ggthemes)
library(googlesheets4)

options(scipen=999)


# 2022 Google Sheet 
# sheet <- "https://docs.google.com/spreadsheets/d/1iS2Sd37hU7LYzakI2fbiotnzYn60cVp0d0pMB9k6zXk/edit#gid=0"

# 2023 Google Sheet
sheet <- "https://docs.google.com/spreadsheets/d/1E7x2W-bWkZGenZTPVmlyGSl0LQPfrHc2s8ZLILKUOGw/edit#gid=0"




### Load files -----

san.lucas.23 <- read_csv(here("data", "san lucas" ,"San_Lucas_USD2023.csv"))


lagunita.23 <- read_csv(here("data", "lagunita" ,"District_and_School_Export_File LAGUNITA 2023.csv"))
# lagunita <- read_csv(here("data","LAgunita.csv"))


soledad <- read_csv(here("data","Soledad_District_and_School_Export_File.csv"))

south.monterey <- read_csv(here("data","South_Monterey_county_joint_union_high_school.csv"))

gonzales <- read_csv(here("data","Gonzales District_and_School_Export_File.csv"))

mpusd <- read_csv(here("data","MPUSD_CAASPP_21-22.csv"))


san.ardo.23 <- read_csv(here("data", "san ardo" ,"sanardoCAASPP_2022_2023_report.csv"))
san.ardo.22 <- read_csv(here("data","San_Ardo_All.csv")) 



king.city.23 <- read_delim(here("data","king city","KCUSD_22.23.csv"))
king.city.22 <- read_csv(here("data","KCUSD.csv")) 
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




san.lucas.22 <- read_csv(here("data","San_Lucas_District_and_School_Export_File.csv")) 

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
 # cleans the dataframe from CERS
 
 clean.df <- function(df) {
     
     df %>% 
         filter(AssessmentType == "Summative",
                Completeness == "Complete") %>%
  #       mutate(GradeLevelWhenAssessed = as.character(GradeLevelWhenAssessed)) %>%
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
 
 mpusd <- clean.df(mpusd)
 
 lagunita.23 <- lagunita.23 %>%
     clean.df()
 
 
 lagunita <- lagunita %>%
     clean.df() %>%
     filter(LanguageCode != "ger",
            Subject != "CAST") %>%
     mutate(Race = case_when(White == "Yes" ~ "White",
                             HispanicOrLatinoEthnicity == "Yes" ~ "Latino",
                             #     NativeHawaiianOrOtherPacificIslander == "Yes" ~ "Pacific Islander",
                             TRUE ~ "Unknown"))
 
 
### Graphs --------- 

 
 # Graphs all assessment results
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
             title = paste0(df[1,2],"\nCount of Students at each Achievement Level"))
}


 san.lucas %>%
     filter(SWD == "Yes") %>%
     graph.wrap()

# Graphs assessment results by grade
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
         title = paste0(df[1,2],"\nCount of Students at each Achievement Level"))

}

# Makes a grid so the grades are all lined up across assessments
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
             title = paste0(df[1,2],"\nCount of Students at each Achievement Level"))
    
}

# Saves the wrap graph
save.wrap <- function(df) {
    
    print(df[1,2])
    
    graph.wrap(df)
    
    ggsave(here("output",paste0(df[1,2], " wrap ", Sys.Date(),".png")), width = 12, height = 7)
}

# Saves the grid graph
save.grid <- function(df) {
    
    print(df[1,2])
    
    graph.grid(df)
    
    ggsave(here("output",paste0(df[1,2], " grid ", Sys.Date(),".png")), width = 12, height = 7)
}

# Saves the overall graph
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


# Run for multiple LEAs 


leas <- c("santa.rita", "san.lucas", "alisal", "san.antonio", "soledad")

leas <- list(santa.rita, san.lucas, alisal, san.antonio, soledad, pg, salinas.city, king.city, mpusd)


map(leas, save.wrap)
map(leas, save.grid)
map(leas, save.overall)


###  Passing Percentage -----

# Calculates the percentage of students meeting or exceeding standards by assessment

passing.perc <- function(df) {
    
    # To save dataframe name and put in final table
    ddff <-     deparse(substitute(df)) 
    
hold <- df %>%
    group_by(Subject) %>%
    mutate(Above = ifelse(ScaleScoreAchievementLevel >= 3, TRUE, FALSE),
           perc = mean(Above)*100) %>%
    select(Subject, perc) %>%
    distinct()%>%
    mutate(district = ddff)

# Posts to google sheet
sheet_append(ss = sheet,
             sheet = "Percent Met or Exceeded",
             data = hold )

hold

}


passing.perc(santa.rita)

san.lucas %>%
    filter(SWD == "Yes") %>%
    passing.perc()
    


### Distance from Standard ------

# Caluclates distance from standard for All students 

dfs <- function(df) {
    
    # Saves dataframe name
    ddff <-     deparse(substitute(df)) 
    
   holder <-  df %>% 
        filter(Subject %in% c("ELA","Math")) %>%
        mutate(ScaleScoreAchievementLevel = factor(ScaleScoreAchievementLevel),
        ) %>%
        left_join(reference2) %>%
        group_by(Subject) %>%
        mutate(dist.standard = ScaleScore - MeetStandard,
               mean.dist.stand = mean(dist.standard))  %>%
        select(Subject,mean.dist.stand) %>%
        distinct() %>%
       mutate(district = ddff)
    
   # Posts to the google sheet
    sheet_append(ss = sheet,
                 sheet = "Distance from Standard",
                 data = holder )
    
    holder
    
}


dfs(santa.rita)

 
 ### Student Group Size ------

# Calculates with student groups are large enough to appear on dashboard
 
 student.group.size <- function(df) {
     
 df %>%
#         mutate(EL = ifelse(EnglishLanguageAcquisitionStatus == "EL", "Yes", NA)) %>%
         filter(Subject %in% c("ELA", "Math")) %>%
         group_by(Subject) %>%
         summarise( across(c(HispanicOrLatinoEthnicity:Filipino,EL), ~  sum(!is.na(.)))) %>%
         pivot_longer(cols = c(HispanicOrLatinoEthnicity:Filipino,EL)) %>%
     filter(value >= 30)
 }

 
 student.group.size(san.lucas) 
 
 
 ### Records ----
 
 
 my.list <- student.group.size(santa.rita) %>%
     select(name) %>%
     distinct() %>%
     as.vector()
 
for (i in my.list) {
    
    ii <-     noquote(i) 
    
    print(ii)
    
    
}
 
 # Calculates Distance from Standard by Student Group listed 
 
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
                 sheet = "Distance from Standard Group",
                data = holder )
    holder
     
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
  
  
  soledad %>% 
      filter(str_detect(DistrictName,"Soledad")) %>%
      elpac.school()  
  
  ggsave(here("output",paste0("Soledad", " ELPAC by School ", Sys.Date(),".png")), width = 12, height = 7)
  

 
 
 #### ELPI ----
 
 # Calculates elpi levels 
 elpi.levels <- function(df,dist) {
     
      
      df %>% 
     filter(Subject =="ELPAC" ,
             str_detect(DistrictName,dist)) %>%
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
                                                                   c(1149, 1499,  1526  , 1554, 1584 , 1614, 1950),
                                                                   labels=1:6),
         Subject == "ELPAC" & GradeLevelWhenAssessed == "12" ~ cut(ScaleScore,
                                                                   c(1149, 1499, 1526 , 1554, 1584 , 1614, 1950),
                                                                   labels=1:6)

     ))
 }
 
  # Compares a district across years to calculate estimated ELPI indicator level
  
 elpi.change <- function(dist, df.old, df.new, filename) {
     
     
     
     ddff <-     deparse(substitute(df.new)) 
     
     
     temp.elpi.new <- elpi.levels(df.new, dist) %>%
         select(StudentIdentifier,
                elpac.new = elpi_level)
     temp.elpi.old <- elpi.levels(df.old, dist) %>%
         select(StudentIdentifier,
                elpac.old = elpi_level)
     
     temp.elpi <- full_join(temp.elpi.old,temp.elpi.new) %>%
         na.omit() %>%
         mutate(elpi.change = as.numeric(elpac.new) - as.numeric(elpac.old),
                elpi.pos = case_when( elpi.change > 0 ~ TRUE,
                                      elpac.new == 6 ~ TRUE,
                                      TRUE ~ FALSE))

     
     # Saves list with students to see which are included in progress calculation
     write_csv(temp.elpi, here("elpi" ,paste0(filename,".csv")))
     
     elpi.perc <- mean(temp.elpi$elpi.pos) 
     
      holder <- tibble_row(elpi.perc, ddff)
     
     # Saves the overall rate to the google sheet
     sheet_append(ss = sheet,
                  sheet = "ELPI",
                  data = holder )
     
     elpi.perc

     
 }
 
 
 
 elpi.change("King City", king.city.22, king.city.23, "King City ELPI 2023")
 
 
 elpi.change("San Lucas", san.lucas.22, san.lucas.23, "San Lucas ELPI 2023")
 
 elpi.change("San Ardo", san.ardo.22, san.ardo.23, "San Ardo ELPI 2023")
 
 
 
  
 # alisal.elpac.21  <-  read_excel(here("data","ELPAC_for_ELPI_-_20_to_21_(Alisal_USD)_2020-21.xlsx") ) 
 # alisal.elpac <- read_excel(here("data","ELPAC_for_ELPI_-_21_to_22_(Alisal_USD)_2021-22.xlsx") ) 
 # 
 # 
 # alisal.elpac.21  <-  read_csv(here("data","ELPAC_for_ELPI_-_20_to_21_(Alisal_USD)_2020-21.csv") ) 
 # alisal.elpac <- read_csv(here("data","ELPAC_for_ELPI_-_21_to_22_(Alisal_USD)_2021-22.csv") ) 
 # 
 # alisal.elpac <- clean.df(alisal.elpac)
 # alisal.elpac.21 <- clean.df(alisal.elpac.21)
 # 
 # 
 # 
 # elpi.change("Alisal", alisal.elpac.21, alisal.elpac, "Alisal ELPI")
 
#  
# scesd.elpac.21  <-  read_csv(here("data","SCESD_Summative_ELPAC_2021.csv") ) 
# scesd.elpac <- read_csv(here("data","SCESD_Summative_ELPAC_2022.csv") ) 
#  
# scesd.elpac <- clean.df(scesd.elpac)
#  scesd.elpac.21 <- clean.df(scesd.elpac.21)
#  
#  
#  elpi.change("Salinas City", scesd.elpac.21, scesd.elpac, "SCESD ELPI")
#  
  

 
### ELPI by School calculations ----
 
 
 school.list <- unique(king.city.23$SchoolName)
 
 for (i in school.list) {
     elpi.change("King City",
                 king.city.22,
                 king.city.23 %>%
                     filter(str_detect(SchoolName,i)),
                 i)
     
 }
 
 
 
 
 
 
 
 
 
 
### Minor mod ----
 
 lagunita.23.k6 <- lagunita.23 %>%
     filter(GradeLevelWhenAssessed %in% c("03","04","05","06"))
 
 
 
 
 ###  All of it ------
  
 san.lucas.23 <- clean.df(san.lucas.23)
  
  overall.graph(san.ardo.23)
  
  graph.wrap(san.ardo.23)
  
  graph.grid(san.ardo.23)
  
  save.overall(san.ardo.23)
  save.wrap(san.ardo.23)
  save.grid(san.ardo.23)
  
  
  passing.perc(san.ardo.23)
  
  
  dfs(san.ardo.23)
  
  student.group.size(san.ardo.23)
  
  
   dfs2(king.city.23,White) 
   dfs2(san.ardo.23,EL) 
  # dfs2(gonzales,Asian) 
  # dfs2(gonzales,Filipino) 
  # dfs2(gonzales,BlackOrAfricanAmerican) 
  # dfs2(gonzales,NativeHawaiianOrOtherPacificIslander) 
   dfs2(san.ardo.23,HispanicOrLatinoEthnicity) 
  

   
 ### By school in a district -------  
   
   
 sol.nest <-   soledad %>% 
     group_by(SchoolName) %>%
       nest() %>%
     walk(graph.wrap(data) )
 
 
soledad2 <-  soledad %>%
     filter(str_detect(DistrictName,"Soledad")) 

soledad2 %>%
     split(soledad2$SchoolName) %>%
     map(~graph.wrap2(.))
 
 
 
 
   # Runs the graph.wrap for every school in a district

   graph.wrap2 <- function(df) {
       
       namer <- unique(df$SchoolName)
       
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
                title = paste0(namer," Count of Students at each Achievement Level"))
       
       
       ggsave(here("output",paste0(df[1,2]," ",namer ," wrap ", Sys.Date(),".png")), width = 12, height = 7)
       
       
       
   }
   

   
   mpusd2 <-  mpusd %>%
       filter(str_detect(DistrictName,"Penin")) 
   
   mpusd2 %>%
       split(mpusd2$SchoolName) %>%
       map(~graph.wrap2(.))
   
### By demo split -------
   
   
   graph.demo <- function(df, demo) {
       
       demodemo <-     deparse(substitute(demo))
       
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
           ggplot( aes( y = Subject, fill = ScaleScoreAchievementLevel)) +
           geom_bar(color = "black") +
           facet_wrap(vars({{demo}}),
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
                title = paste0("Count of Students at each Achievement Level by ", demodemo))
       
   }

   
   graph.demo(san.ardo.23, LanguageCode)
   
  ggsave(here("output",paste0("Lagunita Language ", Sys.Date(),".png")), width = 12, height = 7)
 
  # lagunita %>%
  #     filter(LanguageCode != "ger") %>%
  # graph.demo( LanguageCode)
  # 
  # ggsave(here("output",paste0("Lagunita Language2 ", Sys.Date(),".png")), width = 12, height = 7)
  
  
lagunita2 <- lagunita.23 %>%
      mutate(Race = case_when(White == "Yes" ~ "White",
                              HispanicOrLatinoEthnicity == "Yes" ~ "Latino",
                         #     NativeHawaiianOrOtherPacificIslander == "Yes" ~ "Pacific Islander",
                              TRUE ~ "Unknown"))
  

graph.demo(lagunita2, Race)

ggsave(here("output",paste0("Lagunita Race ", Sys.Date(),".png")), width = 12, height = 7)
