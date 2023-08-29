

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


alisal.23 <- read_csv(here("data","alisal","ELPI_2023_Estimate.csv"))
alisal.22 <- read_csv(here("data","Alisal USD - District_and_School_Export_File.7.13.2022.csv"))
alisal.23.demo <- read_xlsx(here("data","alisal", "Alisal CAASPP_LEA_Student_Demographics_Snapshot_Report_27659610000000_230724131757.xlsx"),
                            skip = 1)


gonzales.23 <- read_csv(here("data","gonz","Gonzales2023.csv"))
gonzales.22 <- read_csv(here("data","Gonzales District_and_School_Export_File.csv"))


king.city.23 <- read_delim(here("data","king city","KCUSD_22.23.csv"))
king.city.22 <- read_csv(here("data","KCUSD.csv")) 
king.city.21 <- read_csv(here("data","KCUSD_20.21.csv")) 
king.city.23.demo <- read_xlsx(here("data","king city", "27660500000000_CAASPP_Student_Score_Data_File_TestedStudentScoreData_2023.xlsx"),
                          skip = 1)
king.city.23.demo <- king.city.23.demo %>%
    select(`Statewide Student Identifier (SSID)` = SSID,
           Gender,
           `CALPADS Special Education`= CALPADSIDEAIndicator,
           `English Learner (EL)` = ELStatus,
           `EL Exit Date` = RFEPDate,
           `Homeless Status` = HomelessStatus,
           `CALPADS Socioeconomically Disadvantage (SED) Status` = EconomicDisadvantageStatus
    ) %>%
    distinct()

lagunita.23 <- read_csv(here("data", "lagunita" ,"District_and_School_Export_File LAGUNITA 2023.csv"))
# lagunita <- read_csv(here("data","LAgunita.csv"))

mpusd <- read_csv(here("data","MPUSD_CAASPP_21-22.csv"))


nmc.23 <- read_csv(here("data", "nmc" ,"North_Monterey_County_USD 22-23 CERS.xls - North_Monterey_County_USD.csv"))
nmc.22 <- read_csv(here("data", "nmc" ,"North_Monterey_County_USD_2021-2022.xlsx - North_Monterey_County_USD_2021-.csv"))
nmc.23.demo <- read_csv(here("data","nmc", "27738250000000_CAASPP_Student_Score_Data_File_EnrolledStudentScoreData_2023.csv"),
                                 skip = 1)
nmc.23.demo <- nmc.23.demo %>%
    select(`Statewide Student Identifier (SSID)` = SSID,
           Gender,
           `CALPADS Special Education`= CALPADSIDEAIndicator,
           `English Learner (EL)` = ELStatus,
           `EL Exit Date` = RFEPDate,
           `Homeless Status` = HomelessStatus,
           `CALPADS Socioeconomically Disadvantage (SED) Status` = EconomicDisadvantageStatus
           ) %>%
    distinct()

pg.22 <- read_csv(here("data","PGUSD_District_and_School_Export_File_071522.csv"))
pg.23 <- read_csv(here("data", "pg","District_and_School_Export_File (1) (1).csv"))
pg.23.demo <- read_xlsx(here("data","pg", "CAASPP_LEA_Student_Demographics_Snapshot_Report_27661340000000_230724170206.xlsx"),
                        skip = 1)


salinas.city.23 <- read_csv(here("data","salinas city", "SCESD_CERS_7-19-23.csv"))
salinas.city.22 <- read_csv(here("data","SCESD_District_and_School_Export_File.csv"))
salinas.city.23.demo <- read_csv(here("data","salinas city", "CAASPP_LEA_Student_Demographics_Snapshot_Report_27661420000000_230720185006.xlsx - Data.csv"),
                                 skip = 1)


salinas.union.23.demo <- read_csv(here("data","salinas union", "27661590000000_CAASPP_Student_Score_Data_File_TestedStudentScoreData_2023_08142023.csv") )


salinas.union.23.demo <- salinas.union.23.demo %>%
    mutate(Subject = case_match(RecordType,
                                "01" ~ "ELA",
                                "02" ~ "Math",
                                "06" ~ "CAST"),
           ScaleScoreAchievementLevel = AchievementLevels,
           ScaleScore,
           GradeLevelWhenAssessed = GradeAssessed,
           AssessmentName = str_c("Grade ",GradeLevelWhenAssessed," ",Subject),
           ) %>%
    select(SSID,
           CALPADSDistrictCode:CALPADSSchoolName,
           Subject,
           ScaleScoreAchievementLevel,
           ScaleScore,
           GradeLevelWhenAssessed,
           AssessmentName,
           
           CALPADSIDEAIndicator:TwoorMoreRaces) %>%
    filter(!is.na(ScaleScore)) %>%
    rename(StudentIdentifier = SSID,
           HispanicOrLatinoEthnicity = HispanicorLatino,
           EL2 = ELStatus,
           ELexit = RFEPDate,
           SWD = CALPADSIDEAIndicator,
           SED = EconomicDisadvantageStatus,
           HOM = HomelessStatus,
           ) %>%
    mutate(ELdash = case_when(EL2 == "Yes" ~ "Yes",
                              ymd(ELexit) >= ymd("2019-06-15") ~ "Yes",
                              TRUE ~ "No"),
           StudentIdentifier = as.numeric(StudentIdentifier)
    ) %>%
    select(-EL2,-ELexit) %>%
    select(-ELEntryDate:-FosterStatus) %>%
    relocate(HispanicOrLatinoEthnicity, .before = SWD) %>%
    mutate(across(HispanicOrLatinoEthnicity:ELdash, ~na_if(., "No")))


# salinas.union.23.demo <- salinas.union.23.demo %>%
#     select(`Statewide Student Identifier (SSID)` = SSID,
#            Gender,
#            `CALPADS Special Education`= CALPADSIDEAIndicator,
#            `English Learner (EL)` = ELStatus,
#            `EL Exit Date` = RFEPDate,
#            `Homeless Status` = HomelessStatus,
#            `CALPADS Socioeconomically Disadvantage (SED) Status` = EconomicDisadvantageStatus
#     ) %>%
#     distinct()


san.antonio <- read_excel(here("data","SAUSDCAASPP2021-22.xlt" ))


san.ardo.23 <- read_csv(here("data", "san ardo" ,"sanardoCAASPP_2022_2023_report.csv"))
san.ardo.22 <- read_csv(here("data","San_Ardo_All.csv")) 
san.ardo.23.demo <- read_xlsx(here("data","san ardo", "CAASPP_LEA_Student_Demographics_Snapshot_Report_27661750000000_230726183447.xlsx"),
                        skip = 1)



san.lucas.22 <- read_csv(here("data","San_Lucas_District_and_School_Export_File.csv")) 
san.lucas.23 <- read_csv(here("data", "san lucas" ,"San_Lucas_USD2023.csv"))


santa.rita <- read_csv(here("data","Santa_Rita_District_and_School_Export_File CAASP assessments 2021-2022.csv"))


soledad.22 <- read_csv(here("data","Soledad_District_and_School_Export_File.csv"))
soledad.23 <- read_csv(here("data","soledad","Soledad_Unified.csv"))
soledad.23.demo <- read_xlsx(here("data","soledad", "CAASPP_LEA_Student_Demographics_Snapshot_Report_27754400000000_230724172438.xlsx"),
                             skip = 1)


south.monterey <- read_csv(here("data","South_Monterey_county_joint_union_high_school.csv"))


spreckels.23 <- read_csv(here("data", "spreckels" ,"SBAC 202223.csv"))
spreckels.22 <- read_csv(here("data", "spreckels" ,"SBAC 202122.csv"))
spreckels.23.demo <- read_xlsx(here("data","spreckels", "CAASPP_LEA_Student_Demographics_Snapshot_Report_27662250000000_230725165207.xlsx"),
                               skip = 1)


wash.23 <- read_csv(here("data", "washington" ,"WUS CAASPP 2022-23.csv"))
wash.22 <- read_csv(here("data", "washington" ,"District_and_School_Export_File2022.csv"))
wash.23.demo <- read_xlsx(here("data","washington", "Copy of 27662330000000_CAASPP_Student_Score_Data_File_TestedStudentScoreData_2023.xlsx"),
                        skip = 1)
wash.23.demo <- wash.23.demo %>%
    select(`Statewide Student Identifier (SSID)` = SSID,
           Gender,
           `CALPADS Special Education`= CALPADSIDEAIndicator,
           `English Learner (EL)` = ELStatus,
           `EL Exit Date` = RFEPDate,
           `Homeless Status` = HomelessStatus,
           `CALPADS Socioeconomically Disadvantage (SED) Status` = EconomicDisadvantageStatus
    ) %>%
    distinct()



# soledad1 <- read_excel(here("data","SUSD CAASPP Results 2022.xlsm"), sheet = 1) %>%
#    mutate( GradeLevelWhenAssessed = factor(GradeLevelWhenAssessed, levels = c("KG",1,2,3,4,5,6,7,8,9,10,11,12)))
# 
# soledad2 <- read_excel(here("data","SUSD CAASPP Results 2022.xlsm"), sheet = 2)%>%
#     mutate( GradeLevelWhenAssessed = factor(GradeLevelWhenAssessed, levels = c("KG",1,2,3,4,5,6,7,8,9,10,11,12)))
# 
# soledad3 <- read_excel(here("data","SUSD CAASPP Results 2022.xlsm"), sheet = 3)%>%
#     mutate( GradeLevelWhenAssessed = factor(GradeLevelWhenAssessed, levels = c("KG",1,2,3,4,5,6,7,8,9,10,11,12)))
# 
# # soledad4 <- read_excel(here("data","SUSD CAASPP Results 2022.xlsm"), sheet = 4)
# soledad5 <- read_excel(here("data","SUSD CAASPP Results 2022.xlsm"), sheet = 5) %>%
#     mutate( GradeLevelWhenAssessed = factor(GradeLevelWhenAssessed, levels = c("KG",1,2,3,4,5,6,7,8,9,10,11,12)))
# 
# 
# soledad <- bind_rows(soledad1,soledad2, soledad3,soledad5) %>%
#     mutate( GradeLevelWhenAssessed = recode_factor( GradeLevelWhenAssessed,
#                                                     "KG"= "KG",
#                                                     "1" = "01",
#                                                     "2" = "02",
#                                                     "3" = "03",
#                                                     "4" = "04",
#                                                     "5" = "05",
#                                                     "6" = "06",
#                                                     "7" = "07",
#                                                     "8" = "08",
#                                                     "9" = "09")) 



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
        guides(fill = guide_legend(reverse = TRUE)) + 
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
           AssessmentName = case_when(AssessmentName == "Kindergarten Summative ELPAC" ~ "Grade  KG Summative ELPAC",
                                      AssessmentName == "Grade 11 ELA Summative" ~ "Grade11 ELA Summative",
                                      AssessmentName == "Grade 11 Math Summative" ~ "Grade11 Math Summative",
                                      AssessmentName == "Grade 10 Summative ELPAC" ~ "Grade10 Summative ELPAC",
                                      AssessmentName == "Grade 11 Summative ELPAC" ~ "Grade11 Summative ELPAC",
                                      AssessmentName == "Grade 12 Summative ELPAC" ~ "Grade12 Summative ELPAC",
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
        guides(fill = guide_legend(reverse = TRUE)) + 
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
               AssessmentName = case_when(AssessmentName == "Kindergarten Summative ELPAC" ~ "Grade  KG Summative ELPAC",
                                          AssessmentName == "Grade 11 ELA Summative" ~ "Grade11 ELA Summative",
                                          AssessmentName == "Grade 11 Math Summative" ~ "Grade11 Math Summative",
                                          AssessmentName == "Grade 10 Summative ELPAC" ~ "Grade10 Summative ELPAC",
                                          AssessmentName == "Grade 11 Summative ELPAC" ~ "Grade11 Summative ELPAC",
                                          AssessmentName == "Grade 12 Summative ELPAC" ~ "Grade12 Summative ELPAC",
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
        guides(fill = guide_legend(reverse = TRUE)) + 
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


graph.wrap(salinas.union.23.demo)

graph.grid(salinas.union.23.demo)

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

salinas.city.23 %>%
    filter(SWD == "Yes") %>%
    passing.perc()
    


temp <- salinas.union.23.demo %>%
    select(Subject, ScaleScoreAchievementLevel) %>%
    group_by(Subject) %>%
    mutate(Above = ifelse(ScaleScoreAchievementLevel >= 3, TRUE, FALSE),
           perc = mean(Above)*100) # %>%
    select(Subject, perc) %>%
    distinct()






temp <- soledad.23 %>%
    group_by(Subject, GradeLevelWhenAssessed) %>%
    transmute(Level1perc = 100*mean(ifelse(ScaleScoreAchievementLevel == 1, TRUE, FALSE)),
              Level2perc = 100*mean(ifelse(ScaleScoreAchievementLevel == 2, TRUE, FALSE)),
              Level3perc = 100*mean(ifelse(ScaleScoreAchievementLevel == 3, TRUE, FALSE)),
              Level4perc = 100*mean(ifelse(ScaleScoreAchievementLevel == 4, TRUE, FALSE)),
              MeetOrExceedperc = 100*mean(ifelse(ScaleScoreAchievementLevel >= 3, TRUE, FALSE)),
  #            Above = ifelse(ScaleScoreAchievementLevel >= 3, TRUE, FALSE),
   #        perc = mean(Above)*100
           ) %>%
#    select(Subject, GradeLevelWhenAssessed, perc) %>%
    distinct()

temp

write_csv(temp, "Soledad Percent Met by Grade Level.csv")



### Distance from Standard ------

# Calculates distance from standard for All students 

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


dfs(salinas.union.23.demo)

 
 ### Student Group Size ------

# Calculates with student groups are large enough to appear on dashboard
 
 student.group.size <- function(df) {
     
 df %>%
#         mutate(EL = ifelse(EnglishLanguageAcquisitionStatus == "EL", "Yes", NA)) %>%
         filter(Subject %in% c("ELA", "Math")) %>%
         group_by(Subject) %>%
         # summarise( across(c(HispanicOrLatinoEthnicity:Filipino,EL), ~  sum(!is.na(.)))) %>%
         # pivot_longer(cols = c(HispanicOrLatinoEthnicity:Filipino,EL)) %>%
         summarise( across(c(HispanicOrLatinoEthnicity:ELdash), ~  sum(!is.na(.)))) %>%
         pivot_longer(cols = c(HispanicOrLatinoEthnicity:ELdash)) %>%
     filter(value >= 30 | name == "HOM" & value >= 15)
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
 
  
  ### Student Growth in Year ----
  
  student.growth <- function(df.old, df.new, filename) {
      
      ddff <-     deparse(substitute(df.new)) 
      
      
      temp.new <- df.new %>%
          filter(Subject %in% c("ELA","Math"),
                 GradeLevelWhenAssessed != "11" ,
                 GradeLevelWhenAssessed != "03") %>% 
          select(StudentIdentifier, SchoolName, GradeLevelWhenAssessed, FirstName, LastOrSurname, Subject, ScaleScore.new = ScaleScore, HispanicOrLatinoEthnicity:ELdash) 
     
           temp.old <- df.old %>%
          filter(Subject %in% c("ELA","Math")) %>% 
          select(StudentIdentifier, Subject, ScaleScore.old = ScaleScore) 
                                  
      
      temp.join <- left_join(temp.new,temp.old) %>%
          # na.omit() %>%
           mutate(ScaleScore.change = as.numeric(ScaleScore.new) - as.numeric(ScaleScore.old),
          )
      
      write_csv(temp.join, here(paste0(filename,".csv")))
      
      temp.join

      
  }
  
  
temp <-   student.growth(pg.22,pg.23, "Pacific Grover 2023 Student Scale Score Change")
  
student.growth(wash.22,wash.23, "Washington 2023 Student Scale Score Change")
  
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
 
 
 school.list <- unique(alisal.23$SchoolName)
 
 for (i in school.list) {
     elpi.change("Alisal",
                 alisal.22,
                 alisal.23 %>%
                     filter(str_detect(SchoolName,i)),
                 i)
     
 }
 
 
 
 
 
 
 
 
 
 
### Minor mod ----
 
 lagunita.23.k6 <- lagunita.23 %>%
     filter(GradeLevelWhenAssessed %in% c("03","04","05","06"))
 
 
 ### Add demo -----
 
 
 add.demo <- function(df, df.demo) {
     
temp <- df.demo %>%
     select(StudentIdentifier = `Statewide Student Identifier (SSID)`,
            Gender,
            EL2 = `English Learner (EL)`,
            ELexit = `EL Exit Date`,
            SWD = `CALPADS Special Education`,
            SED = `CALPADS Socioeconomically Disadvantage (SED) Status`,
            HOM = `Homeless Status`) %>%
     mutate(ELdash = case_when(EL2 == "Yes" ~ "Yes",
                               ymd(ELexit) >= ymd("2019-06-15") ~ "Yes",
                               TRUE ~ "No"),
            StudentIdentifier = as.numeric(StudentIdentifier)
            ) %>%
                select(-EL2,-ELexit) %>%
    mutate(across(Gender:ELdash, ~na_if(., "No")))
 
 
 df %>%
     left_join(temp)
 
 } 
 
 
 
 spreckels.22 <- clean.df(spreckels.22) 
 spreckels.23 <- clean.df(spreckels.23) 
 spreckels.23 <-  add.demo(spreckels.23, spreckels.23.demo)


ss23demo<- soledad.23.demo %>%
    mutate(`Statewide Student Identifier (SSID)` = SSID)

temp <-  add.demo(soledad, ss23demo)
 
 
 ###  All of it ------
  
wash.22 <- wash.22 %>%
    filter(str_detect( DistrictName, "Washington"))


wash.22 <- clean.df(wash.22) 
king.city.23 <- clean.df(king.city.23) 
king.city.23 <-  add.demo(king.city.23, king.city.23.demo)


  overall.graph(salinas.union.23.demo)
  
  graph.wrap(salinas.union.23.demo)
  
  graph.grid(king.city.23)
  
  save.overall(king.city.23)
  save.wrap(king.city.23)
  save.grid(king.city.23)
  
  
  elpi.change("Washington", wash.22, wash.23, "Washington ELPI 2023")
  
  passing.perc(salinas.union.23.demo)
  
  
  dfs(salinas.union.23.demo)
  
  student.group.size(salinas.union.23.demo) %>%
      print(n=26)
  
  
   dfs2(salinas.union.23.demo,White) 
   dfs2(salinas.union.23.demo,ELdash) 
   dfs2(salinas.union.23.demo,Asian) 
       dfs2(salinas.union.23.demo,Filipino) 
       dfs2(salinas.union.23.demo,TwoorMoreRaces) 
         # dfs2(gonzales,BlackOrAfricanAmerican) 
  # dfs2(gonzales,NativeHawaiianOrOtherPacificIslander) 
   dfs2(salinas.union.23.demo,HispanicOrLatinoEthnicity) 
   dfs2(salinas.union.23.demo,SWD) 
   dfs2(salinas.union.23.demo,SED) 
   dfs2(salinas.union.23.demo,HOM) 
   

   
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
                  AssessmentName = case_when(AssessmentName == "Kindergarten Summative ELPAC" ~ "Grade  KG Summative ELPAC",
                                             AssessmentName == "Grade 11 ELA Summative" ~ "Grade11 ELA Summative",
                                             AssessmentName == "Grade 11 Math Summative" ~ "Grade11 Math Summative",
                                             AssessmentName == "Grade 10 Summative ELPAC" ~ "Grade10 Summative ELPAC",
                                             AssessmentName == "Grade 11 Summative ELPAC" ~ "Grade11 Summative ELPAC",
                                             AssessmentName == "Grade 12 Summative ELPAC" ~ "Grade12 Summative ELPAC",
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
           guides(fill = guide_legend(reverse = TRUE)) + 
           labs(y = "",
                x = "",
                fill = "Achievement Level",
                title = paste0(namer," Count of Students at each Achievement Level"))
       
       
       ggsave(here("output",paste0(df[1,2]," ",namer ," wrap ", Sys.Date(),".png")), width = 12, height = 7)
       
       
       
   }
   
   dfs2.school <- function(df,students) {
       
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
                  mean.dist.stand = mean(dist.standard),
                  count = n())  %>%
           select(Subject,mean.dist.stand, count) %>%
           distinct() %>%
           mutate(district = ddff,
                  students = studentsss
           )
       
       # sheet_append(ss = sheet,
       #              sheet = "Distance from Standard Group",
       #              data = holder )
       holder
       
   }
   
   add.school.dfs <- function(df) {
       
       namer <- unique(df$CALPADSSchoolName)
       coder <- unique(df$CALPADSSchoolCode)
       
   waiting.room <- dfs2.school(df %>% mutate(All = "Yes"),All) %>%
       bind_rows(  
           dfs2.school(df,White) ) %>%
       bind_rows(  dfs2.school(df,ELdash) ) %>%
   bind_rows( dfs2.school(df,Asian) )  %>%
   bind_rows( dfs2.school(df,Filipino) )  %>%
   bind_rows( dfs2.school(df,TwoorMoreRaces) )  %>%
   bind_rows( dfs2.school(df,HispanicOrLatinoEthnicity) )  %>%
   bind_rows( dfs2.school(df,SWD) )  %>%
   bind_rows( dfs2.school(df,SED) )  %>%
   bind_rows( dfs2.school(df,HOM) ) %>%
       mutate(SchoolName = namer,
              CDS = coder)
       
   waiting.room


   }
   
   
   
   school.split <-  king.city.23 %>%
       filter(str_detect(DistrictName,"King City")) 
   
   school.split %>%
       split(school.split$SchoolName) %>%
       map(~graph.wrap2(.))
   
   
   
 # Calculate DFS for student groups in district
   
   
   # Testing add.school.dfs()
   salinas.union.23.demo %>%
       filter(str_detect(CALPADSSchoolName,"Washington")) %>%
       add.school.dfs()
   

   
   # Use for SUHSD
    school.split <-  salinas.union.23.demo %>%
        filter(str_detect(CALPADSDistrictName,"Salinas Union")) 
   

    
    # USe for King City or others
    school.split <-  king.city.23 %>%
        rename(CALPADSSchoolName = SchoolName,
               CALPADSSchoolCode = SchoolId,
               TwoorMoreRaces = TwoOrMoreRaces) %>%
        filter(str_detect(DistrictName,"King City")) 
   

    
    # Used as basis for graphing in DFS student group Graph
   
   
holder <-    school.split %>%
    # split(school.split$SchoolName) %>%
     split(school.split$CALPADSSchoolName) %>%
       map_df(~add.school.dfs(.)) %>%
    rename(#Group = students,
           DFS = mean.dist.stand,
           Test = Subject)  %>%
    mutate(Group = case_match(students,
                              "HOM" ~ "Homeless",
                              "SWD" ~ "Students with \nDisabilities",
                              "SED" ~ "Socio-Economically \nDisadvantaged",
                              "HispanicOrLatinoEthnicity" ~ "Latino",
                              "ELdash" ~ "English Learner",
                              .default = students
    ))
  
   
   
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




### Comparison to prior year by grade ----

library(MCOE)


comp.grade.year <- function(df22,df23, assessment = "Math", dist) {
    

df23 %>%
    bind_rows(df22) %>% 
    filter(Subject == assessment) %>%
    mutate(ScaleScoreAchievementLevel = factor(ScaleScoreAchievementLevel),
    ) %>%
    left_join(reference2) %>%
    group_by(SchoolYear, Subject, GradeLevelWhenAssessed ) %>%
    transmute(dist.standard = ScaleScore - MeetStandard,
           mean.dist.stand = mean(dist.standard))  %>%
    select(SchoolYear, Subject, GradeLevelWhenAssessed, DFS = mean.dist.stand) %>%
    distinct() %>%
    mutate(SchoolYear = factor(SchoolYear),
           DFS = as.numeric(DFS)) %>%
    ggplot(aes(x = GradeLevelWhenAssessed, y = DFS, )) +
    geom_col(aes(fill = SchoolYear, 
                 color = "black"),
             position = "dodge2") +
    mcoe_theme +
 #   scale_fill_identity() +
    scale_color_identity() +
    labs(y = "Distance from Standard",
         title = paste0(dist," - ", assessment," CAASPP Results by Grade")
         )

    ggsave(here("output",paste0(dist," - ", assessment," CAASPP Results by Grade ", Sys.Date(),".png")), width = 8, height = 5)
    
}


comp.grade.year(spreckels.22, spreckels.23, "Math", "Spreckels")

comp.grade.year(spreckels.22, spreckels.23, "ELA", "Spreckels")


comp.grade.year(king.city.22, king.city.23, "Math", "King City")




comp.grade.year.meet.exceed <- function(df22,df23, assessment = "Math", dist) {
    
    
    df23 %>%
        bind_rows(df22) %>% 
        filter(Subject == assessment) %>%
        # mutate(ScaleScoreAchievementLevel = factor(ScaleScoreAchievementLevel),
        # ) %>%
        # left_join(reference2) %>%
        group_by(SchoolYear, Subject, GradeLevelWhenAssessed ) %>%
        mutate(Above = ifelse(ScaleScoreAchievementLevel >= 3, TRUE, FALSE),
               perc = mean(Above)*100) %>%
        # transmute(dist.standard = ScaleScore - MeetStandard,
        #           mean.dist.stand = mean(dist.standard))  %>%
        select(SchoolYear, Subject, GradeLevelWhenAssessed, perc) %>%
        distinct() %>%
        mutate(SchoolYear = factor(SchoolYear),
    #           DFS = as.numeric(DFS)
               ) %>%
        ggplot(aes(x = GradeLevelWhenAssessed, y = perc, )) +
        geom_col(aes(fill = SchoolYear, 
                     color = "black"),
                 position = "dodge2") +
        mcoe_theme +
        #   scale_fill_identity() +
        scale_color_identity() +
        labs(y = "Percent Meeting \nor Exceeding",
             title = paste0(dist," - ", assessment," CAASPP Results by Grade")
        )
    
    ggsave(here("output",paste0(dist," - ", assessment," CAASPP Results by Grade ", Sys.Date(),".png")), width = 8, height = 5)
    
}

comp.grade.year.meet.exceed(spreckels.22, spreckels.23, "Math", "Spreckels")

comp.grade.year.meet.exceed(spreckels.22, spreckels.23, "ELA", "Spreckels")









### Calculate by grade level and student group  ----

nmc.23 %>%
    filter(Subject %in% c("ELA","Math")) %>%
   filter(EL == "Yes") %>%    
    group_by(Subject, GradeLevelWhenAssessed) %>%
    mutate(Above = ifelse(ScaleScoreAchievementLevel >= 3, TRUE, FALSE),
           perc = mean(Above)*100) %>%
    select(Subject, perc)  %>%
    distinct() %>%
    print(n=38)
