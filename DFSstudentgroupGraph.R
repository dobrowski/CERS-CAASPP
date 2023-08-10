
# This file is to generate a bar graph to display distance from standard for groups. 

library(googlesheets4)
library(MCOE)


working <- read_sheet(ss = sheet,
           sheet = "Distance from Standard Group") %>%
    mutate(Group = case_match(StudentGroup,
                              "HOM" ~ "Homeless",
                              "SWD" ~ "Students with \nDisabilities",
                              "SED" ~ "Socio-Economically \nDisadvantaged",
                              "HispanicOrLatinoEthnicity" ~ "Latino",
                              "ELdash" ~ "English Learner",
                              .default = StudentGroup
                              ))


dfs.graph <- function(dist, assessment = "ELA", dist.name ) {
    

working %>%
    filter(District == dist,
           Test == assessment) %>%
    mutate(DFS = as.numeric(DFS)) %>%
    ggplot(aes(x = fct_reorder(Group,DFS), y = DFS, )) +
    geom_col(aes(fill = EstimatedColor, 
                 color = "black")) +
    mcoe_theme +
    scale_fill_identity() +
    scale_color_identity() +
    labs(y = "Distance from Standard",
         title = paste0(dist.name," - ",assessment," CAASPP Student Group Results 2023"))


ggsave(here("output",paste0(dist.name, " - ",assessment," CAASPP Student Group Results 2023 ", Sys.Date(),".png")), width = 8, height = 5)    

}


dfs.graph(dist = "king.city.23",
          assessment = "Math",
          dist.name = "King City")



dfs.graph(dist = "nmc.23",
          assessment = "ELA",
          dist.name = "North Monterey County")


### Comparison to prior year ----


library(tidyverse)
library(janitor)
library(MCOE)
library(here)
library(ggthemes)
library(vroom)
library(ggrepel)


con <- mcoe_sql_con()



caaspp.mry <- tbl(con, "CAASPP") %>% 
    filter(County_Code == "27",
           # DistrictCode == "10272",
           Test_Year >= "2022",
           Type_ID == 6) %>%
    collect() 



caaspp.mry2 <- caaspp.mry %>%
    mutate(Subgroup_ID = as.character(Subgroup_ID)) %>%
    left_join_codebook("CAASPP", "Subgroup_ID") %>%
    rename(Subgroup = definition) %>%
 #   left_join(ent2) %>%
    mutate(Type_ID = as.character(Type_ID)) %>%
    left_join_codebook("CAASPP", "Type_ID") %>%
    rename(Entity_Type = definition) %>%
    mutate(across(CAASPP_Reported_Enrollment:Area_4_Percentage_Near_Standard, as.numeric))



### DFS from Dash ------


dash <- tbl(con,"DASH_ALL_2022") %>%
    filter(countyname == "Monterey",
           rtype == "D",
           indicator == "ela" | indicator == "math") %>%
    collect()  %>%
    mutate(Group = case_match(studentgroup,
                              "HOM" ~ "Homeless",
                              "SWD" ~ "Students with \nDisabilities",
                              "SED" ~ "Socio-Economically \nDisadvantaged",
                              "HI" ~ "Latino",
                              "EL" ~ "English Learner",
                              "AS" ~ "Asian",
                              "FI" ~ "Filipino",
                              "WH" ~ "White",
                              .default = studentgroup
    ))


dfs.comp <- function(dist, assessment = "ELA", dist.name ) {
    
    
work.group <-   working %>%
         filter(District == dist,
                Test == assessment) %>%
    select(Group) %>%
    unique() %>%
    flatten()
    
    ass2 <- str_to_lower(assessment) 
    
    dash2 <- dash %>%
        filter(str_detect(districtname, dist.name),
               indicator == ass2,
               Group %in% work.group
                             ) %>%
        select(districtname, indicator, currstatus, Group) %>%
        mutate(EstimatedColor = "Light Gray") %>%
        rename(DFS = currstatus)
    
 

    working %>%
        filter(District == dist,
               Test == assessment) %>%
        mutate(DFS = as.numeric(DFS)) %>%
        bind_rows(dash2) %>%


        ggplot(aes(x = fct_reorder(Group,DFS), y = DFS)) +
        geom_col(aes(fill = EstimatedColor,
                     color = "black"),
                 position = "dodge2") +
        mcoe_theme +
        scale_fill_identity() +
        scale_color_identity() +
        labs(y = "Distance from Standard",
             title = paste0(dist.name," - ",assessment," CAASPP Student Group Results 2023"),
             subtitle = "Gray is 2022 results and Colored bars are 2023 with the estimated Dashboard color")
    
    
    ggsave(here("output",paste0(dist.name, " - ",assessment," CAASPP Student Group Results 2022 and 2023 Comparison ", Sys.Date(),".png")), width = 8, height = 5)    
    
}



dfs.comp(dist = "king.city.23",
          assessment = "ELA",
          dist.name = "King City")
