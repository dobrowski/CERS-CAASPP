

somoco.old <- caaspp.mry2 %>%
    filter(District_Code == "66068",
 #          Test_Id == 2,
           Grade == 13,
          Subgroup_ID  %in% c(1,31, 52, 78, 128, 160)
     #   str_detect(Subgroup, "English learner|disabil|Homeless|Hispanic")
           )





somoco.new <- read_sheet(
    "https://docs.google.com/spreadsheets/d/1E7x2W-bWkZGenZTPVmlyGSl0LQPfrHc2s8ZLILKUOGw/edit#gid=317880530",
    sheet = "Percent Meet by Group")



somoco.new %>%
    bind_rows(somoco.old) %>%
    #    bind_rows(ghs.past) %>%
     filter(Test_Id == 1,
            Grade == 13,
            ) %>%
    mutate(Subgroup = case_match(Subgroup,
                                 "Economically disadvantaged" ~ "Economically \ndisadvantaged",
                                 "Students with disability" ~ "Students with \ndisability",
                                 "Hispanic or Latino" ~ "Hispanic or \nLatino",
                                 .default = Subgroup
                                 )) %>%
    select(Subgroup, Percentage_Standard_Met_and_Above, Test_Year) %>%
    ggplot(aes(x = (Subgroup), y = as.numeric(Percentage_Standard_Met_and_Above)
    )) +
    geom_col(aes(fill = as.factor(Test_Year),
                 color = "black"),
             position = "dodge2") +
    mcoe_theme +
    {if(length(unique(somoco.old$Subgroup)) >=8 )scale_x_discrete(guide = guide_axis(n.dodge = 2))} + #Fixes the overlapping axis labels to make them alternate if lots of columns
    scale_color_identity() +
    labs(y = "Percent Meeting \nor Exceeding",
         title = paste0("ELA CAASPP Results for South Monterey County"),
         subtitle = "Percentage meeting or exceeding standards by student group and year",
         caption = "Note 2021 testing was during the COVID pandemic and should be used with caution."
    )
 
 ggsave(here("output",paste0("South Monterey County - ELA CAASPP Student Group Results 2019 to 2023 Comparison ", Sys.Date(),".png")), width = 8, height = 5)    
 





### Greenfield High ----



caaspp.greenfield.hi <- tbl(con, "CAASPP") %>% 
    filter(County_Code == "27",
            District_Code == "66068",
           Test_Year >= "2019",
           School_Code == "2730174"
           #           Type_ID == 6
    ) %>%
    collect() 



caaspp.greenfield.hi2 <- caaspp.greenfield.hi %>%
    mutate(Subgroup_ID = as.character(Subgroup_ID)) %>%
    left_join_codebook("CAASPP", "Subgroup_ID") %>%
    rename(Subgroup = definition) %>%
    #   left_join(ent2) %>%
    mutate(Type_ID = as.character(Type_ID)) %>%
    left_join_codebook("CAASPP", "Type_ID") %>%
    rename(Entity_Type = definition) %>%
    mutate(across(CAASPP_Reported_Enrollment:Area_4_Percentage_Near_Standard, as.numeric))




ghs <- read_sheet(
"https://docs.google.com/spreadsheets/d/1E7x2W-bWkZGenZTPVmlyGSl0LQPfrHc2s8ZLILKUOGw/edit#gid=317880530",
sheet = "SoMoCo")

ghs.subgroups <- unique(ghs$Subgroup)


ghs.past <- caaspp.mry2 %>%
    filter(County_Code == "27",
                      District_Code == "66068",
           Subgroup %in% ghs.subgroups)


ghs %>%
    bind_rows(caaspp.greenfield.hi2) %>%
#    bind_rows(ghs.past) %>%
    filter(Test_Id == 1,
           Grade == 13,
           Subgroup %in% ghs.subgroups,
           !str_detect(Subgroup,"Initial")) %>%
    select(Subgroup, Percentage_Standard_Met_and_Above, Test_Year) %>%
    ggplot(aes(x = (Subgroup), y = as.numeric(Percentage_Standard_Met_and_Above)
    )) +
    geom_col(aes(fill = as.factor(Test_Year), 
                 color = "black"),
             position = "dodge2") +
    mcoe_theme +
    {if(length(unique(somoco.old$Subgroup)) >=8 )scale_x_discrete(guide = guide_axis(n.dodge = 2))} + #Fixes the overlapping axis labels to make them alternate if lots of columns
    scale_color_identity() +
    labs(y = "Percent Meeting \nor Exceeding",
         title = paste0("ELA CAASPP Results for Greenfield High"),
         subtitle = "Percentage meeting or exceeding standards by student group and year",
         caption = "Note 2021 testing was during the COVID pandemic and should be used with caution."
    )

ggsave(here("output",paste0("Greenfield - ELA CAASPP Student Group Results 2019 to 2023 Comparison ", Sys.Date(),".png")), width = 8, height = 5)    

               
