data_original=D2_D4_O4_LC3_LAMP1_CQ_DOC_TRE_N1_LG_NG_LAMP1_LC3_LAMP1_2022_04_29
data_original %>% mutate_all(~replace(.,is.na(.),0)) -> data_original

#Calculate vesicles per cell and summarize
data_original %>%  
  mutate(LAMP1_per_cell = (`LAMP1(5-500)`+`LAMP1(>500)`)/`Cell #`, 
         LC3_per_cell = (`LC3(5-500)` + `LC3(>500)`)/`Cell #`,
         LAMP1.LC3_per_cell = (`LAMP1+LC3(5-500)`+ `LAMP1+LC3(>500)`)/`Cell #`,
         LAMP1.LC3_per_LAMP1 = (`LAMP1+LC3(5-500)`+ `LAMP1+LC3(>500)`)/(`LAMP1(5-500)`+`LAMP1(>500)`),
         LAMP1.LC3_per_LC3 = (`LAMP1+LC3(5-500)` + `LAMP1+LC3(>500)`)/(`LC3(>500)`+`LC3(5-500)`),
         LC3_not_LAMP1.LC3_per_cell = ((`LC3(5-500)` + `LC3(>500)`)-(`LAMP1+LC3(5-500)`+ `LAMP1+LC3(>500)`))/`Cell #`) %>% 
  mutate_all(~replace(., is.nan(.), 0))  %>% 
  select (Prep, Day, Treatment, Condition, 
          LAMP1_per_cell, LC3_per_cell,LAMP1.LC3_per_cell, LC3_not_LAMP1.LC3_per_cell,
          LAMP1.LC3_per_LAMP1, LAMP1.LC3_per_LC3)   %>% 
  group_by(Prep, Day, Treatment, Condition) %>% 
  summarise(LAMP1_per_cell_average = mean(LAMP1_per_cell), 
            LAMP1_per_cell_average = mean(LAMP1_per_cell), 
            LC3_per_cell_average = mean(LC3_per_cell),
            LAMP1.LC3_per_cell_average = mean(LAMP1.LC3_per_cell),
            LC3_not_LAMP1.LC3_per_cell_average = mean(LC3_not_LAMP1.LC3_per_cell),
            LAMP1.LC3_per_LAMP1_average = mean(LAMP1.LC3_per_LAMP1),
            LAMP1.LC3_per_LC3_average = mean(LAMP1.LC3_per_LC3)) %>% 
  mutate_all(~replace(., is.nan(.), 0)) -> data_sum

#Tables for specific selections
data_sum %>% filter(Treatment == "CTRL") -> data_CTRL
data_sum %>% filter((Treatment == "CTRL" | Treatment == "CQ") & Day == "D2") -> data_CQ_d2
data_sum %>% filter((Treatment == "CTRL" | Treatment == "CQ") & Day == "D4") -> data_CQ_d4
data_sum %>% filter((Treatment == "CTRL"|Treatment == "D+T") & Day == "D2") -> data_doc_tre_d2
data_sum %>% filter((Treatment == "CTRL"|Treatment == "D+T") & Day == "D4") -> data_doc_tre_d4

data_CTRL %>% ggplot(aes(x=Day, y=LC3_per_cell_average, group=Prep))+
  facet_grid(cols=vars(Condition))+ geom_line() +
   geom_point()
   