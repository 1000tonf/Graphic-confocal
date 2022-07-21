library(tidyverse)
#Summarised table with cell area per cell
D2_D4_O4_LC3_LAMP1_CQ_DOC_TRE_N1_LG_NG_LAMP1_LC3_LAMP1_2022_04_29 %>% 
  select(Prep, Day, Treatment, Condition, `Cell #`, `Cell Area`) %>% 
  mutate(Cell_Area_per_cell = `Cell Area`/`Cell #`) %>% 
  group_by(Prep, Day, Treatment, Condition) %>% 
  summarise(Cell_Area_per_cell_average = mean(Cell_Area_per_cell))-> data_cell_area

#Table for D2, N1, NG, CTRL and CQ
data_cell_area %>% filter(Day == "D2" & (Condition == "NG"|Condition == "N1") & 
         (Treatment == "CQ"|Treatment == "CTRL")) -> data_cell_area_D2_N1_NG_CQ_CTRL

#Setting Treatment and condition as factors
data_cell_area_D2_N1_NG_CQ_CTRL$Treatment_f = factor(data_cell_area_D2_N1_NG_CQ_CTRL$Treatment, levels = c("CTRL", "CQ"))
data_cell_area_D2_N1_NG_CQ_CTRL$Condition_f = factor(data_cell_area_D2_N1_NG_CQ_CTRL$Condition, levels = c("N1", "NG"))

#graphic settings
theme_settings = theme(axis.line=element_line(size=1, colour="black"),panel.background=element_rect(fill="white"), 
                       axis.text=element_text(size=12, color="black", face=2), axis.title =element_text(size=14, color="black", face=2) , 
                       title =element_text(size=28, color="black", face=2), strip.text = element_text(size=12, color="black", face=2))

data_cell_area_D2_N1_NG_CQ_CTRL %>% ggplot(aes(x=Treatment_f, y=Cell_Area_per_cell_average, group=Prep, color=Prep, shape=Prep))+
  facet_grid(~Condition_f)+ geom_line(size=1) + geom_point(size=2.5) +
  ylab("Average cell area")+ xlab("") + ylim(0,60000)+ theme_settings


#Table for D4, N1, NG, CTRL and CQ
data_cell_area %>% filter(Day == "D4" & (Condition == "NG"|Condition == "N1") & 
                            (Treatment == "CQ"|Treatment == "CTRL")) -> data_cell_area_D4_N1_NG_CQ_CTRL

#Setting Treatment and condition as factors
data_cell_area_D4_N1_NG_CQ_CTRL$Treatment_f = factor(data_cell_area_D4_N1_NG_CQ_CTRL$Treatment, levels = c("CTRL", "CQ"))
data_cell_area_D4_N1_NG_CQ_CTRL$Condition_f = factor(data_cell_area_D4_N1_NG_CQ_CTRL$Condition, levels = c("N1", "NG"))

#graphic settings
theme_settings = theme(axis.line=element_line(size=1, colour="black"),panel.background=element_rect(fill="white"), 
                       axis.text=element_text(size=12, color="black", face=2), axis.title =element_text(size=14, color="black", face=2) , 
                       title =element_text(size=28, color="black", face=2), strip.text = element_text(size=12, color="black", face=2))

data_cell_area_D4_N1_NG_CQ_CTRL %>% ggplot(aes(x=Treatment_f, y=Cell_Area_per_cell_average, group=Prep, color=Prep, shape=Prep))+
  facet_grid(~Condition_f)+ geom_line(size=1) + geom_point(size=2.5) +
  ylab("Average cell area")+ xlab("") + ylim(0,120000)+ theme_settings

