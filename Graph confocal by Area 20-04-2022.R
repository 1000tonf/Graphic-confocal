library(tidyverse)

#large pc
data_original=read.csv("G:/Confocal/D2 D4 O4 LC3 LAMP1 CQ DOC TRE N1 LG NG LAMP1 LC3-LAMP1 2022-04-19.csv", sep =",", header=TRUE)
rename(data_original, Prep = ?..Prep ) -> data_original
data_original=D2_D4_O4_LC3_LAMP1_CQ_DOC_TRE_N1_LG_NG_LAMP1_LC3_LAMP1_2022_04_29

#Calculate vesicles per cell and summarize
data_original[is.na(data_original)]=0
data_original %>%  
  mutate(LC3_per_cell_area = (`LC3(5-500)`* `Size LC3(5-500)` + `LC3(>500)`* `Size LC3(>500)`)/`Cell Area`) %>% 
  mutate(LAMP1_per_cell_area = (`LAMP1(5-500)`* `Size LAMP1(5-500)`+ `LAMP1(>500)`* `Size LAMP1(>500)`)/`Cell Area`) %>%
  select (Prep, Day, Treatment, Condition, LC3_per_cell_area, LAMP1_per_cell_area)   %>% 
  group_by(Prep, Day, Treatment, Condition) %>% 
  summarise(LC3_per_cell_area_average = mean(LC3_per_cell_area),
            LAMP1_per_cell_area_average = mean(LAMP1_per_cell_area)) -> data_sum


#Tables for specific selections
data_sum %>% filter(Treatment == "CTRL") -> data_CTRL
data_sum %>% filter((Treatment == "CTRL" | Treatment == "CQ") & Day == "D2") -> data_CQ_d2
data_sum %>% filter((Treatment == "CTRL" | Treatment == "CQ") & Day == "D4") -> data_CQ_d4
data_sum %>% filter(Treatment != "CQ" & Day == "D2") -> data_others_d2
data_sum %>% filter(Treatment != "CQ" & Day == "D4") -> data_others_d4
data_sum %>% filter((Treatment == "CTRL"|Treatment == "D+T") & Day == "D2") -> data_doc_tre_d2
data_sum %>% filter((Treatment == "CTRL"|Treatment == "D+T") & Day == "D4") -> data_doc_tre_d4

#graph settings
theme_settings = theme(axis.line=element_line(size=1, colour="black"),axis.text=element_text(size=10, color="black", face=2),axis.title=element_text(face=2), panel.background=element_rect(fill="white"))
scale_fill_settings = scale_fill_grey(start=0.9, end=0.55)
scale_x_settings = scale_x_discrete(limits=c("N1","LG","NG"))
boxplot_settings = geom_boxplot(position=position_dodge(1))
dotplot_settings = geom_dotplot(binaxis = "y", stackdir="center", dotsize=1, position=position_dodge(1))

#graphs LAMP1 CTRL
ggplot(data_CTRL, aes(x=Condition, y=LAMP1_per_cell_area_average, fill = Day)) + ylab("LAMP1/Cell Area") + ylim(0,0.5) + boxplot_settings + dotplot_settings + scale_fill_settings + scale_x_settings + theme_settings

#graphs LAMP1 CQ
ggplot(data_CQ_d2, aes(x=Condition, y=LAMP1_per_cell_area_average, fill = Treatment))+ ylab("LAMP1/Cell Area")+ ggtitle("D2") + ylim(0,0.5) + boxplot_settings + dotplot_settings + scale_fill_settings + scale_x_settings + theme_settings
ggplot(data_CQ_d4, aes(x=Condition, y=LAMP1_per_cell_area_average, fill = Treatment))+ ylab("LAMP1/Cell Area")+ ggtitle("D4") + ylim(0,0.5) + boxplot_settings + dotplot_settings + scale_fill_settings + scale_x_settings + theme_settings

#graphs LAMP1 doc+tre
ggplot(data_doc_tre_d2, aes(x=Condition, y=LAMP1_5_500_per_cell_average, fill = Treatment))+ylab("LAMP1(5-500)/Cell")+ ggtitle("D2") + ylim(0,500)+ boxplot_settings + dotplot_settings + scale_fill_settings + scale_x_settings + theme_settings
ggplot(data_doc_tre_d4, aes(x=Condition, y=LAMP1_5_500_per_cell_average, fill = Treatment))+ylab("LAMP1(5-500)/Cell")+ ggtitle("D4") + ylim(0,500)+ boxplot_settings + dotplot_settings + scale_fill_settings + scale_x_settings + theme_settings

#graphs Lc3 CTRL
ggplot(data_CTRL, aes(x=Condition, y=LC3_per_cell_area_average, fill = Day))+ylab("LC3/Cell Area")+ylim(0,0.15)+ boxplot_settings + dotplot_settings + scale_fill_settings + scale_x_settings + theme_settings

#graphs LC3 CQ
ggplot(data_CQ_d2, aes(x=Condition, y=LC3_per_cell_area_average, fill = Treatment))+ylab("LC3/Cell Area") + ggtitle("D2")+ ylim(0,0.3)+ boxplot_settings + dotplot_settings + scale_fill_settings + scale_x_settings + theme_settings
ggplot(data_CQ_d4, aes(x=Condition, y=LC3_per_cell_area_average, fill = Treatment))+ylab("LC3/Cell Area") + ggtitle("D4")+ ylim(0,0.3)+ boxplot_settings + dotplot_settings + scale_fill_settings + scale_x_settings + theme_settings

#graphs LC3 doc+tre 
ggplot(data_doc_tre_d2, aes(x=Condition, y=LC3_per_cell_area_average, fill = Treatment))+ylab("LC3/Cell Area")+ ggtitle("D2") + ylim(0,40)+ boxplot_settings + dotplot_settings + scale_fill_settings + scale_x_settings + theme_settings
ggplot(data_doc_tre_d4, aes(x=Condition, y=LC3_per_cell_area_average, fill = Treatment))+ylab("LC3/Cell Area")+ ggtitle("D4") + ylim(0,40)+ boxplot_settings + dotplot_settings + scale_fill_settings + scale_x_settings + theme_settings