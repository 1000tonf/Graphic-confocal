library(tidyverse)

#large pc
data_original=read.csv("G:/Confocal/D2 D4 O4 LC3 LAMP1 CQ DOC TRE N1 LG NG LAMP1 LC3-LAMP1 2022-04-19.csv", sep =",", header=TRUE)
rename(data_original, Prep = ?..Prep ) -> data_original
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


#graph settings
theme_settings = theme(axis.line=element_line(size=1, colour="black"),axis.text=element_text(size=10, color="black", face=2),axis.title=element_text(face=2), panel.background=element_rect(fill="white"))
scale_fill_settings = scale_fill_grey(start=0.9, end=0.55)
scale_x_settings = scale_x_discrete(limits=c("N1","LG","NG"))
boxplot_settings = geom_boxplot(position=position_dodge(1))
dotplot_settings = geom_dotplot(binaxis = "y", stackdir="center", dotsize=1, position=position_dodge(1))
line_settings = geom_line()


#graphs LAMP1 CTRL
ggplot(data_CTRL, aes(x=Condition, y=LAMP1_per_cell_average, fill = Day)) + ylab("LAMP1 puncta/Cell") + ylim(0,200) + boxplot_settings + dotplot_settings + scale_fill_settings + scale_x_settings + theme_settings

#graphs LAMP1 CQ
ggplot(data_CQ_d2, aes(x=Condition, y=LAMP1_per_cell_average, fill = Treatment))+ ylab("LAMP1 puncta/Cell")+ ggtitle("D2") + ylim(0,200) + boxplot_settings + dotplot_settings + scale_fill_settings + scale_x_settings + theme_settings
ggplot(data_CQ_d4, aes(x=Condition, y=LAMP1_per_cell_average, fill = Treatment))+ ylab("LAMP1 puncta/Cell")+ ggtitle("D4") + ylim(0,200) + boxplot_settings + dotplot_settings + scale_fill_settings + scale_x_settings + theme_settings

#graphs LAMP1 doc+tre
ggplot(data_doc_tre_d2, aes(x=Condition, y=LAMP1_per_cell_average, fill = Treatment))+ylab("LAMP1 puncta/Cell")+ ggtitle("D2") + ylim(0,150)+ boxplot_settings + dotplot_settings + scale_fill_settings + scale_x_settings + theme_settings
ggplot(data_doc_tre_d4, aes(x=Condition, y=LAMP1_per_cell_average, fill = Treatment))+ylab("LAMP1 puncta/Cell")+ ggtitle("D4") + ylim(0,150)+ boxplot_settings + dotplot_settings + scale_fill_settings + scale_x_settings + theme_settings


#graphs LC3 CTRL
ggplot(data_CTRL, aes(x=Condition, y=LC3_per_cell_average, fill = Day))+ylab("LC3 puncta/Cell")+ylim(0,40)+ boxplot_settings + dotplot_settings + scale_fill_settings + scale_x_settings + theme_settings +
geom_line(data = tibble(x=c(2.75,3.25), y=c(35, 35)), aes(x=x, y=y), inherit.aes=FALSE) + geom_text(data = tibble(x= 3, y= 37), aes(x=x, y=y, label = "p = 0.009"), inherit.aes=FALSE) +
  geom_line(data = tibble(x=c(1.75,2.25), y=c(25, 25)), aes(x=x, y=y), inherit.aes=FALSE) + geom_text(data = tibble(x= 2, y= 27), aes(x=x, y=y, label = "p = 0.085"), inherit.aes=FALSE)

#graphs LC3 CQ
ggplot(data_CQ_d2, aes(x=Condition, y=LC3_per_cell_average, fill = Treatment))+ylab("LC3 puncta/Cell") + ggtitle("D2")+ ylim(0,100)+ boxplot_settings + dotplot_settings + scale_fill_settings + scale_x_settings + theme_settings
ggplot(data_CQ_d4, aes(x=Condition, y=LC3_per_cell_average, fill = Treatment))+ylab("LC3 puncta/Cell") + ggtitle("D4")+ ylim(0,100)+ boxplot_settings + dotplot_settings + scale_fill_settings + scale_x_settings + theme_settings +
geom_line(data = tibble(x=c(1.75,2.25), y=c(40, 40)), aes(x=x, y=y), inherit.aes=FALSE) + geom_text(data = tibble(x= 2, y= 44), aes(x=x, y=y, label = "p = 0.075"), inherit.aes=FALSE)

#graphs LC3 doc+tre 
ggplot(data_doc_tre_d2, aes(x=Condition, y=LC3_per_cell_average, fill = Treatment))+ylab("LC3 puncta/Cell")+ ggtitle("D2") + ylim(0,40)+ boxplot_settings + dotplot_settings + scale_fill_settings + scale_x_settings + theme_settings +
geom_line(data = tibble(x=c(1.75,2.25), y=c(10, 10)), aes(x=x, y=y), inherit.aes=FALSE) + geom_text(data = tibble(x= 2, y= 12), aes(x=x, y=y, label = "p = 0.083"), inherit.aes=FALSE)
ggplot(data_doc_tre_d4, aes(x=Condition, y=LC3_per_cell_average, fill = Treatment))+ylab("LC3 puncta/Cell")+ ggtitle("D4") + ylim(0,40)+ boxplot_settings + dotplot_settings + scale_fill_settings + scale_x_settings + theme_settings +
geom_line(data = tibble(x=c(2.75,3.25), y=c(35, 35)), aes(x=x, y=y), inherit.aes=FALSE) + geom_text(data = tibble(x= 3, y= 37), aes(x=x, y=y, label = "p = 0.022"), inherit.aes=FALSE)

#graphs (LC3+LAMP1)
ggplot(data_CTRL, aes(x=Condition, y=LAMP1.LC3_per_cell_average, fill = Day))+ylab("LC3+LAMP1 puncta/Cell")+ylim(0,35)+ boxplot_settings + dotplot_settings + scale_fill_settings + scale_x_settings + theme_settings

#graphs NOT(LC3+LAMP1)
ggplot(data_CTRL, aes(x=Condition, y=LC3_not_LAMP1.LC3_per_cell_average, fill = Day))+ylab("LC3 puncta/Cell")+ylim(0,25)+ boxplot_settings + dotplot_settings + scale_fill_settings + scale_x_settings + theme_settings


#graphs (LC3+LAMP1)/LC3 CTRL
ggplot(data_CTRL, aes(x=Condition, y=LAMP1.LC3_per_LC3_average, fill = Day))+ylab("LC3 puncta/Cell")+ylim(0,1)+ boxplot_settings + dotplot_settings + scale_fill_settings + scale_x_settings + theme_settings

#graphs (LC3+LAMP1)/LC3 CTRL
ggplot(data_CTRL, aes(x=Condition, y=LAMP1.LC3_per_LAMP1_average, fill = Day))+ylab("LC3 puncta/Cell")+ylim(0,1)+ boxplot_settings + dotplot_settings + scale_fill_settings + scale_x_settings + theme_settings
