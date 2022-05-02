library(tidyverse)

#large pc
data_original=read.csv("G:/Confocal/D2 D4 O4 LC3 LAMP1 CQ DOC TRE N1 LG NG LAMP1 LC3-LAMP1 2022-04-19.csv", sep =",", header=TRUE)
rename(data_original, Prep = ?..Prep ) -> data_original
data_original=D2_D4_O4_LC3_LAMP1_CQ_DOC_TRE_N1_LG_NG_LAMP1_LC3_LAMP1_2022_04_29

#Calculate vesicles per cell and summarize
data_original %>%  
  mutate(LAMP1_5_500_per_cell = `LAMP1(5-500)`/`Cell #`, 
    LAMP1_over500_per_cell = `LAMP1(>500)`/`Cell #`, 
    LC3_5_500_per_cell = `LC3(5-500)`/`Cell #`, 
    LC3_over500_per_cell = `LC3(>500)`/`Cell #`, 
    LAMP1.LC3.5.500_per_LAMP1 = `LAMP1+LC3(5-500)`/`LAMP1(5-500)`, 
    LAMP1.LC3..500_per_LAMP1 = `LAMP1+LC3(>500)`/`LAMP1(>500)`,
    LAMP1.LC3.5.500_per_LC3 = `LAMP1+LC3(5-500)`/`LC3(5-500)`,
    LAMP1.LC3..500_per_LC3 = `LAMP1+LC3(>500)`/`LC3(>500)`)%>% 
  select (Prep, Day, Treatment, Condition, 
    LAMP1_5_500_per_cell, LAMP1_over500_per_cell, 
    LC3_5_500_per_cell, LC3_over500_per_cell, 
    LAMP1.LC3.5.500_per_LAMP1, LAMP1.LC3..500_per_LAMP1, 
    LAMP1.LC3.5.500_per_LC3, LAMP1.LC3..500_per_LC3)   %>% 
  group_by(Prep, Day, Treatment, Condition) %>% 
  summarise(LAMP1_5_500_per_cell_average = mean(LAMP1_5_500_per_cell), 
    LAMP1_over500_per_cell_average = mean(LAMP1_over500_per_cell), 
    LC3_5_500_per_cell_average = mean(LC3_5_500_per_cell), 
    LC3_over500_per_cell_average = mean(LC3_over500_per_cell), 
    LAMP1.LC3.5.500_per_LAMP1_average = mean(LAMP1.LC3.5.500_per_LAMP1),
    LAMP1.LC3..500_per_LAMP1_average = mean(LAMP1.LC3..500_per_LAMP1),
    LAMP1.LC3.5.500_per_LC3_average = mean(LAMP1.LC3.5.500_per_LC3),        
    LAMP1.LC3..500_per_LC3_average = mean(LAMP1.LC3..500_per_LC3)) %>% 
    mutate_all(~replace(., is.nan(.), 0))-> data_sum


#Tables for specific selections
data_sum %>% filter(Treatment == "CTRL") -> data_CTRL
data_sum %>% filter((Treatment == "CTRL" | Treatment == "CQ") & Day == "D2") -> data_CQ_d2
data_sum %>% filter((Treatment == "CTRL" | Treatment == "CQ") & Day == "D4") -> data_CQ_d4
data_sum %>% filter(Treatment != "CQ" & Day == "D2") -> data_others_d2
data_sum %>% filter(Treatment != "CQ" & Day == "D4") -> data_others_d4
data_sum %>% filter((Treatment == "CTRL"|Treatment == "D+T") & Day == "D2") -> data_doc_tre_d2
data_sum %>% filter((Treatment == "CTRL"|Treatment == "D+T") & Day == "D4") -> data_doc_tre_d4

data_frame_list = list(data_CTRL, data_CQ_d2, data_CQ_d4,data_others_d2, data_others_d4, data_doc_tre_d2, data_doc_tre_d4)

#boxplot graph with df=dataframe, a=y axis variable, lab=label y axis, lim sup= limite y axis
box_plot_graph <- function(df, a, f, lab, lim){
  theme_settings = theme(axis.line=element_line(size=1, colour="black"),axis.text=element_text(size=10, color="black", face=2),axis.title=element_text(face=2), panel.background=element_rect(fill="white"))
  scale_fill_settings = scale_fill_grey(start=0.9, end=0.55)
  scale_x_settings = scale_x_discrete(limits=c("N1","LG","NG"))
  boxplot_settings = geom_boxplot(position=position_dodge(1))
  dotplot_settings = geom_dotplot(binaxis = "y", stackdir="center", dotsize=.3, position=position_dodge(1))
  ggplot(df, aes(x=Condition, y=a, fill = f)) + ylab(lab) + ylim(0,lim) + boxplot_settings + dotplot_settings + scale_fill_settings + scale_x_settings + theme_settings  
}

#graph settings
theme_settings = theme(axis.line=element_line(size=1, colour="black"),axis.text=element_text(size=10, color="black", face=2),axis.title=element_text(face=2), panel.background=element_rect(fill="white"))
scale_fill_settings = scale_fill_grey(start=0.9, end=0.55)
scale_x_settings = scale_x_discrete(limits=c("N1","LG","NG"))
boxplot_settings = geom_boxplot(position=position_dodge(1))
dotplot_settings = geom_dotplot(binaxis = "y", stackdir="center", dotsize=1, position=position_dodge(1))
line_settings = geom_line()

box_plot_graph(i, i$LAMP1_5_500_per_cell_average, i$Day, "LAMP1(5-500)/Cell", 500)


#graphs LAMP1 CTRL
ggplot(data_CTRL, aes(x=Condition, y=LAMP1_5_500_per_cell_average, fill = Day)) + ylab("LAMP1(5-500)/Cell") + ylim(0,300) + boxplot_settings + dotplot_settings + scale_fill_settings + scale_x_settings + theme_settings
ggplot(data_CTRL, aes(x=Condition, y=LAMP1_over500_per_cell_average, fill = Day)) + ylab("LAMP1(>500)/Cell")+ylim(0,2.5) + boxplot_settings + dotplot_settings + scale_fill_settings + scale_x_settings + theme_settings

#graphs LAMP1 CQ
ggplot(data_CQ_d2, aes(x=Condition, y=LAMP1_5_500_per_cell_average, fill = Treatment))+ ylab("LAMP1(5-500)/Cell")+ ggtitle("D2") + ylim(0,500) + boxplot_settings + dotplot_settings + scale_fill_settings + scale_x_settings + theme_settings
ggplot(data_CQ_d4, aes(x=Condition, y=LAMP1_5_500_per_cell_average, fill = Treatment))+ ylab("LAMP1(5-500)/Cell")+ ggtitle("D4") + ylim(0,500) + boxplot_settings + dotplot_settings + scale_fill_settings + scale_x_settings + theme_settings
ggplot(data_CQ_d2, aes(x=Condition, y=LAMP1_over500_per_cell_average, fill = Treatment))+ylab("LAMP1(>500)/Cell")+ ggtitle("D2") + ylim(0,2.5) + boxplot_settings + dotplot_settings + scale_fill_settings + scale_x_settings + theme_settings
ggplot(data_CQ_d4, aes(x=Condition, y=LAMP1_over500_per_cell_average, fill = Treatment))+ylab("LAMP1(>500)/Cell")+ ggtitle("D4") + ylim(0,2.5)+ boxplot_settings + dotplot_settings + scale_fill_settings + scale_x_settings + theme_settings

#graphs LAMP1 others
ggplot(data_others_d2, aes(x=Condition, y=LAMP1_5_500_per_cell_average, fill = Treatment))+ylab("LAMP1(5-500)/Cell")+ ggtitle("D2") + ylim(0,500)+ boxplot_settings + dotplot_settings + scale_fill_settings + scale_x_settings + theme_settings
ggplot(data_others_d4, aes(x=Condition, y=LAMP1_5_500_per_cell_average, fill = Treatment))+ylab("LAMP1(5-500)/Cell")+ ggtitle("D4") + ylim(0,500)+ boxplot_settings + dotplot_settings + scale_fill_settings + scale_x_settings + theme_settings
ggplot(data_others_d2, aes(x=Condition, y=LAMP1_over500_per_cell_average, fill = Treatment))+ylab("LAMP1(>500)/Cell")+ ggtitle("D2") + ylim(0,2.5)+ boxplot_settings + dotplot_settings + scale_fill_settings + scale_x_settings + theme_settings
ggplot(data_others_d4, aes(x=Condition, y=LAMP1_over500_per_cell_average, fill = Treatment))+ylab("LAMP1(>500)/Cell")+ ggtitle("D4") + ylim(0,2.5)+ boxplot_settings + dotplot_settings + scale_fill_settings + scale_x_settings + theme_settings

#graphs LAMP1 doc+tre
ggplot(data_doc_tre_d2, aes(x=Condition, y=LAMP1_5_500_per_cell_average, fill = Treatment))+ylab("LAMP1(5-500)/Cell")+ ggtitle("D2") + ylim(0,500)+ boxplot_settings + dotplot_settings + scale_fill_settings + scale_x_settings + theme_settings
ggplot(data_doc_tre_d4, aes(x=Condition, y=LAMP1_5_500_per_cell_average, fill = Treatment))+ylab("LAMP1(5-500)/Cell")+ ggtitle("D4") + ylim(0,500)+ boxplot_settings + dotplot_settings + scale_fill_settings + scale_x_settings + theme_settings
ggplot(data_doc_tre_d2, aes(x=Condition, y=LAMP1_over500_per_cell_average, fill = Treatment))+ylab("LAMP1(>500)/Cell")+ ggtitle("D2") + ylim(0,2.5)+ boxplot_settings + dotplot_settings + scale_fill_settings + scale_x_settings + theme_settings
ggplot(data_doc_tre_d4, aes(x=Condition, y=LAMP1_over500_per_cell_average, fill = Treatment))+ylab("LAMP1(>500)/Cell")+ ggtitle("D4") + ylim(0,2.5)+ boxplot_settings + dotplot_settings + scale_fill_settings + scale_x_settings + theme_settings


#graphs LC3 CTRL
ggplot(data_CTRL, aes(x=Condition, y=LC3_over500_per_cell_average, fill = Day))+ylab("LC3(>500)/Cell")+ylim(0,0.5)+ boxplot_settings + dotplot_settings + scale_fill_settings + scale_x_settings + theme_settings
ggplot(data_CTRL, aes(x=Condition, y=LC3_5_500_per_cell_average, fill = Day))+ylab("LC3(5-500)/Cell")+ylim(0,35)+ boxplot_settings + dotplot_settings + scale_fill_settings + scale_x_settings + theme_settings

#graphs LC3 CQ
ggplot(data_CQ_d2, aes(x=Condition, y=LC3_5_500_per_cell_average, fill = Treatment))+ylab("LC3(5-500)/Cell") + ggtitle("D2")+ ylim(0,100)+ boxplot_settings + dotplot_settings + scale_fill_settings + scale_x_settings + theme_settings
ggplot(data_CQ_d4, aes(x=Condition, y=LC3_5_500_per_cell_average, fill = Treatment))+ylab("LC3(5-500)/Cell") + ggtitle("D4")+ ylim(0,100)+ boxplot_settings + dotplot_settings + scale_fill_settings + scale_x_settings + theme_settings
ggplot(data_CQ_d2, aes(x=Condition, y=LC3_over500_per_cell_average, fill = Treatment))+ylab("LC3(>500)/Cell") + ggtitle("D2")+ ylim(0,2.5)+ boxplot_settings + dotplot_settings + scale_fill_settings + scale_x_settings + theme_settings
ggplot(data_CQ_d4, aes(x=Condition, y=LC3_over500_per_cell_average, fill = Treatment))+ ylab("LC3(>500)/Cell") + ggtitle("D4")+ ylim(0,2.5)+ boxplot_settings + dotplot_settings + scale_fill_settings + scale_x_settings + theme_settings

#graphs LC3 others 
ggplot(data_others_d2, aes(x=Condition, y=LC3_5_500_per_cell_average, fill = Treatment))+ylab("LC3(5-500)/Cell") + ggtitle("D2")+ ylim(0,100)+ boxplot_settings + dotplot_settings + scale_fill_settings + scale_x_settings + theme_settings
ggplot(data_others_d4, aes(x=Condition, y=LC3_5_500_per_cell_average, fill = Treatment))+ylab("LC3(5-500)/Cell") + ggtitle("D4")+ ylim(0,100)+ boxplot_settings + dotplot_settings + scale_fill_settings + scale_x_settings + theme_settings
ggplot(data_others_d2, aes(x=Condition, y=LC3_over500_per_cell_average, fill = Treatment))+ylab("LC3(>500)/Cell") + ggtitle("D2")+ ylim(0,2.5)+ boxplot_settings + dotplot_settings + scale_fill_settings + scale_x_settings + theme_settings
ggplot(data_others_d4, aes(x=Condition, y=LC3_over500_per_cell_average, fill = Treatment))+ylab("LC3(>500)/Cell") + ggtitle("D4")+ ylim(0,2.5)+ boxplot_settings + dotplot_settings + scale_fill_settings + scale_x_settings + theme_settings

#graphs LC3 doc+tre 
ggplot(data_doc_tre_d2, aes(x=Condition, y=LC3_5_500_per_cell_average, fill = Treatment))+ylab("LC3(5-500)/Cell")+ ggtitle("D2") + ylim(0,40)+ boxplot_settings + dotplot_settings + scale_fill_settings + scale_x_settings + theme_settings
ggplot(data_doc_tre_d4, aes(x=Condition, y=LC3_5_500_per_cell_average, fill = Treatment))+ylab("LC3(5-500)/Cell")+ ggtitle("D4") + ylim(0,40)+ boxplot_settings + dotplot_settings + scale_fill_settings + scale_x_settings + theme_settings
ggplot(data_doc_tre_d2, aes(x=Condition, y=LC3_over500_per_cell_average, fill = Treatment))+ylab("LC3(>500)/Cell")+ ggtitle("D2") + ylim(0,2.5)+ boxplot_settings + dotplot_settings + scale_fill_settings + scale_x_settings + theme_settings
ggplot(data_doc_tre_d4, aes(x=Condition, y=LC3_over500_per_cell_average, fill = Treatment))+ylab("LC3(>500)/Cell")+ ggtitle("D4") + ylim(0,2.5)+ boxplot_settings + dotplot_settings + scale_fill_settings + scale_x_settings + theme_settings
