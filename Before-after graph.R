library(tidyverse)
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

#Setting condition as factor
data_CTRL$Condition_f = factor(data_CTRL$Condition, levels = c("N1", "LG", "NG"))
data_CQ_d2$Condition_f = factor(data_CQ_d2$Condition, levels = c("N1", "LG", "NG"))
data_CQ_d4$Condition_f = factor(data_CQ_d4$Condition, levels = c("N1", "LG", "NG"))

#Setting treatment as factor
data_CQ_d2$Treatment_f = factor(data_CQ_d2$Treatment, levels = c("CTRL", "CQ"))
data_CQ_d4$Treatment_f = factor(data_CQ_d4$Treatment, levels = c("CTRL", "CQ"))


#graph settings
theme_settings = theme(axis.line=element_line(size=1, colour="black"),panel.background=element_rect(fill="white"), 
 axis.text=element_text(size=12, color="black", face=2), axis.title =element_text(size=14, color="black", face=2) , 
 title =element_text(size=14, color="black", face=2), strip.text = element_text(size=12, color="black", face=2))

##CTRL###################################

#significant bar coordinates graph CTRL
lines <-tibble(Condition_f = factor(c("LG", "NG"), levels = c("N1", "LG", "NG")),
  x =c(1, 1), xend=c(2,2), y=c(15, 35), yend=y)
pvalues <- tibble(Condition_f = factor(c("LG", "NG"), levels = c("N1", "LG", "NG")), 
  x =c(1.5, 1.5), y=c(17, 37), label = c("p = 0.085", "p = 0.009"))

#before-after graph CTRL
data_CTRL %>% ggplot(aes(x=Day, y=LC3_per_cell_average, group=Prep))+
  facet_grid(~Condition_f)+ geom_line() + ylab("LC3 puncta/Cell")+ylim(0,40)+ geom_point()+theme_settings +
  geom_segment(data=lines, aes(x=x, y=y, xend=xend, yend=yend), inherit.aes = FALSE) +
  geom_text(data=pvalues, aes(x=x, y=y, label=label), inherit.aes = FALSE)

##CTRL LC3+LAMP1###################################

#significant bar coordinates graph CTRL (LC3+LAMP1)
lines_LC3.LAMP1 <-tibble(Condition_f = factor(c("NG"), levels = c("N1", "LG", "NG")),
               x =c(1), xend=c(2), y=c(25), yend=y)
pvalues_LC3.LAMP1 <- tibble(Condition_f = factor(c("NG"), levels = c("N1", "LG", "NG")), 
                  x =c(1.5), y=c(27), label = c("p = 0.035"))
      
#before-after graphs (LC3+LAMP1)
data_CTRL %>% ggplot(aes(x=Day, y=LAMP1.LC3_per_cell_average, group=Prep))+
  facet_grid(~Condition_f)+ geom_line() + ylab("LC3 + LAMP1 puncta/Cell")+ylim(0,40)+ geom_point()+theme_settings+
  geom_segment(data=lines_LC3.LAMP1, aes(x=x, y=y, xend=xend, yend=yend), inherit.aes = FALSE) +
  geom_text(data=pvalues_LC3.LAMP1, aes(x=x, y=y, label=label), inherit.aes = FALSE)

##CTRL not LC3+LAMP1###################################

#significant bar coordinates graph CTRL NOT(LC3+LAMP1)
lines_not_LC3.LAMP1 <-tibble(Condition_f = factor(c("NG"), levels = c("N1", "LG", "NG")),
                         x =c(1), xend=c(2), y=c(17), yend=y)
pvalues_not_LC3.LAMP1 <- tibble(Condition_f = factor(c("NG"), levels = c("N1", "LG", "NG")), 
                            x =c(1.5), y=c(19), label = c("p = 0.001"))

#before-after graphs NOT(LC3+LAMP1)
data_CTRL %>% ggplot(aes(x=Day, y=LC3_not_LAMP1.LC3_per_cell_average, group=Prep))+
  facet_grid(~Condition_f)+ geom_line() + ylab("LC3 only puncta/Cell")+ylim(0,40)+ geom_point()+theme_settings+
  geom_segment(data=lines_not_LC3.LAMP1, aes(x=x, y=y, xend=xend, yend=yend), inherit.aes = FALSE) +
  geom_text(data=pvalues_not_LC3.LAMP1, aes(x=x, y=y, label=label), inherit.aes = FALSE)

##CQ D2 ###################################

#before-after graph CQ d2
data_CQ_d2 %>% ggplot(aes(x=Treatment_f, y=LC3_per_cell_average, group=Prep))+
  facet_grid(~Condition_f)+ geom_line() + ylab("LC3 puncta/Cell")+ xlab("Treatment")+ ggtitle("D2")+ ylim(0,100)+ geom_point()+theme_settings

#before-after graph CQ d4
data_CQ_d4 %>% ggplot(aes(x=Treatment_f, y=LC3_per_cell_average, group=Prep))+
  facet_grid(~Condition_f)+ geom_line() + ylab("LC3 puncta/Cell")+ xlab("Treatment")+ ggtitle("D4")+ ylim(0,100)+ geom_point()+theme_settings


