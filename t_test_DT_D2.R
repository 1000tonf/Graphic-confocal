#t-tests D+T D2

#LAMP1(5-500)##########################################################################

#D+T N1 LAMP1_5_500
data_doc_tre_d2 %>% select(Prep, Treatment, Condition, LAMP1_5_500_per_cell_average) %>%
  filter(Condition == "N1") %>% spread(Treatment, LAMP1_5_500_per_cell_average) -> data_DT_t_test

boxplot(data_DT_t_test$CTRL, data_DT_t_test$`D+T`)

#Ho: Mean diff CTRLxCQ is 0
# one-sided
t.test(data_DT_t_test$CTRL, data_DT_t_test$`D+T`, mu=0, alt="two.sided", paired=T) # with HA842, HA843, HA851 p-value=0.7454



#D+T LG LAMP1_5_500
data_doc_tre_d2 %>% select(Prep, Treatment, Condition, LAMP1_5_500_per_cell_average) %>%
  filter(Condition == "LG") %>% spread(Treatment, LAMP1_5_500_per_cell_average) -> data_DT_t_test

boxplot(data_DT_t_test$CTRL, data_DT_t_test$`D+T`)

#Ho: Mean diff CTRLxCQ is 0
# one-sided
t.test(data_DT_t_test$CTRL, data_DT_t_test$`D+T`, mu=0, alt="two.sided", paired=T) # with HA842, HA843, HA851 p-value=0.3986



#D+T NG LAMP1_5_500
data_doc_tre_d2 %>% select(Prep, Treatment, Condition, LAMP1_5_500_per_cell_average) %>%
  filter(Condition == "NG") %>% spread(Treatment, LAMP1_5_500_per_cell_average) -> data_DT_t_test

boxplot(data_DT_t_test$CTRL, data_DT_t_test$`D+T`)

#Ho: Mean diff CTRLxCQ is 0
# one-sided
t.test(data_DT_t_test$CTRL, data_DT_t_test$`D+T`, mu=0, alt="two.sided", paired=T) # with HA842, HA843, HA851 p-value=0.1116


#LAMP1(>500)##########################################################################


#D+T N1 LAMP1_>500
data_doc_tre_d2 %>% select(Prep, Treatment, Condition, LAMP1_over500_per_cell_average) %>%
  filter(Condition == "N1") %>% spread(Treatment, LAMP1_over500_per_cell_average) -> data_DT_t_test

boxplot(data_DT_t_test$CTRL, data_DT_t_test$`D+T`)

#Ho: Mean diff CTRLxCQ is 0
# one-sided
t.test(data_DT_t_test$CTRL, data_DT_t_test$`D+T`, mu=0, alt="two.sided", paired=T) # with HA842, HA843, HA851 p-value=0.5967



#D+T LG LAMP1_>500
data_doc_tre_d2 %>% select(Prep, Treatment, Condition, LAMP1_over500_per_cell_average) %>%
  filter(Condition == "LG") %>% spread(Treatment, LAMP1_over500_per_cell_average) -> data_DT_t_test

boxplot(data_DT_t_test$CTRL, data_DT_t_test$`D+T`)

#Ho: Mean diff CTRLxCQ is 0
# one-sided
t.test(data_DT_t_test$CTRL, data_DT_t_test$`D+T`, mu=0, alt="two.sided", paired=T) # with HA842, HA843, HA851 p-value=0.5569



#D+T NG LAMP1_>500
data_doc_tre_d2 %>% select(Prep, Treatment, Condition, LAMP1_over500_per_cell_average) %>%
  filter(Condition == "NG") %>% spread(Treatment, LAMP1_over500_per_cell_average) -> data_DT_t_test

boxplot(data_DT_t_test$CTRL, data_DT_t_test$`D+T`)

#Ho: Mean diff CTRLxCQ is 0
# one-sided
t.test(data_DT_t_test$CTRL, data_DT_t_test$`D+T`, mu=0, alt="two.sided", paired=T) # with HA842, HA843, HA851 p-value=0.6651


#LC3(5-500)##########################################################################


#D+T N1 LC3(5-500)
data_doc_tre_d2 %>% select(Prep, Treatment, Condition, LC3_5_500_per_cell_average) %>%
  filter(Condition == "N1") %>% spread(Treatment, LC3_5_500_per_cell_average) -> data_DT_t_test

boxplot(data_DT_t_test$CTRL, data_DT_t_test$`D+T`)

#Ho: Mean diff CTRLxCQ is 0
# one-sided
t.test(data_DT_t_test$CTRL, data_DT_t_test$`D+T`, mu=0, alt="two.sided", paired=T) # with HA842, HA843, HA851 p-value=0.7555



#D+T LG LC3(5-500)
data_doc_tre_d2 %>% select(Prep, Treatment, Condition, LC3_5_500_per_cell_average) %>%
  filter(Condition == "LG") %>% spread(Treatment, LC3_5_500_per_cell_average) -> data_DT_t_test

boxplot(data_DT_t_test$CTRL, data_DT_t_test$`D+T`)

#Ho: Mean diff CTRLxCQ is 0
# one-sided
t.test(data_DT_t_test$CTRL, data_DT_t_test$`D+T`, mu=0, alt="two.sided", paired=T) # with HA842, HA843, HA851 p-value=0.0827



#D+T NG LC3(5-500)
data_doc_tre_d2 %>% select(Prep, Treatment, Condition, LC3_5_500_per_cell_average) %>%
  filter(Condition == "NG") %>% spread(Treatment, LC3_5_500_per_cell_average) -> data_DT_t_test

boxplot(data_DT_t_test$CTRL, data_DT_t_test$`D+T`)

#Ho: Mean diff CTRLxCQ is 0
# one-sided
t.test(data_DT_t_test$CTRL, data_DT_t_test$`D+T`, mu=0, alt="two.sided", paired=T) # with HA842, HA843, HA851 p-value=0.8069



#LC3(>500)##########################################################################


#D+T N1 LC3(>500)
data_doc_tre_d2 %>% select(Prep, Treatment, Condition, LC3_over500_per_cell_average) %>%
  filter(Condition == "N1") %>% spread(Treatment, LC3_over500_per_cell_average) -> data_DT_t_test

boxplot(data_DT_t_test$CTRL, data_DT_t_test$`D+T`)

#Ho: Mean diff CTRLxCQ is 0
# one-sided
t.test(data_DT_t_test$CTRL, data_DT_t_test$`D+T`, mu=0, alt="two.sided", paired=T) # with HA842, HA843, HA851 p-value=0.6915



#D+T LG LC3(>500)
data_doc_tre_d2 %>% select(Prep, Treatment, Condition, LC3_over500_per_cell_average) %>%
  filter(Condition == "LG") %>% spread(Treatment, LC3_over500_per_cell_average) -> data_DT_t_test

boxplot(data_DT_t_test$CTRL, data_DT_t_test$`D+T`)

#Ho: Mean diff CTRLxCQ is 0
# one-sided
t.test(data_DT_t_test$CTRL, data_DT_t_test$`D+T`, mu=0, alt="two.sided", paired=T) # with HA842, HA843, HA851 p-value=0.4226



#D+T NG LC3(>500)
data_doc_tre_d2 %>% select(Prep, Treatment, Condition, LC3_over500_per_cell_average) %>%
  filter(Condition == "NG") %>% spread(Treatment, LC3_over500_per_cell_average) -> data_DT_t_test

boxplot(data_DT_t_test$CTRL, data_DT_t_test$`D+T`)

#Ho: Mean diff CTRLxCQ is 0
# one-sided
t.test(data_DT_t_test$CTRL, data_DT_t_test$`D+T`, mu=0, alt="two.sided", paired=T) # with HA842, HA843, HA851 p-value=0.4226
