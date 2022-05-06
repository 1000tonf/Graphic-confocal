#t-tests D+T D4

#LAMP1(5-500)##########################################################################

#D+T N1 LAMP1_5_500
data_doc_tre_d4 %>% select(Prep, Treatment, Condition, LAMP1_5_500_per_cell_average) %>%
  filter(Condition == "N1") %>% spread(Treatment, LAMP1_5_500_per_cell_average) -> data_DT_t_test

boxplot(data_DT_t_test$CTRL, data_DT_t_test$`D+T`)

#Ho: Mean diff CTRLxDT is 0
# two-sided
t.test(data_DT_t_test$CTRL, data_DT_t_test$`D+T`, mu=0, alt="two.sided", paired=T) # with HA842, HA843, HA851 p-value=0.483



#D+T LG LAMP1_5_500
data_doc_tre_d4 %>% select(Prep, Treatment, Condition, LAMP1_5_500_per_cell_average) %>%
  filter(Condition == "LG") %>% spread(Treatment, LAMP1_5_500_per_cell_average) -> data_DT_t_test

boxplot(data_DT_t_test$CTRL, data_DT_t_test$`D+T`)

#Ho: Mean diff CTRLxDT is 0
# two-sided
t.test(data_DT_t_test$CTRL, data_DT_t_test$`D+T`, mu=0, alt="two.sided", paired=T) # with HA842, HA843, HA851 p-value=0.6707



#D+T LG LAMP1_5_500
data_doc_tre_d4 %>% select(Prep, Treatment, Condition, LAMP1_5_500_per_cell_average) %>%
  filter(Condition == "NG") %>% spread(Treatment, LAMP1_5_500_per_cell_average) -> data_DT_t_test

boxplot(data_DT_t_test$CTRL, data_DT_t_test$`D+T`)

#Ho: Mean diff CTRLxDT is 0
# two-sided
t.test(data_DT_t_test$CTRL, data_DT_t_test$`D+T`, mu=0, alt="two.sided", paired=T) # with HA842, HA843, HA851 p-value=0.2961


#LAMP1(>500)##########################################################################


#D+T N1 LAMP1_>500
data_doc_tre_d4 %>% select(Prep, Treatment, Condition, LAMP1_over500_per_cell_average) %>%
  filter(Condition == "N1") %>% spread(Treatment, LAMP1_over500_per_cell_average) -> data_DT_t_test

boxplot(data_DT_t_test$CTRL, data_DT_t_test$`D+T`)

#Ho: Mean diff CTRLxDT is 0
# two-sided
t.test(data_DT_t_test$CTRL, data_DT_t_test$`D+T`, mu=0, alt="two.sided", paired=T) # with HA842, HA843, HA851 p-value=0.196



#D+T LG LAMP1_>500
data_doc_tre_d4 %>% select(Prep, Treatment, Condition, LAMP1_over500_per_cell_average) %>%
  filter(Condition == "LG") %>% spread(Treatment, LAMP1_over500_per_cell_average) -> data_DT_t_test

boxplot(data_DT_t_test$CTRL, data_DT_t_test$`D+T`)

#Ho: Mean diff CTRLxDT is 0
# two-sided
t.test(data_DT_t_test$CTRL, data_DT_t_test$`D+T`, mu=0, alt="two.sided", paired=T) # with HA842, HA843, HA851 p-value=0.3634



#D+T NG LAMP1_>500
data_doc_tre_d4 %>% select(Prep, Treatment, Condition, LAMP1_over500_per_cell_average) %>%
  filter(Condition == "NG") %>% spread(Treatment, LAMP1_over500_per_cell_average) -> data_DT_t_test

boxplot(data_DT_t_test$CTRL, data_DT_t_test$`D+T`)

#Ho: Mean diff CTRLxDT is 0
# two-sided
t.test(data_DT_t_test$CTRL, data_DT_t_test$`D+T`, mu=0, alt="two.sided", paired=T) # with HA842, HA843, HA851 p-value=0.1596


#LC3(5-500)##########################################################################


#D+T N1 LC3_5_500
data_doc_tre_d4 %>% select(Prep, Treatment, Condition, LC3_5_500_per_cell_average) %>%
  filter(Condition == "N1") %>% spread(Treatment, LC3_5_500_per_cell_average) -> data_DT_t_test

boxplot(data_DT_t_test$CTRL, data_DT_t_test$`D+T`)

#Ho: Mean diff CTRLxDT is 0
# two-sided
t.test(data_DT_t_test$CTRL, data_DT_t_test$`D+T`, mu=0, alt="two.sided", paired=T) # with HA842, HA843, HA851 p-value=0.4856



#D+T LG LC3_5_500
data_doc_tre_d4 %>% select(Prep, Treatment, Condition, LC3_5_500_per_cell_average) %>%
  filter(Condition == "LG") %>% spread(Treatment, LC3_5_500_per_cell_average) -> data_DT_t_test

boxplot(data_DT_t_test$CTRL, data_DT_t_test$`D+T`)

#Ho: Mean diff CTRLxDT is 0
# two-sided
t.test(data_DT_t_test$CTRL, data_DT_t_test$`D+T`, mu=0, alt="two.sided", paired=T) # with HA842, HA843, HA851 p-value=0.2163



#D+T LG LC3_5_500
data_doc_tre_d4 %>% select(Prep, Treatment, Condition, LC3_5_500_per_cell_average) %>%
  filter(Condition == "NG") %>% spread(Treatment, LC3_5_500_per_cell_average) -> data_DT_t_test

boxplot(data_DT_t_test$CTRL, data_DT_t_test$`D+T`)

#Ho: Mean diff CTRLxDT is 0
# two-sided
t.test(data_DT_t_test$CTRL, data_DT_t_test$`D+T`, mu=0, alt="two.sided", paired=T) # with HA842, HA843, HA851 p-value=0.02171


#LC3(>500)##########################################################################


#D+T N1 LC3_>500
data_doc_tre_d4 %>% select(Prep, Treatment, Condition, LC3_over500_per_cell_average) %>%
  filter(Condition == "N1") %>% spread(Treatment, LC3_over500_per_cell_average) -> data_DT_t_test

boxplot(data_DT_t_test$CTRL, data_DT_t_test$`D+T`)

#Ho: Mean diff CTRLxDT is 0
# two-sided
t.test(data_DT_t_test$CTRL, data_DT_t_test$`D+T`, mu=0, alt="two.sided", paired=T) # with HA842, HA843, HA851 p-value=0.4226



#D+T LG LC3_>500
data_doc_tre_d4 %>% select(Prep, Treatment, Condition, LC3_over500_per_cell_average) %>%
  filter(Condition == "LG") %>% spread(Treatment, LC3_over500_per_cell_average) -> data_DT_t_test

boxplot(data_DT_t_test$CTRL, data_DT_t_test$`D+T`)

#Ho: Mean diff CTRLxDT is 0
# two-sided
t.test(data_DT_t_test$CTRL, data_DT_t_test$`D+T`, mu=0, alt="two.sided", paired=T) # with HA842, HA843, HA851 p-value=0.4226



#D+T NG LC3_>500
data_doc_tre_d4 %>% select(Prep, Treatment, Condition, LC3_over500_per_cell_average) %>%
  filter(Condition == "NG") %>% spread(Treatment, LC3_over500_per_cell_average) -> data_DT_t_test

boxplot(data_DT_t_test$CTRL, data_DT_t_test$`D+T`)

#Ho: Mean diff CTRLxDT is 0
# two-sided
t.test(data_DT_t_test$CTRL, data_DT_t_test$`D+T`, mu=0, alt="two.sided", paired=T) # with HA842, HA843, HA851 p-value=0.3946
