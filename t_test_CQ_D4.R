#t-tests CQ D4

#LAMP1(5-500)##########################################################################

#CQ N1 LAMP1_5_500
data_CQ_d4 %>% select(Prep, Treatment, Condition, LAMP1_5_500_per_cell_average) %>%
  filter(Condition == "N1") %>% spread(Treatment, LAMP1_5_500_per_cell_average) -> data_CQ_t_test

boxplot(data_CQ_t_test$CTRL, data_CQ_t_test$CQ)

#Ho: Mean diff CTRLxCQ is 0
# one-sided
t.test(data_CQ_t_test$CTRL, data_CQ_t_test$CQ, mu=0, alt="less", paired=T) # with HA842, HA843, HA851 p-value=0.8933



#CQ LG LAMP1_5_500
data_CQ_d4 %>% select(Prep, Treatment, Condition, LAMP1_5_500_per_cell_average) %>%
  filter(Condition == "LG") %>% spread(Treatment, LAMP1_5_500_per_cell_average) -> data_CQ_t_test

boxplot(data_CQ_t_test$CTRL, data_CQ_t_test$CQ)

#Ho: Mean diff CTRLxCQ is 0
# one-sided
t.test(data_CQ_t_test$CTRL, data_CQ_t_test$CQ, mu=0, alt="less", paired=T) # with HA842, HA843, HA851 p-value=0.7354



#CQ NG LAMP1_5_500
data_CQ_d4 %>% select(Prep, Treatment, Condition, LAMP1_5_500_per_cell_average) %>%
  filter(Condition == "NG") %>% spread(Treatment, LAMP1_5_500_per_cell_average) -> data_CQ_t_test

boxplot(data_CQ_t_test$CTRL, data_CQ_t_test$CQ)

#Ho: Mean diff CTRLxCQ is 0
# one-sided
t.test(data_CQ_t_test$CTRL, data_CQ_t_test$CQ, mu=0, alt="less", paired=T) # with HA842, HA843, HA851 p-value=0.2307


#LAMP1(>500)##########################################################################


#CQ N1 LAMP1_>500
data_CQ_d4 %>% select(Prep, Treatment, Condition, LAMP1_over500_per_cell_average) %>%
  filter(Condition == "N1") %>% spread(Treatment, LAMP1_over500_per_cell_average) -> data_CQ_t_test

boxplot(data_CQ_t_test$CTRL, data_CQ_t_test$CQ)

#Ho: Mean diff CTRLxCQ is 0
# one-sided
t.test(data_CQ_t_test$CTRL, data_CQ_t_test$CQ, mu=0, alt="less", paired=T) # with HA842, HA843, HA851 p-value=0.895



#CQ LG LAMP1_>500
data_CQ_d4 %>% select(Prep, Treatment, Condition, LAMP1_over500_per_cell_average) %>%
  filter(Condition == "LG") %>% spread(Treatment, LAMP1_over500_per_cell_average) -> data_CQ_t_test

boxplot(data_CQ_t_test$CTRL, data_CQ_t_test$CQ)

#Ho: Mean diff CTRLxCQ is 0
# one-sided
t.test(data_CQ_t_test$CTRL, data_CQ_t_test$CQ, mu=0, alt="less", paired=T) # with HA842, HA843, HA851 p-value=0.4486



#CQ NG LAMP1_>500
data_CQ_d4 %>% select(Prep, Treatment, Condition, LAMP1_over500_per_cell_average) %>%
  filter(Condition == "NG") %>% spread(Treatment, LAMP1_over500_per_cell_average) -> data_CQ_t_test

boxplot(data_CQ_t_test$CTRL, data_CQ_t_test$CQ)

#Ho: Mean diff CTRLxCQ is 0
# one-sided
t.test(data_CQ_t_test$CTRL, data_CQ_t_test$CQ, mu=0, alt="less", paired=T) # with HA842, HA843, HA851 p-value=0.1997


#LC3(5-500)##########################################################################


#CQ N1 LC3(5-500)
data_CQ_d4 %>% select(Prep, Treatment, Condition, LC3_5_500_per_cell_average) %>%
  filter(Condition == "N1") %>% spread(Treatment, LC3_5_500_per_cell_average) -> data_CQ_t_test

boxplot(data_CQ_t_test$CTRL, data_CQ_t_test$CQ)

#Ho: Mean diff CTRLxCQ is 0
# one-sided
t.test(data_CQ_t_test$CTRL, data_CQ_t_test$CQ, mu=0, alt="less", paired=T) # with HA842, HA843, HA851 p-value=0.1832



#CQ LG LC3(5-500)
data_CQ_d4 %>% select(Prep, Treatment, Condition, LC3_5_500_per_cell_average) %>%
  filter(Condition == "LG") %>% spread(Treatment, LC3_5_500_per_cell_average) -> data_CQ_t_test

boxplot(data_CQ_t_test$CTRL, data_CQ_t_test$CQ)

#Ho: Mean diff CTRLxCQ is 0
# one-sided
t.test(data_CQ_t_test$CTRL, data_CQ_t_test$CQ, mu=0, alt="less", paired=T) # with HA842, HA843, HA851 p-value=0.07493



#CQ NG LC3(5-500)
data_CQ_d4 %>% select(Prep, Treatment, Condition, LC3_5_500_per_cell_average) %>%
  filter(Condition == "NG") %>% spread(Treatment, LC3_5_500_per_cell_average) -> data_CQ_t_test

boxplot(data_CQ_t_test$CTRL, data_CQ_t_test$CQ)

#Ho: Mean diff CTRLxCQ is 0
# one-sided
t.test(data_CQ_t_test$CTRL, data_CQ_t_test$CQ, mu=0, alt="less", paired=T) # with HA842, HA843, HA851 p-value=0.2489


#LC3(>500)##########################################################################


#CQ N1 LC3(>500)
data_CQ_d4 %>% select(Prep, Treatment, Condition, LC3_over500_per_cell_average) %>%
  filter(Condition == "N1") %>% spread(Treatment, LC3_over500_per_cell_average) -> data_CQ_t_test

boxplot(data_CQ_t_test$CTRL, data_CQ_t_test$CQ)

#Ho: Mean diff CTRLxCQ is 0
# one-sided
t.test(data_CQ_t_test$CTRL, data_CQ_t_test$CQ, mu=0, alt="less", paired=T) # with HA842, HA843, HA851 p-value=0.7228



#CQ LG LC3(>500)
data_CQ_d4 %>% select(Prep, Treatment, Condition, LC3_over500_per_cell_average) %>%
  filter(Condition == "LG") %>% spread(Treatment, LC3_over500_per_cell_average) -> data_CQ_t_test

boxplot(data_CQ_t_test$CTRL, data_CQ_t_test$CQ)

#Ho: Mean diff CTRLxCQ is 0
# one-sided
t.test(data_CQ_t_test$CTRL, data_CQ_t_test$CQ, mu=0, alt="less", paired=T) # with HA842, HA843, HA851 p-value=0.7485



#CQ NG LC3(>500)
data_CQ_d4 %>% select(Prep, Treatment, Condition, LC3_over500_per_cell_average) %>%
  filter(Condition == "NG") %>% spread(Treatment, LC3_over500_per_cell_average) -> data_CQ_t_test

boxplot(data_CQ_t_test$CTRL, data_CQ_t_test$CQ)

#Ho: Mean diff CTRLxCQ is 0
# one-sided
t.test(data_CQ_t_test$CTRL, data_CQ_t_test$CQ, mu=0, alt="less", paired=T) # with HA842, HA843, HA851 p-value=0.08149
