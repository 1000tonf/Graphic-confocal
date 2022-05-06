#t-tests CQ D2

#LAMP1(5-500)##########################################################################

#CQ N1 LAMP1_5_500
data_CQ_d2 %>% select(Prep, Treatment, Condition, LAMP1_5_500_per_cell_average) %>%
  filter(Condition == "N1") %>% spread(Treatment, LAMP1_5_500_per_cell_average) -> data_CQ_t_test

boxplot(data_CQ_t_test$CTRL, data_CQ_t_test$CQ)

#Ho: Mean diff CTRLxCQ is 0
# one-sided
t.test(data_CQ_t_test$CTRL, data_CQ_t_test$CQ, mu=0, alt="less", paired=T) # with HA842, HA843, HA851 p-value=0.125



#CQ LG LAMP1_5_500
data_CQ_d2 %>% select(Prep, Treatment, Condition, LAMP1_5_500_per_cell_average) %>%
  filter(Condition == "LG") %>% spread(Treatment, LAMP1_5_500_per_cell_average) -> data_CQ_t_test

boxplot(data_CQ_t_test$CTRL, data_CQ_t_test$CQ)

#Ho: Mean diff CTRLxCQ is 0
# one-sided
t.test(data_CQ_t_test$CTRL, data_CQ_t_test$CQ, mu=0, alt="less", paired=T) # with HA842, HA843, HA851 p-value=0.5177



#CQ NG LAMP1_5_500
data_CQ_d2 %>% select(Prep, Treatment, Condition, LAMP1_5_500_per_cell_average) %>%
  filter(Condition == "NG") %>% spread(Treatment, LAMP1_5_500_per_cell_average) -> data_CQ_t_test

boxplot(data_CQ_t_test$CTRL, data_CQ_t_test$CQ)

#Ho: Mean diff CTRLxCQ is 0
# one-sided
t.test(data_CQ_t_test$CTRL, data_CQ_t_test$CQ, mu=0, alt="less", paired=T) # with HA842, HA843, HA851 p-value=0.534


#LAMP1(>500)##########################################################################


#CQ N1 LAMP1_>500
data_CQ_d2 %>% select(Prep, Treatment, Condition, LAMP1_over500_per_cell_average) %>%
  filter(Condition == "N1") %>% spread(Treatment, LAMP1_over500_per_cell_average) -> data_CQ_t_test

boxplot(data_CQ_t_test$CTRL, data_CQ_t_test$CQ)

#Ho: Mean diff CTRLxCQ is 0
# one-sided
t.test(data_CQ_t_test$CTRL, data_CQ_t_test$CQ, mu=0, alt="less", paired=T) # with HA842, HA843, HA851 p-value=0.1667



#CQ LG LAMP1_>500
data_CQ_d2 %>% select(Prep, Treatment, Condition, LAMP1_over500_per_cell_average) %>%
  filter(Condition == "LG") %>% spread(Treatment, LAMP1_over500_per_cell_average) -> data_CQ_t_test

boxplot(data_CQ_t_test$CTRL, data_CQ_t_test$CQ)

#Ho: Mean diff CTRLxCQ is 0
# one-sided
t.test(data_CQ_t_test$CTRL, data_CQ_t_test$CQ, mu=0, alt="less", paired=T) # with HA842, HA843, HA851 p-value=0.1994



#CQ NG LAMP1_>500
data_CQ_d2 %>% select(Prep, Treatment, Condition, LAMP1_over500_per_cell_average) %>%
  filter(Condition == "NG") %>% spread(Treatment, LAMP1_over500_per_cell_average) -> data_CQ_t_test

boxplot(data_CQ_t_test$CTRL, data_CQ_t_test$CQ)

#Ho: Mean diff CTRLxCQ is 0
# one-sided
t.test(data_CQ_t_test$CTRL, data_CQ_t_test$CQ, mu=0, alt="less", paired=T) # with HA842, HA843, HA851 p-value=0.9714


#LC3(5-500)##########################################################################


#CQ N1 LC3(5-500)
data_CQ_d2 %>% select(Prep, Treatment, Condition, LC3_5_500_per_cell_average) %>%
  filter(Condition == "N1") %>% spread(Treatment, LC3_5_500_per_cell_average) -> data_CQ_t_test

boxplot(data_CQ_t_test$CTRL, data_CQ_t_test$CQ)

#Ho: Mean diff CTRLxCQ is 0
# one-sided
t.test(data_CQ_t_test$CTRL, data_CQ_t_test$CQ, mu=0, alt="less", paired=T) # with HA842, HA843, HA851 p-value=0.2036



#CQ LG LC3(5-500)
data_CQ_d2 %>% select(Prep, Treatment, Condition, LC3_5_500_per_cell_average) %>%
  filter(Condition == "LG") %>% spread(Treatment, LC3_5_500_per_cell_average) -> data_CQ_t_test

boxplot(data_CQ_t_test$CTRL, data_CQ_t_test$CQ)

#Ho: Mean diff CTRLxCQ is 0
# one-sided
t.test(data_CQ_t_test$CTRL, data_CQ_t_test$CQ, mu=0, alt="less", paired=T) # with HA842, HA843, HA851 p-value=0.2803



#CQ NG LC3(5-500)
data_CQ_d2 %>% select(Prep, Treatment, Condition, LC3_5_500_per_cell_average) %>%
  filter(Condition == "NG") %>% spread(Treatment, LC3_5_500_per_cell_average) -> data_CQ_t_test

boxplot(data_CQ_t_test$CTRL, data_CQ_t_test$CQ)

#Ho: Mean diff CTRLxCQ is 0
# one-sided
t.test(data_CQ_t_test$CTRL, data_CQ_t_test$CQ, mu=0, alt="less", paired=T) # with HA842, HA843, HA851 p-value=0.8544



#LC3(>500)##########################################################################


#CQ N1 LC3(>500)
data_CQ_d2 %>% select(Prep, Treatment, Condition, LC3_over500_per_cell_average) %>%
  filter(Condition == "N1") %>% spread(Treatment, LC3_over500_per_cell_average) -> data_CQ_t_test

boxplot(data_CQ_t_test$CTRL, data_CQ_t_test$CQ)

#Ho: Mean diff CTRLxCQ is 0
# one-sided
t.test(data_CQ_t_test$CTRL, data_CQ_t_test$CQ, mu=0, alt="less", paired=T) # with HA842, HA843, HA851 p-value=0.8981



#CQ LG LC3(>500)
data_CQ_d2 %>% select(Prep, Treatment, Condition, LC3_over500_per_cell_average) %>%
  filter(Condition == "LG") %>% spread(Treatment, LC3_over500_per_cell_average) -> data_CQ_t_test

boxplot(data_CQ_t_test$CTRL, data_CQ_t_test$CQ)

#Ho: Mean diff CTRLxCQ is 0
# one-sided
t.test(data_CQ_t_test$CTRL, data_CQ_t_test$CQ, mu=0, alt="less", paired=T) # with HA842, HA843, HA851 p-value=0.2113



#CQ LG LC3(>500)
data_CQ_d2 %>% select(Prep, Treatment, Condition, LC3_over500_per_cell_average) %>%
  filter(Condition == "NG") %>% spread(Treatment, LC3_over500_per_cell_average) -> data_CQ_t_test

boxplot(data_CQ_t_test$CTRL, data_CQ_t_test$CQ)

#Ho: Mean diff CTRLxCQ is 0
# one-sided
t.test(data_CQ_t_test$CTRL, data_CQ_t_test$CQ, mu=0, alt="less", paired=T) # with HA842, HA843, HA851 p-value=0.2113
