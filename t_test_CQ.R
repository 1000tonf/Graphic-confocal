#t-tests CQ D2xD4

#LC3#################################################################################


#CQ N1 LC3
data_CQ %>% select(Prep, Day, Treatment, Condition, LC3_per_cell_average) %>%
  filter(Condition == "N1") %>% spread(Day, LC3_per_cell_average) -> data_CQ_N1_t_test

boxplot(data_CQ_N1_t_test$D2, data_CQ_N1_t_test$D4)

#Ho: Mean diff D2xD4 is 0
# one-sided
t.test(data_CQ_N1_t_test$D2, data_CQ_N1_t_test$D4, mu=0, alt="less", paired=T) # with HA842, HA843, HA851 p-value=0.1642



#CQ LG LC3
data_CQ %>% select(Prep, Day, Treatment, Condition, LC3_per_cell_average) %>%
  filter(Condition == "LG") %>% spread(Day, LC3_per_cell_average) -> data_CQ_LG_t_test

boxplot(data_CQ_LG_t_test$D2, data_CQ_LG_t_test$D4)

#Ho: Mean diff D2xD4 is 0
# one-sided
t.test(data_CQ_LG_t_test$D2, data_CQ_LG_t_test$D4, mu=0, alt="less", paired=T) # with HA842, HA843, HA851 p-value=0.07481



#CQ NG LC3
data_CQ %>% select(Prep, Day, Treatment, Condition, LC3_per_cell_average) %>%
  filter(Condition == "NG") %>% spread(Day, LC3_per_cell_average) -> data_CQ_NG_t_test

boxplot(data_CQ_NG_t_test$D2, data_CQ_NG_t_test$D4)

#Ho: Mean diff D2xD4 is 0
# one-sided
t.test(data_CQ_NG_t_test$D2, data_CQ_NG_t_test$D4, mu=0, alt="less", paired=T) # with HA842, HA843, HA851 p-value=0.08672


#LC3notLAMP1#################################################################################


#CQ N1 LC3notLAMP1
data_CQ %>% select(Prep, Day, Treatment, Condition, LC3_not_LAMP1.LC3_per_cell_average) %>%
  filter(Condition == "N1") %>% spread(Day, LC3_not_LAMP1.LC3_per_cell_average) -> data_CQ_N1_notLAMP1_t_test

boxplot(data_CQ_N1_notLAMP1_t_test$D2, data_CQ_N1_notLAMP1_t_test$D4)

#Ho: Mean diff D2xD4 is 0
# one-sided
t.test(data_CQ_N1_notLAMP1_t_test$D2, data_CQ_N1_notLAMP1_t_test$D4, mu=0, alt="less", paired=T) # with HA842, HA843, HA851 p-value=0.1298



#CQ LG LC3notLAMP1
data_CQ %>% select(Prep, Day, Treatment, Condition, LC3_not_LAMP1.LC3_per_cell_average) %>%
  filter(Condition == "LG") %>% spread(Day, LC3_not_LAMP1.LC3_per_cell_average) -> data_CQ_LG_notLAMP1_t_test

boxplot(data_CQ_LG_notLAMP1_t_test$D2, data_CQ_LG_notLAMP1_t_test$D4)

#Ho: Mean diff D2xD4 is 0
# one-sided
t.test(data_CQ_LG_notLAMP1_t_test$D2, data_CQ_LG_notLAMP1_t_test$D4, mu=0, alt="less", paired=T) # with HA842, HA843, HA851 p-value=0.1081



#CQ LG LC3notLAMP1
data_CQ %>% select(Prep, Day, Treatment, Condition, LC3_not_LAMP1.LC3_per_cell_average) %>%
  filter(Condition == "NG") %>% spread(Day, LC3_not_LAMP1.LC3_per_cell_average) -> data_CQ_NG_notLAMP1_t_test

boxplot(data_CQ_NG_notLAMP1_t_test$D2, data_CQ_NG_notLAMP1_t_test$D4)

#Ho: Mean diff D2xD4 is 0
# one-sided
t.test(data_CQ_NG_notLAMP1_t_test$D2, data_CQ_NG_notLAMP1_t_test$D4, mu=0, alt="less", paired=T) # with HA842, HA843, HA851 p-value=0.0965
