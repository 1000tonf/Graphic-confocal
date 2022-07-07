#t-tests CQ D4

#LAMP1##########################################################################

#CQ D4 N1 LAMP1
data_CQ_d4 %>% select(Prep, Treatment, Condition, LAMP1_per_cell_average) %>%
  filter(Condition == "N1") %>% spread(Treatment, LAMP1_per_cell_average) -> data_CQ_t_test

boxplot(data_CQ_t_test$CTRL, data_CQ_t_test$CQ)

#Ho: Mean diff CTRLxCQ is 0
# one-sided
t.test(data_CQ_t_test$CTRL, data_CQ_t_test$CQ, mu=0, alt="less", paired=T) # with HA842, HA843, HA851 p-value=0.8946

###

#CQ D4 LG LAMP1
data_CQ_d4 %>% select(Prep, Treatment, Condition, LAMP1_per_cell_average) %>%
  filter(Condition == "LG") %>% spread(Treatment, LAMP1_per_cell_average) -> data_CQ_t_test

boxplot(data_CQ_t_test$CTRL, data_CQ_t_test$CQ)

#Ho: Mean diff CTRLxCQ is 0
# one-sided
t.test(data_CQ_t_test$CTRL, data_CQ_t_test$CQ, mu=0, alt="less", paired=T) # with HA842, HA843, HA851 p-value=0.7344

###

#CQ D4 NG LAMP1
data_CQ_d4 %>% select(Prep, Treatment, Condition, LAMP1_per_cell_average) %>%
  filter(Condition == "NG") %>% spread(Treatment, LAMP1_per_cell_average) -> data_CQ_t_test

boxplot(data_CQ_t_test$CTRL, data_CQ_t_test$CQ)

#Ho: Mean diff CTRLxCQ is 0
# one-sided
t.test(data_CQ_t_test$CTRL, data_CQ_t_test$CQ, mu=0, alt="less", paired=T) # with HA842, HA843, HA851 p-value=0.2297


#LC3##########################################################################


#CQ D4 N1 LC3
data_CQ_d4 %>% select(Prep, Treatment, Condition, LC3_per_cell_average) %>%
  filter(Condition == "N1") %>% spread(Treatment, LC3_per_cell_average) -> data_CQ_t_test

boxplot(data_CQ_t_test$CTRL, data_CQ_t_test$CQ)

#Ho: Mean diff CTRLxCQ is 0
# one-sided
t.test(data_CQ_t_test$CTRL, data_CQ_t_test$CQ, mu=0, alt="less", paired=T) # with HA842, HA843, HA851 p-value=0.1837

###

#CQ D4 LG LC3
data_CQ_d4 %>% select(Prep, Treatment, Condition, LC3_per_cell_average) %>%
  filter(Condition == "LG") %>% spread(Treatment, LC3_per_cell_average) -> data_CQ_t_test

boxplot(data_CQ_t_test$CTRL, data_CQ_t_test$CQ)

#Ho: Mean diff CTRLxCQ is 0
# one-sided
t.test(data_CQ_t_test$CTRL, data_CQ_t_test$CQ, mu=0, alt="less", paired=T) # with HA842, HA843, HA851 p-value=0.07493

###

#CQ D4 NG LC3(5-500)
data_CQ_d4 %>% select(Prep, Treatment, Condition, LC3_per_cell_average) %>%
  filter(Condition == "NG") %>% spread(Treatment, LC3_per_cell_average) -> data_CQ_t_test

boxplot(data_CQ_t_test$CTRL, data_CQ_t_test$CQ)

#Ho: Mean diff CTRLxCQ is 0
# one-sided
t.test(data_CQ_t_test$CTRL, data_CQ_t_test$CQ, mu=0, alt="less", paired=T) # with HA842, HA843, HA851 p-value=0.2448


#LC3notLAMP1#################################################################################


#CQ D4 N1 LC3notLAMP1
data_CQ_d4 %>% select(Prep, Treatment, Condition, LC3_not_LAMP1.LC3_per_cell_average) %>%
  filter(Condition == "N1") %>% spread(Treatment, LC3_not_LAMP1.LC3_per_cell_average) -> data_CQ_N1_notLAMP1_t_test

boxplot(data_CQ_N1_notLAMP1_t_test$CTRL, data_CQ_N1_notLAMP1_t_test$CQ)

#Ho: Mean diff CTRLxCQ is 0
# one-sided
t.test(data_CQ_N1_notLAMP1_t_test$CTRL, data_CQ_N1_notLAMP1_t_test$CQ, mu=0, alt="less", paired=T) # with HA842, HA843, HA851 p-value=0.1367

###

#CQ D4 LG LC3notLAMP1
data_CQ_d4 %>% select(Prep, Treatment, Condition, LC3_not_LAMP1.LC3_per_cell_average) %>%
  filter(Condition == "LG") %>% spread(Treatment, LC3_not_LAMP1.LC3_per_cell_average) -> data_CQ_LG_notLAMP1_t_test

boxplot(data_CQ_LG_notLAMP1_t_test$CTRL, data_CQ_LG_notLAMP1_t_test$CQ)

#Ho: Mean diff CTRLxCQ is 0
# one-sided
t.test(data_CQ_LG_notLAMP1_t_test$CTRL, data_CQ_LG_notLAMP1_t_test$CQ, mu=0, alt="less", paired=T) # with HA842, HA843, HA851 p-value=0.1073

###

#CQ D4 NG LC3notLAMP1
data_CQ_d4 %>% select(Prep, Treatment, Condition, LC3_not_LAMP1.LC3_per_cell_average) %>%
  filter(Condition == "NG") %>% spread(Treatment, LC3_not_LAMP1.LC3_per_cell_average) -> data_CQ_NG_notLAMP1_t_test

boxplot(data_CQ_NG_notLAMP1_t_test$CTRL, data_CQ_NG_notLAMP1_t_test$CQ)

#Ho: Mean diff CTRLxCQ is 0
# one-sided
t.test(data_CQ_NG_notLAMP1_t_test$CTRL, data_CQ_NG_notLAMP1_t_test$CQ, mu=0, alt="less", paired=T) # with HA842, HA843, HA851 p-value=0.6265
