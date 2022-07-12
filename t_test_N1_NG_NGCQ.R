#t-test N1 NG NG-CQ Day 4

#LC3####################

data_CQ_d4 %>% select(Prep, Day, Condition_Treatment, LC3_per_cell_average) %>%
  filter(Day == "D4") %>% spread(Condition_Treatment, LC3_per_cell_average) %>% 
  mutate(NGCQ = `NG-CQ`)-> data__N1xNGxNGCQ_t_test

#CTRL N1xNG
boxplot(data__N1xNGxNGCQ_t_test$N1, data__N1xNGxNGCQ_t_test$NG)

#Ho: Mean diff D2xD4 is 0
# one-sided
t.test(data__N1xNGxNGCQ_t_test$N1, data__N1xNGxNGCQ_t_test$NG, mu=0, alt="less", paired=T) # with HA842, HA843, HA851 p-value=0.01051



#CTRL NGxNG-CQ 
boxplot(data__N1xNGxNGCQ_t_test$NG, data__N1xNGxNGCQ_t_test$NGCQ)

#Ho: Mean diff D2xD4 is 0
# one-sided
t.test(data__N1xNGxNGCQ_t_test$NG, data__N1xNGxNGCQ_t_test$NGCQ, mu=0, alt="less", paired=T) # with HA842, HA843, HA851 p-value=0.2448

#LC3notLAMP1####################

data_CQ_d4 %>% select(Prep, Day, Condition_Treatment, LC3_not_LAMP1.LC3_per_cell_average) %>%
  filter(Day == "D4") %>% spread(Condition_Treatment, LC3_not_LAMP1.LC3_per_cell_average) %>% 
  mutate(NGCQ = `NG-CQ`)-> data__N1xNGxNGCQ_t_test

#CTRL N1xNG
boxplot(data__N1xNGxNGCQ_t_test$N1, data__N1xNGxNGCQ_t_test$NG)

#Ho: Mean diff D2xD4 is 0
# one-sided
t.test(data__N1xNGxNGCQ_t_test$N1, data__N1xNGxNGCQ_t_test$NG, mu=0, alt="less", paired=T) # with HA842, HA843, HA851 p-value=0.003772


#CTRL NGxNG-CQ 
boxplot(data__N1xNGxNGCQ_t_test$NG, data__N1xNGxNGCQ_t_test$NGCQ)

#Ho: Mean diff D2xD4 is 0
# one-sided
t.test(data__N1xNGxNGCQ_t_test$NG, data__N1xNGxNGCQ_t_test$NGCQ, mu=0, alt="less", paired=T) # with HA842, HA843, HA851 p-value=0.6265
