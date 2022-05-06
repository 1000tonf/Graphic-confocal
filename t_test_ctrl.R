#t-tests CTRL D2xD4

#LAMP1(5-500)##########################################################################

#CTRL N1 LAMP1_5_500
data_CTRL %>% select(Prep, Day, Treatment, Condition, LAMP1_5_500_per_cell_average) %>%
  filter(Condition == "N1") %>% spread(Day, LAMP1_5_500_per_cell_average) -> data_CTRL_N1_t_test

boxplot(data_CTRL_N1_t_test$D2, data_CTRL_N1_t_test$D4)

#Ho: Mean diff D2xD4 is 0
# one-sided
t.test(data_CTRL_N1_t_test$D2, data_CTRL_N1_t_test$D4, mu=0, alt="less", paired=T) # with HA842, HA843, HA851 p-value=0.1118



#CTRL LG LAMP1_5_500
data_CTRL %>% select(Prep, Day, Treatment, Condition, LAMP1_5_500_per_cell_average) %>%
  filter( Condition == "LG") %>% 
  spread(Day, LAMP1_5_500_per_cell_average) -> data_CTRL_LG_t_test

boxplot(data_CTRL_LG_t_test$D2, data_CTRL_LG_t_test$D4)

#Ho: Mean diff D2xD4 is 0
# one-sided
t.test(data_CTRL_LG_t_test$D2, data_CTRL_LG_t_test$D4, mu=0, alt="less", paired=T) # with HA842, HA843, HA851 p-value=0.1473



#CTRL NG LAMP1_5_500
data_CTRL %>% select(Prep, Day, Treatment, Condition, LAMP1_5_500_per_cell_average) %>%
  filter( Condition == "NG") %>% 
  spread(Day, LAMP1_5_500_per_cell_average) -> data_CTRL_NG_t_test

boxplot(data_CTRL_NG_t_test$D2, data_CTRL_NG_t_test$D4)

#Ho: Mean diff D2xD4 is 0
# one-sided
t.test(data_CTRL_NG_t_test$D2, data_CTRL_NG_t_test$D4, mu=0, alt="less", paired=T) # with HA842, HA843, HA851 p-value=0.2552


#LAMP1(>500)###############################################################################


#CTRL N1 LAMP1_>500
data_CTRL %>% select(Prep, Day, Treatment, Condition, LAMP1_over500_per_cell_average) %>%
  filter(Condition == "N1") %>% spread(Day, LAMP1_over500_per_cell_average) -> data_CTRL_N1_t_test

boxplot(data_CTRL_N1_t_test$D2, data_CTRL_N1_t_test$D4)

#Ho: Mean diff D2xD4 is 0
# one-sided
t.test(data_CTRL_N1_t_test$D2, data_CTRL_N1_t_test$D4, mu=0, alt="less", paired=T) # with HA842, HA843, HA851 p-value=0.1205



#CTRL LG LAMP1_>500
data_CTRL %>% select(Prep, Day, Treatment, Condition, LAMP1_over500_per_cell_average) %>%
  filter( Condition == "LG") %>% 
  spread(Day, LAMP1_over500_per_cell_average) -> data_CTRL_LG_t_test

boxplot(data_CTRL_LG_t_test$D2, data_CTRL_LG_t_test$D4)

#Ho: Mean diff D2xD4 is 0
# one-sided
t.test(data_CTRL_LG_t_test$D2, data_CTRL_LG_t_test$D4, mu=0, alt="less", paired=T) # with HA842, HA843, HA851 p-value=0.1716



#CTRL NG LAMP1_>500
data_CTRL %>% select(Prep, Day, Treatment, Condition, LAMP1_over500_per_cell_average) %>%
  filter( Condition == "NG") %>% 
  spread(Day, LAMP1_over500_per_cell_average) -> data_CTRL_NG_t_test

boxplot(data_CTRL_NG_t_test$D2, data_CTRL_NG_t_test$D4)

#Ho: Mean diff D2xD4 is 0
# one-sided
t.test(data_CTRL_NG_t_test$D2, data_CTRL_NG_t_test$D4, mu=0, alt="less", paired=T) # with HA842, HA843, HA851 p-value=0.06723


#LC3(5-500)#################################################################################


#CTRL N1 LC3_5_500
data_CTRL %>% select(Prep, Day, Treatment, Condition, LC3_5_500_per_cell_average) %>%
  filter(Condition == "N1") %>% spread(Day, LC3_5_500_per_cell_average) -> data_CTRL_N1_t_test

boxplot(data_CTRL_N1_t_test$D2, data_CTRL_N1_t_test$D4)

#Ho: Mean diff D2xD4 is 0
# one-sided
t.test(data_CTRL_N1_t_test$D2, data_CTRL_N1_t_test$D4, mu=0, alt="less", paired=T) # with HA842, HA843, HA851 p-value=0.2318



#CTRL LG LC3_5_500
data_CTRL %>% select(Prep, Day, Treatment, Condition, LC3_5_500_per_cell_average) %>%
  filter( Condition == "LG") %>% 
  spread(Day, LC3_5_500_per_cell_average) -> data_CTRL_LG_t_test

boxplot(data_CTRL_LG_t_test$D2, data_CTRL_LG_t_test$D4)

#Ho: Mean diff D2xD4 is 0
# one-sided
t.test(data_CTRL_LG_t_test$D2, data_CTRL_LG_t_test$D4, mu=0, alt="less", paired=T) # with HA842, HA843, HA851 p-value=0.08478



#CTRL NG LC3_5_500
data_CTRL %>% select(Prep, Day, Treatment, Condition, LC3_5_500_per_cell_average) %>%
  filter( Condition == "NG") %>% 
  spread(Day, LC3_5_500_per_cell_average) -> data_CTRL_NG_t_test

boxplot(data_CTRL_NG_t_test$D2, data_CTRL_NG_t_test$D4)

#Ho: Mean diff D2xD4 is 0
# one-sided
t.test(data_CTRL_NG_t_test$D2, data_CTRL_NG_t_test$D4, mu=0, alt="less", paired=T) # with HA842, HA843, HA851 p-value=0.009


#LC3(>500)#################################################################################



#CTRL N1 LC3_>500
data_CTRL %>% select(Prep, Day, Treatment, Condition, LC3_over500_per_cell_average) %>%
  filter(Condition == "N1") %>% spread(Day, LC3_over500_per_cell_average ) -> data_CTRL_N1_t_test

boxplot(data_CTRL_N1_t_test$D2, data_CTRL_N1_t_test$D4)

#Ho: Mean diff D2xD4 is 0
# one-sided
t.test(data_CTRL_N1_t_test$D2, data_CTRL_N1_t_test$D4, mu=0, alt="less", paired=T) # with HA842, HA843, HA851 p-value=0.3527



#CTRL LG LC3_>500
data_CTRL %>% select(Prep, Day, Treatment, Condition, LC3_over500_per_cell_average) %>%
  filter( Condition == "LG") %>% 
  spread(Day, LC3_over500_per_cell_average) -> data_CTRL_LG_t_test

boxplot(data_CTRL_LG_t_test$D2, data_CTRL_LG_t_test$D4)

#Ho: Mean diff D2xD4 is 0
# one-sided
t.test(data_CTRL_LG_t_test$D2, data_CTRL_LG_t_test$D4, mu=0, alt="less", paired=T) # with HA842, HA843, HA851 p-value=0.2113



#CTRL LG LC3_>500
data_CTRL %>% select(Prep, Day, Treatment, Condition, LC3_over500_per_cell_average) %>%
  filter( Condition == "NG") %>% 
  spread(Day, LC3_over500_per_cell_average) -> data_CTRL_NG_t_test

boxplot(data_CTRL_NG_t_test$D2, data_CTRL_NG_t_test$D4)

#Ho: Mean diff D2xD4 is 0
# one-sided
t.test(data_CTRL_NG_t_test$D2, data_CTRL_NG_t_test$D4, mu=0, alt="less", paired=T) # with HA842, HA843, HA851 p-value=0.2016


