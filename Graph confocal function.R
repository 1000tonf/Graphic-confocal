#boxplot graph with df=dataframe, a=y axis variable, lab=label y axis, lim sup= limite y axis
box_plot_graph <- function(df, a, f, lab, lim){
  theme_settings = theme(axis.line=element_line(size=1, colour="black"),axis.text=element_text(size=10, color="black", face=2),axis.title=element_text(face=2), panel.background=element_rect(fill="white"))
  scale_fill_settings = scale_fill_grey(start=0.9, end=0.55)
  scale_x_settings = scale_x_discrete(limits=c("N1","LG","NG"))
  boxplot_settings = geom_boxplot(position=position_dodge(1))
  dotplot_settings = geom_dotplot(binaxis = "y", stackdir="center", dotsize=.3, position=position_dodge(1))
  ggplot(df, aes(x=Condition, y=a, fill = f)) + ylab(lab) + ylim(0,lim) + boxplot_settings + dotplot_settings + scale_fill_settings + scale_x_settings + theme_settings  
}

box_plot_graph(i, i$LAMP1_5_500_per_cell_average, i$Day, "LAMP1(5-500)/Cell", 500)