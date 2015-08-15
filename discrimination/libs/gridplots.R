# for (i in society)
# {
#   temp = subset(statesNames, class==i & race=="green")
#   num.green = round(num.grids^2 * temp$final)
#   temp = subset(statesNames, class==i & race=="blue")
#   num.blue = round(num.grids^2 * temp$final)
#   num.white = num.grids^2 - num.blue-num.green
#   pos = df$class == i
#   temp = c( rep("green", num.green), rep("white", num.white),  rep("blue", num.blue))
#   df$race[pos] = temp
# }
# 
# ggplot(df, aes(x, y, fill=race)) + 
#   geom_tile(colour="white", lwd=1) + 
#   scale_fill_manual(values=c("white", "green","blue")) +
#   theme(panel.background=element_blank(),
#         axis.text=element_blank(),
#         axis.ticks=element_blank(),
#         axis.title=element_blank()) +
#   guides(fill=FALSE) + facet_wrap(~class, ncol=2)

# p = ggplot(statesNames, aes(x=race, y=init, fill=race)) + geom_bar(stat="identity") + facet_wrap(~class, ncol=2)
# p = p + theme_few()
# p = p + theme(axis.text.x=element_blank(), axis.text.y=element_blank(), axis.ticks = element_blank()) + fte_theme()

#Initialise initial distribution
# unemployed = proportion.blue
# blue.distribution = data.frame(upper = min(unemployed, 1/length(society)*racism/(racism+1)), lower = NA)
# unemployed = max(0, proportion.blue - 1/length(society)*racism/(racism+1))
# for (i in 2:length(society))
# {
#   blue.distribution[,i] = min(unemployed, (1/length(society))*racism/(racism+1))
#   unemployed = max(0, unemployed - blue.distribution[,i]) 
# }
# green.distribution = 1/length(society) - blue.distribution


num.grids = 10
df = expand.grid(class = factor(classes), x = 1:num.grids,y = 1:num.grids)
df = arrange(df, class)
df$race = "blue"

