library('ompr')
library('magrittr')
library('ROI.plugin.glpk')
library('ompr.roi')
library('ggplot2')

budget <- 200
amount_for_bench <- 50
RBs <- 2
WRs <- 2
TEs <- 1
QBs <- 1
Flexs <- 1
SFlexs <- 0
DSTs <- 1
Ks <- 1
tm_size <- RBs+WRs+TEs+QBs+Flexs+SFlexs+DSTs+Ks


dsetwd(getSrcDirectory()[1])
data <- read.csv('./data/proj_cost_data_half.csv',stringsAsFactors = FALSE)
nplayers <- length(data$Players)


to_spend <- budget-amount_for_bench
model <- MIPModel() %>%
  add_variable(x[i],i=1:nplayers,type='binary') %>%
  set_objective(sum_expr(x[i] * data$Proj[i], i = 1:nplayers)) %>%
  add_constraint(sum_expr(x[i] * data$RB[i], i = 1:nplayers)>=RBs) %>%
  add_constraint(sum_expr(x[i] * data$WR[i], i = 1:nplayers)>=WRs) %>%
  add_constraint(sum_expr(x[i] * data$TE[i], i = 1:nplayers)>=TEs) %>%
  add_constraint(sum_expr(x[i] * data$QB[i], i = 1:nplayers)>=QBs) %>%
  add_constraint(sum_expr(x[i] * data$QB[i], i = 1:nplayers)<=(QBs+SFlexs)) %>%
  add_constraint(sum_expr(x[i] * data$Flex[i], i = 1:nplayers)<=(SFlexs+Flexs+RBs+WRs+TEs)) %>%
  add_constraint(sum_expr(x[i] * data$K[i], i = 1:nplayers)<=Ks) %>%
  add_constraint(sum_expr(x[i] * data$DST[i], i = 1:nplayers)<=DSTs) %>%
  add_constraint(sum_expr(x[i] * data$Cost[i], i = 1:nplayers)<=to_spend) %>%
  solve_model(with_ROI(solver="glpk", verbose = TRUE))


vec <- get_solution(model,x[i])
team <- data$Players[vec$value==1]
print(team)

tm_cost <- rep(0,tm_size)
tm_proj <- rep(0,tm_size)
for(i in 1:tm_size){
  idx <- which(data$Players==team[i])
  tm_cost[i]<-data$Cost[idx]
  tm_proj[i]<-data$Proj[idx]
}

summary_df <- data.frame('Player'=team,'Cost'=tm_cost,'Proj'=tm_proj)
print(paste('Team Cost: ',as.character(sum(summary_df$Cost)),sep=''))
print(paste('Team Proj: ',as.character(sum(summary_df$Proj)),sep=''))

