
tsF = run_lakemodel(param,0.01)
t = seq(0,129600/(60*24),by=1/24)
N.ts <- data.frame(Days = t, Concentration = tsF[,1], Var = rep("Nutrients",length(t)))
P.ts <- data.frame(Days = t, Concentration = tsF[,2], Var = rep("Phytoplankton",length(t)))
i.ts <- data.frame(Days = t, Concentration = tsF[,3], Var = rep("Inflow",length(t)))
DATA.ts <- rbind(P.ts, N.ts, i.ts)

p <- ggplot(data = DATA.ts, aes(x=Days,y=Concentration)) + geom_line()+facet_grid(Var ~ ., scales = "free") + xlim(c(0,50))
p

