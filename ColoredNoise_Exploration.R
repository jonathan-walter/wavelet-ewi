
#install.packages("devtools")
#install.packages("Rcpp")
#devtools::install_github("japilo/colorednoise")
require(colorednoise)
require(wsyn)

daily.phi = 0.5
min.per.month = 43829.0639
min.per.day = 1440
days = 100

a = colored_noise(timesteps = days, mean = 0, sd = 1, phi = daily.phi)
b = colored_noise(timesteps = days*min.per.day, mean = 0, sd = 1, phi =  daily.phi^(1/(min.per.day)))

plotmag(wt(a-mean(a),1:length(a)))
c <- b[seq(1,days*min.per.day,1440)]
plotmag(wt(c-mean(c),1:length(c)))



     