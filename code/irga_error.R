dat = subset(data.all$AT_faster, species == "RASA")

unique(dat$curveID)

dat.a = subset(dat, curveID == 75)


dat.a$CO2_s - dat.a$CO2_r


unique(data.all$AT_faster$curveID)

a = subset(data.all$AT_faster, curveID == 24)
a$co2_adj

tot = 0
for (i in unique(data.all$AT_faster$curveID)) {
  curdat = subset(data.all$AT_faster, curveID == i)
  len = dim(curdat)[1]
  adj = curdat$co2_adj[len]
  del = curdat$CO2_s[len] - curdat$CO2_r[len]
  print(adj/del)
  tot = tot + abs(adj/del)
}

tot/12


ggplot(data = a, aes(x = Tleaf, y = A)) + geom_point()


