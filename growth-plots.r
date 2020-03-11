# data from https://www.who.int/emergencies/diseases/novel-coronavirus-2019/situation-reports
who = read.csv('who.csv')
who$china = who$cases - who$outside

who$date = as.POSIXlt("21-Jan-2020", format="%d-%b-%Y") + 24*3600*(1:dim(who)[1]-1)

who$new = c(0, diff(who$cases))
who$new.outside = c(0, diff(who$outside))

# Graph from https://www.nytimes.com/interactive/2020/world/asia/china-coronavirus-contain.html
# Data digitized using https://apps.automeris.io/wpd/
nyt = read.csv("nyt.csv")

# ordering is a bit off due digitizing noise
nyt = nyt[order(nyt$day),]

# now we apply an estimated offset to the days in the digitized data
z = approx(nyt$day, nyt$cases, xout=(20:30)+0.5)
z$x = z$x - 19

# and plot

plot(cases/1000 ~ date, who, type='b', xlab='Day', ylab='Cases (x1000)', log='y', ylim=c(0.1,1.3*max(who$cases/1000)))
points(outside/1000 ~ date, who, type='b', col='red')
points(china/1000 ~ date, who, type='b', col='green')
#lines(y~x, z, col='red', type='b')
#legend(2, 14000, legend=c("NYT", "WHO"), fill=c('red','black'), border=NA)
lines(as.POSIXlt("20-Jan-2020", format="%d-%b-%Y")+(1:11)*24*3600, 160*exp((1:11)/2.4)/1000, col='red')
lines(1:11, 160*exp((1:11)/2.4)/1000, col='red')
lines(as.POSIXlt("20-Jan-2020", format="%d-%b-%Y")+(8:19)*24*3600, 1100*exp((8:19)/5)/1000, col='green',lwd=2)

legend(as.POSIXlt("21-Jan-2020", format="%d-%b-%Y"), 150, fill=c('red', 'green'), 
       legend=c("Early growth", "After first containment measures"))

dev.new()
plot(new ~ date, who, xlab="Day", ylab="New cases", type='b', ylim=c(0,5000), cex=0.7)
polygon(c(who$date, max(who$date), min(who$date)), c(who$new.outside,0,0), col=rgb(1,0,0,alpha=0.4), border=F)

polygon(c(who$date, max(who$date), min(who$date)), c(who$new - who$new.outside,0,0), col=rgb(0,1,0,alpha=0.3), border=F)

lines(who$date, who$new.outside, type='b', cex=0.4, pch=24, col=rgb(1,0,0), bg=rgb(1,0,0))
lines(who$date, who$new - who$new.outside, type='b', cex=0.4, pch=23, col=rgb(0,1,0), bg=rgb(0,1,0))

legend(as.POSIXlt("21-Jan-2020", format="%d-%b-%Y"),5000, pch=c(21,24,23), legend=c("Total", "Non-China", "China"), pt.bg=c('white', rgb(1,0,0,alpha=0.7), rgb(0,1,0,alpha=0.9)), col=c('black', rgb(1,0,0,alpha=0.7), rgb(0,1,0,alpha=0.9)))
