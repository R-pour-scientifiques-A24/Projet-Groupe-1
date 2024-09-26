freqvei<-table(volcan$vei)
freqvei
barplot(freqvei)
boxplot(volcan$vei)

min(volcan$start_year)
max(volcan$start_year)
hist(volcan$start_year, xlim=c(-11345,2020), breaks=50)
hist(volcan$start_year, xlim=c(1500,2020), breaks=1000)
        