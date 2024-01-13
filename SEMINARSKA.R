dataset <- read.csv("Petrol Dataset June 23 2022 -- Version 2.csv", stringsAsFactors=TRUE)
View(dataset)
yearlyGallons = dataset$Yearly.Gallons.Per.Capita
n = length(yearlyGallons)
sort(yearlyGallons)
range(yearlyGallons)
breaks = seq(2, 3684, by=263)
yearlyGallons.int = cut(yearlyGallons, breaks, right=FALSE)
freq = table(yearlyGallons.int)
colors = c("red", "blue", "pink", "green", "purple", "cyan", "brown")
hist(yearlyGallons, right = FALSE, col=colors, main="Годишна потрошувачка по жител во галони", xlab = "граници на интервали", ylab = "честоти")
rel.freq = freq / n
rel.freq
old = options(digits=2)
rel.freq
old = options(digits = 1)
rel.freq
old = options(digits = 1)
rel.freq
options(old)
cbind(freq, rel.freq)
cum.freq = cumsum(freq)
cbind(freq, rel.freq, cum.freq)
rel.cum.freq = cumsum(freq) / n
rel.cum.freq2 = cumsum(rel.cum.freq)
p.freq = rel.freq * 100
p.cum.freq = rel.cum.freq * 100
cbind(freq, rel.freq, cum.freq, p.freq, p.cum.freq)
yearlyGallons.table = cbind(freq, rel.freq, cum.freq, p.freq, p.cum.freq)
mid = c()
for (i in 1:length(breaks)-1){mid = c(mid, (breaks[i]+breaks[i+1])/2)}
cum.freq0 = c(0, cumsum(freq))
p.cum.freq0 = c(0, p.cum.freq)
plot(breaks, p.cum.freq0, main="Годишна потрошувачка по жител во галони", xlab = "интервали", ylab = "кумулативни честоти во %")
lines(breaks, p.cum.freq0)
cum.rel.freq = cum.freq / n
p.cum.rel.freq = cum.rel.freq * 100
cum.rel.freq = cum.freq / n
old = options(digits = 2)
cum.rel.freq
old = options(digits = 2)
p.cum.rel.freq = cum.rel.freq * 100
p.rel.freq = rel.freq * 100
p.rel.cum.freq = rel.cum.freq * 100
cbind(mid, freq, rel.freq, p.rel.freq, cum.freq, rel.cum.freq, p.rel.cum.freq)
yearlyGallons.table = cbind(mid, freq, rel.freq, p.rel.freq, cum.freq, rel.cum.freq, p.rel.cum.freq)
#install.packages("xlsx")
#library("xlsx")
#write.xlsx(yearlyGallons.table, file = "yearlyGallons.xlsx", append = FALSE)

pricePerGallon=dataset$Price.Per.Gallon..USD.
stem(pricePerGallon, scale = 3)

plot(yearlyGallons, pricePerGallon, xlab = "Годишна потрошувачка по жител во галони", ylab = "Цена по галон во долари")
cor(yearlyGallons, pricePerGallon)

prosek=mean(pricePerGallon)
medijana=median(pricePerGallon)


mode = function(){
  return(sort(-table(pricePerGallon))[1])
}

mode()

quantile(pricePerGallon)
range(pricePerGallon)
IQR(pricePerGallon)
var(pricePerGallon)
sd(pricePerGallon)


primerok = sample(dataset$Price.Per.Liter..USD., size = 30)
primerok
obem.primerok = length(primerok)
obem.primerok
prosek.primerok = mean(primerok)
prosek.primerok
standardnaDevijacija.primerok = sd(primerok)
standardnaDevijacija.primerok

standardnaGreska = prosek.primerok/sqrt(obem.primerok)
standardnaGreska

MarginalnaGreska = qnorm(.975)*standardnaGreska
MarginalnaGreska

prosek.primerok + c(-MarginalnaGreska, MarginalnaGreska)


prosek.populacija = mean(dataset$Price.Per.Liter..USD.)
prosek.populacija

z = (prosek.primerok - prosek.populacija)/(standardnaDevijacija.primerok/sqrt(obem.primerok))
z

alfa = .05
z.half.alfa = qnorm(1-alfa/2)
c(-z.half.alfa, z.half.alfa)


primerok.mal = sample(dataset$Price.Per.Liter..USD., size = 15)
primerok.mal
shapiro.test(primerok.mal)

Liters = dataset$Price.Per.Liter..USD.
Gallons = dataset$Price.Per.Gallon..USD.
regresija = lm(Liters ~ Gallons)
regresija
plot(Gallons, Liters, xlab = "Цена по галон во долари", ylab = "Цена по литар во долари")
abline(regresija)
cor(Liters,Gallons)
summary(regresija)$r.squared




