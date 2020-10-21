
###########################################################################
## Sæt working directory

## I RStudio kan man nemt sætte working directory med menuen 
## "Session -> Set Working Directory -> To Source File Location" 
## Bemærk: i R bruges kun "/" til separering i stier 
## (altså ingen backslash).
setwd("C:\\Users\\bruger\\Desktop\\Mapper\\BMI Projekt\\bmi1")
install.packages("rmarkdown")


###########################################################################
## Indlæs data

## Indlæs data fra bmi1_data.csv
D <- read.table("bmi1_data.csv", header=TRUE, sep=";", as.is=TRUE)


###########################################################################
## Simpel opsummering af data
## Opgave a)

## Dimensionen af D (antallet af rækker og søjler)
dim(D)
## Søjle-/variabelnavne
names(D)
## De første rækker/observationer
head(D)
## De sidste rækker/observationer
tail(D)
## Udvalgte opsummerende størrelser
summary(D)
## En anden type opsummering af datasættet
str(D)


###########################################################################
## Beregn BMI

## Beregn BMI og tilføj som ny variabel i D
D$bmi <- D$weight/(D$height/100)^2


###########################################################################
## Histogram (empirisk tæthed)
## Opgave b)

## Histogram der beskriver den empiriske tæthed for BMI
## (histogram for BMI normaliseret så arealet er lig 1)
hist(D$bmi, xlab="BMI", prob=TRUE)


###########################################################################
## Deldatasæt vha. 'subset'

## Opdel i to deldatasæt (hhv. kvinder og mænd)
Dfemale <- subset(D, gender == 0)
Dmale <- subset(D, gender == 1)


###########################################################################
## Density histogrammer for kvinder hhv. mænd
## Opgave c)

## Density histogrammer der beskriver den empiriske
## tæthed for BMI for hhv. kvinder og mænd
hist(Dfemale$bmi, main = "Histogram of Density Females BMI", col = "red",xlab="BMI (kvinder)", prob=TRUE)
hist(Dmale$bmi, main = "Histogram of Density Males BMI", xlab="BMI (mænd)", col = "blue" , prob=TRUE)


###########################################################################
## Boxplot opdelt efter køn
## Opgave d)

## Boxplot af BMI opdelt efter køn
boxplot(Dfemale$bmi, Dmale$bmi, names=c("Kvinder", "Mænd"), 
        xlab="Køn", col=c('red','blue'), ylab="BMI")


###########################################################################
## Opsummerende størrelser for BMI
## Opgave e)

## Antal observationer i alt
## (medregner ej eventuelle manglende værdier)
sum(!is.na(D$bmi))
## Antal observationer for kvinder
sum(!is.na(Dfemale$bmi))
## Antal observationer for mænd
sum(!is.na(Dmale$bmi))
## Stikprøvegennemsnit (ej kønsopdelt)
mean(D$bmi, na.rm=TRUE)
## Stikprøvevarians (ej kønsopdelt)
var(D$bmi, na.rm=TRUE)
## Stikprøvegennemsnit for kvinder
mean(Dfemale$bmi, na.rm = TRUE)
## Stikprøvevarians for kvinder
var(Dfemale$bmi, na.rm = TRUE)
## Stikprøvegennemsnit for mænd
mean(Dmale$bmi, na.rm = TRUE)
## Stikprøvevarians for mænd
var(Dmale$bmi, na.rm = TRUE)
## Standardafvigelse i alt
sd (D$bmi)
## Standardafvigelse for kvinder
sd (Dfemale$bmi, na.rm = TRUE)
## Standardafvigelse for mænd
sd (Dmale$bmi, na.rm = TRUE)
## Nedre Kvartil Øvre Kvartil og Median i alt
summary(D$bmi)
## Nedre Kvartil Øvre Kvartil og Median for kvinder
summary(Dfemale$bmi)
## Nedre Kvartil Øvre Kvartil og Median for mænd
summary (Dmale$bmi)

quantile(Dfemale$bmi)
quantile(Dmale$bmi)
## osv. 
##
## Argumentet 'na.rm=TRUE' sørger for at størrelsen
## udregnes selvom der eventuelt er manglende værdier


###########################################################################
## qq-plot til modelkontrol
## Opgave f)

## Ny variabel 'logbmi' med log-transformeret BMI
D$logbmi <- log(D$bmi)
## qq-plot for log-transformeret BMI
qqnorm(D$logbmi)
qqline(D$logbmi)

logvarbmi<-var(D$logbmi)
logmeanbmi<-mean(D$logbmi)

mean(D$bmi)
var(D$bmi)

layout(matrix(c(1,2), 1,2))

qqnorm(D$logbmi, main = "QQ plot af normalfordeling")
hist(D$logbmi, main = "Histogram af normalfordeling", xlab = "BMI", prob=TRUE, col = "pink")

###########################################################################
## Beregning af konfidensintervaller
## Opgave g)

plusLogBMI <- mean(D$logbmi)+(qt(0.975,length(D$logbmi)-1))*(sd(D$logbmi)/sqrt(145))
minusLogbmi <- mean(D$logbmi)-(qt(0.975,length(D$logbmi)-1))*(sd(D$logbmi)/sqrt(145))

exp(mean(D$logbmi))

plusBmi <- exp(plusLogBMI)
minusBmi <- exp(minusLogbmi)

###########################################################################
## Udregning af Hypoteste test
## Opgave h)

n <- length(D$logbmi)
## t værdien bliver beregnet
tobs <- ((mean(D$logbmi)-log(25))/(sd(D$logbmi)/sqrt(n)))
## beregner p-værdien
pvalue <- 2 * (1-pt(abs(tobs), df=n-1))
pvalue

## T-test for en enkelt stikprøve
## Opgave h) Given kode til kontrol 

sd(D$logbmi)

t.test(D$logbmi, mu=log(25))

## T-test for en enkelt stikprøve foretaget på log-transformeret BMI
t.test(D$logbmi, mu=log(25))


###########################################################################
## Statistiske modeller og qqplot
## Opgave i)

Dmale$logbmi <- log(Dmale$bmi)
Dfemale$logbmi <- log(Dfemale$bmi)

meanMaleLogBMI <- mean(Dmale$logbmi)
meanFemaleLogBMI <- mean(Dfemale$logbmi)
varMaleLogBMI <- var(Dmale$logbmi)
varFemaleLogBMI <- var(Dfemale$logbmi)

layout(matrix(c(1,2), 1,2))

qqnorm(Dmale$logbmi, main = "Log BMI for mænd")
qqline(Dmale$logbmi)

qqnorm(Dmale$bmi, main = "BMI for mænd")
qqline(Dmale$bmi)

layout(matrix(c(1,2), 1,2))

qqnorm(Dfemale$logbmi, main = "Log BMI for kvinder")
qqline(Dfemale$logbmi)

qqnorm(Dfemale$logbmi, main = "BMI for kvinder")
qqline(Dfemale$logbmi)

###########################################################################
## Konfidensinterval (KI) for middelværdi og median
## Opgave j)

sd(Dmale$logbmi)
length(Dmale$logbmi)

sd(Dfemale$logbmi)
length(Dfemale$logbmi)

## interval for mænd
PlusMale <- mean(Dmale$logbmi)+(qt(0.975,length(Dmale$logbmi)-1))*(sd(Dmale$logbmi)/sqrt(length(Dmale$logbmi)))
MinusMale <- mean(Dmale$logbmi)-(qt(0.975,length(Dmale$logbmi)-1))*(sd(Dmale$logbmi)/sqrt(length(Dmale$logbmi)))

PlusMale
MinusMale

exp(PlusMale)
exp(MinusMale)

##interval for kvinder 
PlusFemale <- mean(Dfemale$logbmi)+(qt(0.975,length(Dfemale$logbmi)-1))*(sd(Dfemale$logbmi)/sqrt(length(Dfemale$logbmi)))
MinusFemale <- mean(Dfemale$logbmi)-(qt(0.975,length(Dfemale$logbmi)-1))*(sd(Dfemale$logbmi)/sqrt(length(Dfemale$logbmi)))

PlusFemale
MinusFemale

exp(PlusFemale)
exp(MinusFemale)

qt(0.975, length(Dfemale$logbmi)-1)
qt(0.975, length(Dmale$logbmi)-1)

## Udtag data kun for kvinder
## Bruges som kontrol
Dfemale <- subset(D, gender == 0)
## KI for middelværdien af log-BMI for kvinder
KI <- t.test(Dfemale$logbmi, conf.level=0.95)$conf.int
KI
## Transformer tilbage for at få KI for median BMI for kvinder
exp(KI)


###########################################################################
## Welch t-test for sammenligning af to stikprøver
## Opgave k

sdMaleLogBMI <- sd(Dmale$logbmi)
sdFemaleLogBMI <- sd(Dfemale$logbmi)

tobs <- abs(((meanMaleLogBMI-meanFemaleLogBMI)-0)/(sqrt((sdMaleLogBMI^2/length(Dmale$logbmi))+(sdFemaleLogBMI^2/length(Dfemale$logbmi)))))

v <- ((sdMaleLogBMI^2/length(Dmale$logbmi))+(sdFemaleLogBMI^2/length(Dfemale$logbmi)))^2/(((sdMaleLogBMI^2/length(Dmale$logbmi))^2/(length(Dmale$logbmi)-1))+((sdFemaleLogBMI^2/length(Dfemale$logbmi))^2/(length(Dfemale$logbmi)-1)))

tobs
v

2*(1-pt(tobs, v))

## Sammenligning af logBMI for kvinder og mænd
t.test(D$logbmi[D$gender == 0], D$logbmi[D$gender == 1])


###########################################################################
## Beregning af korrelation
## Opgave l

install.packages("dplyr")

install.packages("ggplot2")

bmiData <- D %>%
  group_by(gender) %>%
  summarize (meanbmi = mean(bmi), sdbmi = sd(bmi), countbmi=n(), sebmi = (sdbmi/(sqrt(countbmi))))

bmiData[["gender"]] <- c("Female", "Male")

bmiPlot <- ggplot(bmiData, aes(x=gender, y=meanbmi, fill=gender)) +
  geom_bar(stat = "identity", position = position_dodge())  +
  geom_errorbar(aes(ymin=meanbmi-sebmi, ymax=meanbmi+sebmi), width = .2) +
  scale_fill_manual(values=c('red','blue'), guide=FALSE) +
  labs(x="Køn", y="BMI") + 
  coord_cartesian(ylim=c(20,27)) +
  theme_classic()

bmiPlot

## Beregning af korrelation mellem udvalgte variable
cor(D[,c("weight","fastfood","bmi")], use="pairwise.complete.obs")

  
###########################################################################
## Beregning af korrelation
## Opgave m

Kovarians_bmiWeight <- 1/(length(D$bmi)-1)*sum((D$bmi-mean(D$bmi))*(D$weight-mean(D$weight)))
Kovarians_bmiWeight

Korrelationskeoficient_bmiWeight <- Kovarians_bmiWeight/(sd(D$bmi)*sd(D$weight))
Korrelationskeoficient_bmiWeight

Kovarians_bmifast <- 1/(length(D$bmi)-1)*sum((D$bmi-mean(D$bmi))*(D$fastfood-mean(D$fastfood)))
Kovarians_bmifast

Korrelationskeoficient_bmifast <- Kovarians_bmifast/(sd(D$bmi)*sd(D$fastfood))
Korrelationskeoficient_bmifast

Kovarians_weightfast <- 1/(length(D$weight)-1)*sum((D$weight-mean(D$weight))*(D$fastfood-mean(D$fastfood)))
Kovarians_weightfast

Korrelationskeoficient_weightfast <- Kovarians_weightfast/(sd(D$weight)*sd(D$fastfood))
Korrelationskeoficient_weightfast

layout(matrix(c(1,2,3), 1,3))

plot(D$bmi, D$weight, main = "BMI and Weight", xlab="BMI", ylab = "Weight", pch = 19, frame = FALSE)
plot(D$bmi, D$fastfood, main = "BMI and Fastfood", xlab="BMI", ylab = "Fastfood", pch = 19, frame = FALSE)
plot(D$weight, D$fastfood, main = "Weight and Fastfood", xlab="Weight", ylab = "Fastfood", pch = 19, frame = FALSE)
###########################################################################
## Delmængder i R

## Ekstra bemærkning om måder at udtage delmænger i R
##
## En logisk (logical) vektor med sandt (TRUE) eller falsk (FALSE) for 
## hver værdi i en kolonne i D - f.eks: Find alle kvinder i datasættet
D$gender == 0
## Vektoren kan bruges til at udtage data for kvinderne
D[D$gender == 0, ]
## Alternativt kan man bruge funktionen 'subset'
subset(D, gender == 0)
## Mere komplekse logiske udtryk kan laves, f.eks.: 
## Find alle kvinder under 55 kg
subset(D, gender == 0 & weight < 55)


###########################################################################
## Flere R-tips

## Lav en for-løkke med beregning af et par opsummerende størrelser
## og gem resultatet i en ny data.frame
Tbl <- data.frame()
for(i in 0:1){
  Tbl[i+1, "mean"] <- mean(D$bmi[D$gender == i])
  Tbl[i+1, "var"] <- var(D$bmi[D$gender == i])
}
row.names(Tbl) <- c("Kvinder","Mænd")
## Se hvad der er i Tbl
Tbl

## I R er der endnu mere kortfattede måder sådanne udregninger kan 
## udføres. For eksempel
aggregate(D$bmi, by=list(D$gender), function(x){ 
  c(mean=mean(x), var=var(x)) 
})
## Se flere smarte funktioner med: ?apply, ?aggregate og ?lapply
## og for ekstremt effektiv databehandling se f.eks. pakkerne: dplyr,
## tidyr, reshape2 og ggplot2.

## LaTeX tips:
##
## R-pakken "xtable" kan generere LaTeX tabeller og skrive dem direkte 
## ind i en fil, som derefter kan inkluderes i et .tex dokument.
## 
## R-pakken "knitr" kan anvendes meget elegant til at lave et .tex 
## dokument der inkluderer R-koden direkte i dokumentet. Dette 
## dokument og bogen er lavet med knitr.

