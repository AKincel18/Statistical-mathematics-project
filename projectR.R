#WCZYTYWANIE DANYCH
#setwd("D:\\Studia\\MS\\PROJEKT")
fileDolnoslaskie<-read.table("dane1.txt")
fileLubuskie<-read.table("dane2.txt")


################################################################
###                        ZADANIE 1                         ###
################################################################

#funkcja na mode
getmode <- function(v1) {
  uniqv <- unique(v1)
  uniqv[which.max(tabulate(match(v1, uniqv)))]
}

#D?ugo?? pliku
Ddlug = length(fileDolnoslaskie$V1)
Ldlug = length(fileLubuskie$V1)

#?rednia arytmetyczna jest
Dsredniaarytmetyczna = mean(fileDolnoslaskie$V1)
Lsredniaarytmetyczna = mean(fileLubuskie$V1)

#Mediana jest
Dmedian = median(fileDolnoslaskie$V1)
Lmedian = median(fileLubuskie$V1)

#Moda
Dmode <- getmode(fileDolnoslaskie$V1)
Lmode <- getmode(fileLubuskie$V1)

#Kwantyle
Dquant1=quantile(fileDolnoslaskie$V1,c(0.25))
Dquant3=quantile(fileDolnoslaskie$V1,c(0.75))
Lquant1=quantile(fileLubuskie$V1,c(0.25))
Lquant3=quantile(fileLubuskie$V1,c(0.75))

#Max i Min
Dmax = max(fileDolnoslaskie$V1)
Dmin = min(fileDolnoslaskie$V1)
Lmax = max(fileLubuskie$V1)
Lmin = min(fileLubuskie$V1)


#Wariancja dla estymatora nieobci??onego jest
DwariancjaNO = var(fileDolnoslaskie$V1)
LwariancjaNO = var(fileLubuskie$V1)

#Wariancja dla estmatora obci??onego
DwariancjaOB = DwariancjaNO * (Ddlug-1)/Ddlug 
LwariancjaOB = LwariancjaNO * (Ldlug-1)/Ldlug 

#Odchylenie Standardowe dla estymatora nieobci??onego jest
Dodch_standNO = sd(fileDolnoslaskie$V1)
Lodch_standNO = sd(fileLubuskie$V1)

#Odchylenie Standardowe dla estymatora obi??onego
Dodch_standOB = sqrt(DwariancjaOB)
Lodch_standOB = sqrt(LwariancjaOB)

#Odchylenie ?rednie jest
Dodch_srednie =sum(abs(fileDolnoslaskie$V1-Dsredniaarytmetyczna))/Ddlug
Lodch_srednie =sum(abs(fileLubuskie$V1-Lsredniaarytmetyczna))/Ldlug

#Wsp??czynnik zmienno?ci dla estymatora obci??onego  jest
Dwsp_zmiennOB=Dodch_standOB/Dsredniaarytmetyczna
Lwsp_zmiennOB = Lodch_standOB/Lsredniaarytmetyczna

#Wsp??czynnik zmienno?ci dla estymatora nieobci??onego
Dwsp_zmiennNO=Dodch_standNO/Dsredniaarytmetyczna
Lwsp_zmiennNO = Lodch_standNO/Lsredniaarytmetyczna

#Rozst?p
Drozstep= Dmax - Dmin
Lrozstep = Lmax - Lmin

#Wsp?lczynnik sko?no?ci
Dwsp_skos = (Dsredniaarytmetyczna - Dmode)/Dodch_standOB
Lwsp_skos = (Lsredniaarytmetyczna - Lmode)/Lodch_standOB


#Odchylenie ?wiartkowe jest
Dodchy_cw = (Dquant3-Dquant1)/2
Lodchy_cw = (Lquant3-Lquant1)/2

#Funkcja do obliczania momentu centralnego
fMomCent <- function(a,n)
{
  dlug <- length(a)
  srednia <- sum(a) / dlug
  wynik <- 0
  for(i in 1:dlug) {
    wynik <- wynik + (a[i] - srednia) ^ n
  }
  wynik <- wynik / dlug
  return(wynik)
}

#Kurtoza
Dkurtoza <- fMomCent(fileDolnoslaskie$V1, 4)/ Dodch_standOB ^ 4
Lkurtoza <- fMomCent(fileLubuskie$V1, 4)/ Lodch_standOB ^ 4


#Eksces
Deksces = Dkurtoza - 3
Leksces = Lkurtoza - 3

#SZEREGI ROZDZIELCZE

#Ilosc przedzialow kasowych K
DK = round(sqrt(Ddlug))
LK = round(sqrt(Ldlug))

#Szeroko?? przedzia?u klasowego
DszerPrze = Drozstep / DK
LszerPrze = Lrozstep / LK

#Punkty przeci?cia
DpunktPrzec = seq(Dmin, Dmax + 0.1, by=DszerPrze)
LpunktPrzec = seq(Lmin, Lmax , by=LszerPrze)

#Tworzenie przedzialow
Dprzedzial = cut(fileDolnoslaskie$V1,breaks=7,labels=DpunktPrzec,right=FALSE,include.lowest=TRUE)
abc=length(Dprzedzial[3])
DszRozdz = table(Dprzedzial)
cat(Dprzedzial)
Dhh=hist(fileDolnoslaskie$V1,DpunktPrzec,col="grey",xlab="Wysoko?? plon?w w t/ha",ylab="Cz?sto?? wyst?powania",main="Wojewodztwo Dolno?l?skie",axes=FALSE)
axis(2)
axis(1,at=DpunktPrzec,labels=DpunktPrzec)
Lhh=hist(fileLubuskie$V1,LpunktPrzec,col="grey",xlab="Wysoko?? plon?w w t/ha",ylab="Cz?sto?? wyst?powania",main="Wojewodztwo Lubuskie",axes=FALSE)
axis(2,at=c(0,3,6,9,12,15,18),labels=c(0,3,6,9,12,15,18))
axis(1,at=round(LpunktPrzec,2),labels=round(LpunktPrzec,2))

fMedRozd <- function(h,l,s)
{
  pom=0
  for (i in 1:length(h$counts))
  {
    pom=pom+h$counts[i]
    if (l/2 < pom)
      return (h$breaks[i]+((l/2)-(pom-h$counts[i]))*s/h$counts[i])
  }
}
#Mediana 
DmedianRozd <- fMedRozd(Dhh,Ddlug,DszerPrze)
LmedianRozd <- fMedRozd(Lhh,Ldlug,LszerPrze)
fModRozd <- function(h,l,s)
{
  pom=0
  for (i in 1:length(h$counts))
  {
    pom=pom+h$counts[i]
    if (l/2 < pom)
      return (h$breaks[i]+(h$counts[i]-h$counts[i+1])*s/(2*h$counts[i] - h$counts[i-1] - h$counts[i+1]))
  }
}
#Moda
DmodeRozd <- fModRozd(Dhh,Ddlug,DszerPrze)
LmodeRozd <- fModRozd(Lhh,Ldlug,LszerPrze)

#?rednia arytmetyczna
DsredniaarytmetycznaRozd = sum(Dhh$mids * Dhh$counts)/sum(Dhh$counts)
LsredniaarytmetycznaRozd = sum(Lhh$mids * Lhh$counts)/sum(Lhh$counts)

#Wariancja
DwariancjaOBRozd = sum(Dhh$mids^2 *Dhh$counts)/sum(Dhh$counts) - DsredniaarytmetycznaRozd^2
LwariancjaOBRozd = sum(Lhh$mids^2 *Lhh$counts)/sum(Lhh$counts) - LsredniaarytmetycznaRozd^2

#Odchylenie standardowe
Dodch_standOBRozd = sqrt(DwariancjaOBRozd)
Lodch_standOBRozd = sqrt(LwariancjaOBRozd)

#Wspolczynnik zmienno?ci
Dwsp_zmiennOBRozd = Dodch_standOBRozd/DsredniaarytmetycznaRozd
Lwsp_zmiennOBRozd = Lodch_standOBRozd/LsredniaarytmetycznaRozd

#Wspolczynnik skosnosci
Dwsp_skosRozd = (sum(((Dhh$mids - DsredniaarytmetycznaRozd)^3)*Dhh$counts)/Ddlug)/(Dodch_standOBRozd^3)
Lwsp_skosRozd= (sum(((Lhh$mids - LsredniaarytmetycznaRozd)^3)*Lhh$counts)/Ldlug)/(Lodch_standOBRozd^3)

#Odchylenie srednie
Dodch_srednieRozd = sum(abs((Dhh$mids - DsredniaarytmetycznaRozd))*Dhh$counts)/Ddlug
Lodch_srednieRozd = sum(abs((Lhh$mids - LsredniaarytmetycznaRozd))*Lhh$counts)/Ldlug

#Kurtoza
DkurtozaRozd = (sum(((Dhh$mids - DsredniaarytmetycznaRozd)^4)*Dhh$counts)/Ddlug)/(Dodch_standOBRozd^4)
LkurtozaRozd = (sum(((Lhh$mids - LsredniaarytmetycznaRozd)^4)*Lhh$counts)/Ldlug)/(Lodch_standOBRozd^4)

#Eksces
DekscesRozd = DkurtozaRozd-3
LekscesRozd = LkurtozaRozd-3

fKwartRozd <- function(h,l,s,nr)
{
  pom=0
  for (i in 1:length(h$counts))
  {
    pom=pom+h$counts[i]
    if (l*nr/4 < pom)
      return (h$breaks[i]+((l*nr/4)-(pom-h$counts[i]))*s/h$counts[i])
  }
}
#Kwartyle
#0.25
Dquant1Rozd <- fKwartRozd(Dhh,Ddlug,DszerPrze,1)
Lquant1Rozd <- fKwartRozd(Lhh,Ldlug,LszerPrze,1)
#0.75
Dquant3Rozd <- fKwartRozd(Dhh,Ddlug,DszerPrze,3)
Lquant3Rozd <- fKwartRozd(Lhh,Ldlug,LszerPrze,3)
################################################################
###                        ZADANIE 2                         ###
################################################################

fZad2 <- function(dane)
{
  
  dane = sort(dane) # sortowanie
  n = length(dane) # Ilosc elementow
  
  D = 0.886/sqrt (length(dane)) #dane z tabelki
  
  
  p = pnorm((dane - mean(dane))/sd(dane)) #F0(x)- funkcja rozkladu normalnego
  
  # seq - Sequence Generation
  Dplus = max(seq(1:n)/n - p)
  Dminus = max(p - (seq(1:n) - 1)/n)
  Dn = max(Dplus, Dminus)
  cat("D wynosi:   ", D, "\n")
  cat("Dn wynosi:   ", Dn, "\n")
  if(Dn < D) 
  { cat("Wysokosci plonow pszenicy maja rozklad normalny.\n") } 
  else 
  { cat("Wysokosci plonow pszenicy nie maja rozkladu normalnego.\n") }
  
}
writeLines("H0 - rozklad normalny\n")
writeLines("H1 - brak rozkladu normalnego\n")

writeLines("dla Dolno?l?skiego:")
fZad2(fileDolnoslaskie$V1)

writeLines("dla Lubuskiego:")
fZad2(fileLubuskie$V1)
################################################################
###                        ZADANIE 3                         ###
################################################################
###                         model 2                          ###
################################################################
fZad3 <- function()
{
  #dane
  D_wspol_istot = 0.05
  D_H0 = 4.7
  
  D_U = ((Dsredniaarytmetyczna - D_H0)/ Dodch_standOB) * sqrt(Ddlug-1)
  
  D_k_lewy =qt((D_wspol_istot/2),Ddlug-1)  
  D_k_prawy = qt((1-D_wspol_istot/2), Ddlug-1)  
  
  if(D_U <= D_k_lewy || D_U >= D_k_prawy){
    writeLines("Istniej? podstawy do odrzucenia hipotezy H0 na rzecz hiptezy alternatywnej\n")
  }else{
    writeLines("Brak podstaw do odrzuenia hipotezy H0")
  }
}
fZad3()





################################################################
###                        ZADANIE 4                         ###
################################################################
###                         model 1                          ###
################################################################
fZad4 <- function()
{
  #dane
  L_wspol_istot = 0.05
  L_H0 = 0.6
  
  L_chi_kwadrat = (Ldlug*(Lodch_standOB^2))/(L_H0^2)
  
  L_chi_lewy = qchisq(L_wspol_istot/2,Ldlug-1) #31.5549
  
  L_chi_prawy = qchisq(1-L_wspol_istot/2,Ldlug-1) #70.2224
  
  
  if((L_chi_kwadrat <= L_chi_lewy && L_chi_kwadrat > 0) || L_chi_kwadrat >= L_chi_prawy){
    writeLines("Istniej? podstawy do odrzucenia hipotezy H0 na rzecz hiptezy alternatywnej\n")
  }else{
    writeLines("Brak podstaw do odrzuenia hipotezy H0")
  }
}

fZad4()


################################################################
###                        ZADANIE 5                         ###
################################################################

#HIPOTEZA ZEROWA: Wysoko?? plon?w pszenicy w gospodarstwach 
#wojew?dztwa lubuskiego jest wi?ksza ni? w Dolno?l?skim


#HIPOTEZA ALTERNATYWNA: Wysoko?? plon?w pszenicy w gospodarstwach 
#wojew?dztwa lubuskiego nie jest wi?ksza ni? w Dolno?l?skim


fZad5_1 <- function()
{
  #Dane
  alfa = 0.05
  #Fisher model 2
  
  t=(Lsredniaarytmetyczna - Dsredniaarytmetyczna)/sqrt((((Ddlug*(Dodch_standOB)^2)+(Ldlug*(Lodch_standOB)^2))/(Ddlug+Ldlug-2))*(Ddlug+Ldlug)/(Ddlug*Ldlug))
  qt1=qt(alfa/2,Ddlug+Ldlug-2)
  qt2=qt(1-(alfa/2),Ddlug+Ldlug-2)
  if(t<=qt1 || t>=qt2)
  {
    writeLines("Mozna odrzucic na rzecz alternatywnej")
    writeLines("Wynika z tego, ze wariancje sa rozne")
    writeLines("Mozemy uzyc modelu 3 - Coxa")
    
    #Wywolanie funkcji Koxa
    fZad5_2()
  }else
  {
    writeLines("Nie mozna odrzucic hipotezy zerowej na rzecz alternatywnej")
    writeLines("Nie mozemy stwierdzic, ze wysokosc plonow w wojewodztwie Lubuskim jest wieksza")
  }
  
  #Cox model 3
  
}
fZad5_2 <- function()
{
  C = (Lsredniaarytmetyczna-Dsredniaarytmetyczna )/ (sqrt((((Dodch_standNO ^2) / Ddlug) + (Lodch_standNO ^2) / Ldlug) ))
  cp = ((Dodch_standNO^2/Ddlug)*qt(1-alfa,Ddlug-1) + (Lodch_standNO^2/Ldlug)*qt(1-alfa,Ldlug-1))/((Dodch_standNO^2/Ddlug)+(Lodch_standNO^2/Ldlug))
  if (C>=cp)
  {
    writeLines("Mozemy odrzucic hipoteze zerowa na rzecz alternatywnej")
    writeLines("Mozemy wiec stwierdzi?, ?e wysokosc plonow w wojewodztwie Lubuskim jest wieksza niz w Dolnoslaskim")
  }
}
fZad5_1()
################################################################
#Lodch_przecietne = sqrt(Lwariancja)/sqrt(length(fileLubuskie$V1))

#cat(format('ja', width = 20), format('ja', width = 151))
#cat(sort(fileLubuskie$V1))
#cat(Dprzedzial)
#rm(list=ls())


