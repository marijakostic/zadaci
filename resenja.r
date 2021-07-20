                     ##################################
                     ####     Domaci zadatak      #####
                     ####   Marija Kostic 286/14  #####
                     ##################################
 
                     
                         ##### Prvi zadatak #####

                                 ## (a) ##
                     
# Prvo implementiramo datu funkciju f(x)
                     
f<-function(x)
{
  y<-exp(-x^2/2)*(sin(6*x)^2+3*(cos(x)^2)+(sin(4*x))^2+1)
  return(y)
}
                     
# zatim biramo proizvoljno 1000 tacaka izmedju -5 i5
x<-seq(-5,+5,length.out=1000) 
                     
# i crtamo grafik funkcije f(x)
plot(x,f(x),main = "Grafik funkcije f(x)",xlab="x-osa",type ="p",col="palevioletred2")
lines(x,f(x),col="black")
                     
# Implementiramo gustinu standardne normalne raspodele g(x)
g<-function(x)
{
  y<-exp(-x^2/2)/sqrt(2*pi)
  return(y)
}
                     
# Razmotricemo odnos f(x)/g(x): 
#         f(x)/g(x)=sqrt(2*pi)*(sin(6*x)^2+3*(cos(x)^2)+sin(4*x)^2+1)
# Posto su trigonometrijske funkcije ogranicene odozgo i kolicnik f(x)/g(x)
# ce biti ogranicen odozgo.
# Tu gornju granicu cemo pronaci pomocu funkcije optimize.
                     
# Funkcija na osnovu koje radimo optimizaciju
h<-function(x)
{
  fx<-exp(-x^2/2)*(sin(6*x)^2+3*(cos(x)^2)+(sin(4*x))^2+1)
  gx<-exp(-x^2/2)/sqrt(2*pi)
  fx/gx
}
M<-optimize(h, interval=c(0,1),maximum=T)$objective
                     
# Ispisujemo dobijenu gornju granicu
M 
                     
# Sada kada smo odredili gornju granicu M,
# proveravamo da li se gustina standardne normalne raspodele moze 
# koristiti kao pomocna gustina u ovom algoritmu.
# Crtamo grafik funkcije M*g(x)
g1<-M*exp(-x^2/2)/sqrt(2*pi)
lines(x,g1,col="black")
                     
# Iz dobijenog grafika vidimo da g(x) moze biti pomocna funkcija u 
# algoritmu za generisanje slucajnih brojeva.
                     

                                 ## (b) ##
# Acceptance Rejection metoda: 
# X stimulisemo na sledeci nacin:
       # generisemo slucajan br y iz raspodele G *
       # generisemo slucajan br u iz U[0,1]
       # ako je u< ili = od f(y)/M*g(y) prihvatamo ga kao slucajn br iz raspodele,
       # inace se vracamo ispocetka*

# Generisemo slucajne brojeve koriscenjem Acceptance Rejection metoda
# iz gustine date izrazom f(x)

Nsimulacija<-2500

# Uvodimo promenljivu koja ce nam brojati pokusaje
br_pokusaja<-0 
# zatim uvodimo promenljivu koja ce nam brojati uspehe
br_pronadjenih<-0 
# i prazan vektor u koji cemo smestati dobijene slucajne brojeve.
SB<-c() 

# Pomocu while petlje generisemo slucajne brojeve.
# Petlja ce se prekinuti kada broj generisanih slucajnih brojeva 
# koji ispunjavaju uslov: u< ili = od f(y)/M*g(y)
# bude jednak trazenom broju 2500.

# U vektor y smestamo generisan slucajan broj iz normalne raspodele, a
# u vektor u smestamo generisan slucajn broj iz ravnomerne raspodele
# Ako je ispunjen uslov: u< ili = od f(y)/M*g(y),
# vrednost y smestamo u vektor SB i brojac pronadjenih br povecavamo za 1

while(br_pronadjenih<Nsimulacija )
{
  y<-rnorm(1) 
  u<-runif(1) #generisemo slucajan broj iz ravnomerne raspodele
  
  if(u<=f(y)/(M*g(y)))
  {
    SB<-c(SB,y)
    br_pronadjenih<-br_pronadjenih+1
  }
  br_pokusaja<-br_pokusaja+1
  
}
#Ispisujemo dobijenih 2500 slucajnih brojeva
SB

                                   ## (v) ##
# DODATAK U WORDU
# Verovatnocu prihvatanja dobijamo kao kolicnik pronadjenih br i br pokusaja
verovatnoca_prihvatanja<-br_pronadjenih/br_pokusaja 
procenat_prihvacenih<-verovatnoca_prihvatanja*100

# Ispisujemo procenat prihvacenih
procenat_prihvacenih

# 
# Na osnovu verovatnoce prihvatanja br od svih generisanih br iz gustine g,
# trazimo normalizujucu konstantu za funkciju f
normalizujuca_konstanta<-1/(verovatnoca_prihvatanja*M)

# Ispisujemo normalizujucu konstantu za funkciju f
normalizujuca_konstanta

# Crtamo histogram generisanih slucajnih brojeva iz gustine date izrazom f(x)
hist(SB,freq=F,breaks=100,,main="Histogram generisanih slucajnih br",col="plum3")
# Uporedjujemo dobijeni histogram sa grafikom normalizovane gustine
SBRng<-seq( min(SB), max(SB), length.out=100)
lines(SBRng,normalizujuca_konstanta * f(SBRng),lwd=2,col="purple4")

                             #### Drugi zadatak ####

                                    ## (a) ##

# Optimalna vrednost za M mora da zadovoljava uslov: Pi(Theta;X)/Pi(Theta)<=M
# gde je Pi(Theta;X) aposteriorna raspodela,a Pi(Theta) apriorna raspodela
# Nakon sredjivanja izraza Pi(Theta;X)/Pi(Theta)<=M, zakljucujemo da nam je
# optimalna vrednost za M maksimalna vrednost funkcije verodostojnosti
# DODATAK U WORDU

                                    ## (b) ##

teta0<-10
n<-10
Nsim<-100
# Za pocetak cemo izvuci n uzoraka iz normalne raspodele N(teta0,1)

Xsamples<-rnorm(n,teta0,1)
# Funkcija na osnovu koje radimo optimizaciju je L=aposteriorna/apriorna gustina

L<-function (teta)
{
  prod(dnorm(Xsamples,teta,1))*pi*sqrt(2*pi)^(length(Xsamples))
}
# Iz dela pod (a) znamo da je optimalna vrednost M maksimum funkcije 
# verodostojnosti po teta i tu vrednost nalazimo preko optimize

M<-optimize(L,interval<-range(Xsamples),maximum=TRUE)$objective
M

# Predstavimo aposteriornu gustinu na sledeci nacin:
posteriorna<-function(teta,Xsamples)
{
  I1<-outer(Xsamples,rep(1,length(teta)))
  I2<-outer(rep(1,length(Xsamples)),teta)
  Rez<-exp(-0.5*(I1-I2)^2)
  apply(Rez,2,prod)/((1+teta^2))
}

# Sada mozemo generisati 100 slucajnih brojeva
# Uvodimo prazan vektor SB u koji cemo smestati dobijene slucajne br
SB<-NULL

# Slucajne br generisemo uz pomoc while petlje 
# Genrisemo slucajne brojeve iz Kosijeve raspodele i smestamo ih u vektor y,a u 
# vektor u SB smestamo slucajne br iz vektora y koji ispunjavaju uslov
# u< ili = od f(y)/M*g(y) gde je u vektor slucajnih br iz U[0,1] raspodele
# Proveravamo koliko generisanih slucajnih br zadovoljava uslov i ako je 
# nadjeni br veci ili jednak od Nsim prekidamo petlju tj kada lenght(SB)>=Nsim
# Posto broj generisanih slucajnih brojeva moze biti veca od trazenog, 
# iz vektora SB cemo izdvojiti prvih Nsim clanova


while(length(SB)<Nsim)
{
  y<-rcauchy(Nsim) 
  SB<-c(SB,y[runif(Nsim)*M<=posteriorna(y,Xsamples)/dcauchy(y)])
}
SB<-SB[1:Nsim]
SB
hist(SB,freq=F,col="darkslategray2")

# Da bismo nasli Bajesovu ocenu pretpostavicemo da je funkcija gubitaka srednjekvadratna.
# Tada ce ocena za teta biti matematicko ocekivanje aposteriorne raspodele
# Posto je vektor SB dobijen Acceptance Rejection metodom iz aposteriorne raspodele,
# Bajesova ocena za teta ce biti srednja vrednost vektora SB

Bajesova_ocena1<-mean(SB)
Bajesova_ocena1

# Ako uzmemo da je funkcija gubitka apsolutna, ocena za teta ce biti medijana:
Bajesova_ocena2<-median(SB)
Bajesova_ocena2

# 95%-ni empirijski interval prekrivanja cemo naci odbacivanjem 
# najmanjih i najvecih 2.5 % elemenata u nizu dobijenih tacaka iz aposteiriorne raspodele.
# Za to cemo koristiti funkciju quantile.

# Za pocetak moramo da sortiramo vektor slucajnib brojeva,a zatim odbacujemo po
# 2.5% sa svakog kraja
S<-sort(SB) 
p<-c(2.5,97.5)/100
Q1<-quantile(S,p)
Q2<-as.numeric(Q1) 
# Ispisujemo dobijeni 95% interval poverenja
Q2

# Pravimo vektor sa slucajnim br koji su upali u 95% interval poverenja

i<-1
SB2<-NULL
while(i<=length(S)){
  if(S[i]>=Q2[1] && S[i]<=Q2[2])
    SB2<-c(SB2, S[i])
  i<-i+1
}
# ISpisujemo dobijeni vektor
SB2 


                              #### Treci zadatak ####

# Dati integral cemo racunati pomocu Monte Karlo metode pogodaka i promasaja
# Sa I cemo obeleziti integral u granica 0 do 1 funkcije 1/(x+1)dx
# Za x vazi 0<=x<=1 pa cemo za konstante a i b izabrati a=0,b=1(granice integrala)
# Podintegralna funkcija g(x)=1/(x+1) je odozgo ogranicena sa 1
# tj vazi 0<=g(x)<=1 zato cemo za konstantu c koja predstavlja maksimalnu vrednost 
# funkcije na intervalu [a,b] izbrati c=1.

c<-1  
a<-0  
b<-1  

# Proizvoljno biramo gresku aproksimacije epsilon
epsilon<-0.001 
# i nivo poverenja alfa
alfa<-0.95 

# Broj potrebnih eksperimenata N mozemo odrediti preko
# P{abs(teta-I)<=epsilon}=alfa
# Za dovoljno veliko N vazi P{abs(teta*)<=z}=alfa,
# gde teta* primenom centralne granicne teoreme ima standardnu normalnu raspodelu,a
# z nalazimo tako da je F(z)=(1+alfa)/2 , gde je F funkcija raspodele stand.nor.raspodele
z<-qnorm((1+alfa)/2)

# Koristeci aproksimaciju Obim uzorka N dobijamo :
N<-trunc((c^2*(b-a)^2*z^2)/(4*epsilon^2))+1 

# Generisemo N nezavisnih slucajnih vektora (X_1,Y_1),...,(X_N,Y_N)

U<-runif(N) 
V<-runif(N)

# SLucajni vektori (X_i,Y_i) su uzorci iz 2D uniformne raspodele na pravougaoniku
# cije su koordinate na x-osi a i b, a na y-osi 0 i c
X<-a+(b-a)*U
Y<-c*V

# Implementiramo podintegralnu funkciju g(x)
g<-1/(X+1)

# Na osnovu zakona velikih brojeva, verovatnoca da se slucajan vektor (X,Y)
# nadje ispod krive g(x) se moze oceniti sa p*=Nh/N, gde je
# Nh broj slucajeva kada je Y_i<=g(X_i), i=1,...,N
Np<-sum(Y<=g)

#Integral I se moze oceniti sa teta: 
teta<-c*(b-a)*Np/N

# Svaki od N pokusaja ima Bernulijevu raspodelu sa verovatnocom p,pa slucajna
# velicina Np ima binomnu raspodelu sa parametrima (N,p)
# Disperzija ocene teta je tada :
#      D(teta)=c^2*(b-a)^2*p*(1-p)/N
# Koristimo aproksimaciju p*(1-p) je priblizno jednako 1/4, 
# (Np je oblika p*(1-p) i kao fja po p ona dostize maksimum za p=1/2,a to je 1/4)

D<-c^2*(b-a)^2/(4*N) #disperzija
# Interval poverenja nalazimo [teta-z*sqrt(D),teta+z*sqrt(D)]
levi_kraj_intervala<-teta-z*sqrt(D) 
desni_kraj_intervala<-teta+z*sqrt(D) 
# Interval poverenja nalazimo spajanjem levog i desnog kraja
interval_poverenja<-c(levi_kraj_intervala,desni_kraj_intervala) 

# Dobijena ocena nikad nece biti van dobijenog intervala
stvarna_vrednost<-(log(2))
interval_poverenja
teta
stvarna_vrednost

# Mi smo trazili da sa verovatnocom 0.95 ocena ne odstupa za vise od 0.001 
# od stavrne vrednosti, to znaci da greska u odnosu na stvarnu vrednost moze
# uvek da bude tek na trecoj decimali.

                             ##### Cetvrti zadatak #####
# DODATAK U WORDU
# Slucajne velicina X_i|teta ima N(m,teta) raspodelu, gde je m poznato, a
# teta=sigma^2 nepoznato , i=1,...,n
# Apriorna raspodela za teta je IG(a,b)

# Gustina normalne N(m,teta) raspodele je
# f(X_i|teta)=1/sqrt(2*pi*teta)*exp((-(x-m)^2)/(2*teta))
# Funkcija verodostojnosti je proizvod gustina normalnih raspodle:
# L(X_i|teta)=(1/sqrt(2*pi*teta))^n*exp((-suma(X_i-m)^2)/(2*teta))
# suma i prozivod idu od 1 do n
# Apriorna raspodela za teta je
# q(teta)=teta^(-a-1)*exp(-b/teta)*b^a/gamma(a)

# Aposteriornu raspodelu dobijamo 
# q(teta|X)=(L(X_i|teta)*q(teta))/ integral(L(X_i|teta)*q(teta))
# sredjivanjem dobijemo da teta|X ima IG(n/2+a,1/2*suma(X_i-m)^2+b)

# Instalirajmo i ukljucimo paket za inverznu gama raspodelu:
install.packages("invgamma")
library(invgamma)

Grafik<- function (m,a,b)
{ 
  set.seed(5)
  #uzorak obima 1 generisemo preko teta0
  teta0<-2 
  x<-rnorm(1,m,teta0)
  teta<-seq(0.001,5,0.001) 
  
  apriorna<-dinvgamma(teta,a,b)
  apriorna<-apriorna/sum(apriorna)
  # aproksimacija apriorne
  
  fja_verodostojnosti<-dnorm(x,m,teta)  
  fja_verodostojnosti<-fja_verodostojnosti/sum(fja_verodostojnosti) 
  # aproksimacija fje verodostojnosti
  
  # posteriorna<-fja_verodostojnosti*apriorna
  # Sada cemo primeniti ono sto smo nasli da teta|X ima IG(n/2+a,1/2*suma(X_i-m)^2+b)
  posteriorna<-dinvgamma(teta,1/2+a,1/2*(x-m)^2+b)
  posteriorna<-posteriorna/sum(posteriorna) # aproksimacija aposteriorne
  
  plot(teta,apriorna,type="l",main="Apriorna,aposteriorna f-ja i funkcija verodostojnosti",
       xlab=expression(teta),ylab="Gustina",col="deeppink")
  lines(teta,fja_verodostojnosti,lty=2,col="lightpink") 
  lines(teta,posteriorna,lty=1,lwd=2,col="deeppink4")
  legend("topright",c("Apriorna gustina","Fja verodostojnosti",
                      "Posteriorna gustina"), lty=c(1,2,1),lwd=c(1,1,2),cex=0.6) 
  
}
# Testiramo funkciju
par(mfrow=c(1,2))
Grafik(2,3,5)
Grafik(0,3,2)
# Napomena: Osim instaliranja paketa za inverznu gama raspodelu mogli smo 
# da uradimo i na sledeci nacin:
# Ako X ima inverznu gama raspodelu sa parametrima alfa i beta onda 1/X ima 
# gama raspodelu sa parametrima alfa i 1/beta





