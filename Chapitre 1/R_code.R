
############################################################
##    Codes R -- Chapitre 1
## Auteur: Mohamed Essaied Hamrita
##         mhamrita@gmail.com
###########################################################

# 20 clients d'une plateforme

xi=rep(c(0:4), times=c(3,5,7,4,1))
tab=as.data.frame(table(xi))
fi=tab$Freq/sum(tab$Freq)
pi=fi*100
Fi=cumsum(fi)
tab=cbind(tab, fi=fi,pi=pi,Fi=Fi)
tab

# barplot

barplot(table(xi))

# diagramme en secteur

pie(table(xi))

# Fonction de répartirtion (ecdf)

plot(ecdf(xi), col="blue", lwd=3)

#   Salaires data

salaires = c( 3.2, 4.1, 2.8, 3.9, 4.5, 3.0, 3.6, 4.2, 3.8, 5.1, 2.5, 
              3.7, 4.3, 3.4, 4.8, 3.1, 3.9, 4.0, 3.5, 4.6, 2.9, 3.3, 4.4, 3.8, 4.2,
              3.6, 4.7, 3.2, 4.1, 3.9, 4.4, 3.0, 3.8, 4.3, 3.7, 4.5, 3.4, 4.0, 3.6, 
              4.2, 3.9, 4.1, 3.3, 4.6, 3.5, 4.3, 3.8, 4.4, 3.2, 4.0)

n= length(salaires) # 50
min_s = min(salaires) # 2.5
max_s = max(salaires) # 5.1
etendue <- max_s - min_s # 2.6
# Règle de Sturges :
k = 1 + 3.3 * log10(n)
amplitude <- etendue / k

borneInf <- 2.5
bornes <- seq(borneInf, by = amplitude, len=(k+1))
h <- hist(salaires, breaks = bornes, right = FALSE,
          plot = FALSE)
h$counts

# histogramme

hist(salaires, breaks=bornes)

# Ogive

install.packages("actuar")

library(actuar)

plot(ogive(xi))

plot(ogive(salaires))

# Paramètres centrales

mean(xi)   # moyenne

median(xi) # médiane

median(c(0, 0, 1, 1, 1, 2, 3, 3, 3, 4, 4))

quartile(xi, 0.5)    # médiane

install.packages("modeest")
library(modeest)

mfv(xi)       # mode

mfv(salaires) # classe modale

# autres moyennes

install.packages("psych")

library(psych)

geometric.mean(xi[xi>0])      # moyenne géométrique
geometric.mean(salaires)

harmonic.mean(xi[xi>0])       # moyenne harmonique
harmonic.mean(salaires)

# quartiles, deciles, centiles

quantile(xi, c(0.25,0.5,0.75))   # Q1, Q2, Q3

quantile(xi, c(0.1, 0.9))        # D1, D9

quantile(xi, c(0.01,0.99))       # C1, C99

# variance / écart-type

vx=(n-1)/n*var(salaires)  # variance
vx

sx=sqrt(vx)
sx

xa=c(2, 17, 7, 18, 3, 13); na=length(xa)
xb=c(8, 12, 9, 11); nb=length(xb)

va=(na-1)/na*var(xa)
va

sqrt(va)

vb=(nb-1)/nb*var(xb)
vb

sqrt(vb)

# coefficient de variation

sd(xi)/mean(xi)

# coefficient d'asymétrie de Fisher

mu_3=1/n*sum((salaires-mean(salaires))^3)
sig3=(1/n*sum((salaires-mean(salaires))^2))^(3/2)
(gamma_1=mu_3/sig3)

# ou encore, en utilisant la fonction skew depuis la 
# bibliotèque psych

skew(salaires, type=1)



# coefficient d'aplatissement

mu4=mean((salaires-mean(salaires))^4)
mu2=mean((salaires-mean(salaires))^2)
mu4/mu2^2-3

# ou encore
kurtosi(salaires, type=1)


