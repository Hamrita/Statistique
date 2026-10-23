
#=============================================
#    Chapitre 2: Distribution bivariée
#=============================================

# Exemple 1

Fem=c(2,1); Mas=c(2,5)
tab=cbind(Fem,Mas)
rownames(tab)=c("Bleux", "Noirs")
tab

# Distribution marginales

ni.=rowSums(tab)
ni.

n.j=colSums(tab)
n.j

# Tableau des fréquences

fij=prop.table(tab)
fij

fi.=rowSums(fij)
fi.

f.j=colSums(fij)
f.j

# Test d'indépendance

fi. * f.j == fij

# Test de Khi-deux

K=chisq.test(tab, correct = F)

# X2 espérée

K$expected

# Khi2 observée

K$statistic

# ddl

K$parameter

# Khi2 tabulée (alpha=5%)

qchisq(0.05, K$parameter, lower.tail =  F)

# Exercice (Age/Canal d'achat)

tab2=data.frame("Mobile"=c(20,10,5), "Ordinateur"=c(5,15,10), 
                "Magasin"=c(5,10,20))
rownames(tab2)=c("18-25", "26-40", "41+")

tab2

k2=chisq.test(tab2, correct = F)

# khi-deux espérée
k2$expected

k2$statistic  # khi-deux observée

k2$parameter  # ddl

k2_t=qchisq(0.05, k2$parameter, lower.tail = F)  # khi-deux tabulée

k2_t

k2$statistic < k2_t # (si TRUE, on accepte l'hypothèse d'indépendance)

# covariance, corrélation et régression

X=c(15, 22, 30, 35, 42, 50, 55,70)
Y=c(1.2, 1.5, 2.0, 2.4, 3.1, 3.8, 4.2, 5.5)

# nuage de points

plot(X,Y, pch=16)

n=length(X)  # taille de l'échantillon

x.bar=mean(X); x.bar

y.bar=mean(Y); y.bar

cov.xy=(n-1)/n*cov(X,Y); cov.xy

var.x=(n-1)/n*var(X); var.x

var.y=(n-1)/n*var(Y); var.y

cor.xy=cor(X,Y); cor.xy

# Exemple: prix immobilier

x=c(20 , 47 , 21 , 52 , 45 , 74 , 87 , 98 , 54 , 123)
y=c(35 , 89 , 45 , 75 , 85 , 118 , 173 , 168 , 91 , 226)

nn=length(x)

plot(x,y, pch=16)

cc=cor(x,y); cc

# y=ax+b

a=cov(x,y)/var(x); a

b=mean(y)-a*mean(x); b

# on peut aussi déterminer l'équation de la régression
# de y en fonction de x autrement

lm(y ~ x)

# droite de x en fonction de y

a.prime=cov(x,y)/var(y); a.prime

b.prime=mean(x)-a.prime*mean(y); b.prime

# ou encore

lm(x ~ y)

# prévision

lm1=lm(y ~ x)
predict(lm1, newdata = data.frame(x=60))

#  Graphique

plot(x,y, pch=16, xlab="X: surface", ylab="Y: Prix", col="red3",
     panel.first = grid())
abline(a=b, b=a, lwd=2)

