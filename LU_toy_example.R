library(memoise)




# 1) Function 'Sum' takes a set X and a function F, and returns the sum of all the F(x)s, where x is in X. For some reason one needs to vectorize the function 
 
 Sum<-function(X,F)
 {return(sum(Vectorize(F)(X)))}


#SoftMax function and temperature
temp<-4
SoftMax<-function(x)
{exp(temp*x)}		

#4 Observations: 2 worlds, conditional proba for each observation
o1<-c(.9,.1)
o2<-c(.1,.9)
o3<-c(1,0)
o4<-c(0,1)

#List of all observations
O<-cbind(o1=o1,o2=o2,o3=o3,o4=o4)

Proba_obs<-c(.25,.25,.25,.25)

#Proba on worlds = sum_o P(w|o)*P(w)
#JOint Proba = P(w|o)*P(o)

JointProba<-function(w,o)
{Proba_obs[o]*O[w,o]}

JointProbaTable<-outer(1:2, 1:4, Vectorize(JointProba))
row.names(JointProbaTable)<-c("w1", "w2")
colnames(JointProbaTable)<-c("o1", "o2", "o3", "o4")

####LEXICA
l1<-rbind(c(1,0), c(0,1), c(1,1))
row.names(l1)<-c("m1", "m2","m3")
colnames(l1)<-c("w1","w2")
l2<-rbind(c(1,1),c(1,1), c(1,1))
row.names(l2)<-c("m1", "m2", "m3")
colnames(l2)<-c("w1","w2")


LEX<-list(l1=l1,l2=l2)

ProbaLEX<-c(.5, .5)

####L0


L0prop<-memoise(function(w,o,m,l)
{JointProbaTable[w,o]*LEX[[l]][m,w]})

L0norm<-memoise(function(m,l)
{Sum(1:2, function(w){Sum(1:4, function(o){L0prop(w,o,m,l)})})})

L0<-memoise(function(w,o,m,l)
{L0prop(w,o,m,l)/L0norm(m,l)})



###S1

U1<-memoise(function(m,o,l)
{Sum(1:2, function(w)
	{if (O[w,o]==0) return(0)
	 else	
	 return(O[w,o]*log(L0(w,o,m,l)))
		})
	})


S1prop<-memoise(function(m,o,l)
{SoftMax(U1(m,o,l))})

S1norm<-memoise(function(o,l)
{Sum(1:3, function(m){S1prop(m,o,l)})})

S1<-memoise(function(m,o,l)
{S1prop(m,o,l)/S1norm(o,l)})


###L1

L1prop<-memoise(function(w,o,m)
{JointProbaTable[w,o]*Sum(1:2, function(l){ProbaLEX[l]*S1(m,o,l)})})

L1norm<-memoise(function(m)
{Sum(1:2, function(w){Sum(1:4, function(o){L1prop(w,o,m)})})})


L1<-memoise(function(w,o,m)
{L1prop(w,o,m)/L1norm(m)})

#Lk and Sk

#Sk

U<-memoise(function(k,m,o)
{Sum(1:2, function(w)
{if (O[w,o]== 0) return(0)
  	else
  	return(O[w,o]*log(L(k-1,w,o,m)))})})

Sprop<-memoise(function(k,m,o)
{SoftMax(U(k,m,o))})


Snorm<-memoise(function(k,o)
{Sum(1:3,function(m){Sprop(k,m,o)})})

S<-memoise(function(k,m,o)
{Sprop(k,m,o)/Snorm(k,o)})


Stable<-function(k)
{table<-outer(1:4, 1:3, Vectorize(function(o,m){S(k,m,o)}))
 colnames(table)<-c("m1", "m2", "m3")
 row.names(table)<-c("o1", "o2", "o3","o4")
 return(table)
 }
#Lk

Lprop<-memoise(function(k,w,o,m)
{JointProbaTable[w,o]*S(k,m,o)})

Lnorm<-memoise(function(k,m)
{Sum(1:2, function(w){Sum(1:4, function(o){Lprop(k,w,o,m)})})})


L<-memoise(function(k,w,o,m)
{if (k==1) return(L1(w,o,m))
 else return(Lprop(k,w,o,m)/Lnorm(k,m))})
 
#Ln = marginalized on o
Lw<-function(k,w,m)
{Sum(1:4, function(o){L(k,w,o,m)})}

 
Ltable<-function(k)
{table<-outer(1:2, 1:3, Vectorize(function(w,m){Lw(k,w,m)}))
 	colnames(table)<-c("m1", "m2", "m3")
row.names(table)<-c("w1","w2")
return(table)}
	

