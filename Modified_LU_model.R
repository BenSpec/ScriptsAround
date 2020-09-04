#####Version where speaker does not care about communicating e, i.e. we change the utility function. This one works with high lambda! (not sure why high lambda is necessary)

library(memoise)




# 1) Function 'Sum' takes a set X and a function F, and returns the sum of all the F(x)s, where x is in X. For some reason one needs to vectorize the function 
 
 Sum<-function(X,F)
 {return(sum(Vectorize(F)(X)))}



#Binomial Function, used to construct various distributions
binom<-function(n,k,p){(factorial(n)/(factorial(n-k)*factorial(k)))*(p^k)*((1-p)^(n-k))}



		
#SoftMax function and temperature
temp<-40
SoftMax<-function(x)
{exp(temp*x)}		

#Prior distribution on values - let's say there are between 0 and 8, so 9 values. Central value is 4


#Prior distributions on half-length of the interval:from 0 to 4, so 5 possibilities
i<-rep(1/5, 5)


#turning this into a probability function over halflegnth: Prob(0) corresponds to the 1st item in vector 1, so we need a '+1"
Prob_i<-memoise(function(y)
{i[y+1]})



#Contructing epistemic states, e, think of them as posterior distributions of the speaker after speaker has observed e
#EpisUniform(k) returns the distribution with support [4-k, 4+k], uniform on its support. EpisUniform(k) is a *function*: EpisUniform(k)(n) is the probability of n relative to a uniform distribution whose support is [4-k, 4+k]
EpisUniform<-memoise(function(k){function(n){if (abs(4-n)>k) return(0)
									 else return(1/((2*k)+1))}})

#EpisPeaked(k) returns the distribution with support [4-k,4+k] which is binomial translated on to the interval, with #k > 0. Again, it returns a function. EpisPeaked(k)(n) returns the probability of n relative to a `peaked' distribution whose suppoer is [4-k, 4+k]

EpisPeaked<-memoise(function(k){function(n){if (abs(4-n)>k) return(0) 
									else return(binom((2*k),(n+k-4),0.5))}})


#associating epistemic states with numbers, 0 is EpisUniform(0) [ie: speaker believes 'exactly4'], from k = 1 to k =  4 it's #EpisUniform(k), and from k=5 to k=8, its EpisPeaked(k-4))

#So there are exactly 9 epistemic states, nepis<-9

nepis<-9
nworlds<-9

#state(e)(n)= P(n|epistemic state e) = probability of n in the distribution induced by observation e.


state<-memoise(function(x)
{if (x<5) return(EpisUniform(x))
 else return(EpisPeaked(x-4))	
	})							


#P(n|e), noted P_n_given_e

P_n_given_e<-memoise(function(n,e)
{state(e)(n)})

#Table P(n|e)

outer(0:(nworlds-1), 0:(nepis-1), Vectorize(P_n_given_e))

#Probability Distribution over epistemic states

#Not used here: unifrom distribution on epistemic statesProba_epis<-memoise(function(e){1/nepis})

#Used here: arbitrary numbers, meant to ensure that the probability distribution over states of the world (numbers) is approximately uniform

ProbaEpisAux<-c(2,8,32,128,512,2,8,32,128)

ProbaEpis<-ProbaEpisAux/sum(ProbaEpisAux)

#Turning_it_into a function
Proba_epis<-memoise(function(e){ProbaEpis[e+1]})

#Deriving the JointProbability Distriubtion over (worlds, epistemic states): P(n) = P(n|e)*P(e) 
#Important: P(n|epistemic state = e) is the same as P_n_given_e(n,e)

JointProba<-memoise(function(n,e)
{P_n_given_e(n,e)*Proba_epis(e)})

#Turning it into a table
JointProbaTable<-outer(0:(nworlds-1), 0:(nepis-1), Vectorize(JointProba))

#Marginalizing to get the probability distribution on states (numbers): Prob_n. Again everything is a function.

Prob_n<-memoise(function(n)
{Sum(0:(nepis-1), function(e){JointProba(n,e)})})


#Prob of 'n is between [4-y, 4+y]
Prob_between<-memoise(function(y)
{Sum((4-y):(4+y), Prob_n)})

#Truthvalue of 'x is in ther interval [4-y, 4+y]', as a function of x and y
test_between<-memoise(function(x,y)
{(4-y)<=x & x <= (4+y)})




#Between messages
#between(k) gives the interpreration of 'between (4-k, 4+k). So between(k)(j) is true (1) iff abs(4-j)≤k

between<-memoise(function(k)
{function(n){if (abs(4-n)>k) return(0)
		else return(1)}    })   
		
		
		
#associating messages with numbers: 0 is 'between 4-0 and 4+0', ....4 is 'between 0 and 8', 5 is 'around 4'. So there are 6 messages, nmessages=6

nmessages<-6

#Number of 'worlds' = 9, all values from 0 to 8

#L0

#L0(n,e|u,i)  for u==5, this is 'around', which is interpreted as 'between(i)', i.e. 'between 4-i and 4+i'.

L0prop<-memoise(function(n,e,u,i)
{if (u==5) return(between(i)(n)*JointProba(n,e))
 else return(between(u)(n)*JointProba(n,e))
	})

L0norm<-memoise(function(u,i)
{Sum(0:(nworlds-1), function(n){Sum(0:(nepis-1), function(e){L0prop(n,e,u,i)})})})


L0<-memoise(function(n,e,u,i)
{L0prop(n,e,u,i)/L0norm(u,i)})


L0n<-function(n,u,i)
{Sum(0:(nepis-1), function(e){L0(n,e,u,i)})}

#S1(u|e,i), in terms of U1(u|e,i) propto sum_n P(n|e)*log(L0(n|u,i)) - c(u)

#cost: a function from messages to cost. Here, all costs = 0
cost<-memoise(function(u)
{return(0)})


U1<-memoise(function(u,e,i)
{Sum(0:(nworlds-1), function(n)
  {if (P_n_given_e(n,e) == 0) return(0)
  	else
  	return(P_n_given_e(n,e)*log(Sum(0:(nepis-1), function(f){L0(n,f,u,i)})))})	- cost(u)})	

S1prop<-memoise(function(u,e,i)
{SoftMax(U1(u,e,i))})
	
S1norm<-memoise(function(e,i)
{Sum(0:(nmessages-1),function(u){S1prop(u,e,i)})})

S1<-memoise(function(u,e,i)
{S1prop(u,e,i)/S1norm(e,i)})


U1table<-function(i)
{outer(0:(nepis-1), 0:(nmessages-1), Vectorize(function(e,u){U1(u,e,i)}))}

S1table<-function(i)
{outer(0:(nepis-1), 0:(nmessages-1), Vectorize(function(e,u){S1(u,e,i)}))}

###L1(n,e|u) propto P(n,e)*sum_i P(i)*S1(u|e,i)

L1prop<-memoise(function(n,e,u)
{JointProba(n,e)*Sum(0:4, function(i){Prob_i(i)*S1(u,e,i)})})
		  
L1norm<-memoise(function(u)
{Sum(0:(nworlds-1), function(n){Sum(0:(nepis-1), function(e){L1prop(n,e,u)})})})

L1<-memoise(function(n,e,u)
{L1prop(n,e,u)/L1norm(u)})


#Marginalizing on e: L1n(n,u) = L1(n|u)

L1n<-function(n,u)
{Sum(0:(nepis-1), function(e){L1(n,e,u)})}

L1table<- outer(0:(nworlds-1), 0:(nmessages-1), Vectorize(L1n))
colnames(L1table)<-c("Exactly4", "b_3_5", "b_2_6", "b_1_7","b_0_8", "around")
row.names(L1table)<-c(0,1,2,3,4,5,6,7,8)


#Lk and Sk

#Sk

U<-memoise(function(k,u,e)
{Sum(0:(nworlds-1), function(n)
{if (P_n_given_e(n,e) == 0) return(0)
  	else
  	return(P_n_given_e(n,e)*log(Sum(0:(nepis-1), function(f){L(k-1,n,f,u)})))}) -cost(u)})
  	
Sprop<-memoise(function(k,u,e)
{SoftMax(U(k,u,e))})

Snorm<-memoise(function(k,e)
{Sum(0:(nmessages-1),function(u){Sprop(k,u,e)})})

S<-memoise(function(k,u,e)
{Sprop(k,u,e)/Snorm(k,e)})

Stable<-function(k)
{outer(0:(nepis-1), 0:(nmessages-1), Vectorize(function(e,u){S(k,u,e)}))}


#Lk

Lprop<-memoise(function(k,n,e,u)
{JointProba(n,e)*S(k,u,e)})

Lnorm<-memoise(function(k,u)
{Sum(0:(nworlds-1), function(n){Sum(0:(nepis-1), function(e){Lprop(k,n,e,u)})})})


L<-memoise(function(k,n,e,u)
{if (k==1) return(L1(n,e,u))
 else return(Lprop(k,n,e,u)/Lnorm(k,u))})
 
#Ln = marginalized on e
Ln<-function(k,n,u)
{Sum(0:(nepis-1), function(e){L(k,n,e,u)})}

 
Ltable<-function(k)
{table<-outer(0:(nworlds-1), 0:(nmessages-1), Vectorize(function(n,u){Ln(k,n,u)}))
 	colnames(table)<-c("Exactly4", "b_3_5", "b_2_6", "b_1_7","b_0_8", "around")
row.names(table)<-c(0,1,2,3,4,5,6,7,8)
return(table)}