library(memoise)




# 1) Function 'Sum' takes a set X and a function F, and returns the sum of all the F(x)s, where x is in X. For some reason one needs to vectorize the function 
 
 Sum<-function(X,F)
 {return(sum(Vectorize(F)(X)))}



#Binomial Function, used to construct various distributions
binom<-function(n,k,p){(factorial(n)/(factorial(n-k)*factorial(k)))*(p^k)*((1-p)^(n-k))}



#Kulback-Leibler Divergence, on distributions on values from 0 to (nworlds-1). The arguments of the function KL are themselves functions (probability distributions)
KL<-function(E,L) 
{Sum(0:(nworlds-1), function(k){
		if (E(k)==0) return(0)
		else return(E(k)*log(E(k)/L(k)))})}
		
		
#SoftMax function and temperature
temp<-10
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

#EpisPeaked(k) returns the distribution with support [4-k,4+k] which is binomial translated on to the interval, with #k > 0. Again, it returns a function. EpisPeaked(k)(n) returns the probability of n relative to a `peaked' distribution whose support is [4-k, 4+k]

EpisPeaked<-memoise(function(k){function(n){if (abs(4-n)>k) return(0) 
									else return(binom((2*k),(n+k-4),0.5))}})


#associating epistemic states with numbers, 0 is EpisUniform(0) [ie: speaker believes 'exactly4'], from k = 1 to k =  4 it's #EpisUniform(k), and from k=5 to k=8, it's EpisPeaked(k-4))

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

EpisTable<-outer(0:(nworlds-1), 0:(nepis-1), Vectorize(P_n_given_e))
colnames(EpisTable)<-c("=4","u_3_5", "u_2_6", "u_1_7", "u_0_8","p_3_5", "p_2_6", "p_1_7", "p_0_8")
row.names(EpisTable)<-c(0,1,2,3,4,5,6,7,8)



#Probability Distribution over epistemic states

#Not used here: unifrom distribution on epistemic states Proba_epis<-memoise(function(e){1/nepis})

#Used here: arbitrary numbers, meant to ensure that the probability distribution over states of the world (numbers) is reasonable...

ProbaEpisAux<-c(1,4,16,64,256,1,4,16,64)
#ProbaEpisAux<-c(1,2,3,4,5,2,3,4,5)
#ProbaEpis<-c(1/426, 2/213,8/213, 32/213, 128/213, 1/426,2/213,8/213,32/213)
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

#Listeral Listener interpretation after 'around 4': conditionalizes the prior on the information that 'n is in [4-i, 4+i]' = Prob 'n=x|n is in [4-i, 4+i]'
#What's below corresponds to Bayesian update after learning  'n is in [4-i, 4+i]', derived analytically in my note
#Laround is a function Laround(n,e)= posterior probability of n after conditionalizing with 'n is in [4-i, 4+i]



Laround_prop<-memoise(function(n,e)
{Num<-JointProba(n,e)*Sum(0:4, function(y){test_between(n,y)*Prob_i(y)})})

Laround_norm<-sum(outer(0:(nworlds-1), 0:(nepis-1), Vectorize(Laround_prop)))

Laround<-memoise(function(n,e)
{Laround_prop(n,e)/Laround_norm})


 
#Other_messages
#between(k) gives the interpreration of 'between (4-k, 4+k). So between(k)(j) is true (1) iff abs(4-j)≤k

between<-memoise(function(k)
{function(n){if (abs(4-n)>k) return(0)
		else return(1)}    })   

#Listener after interpreting between(4-k, 4+k)
Lbetween_prop<-memoise(function(k){function(n,e){JointProba(n,e)*between(k)(n)}})
Lbetween_norm <-memoise(function(k){Sum(0:8, function(n){Sum(0:8, function(e){Lbetween_prop(k)(n,e)})})})
Lbetween<-memoise(function(k){function(n,e){Lbetween_prop(k)(n,e)/Lbetween_norm(k)}})

#associating messages with numbers: 0 is 'between 4-0 and 4+0', ....4 is 'between 0 and 8', 5 is 'around 4'. So there are 6 messages, nmessages=6

nmessages<-6

#Number of 'worlds' = 9, all values from 0 to 8


#Listener  L0 that takes a world n [world identified by the value of n] and a message u, and returns the probability of the world according to the posterior #after hearing the message. In the case of message '5', the posterior is Laround, in other cases is Lbetween(k).
#set of messages:
#- all 'between' centered on 4, plus 'around 4'



L0<-function(n,e,u)
{if (u==5) return(Laround(n,e))
	else return(Lbetween(u)(n,e))}

L0n<-memoise(function(n,u)
{if (u==5) return(Sum(0:8, function(e){Laround(n,e)}))
 else return(Sum(0:8, function(e){Lbetween(u)(n,e)}))
	})


L0table<- outer(0:(nworlds-1), 0:(nmessages-1), Vectorize(L0n))
colnames(L1table)<-c("Exactly4", "b_3_5", "b_2_6", "b_1_7","b_0_8", "around")
row.names(L1table)<-c(0,1,2,3,4,5,6,7,8)

#Pragamtic speaker S1 who talks to L1, we need to have a set of possible epistemic states: all uniform states, + all truncated binomial states. Each 'epistemic state' is a probability distribution



#Define L_k, L(k,n,e|u),  as listener of level k  L(k,n,e|u) \propto JointProba(n,e)*S((k-1), u, e)

L_prop<-memoise(function(k,n,e,u)
{JointProba(n,e)*S(k,u,e)})

L_norm<-memoise(function(k,u)
{Sum(0:(nworlds-1), function(n){Sum(0:(nepis-1), function(e){L_prop(k,n,e,u)})})})


L<-memoise(function(k,n,e,u)
{if (k==0) return(L0(n,e,u))
	else return(L_prop(k,n,e,u)/L_norm(k,u))})
	
	
	##Speaker 
	## Utility Uk as a function of L(k-1)
	#utility function for S1 as in official RSA, cares about o and u, not directly using KL


#cost: a function from messages to cost. Here, all costs = 0
cost<-function(u)
{return(0)}

	
U<-memoise(function(k,u,e)
{Sum(0:(nworlds-1), function(n)
{if (P_n_given_e(n,e) == 0) return(0)
 else
 return(P_n_given_e(n,e)*log(L((k-1),n,e,u)))}) - cost(u)})
	
Sprop<-memoise(function(k,u,e)	
	{SoftMax(U(k,u,e))})
	
Snorm<-memoise(function(k,e)
{Sum(0:(nmessages-1), function(u){Sprop(k,u,e)})})

S<-memoise(function(k,u,e)
{return(Sprop(k,u,e)/Snorm(k,e))	})
 

Ln<-function(k,n,u)
{Sum(0:(nepis-1), function(e){L(k,n,e,u)})}

Le<-function(k,e,u)
{Sum(0:(nworlds-1), function(n){L(k,n,e,u)})}


#Listerner's posterior over variable of interest, at level k

Ln_table<-function(k)
{table<-outer(0:(nworlds-1), 0:(nmessages-1), Vectorize(function(n,u){Ln(k,n,u)}))
 	colnames(table)<-c("Exactly4", "b_3_5", "b_2_6", "b_1_7","b_0_8", "around")
row.names(table)<-c(0,1,2,3,4,5,6,7,8)
return(table)}
	
	
#Listener's posterior over 	observations, at level k
Le_table<-function(k)
{table<-outer(0:(nepis-1), 0:(nmessages-1), Vectorize(function(e,u){Le(k,e,u)}))
 	colnames(table)<-c("Exactly4", "b_3_5", "b_2_6", "b_1_7","b_0_8", "around")
row.names(table)<-c("Exactly4","u_3_5", "u_2_6", "u_1_7", "u_0_8","p_3_5", "p_2_6", "p_1_7", "p_0_8")
return(table)}


Stable<-function(k)
{table<-outer(0:(nepis-1),0:(nmessages - 1), Vectorize(function(e,u){S(k,u,e)}))
colnames(table)<-c("Exactly4", "b_3_5", "b_2_6", "b_1_7","b_0_8", "around")
row.names(table)<-c("Exactly4","u_3_5", "u_2_6", "u_1_7", "u_0_8","p_3_5", "p_2_6", "p_1_7", "p_0_8")
return(table)}


									


 
