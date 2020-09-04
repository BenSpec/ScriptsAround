library(memoise)


#Auxiliary Functions

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
SoftMax<-function(x)
{exp(temp*x)}		


#*****MODEL****

#temperature parameter
temp<-10

#Range of the variable of interest between 0 and 8, so 9 values. Central value is 4


#Prior distributions on half-length of the interval:from 0 to 4, so 5 possibilities
i<-rep(1/5, 5)


#turning this into a probability function over halflegnth: Prob(0) corresponds to the 1st item in vector 1, so we need a '+1"
Prob_i<-memoise(function(y)
{i[y+1]})



#Contructing epistemic states, e, think of them as posterior distributions of the speaker after speaker has observed e [e is o in the paper]

#EpisUniform(k) returns the distribution with support [4-k, 4+k], uniform on its support. EpisUniform(k) is a *function*: EpisUniform(k)(n) is the probability of n relative to a uniform distribution whose support is [4-k, 4+k]
EpisUniform<-memoise(function(k){function(n){if (abs(4-n)>k) return(0)
									 else return(1/((2*k)+1))}})

#EpisPeaked(k) returns the distribution with support [4-k,4+k] which is binomial shifted to the interval, with k > 0. Again, it returns a function. EpisPeaked(k)(n) returns the probability of n relative to a `peaked' distribution whose support is [4-k, 4+k]

EpisPeaked<-memoise(function(k){function(n){if (abs(4-n)>k) return(0) 
									else return(binom((2*k),(n+k-4),0.5))}})


#associating epistemic states (i.e. observations) with numbers, 0 is EpisUniform(0) [ie: speaker believes 'exactly4'], from k = 1 to k =  4 it's #EpisUniform(k), and from k=5 to k=8, #it's EpisPeaked(k-4))
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

#Not used here: unifrom distribution on epistemic states Proba_epis<-memoise(function(e){1/nepis})

#Used here: arbitrary numbers, meant to ensure that the probability distribution over states of the world (numbers) is reasonable...

#Weights
ProbaEpisAux<-c(2,8,32,128,512,2,8,32,128)

#Probabilities after normalizing
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


#
#Listeral Listener interpretation after 'around 4': conditionalizes the prior on the information that 'n is in [4-i, 4+i]' = Prob 'n=x|n is in [4-i, 4+i]'
#What's below corresponds to Bayesian update after learning  'n is in [4-i, 4+i]' 
#Laround is a function Laround(n)= posterior probability of n after conditionalizing with 'n is in [4-i, 4+i]

LaroundProp<-memoise(function(x)
{Prob_n(x)*Sum(0:4, function(y){test_between(x,y)*Prob_i(y)})})

LaroundNorm<-Sum(0:8, LaroundProp)

Laround<-memoise(function(x)
{LaroundProp(x)/LaroundNorm})
 
#Other_messages
#between(k) gives the interpreration of 'between (4-k, 4+k). So between(k)(j) is true (1) iff abs(4-j)≤k

between<-memoise(function(k)
{function(n){if (abs(4-n)>k) return(0)
		else return(1)}    })   

#Listener after interpreting between(4-k, 4+k)
Lbetween_prop<-memoise(function(k){function(n){Prob_n(n)*between(k)(n)}})
Lbetween_norm <-memoise(function(k){Sum(0:8, function(n){Lbetween_prop(k)(n)})})
Lbetween<-memoise(function(k){function(n){Lbetween_prop(k)(n)/Lbetween_norm(k)}})

#associating messages with numbers: 0 is 'between 4-0 and 4+0', ....4 is 'between 0 and 8', 5 is 'around 4'. So there are 6 messages, nmessages=6

nmessages<-6

#Number of 'worlds' = 9, all values from 0 to 8


#Listener  L0 that takes a world n [world identified by the value of n] and a message u, and returns the probability of the world according to the posterior #after hearing the message. In the case of message '5', the posterior is Laround, in other cases is Lbetween(k).
#set of messages:
#- all 'between' centered on 4, plus 'around 4'


L0<-memoise(function(n,u)
{if (u==5) return(Laround(n))
 else return(Lbetween(u)(n))
	})


L0table<- outer(0:(nworlds-1), 0:(nmessages-1), Vectorize(L0))
colnames(L0table)<-c("Exactly4", "b_3_5", "b_2_6", "b_1_7","b_0_8", "around")
row.names(L0table)<-c(0,1,2,3,4,5,6,7,8)





#Define recursively L and S---more to say - we need a joint distribution on worlds and epistemic states. Assume they are independent
#We need JointProba on (n,e). The 'epistemic states' we have defined are basically P(...|e).

#Define L_k, L(k,n|u),  as listener of level k  L(k,n|u) \propto sum_e JointProba(n,e)*S((k-1), u, e)

L_prop<-memoise(function(k,n,u)
{Sum(0:(nepis-1), function(e){JointProba(n,e)*S(k,u,e)})}) 

L_norm<-memoise(function(k,u)
{Sum(0:(nworlds-1), function(n){L_prop(k,n,u)})})

L<-memoise(function(k,n,u)
{if (k==0) return(L0(n,u))
	else return(L_prop(k,n,u)/L_norm(k,u))})
	
#Speaker

#cost: a function from messages to cost. Here, all costs = 0
cost<-function(u)
{return(0)}

	#utility function for Sk
#Uk(u,epis) = - KL divergence between L(k-1)_u and epis, where L(k-1)_u is the posterior distribution of level(k-1)-speaker after processsing message u, and epis is the epistemic state of the speaker


	
U<-memoise(function(k,u,e)
{-KL(state(e), function(n){L((k-1),n,u)})})

#SoftMax rule

Sprop<-memoise(function(k,u,e)	
	{SoftMax(U(k,u,e))})
	
Snorm<-memoise(function(k,e)
{Sum(0:(nmessages-1), function(u){Sprop(k,u,e)})})

S<-memoise(function(k,u,e)
{return(Sprop(k,u,e)/Snorm(k,e))	})


###Tables
 
Ltable<-function(k)
{table<-outer(0:(nworlds-1), 0:(nmessages-1), Vectorize(function(n,u){L(k,n,u)}))
 	colnames(table)<-c("Exactly4", "b_3_5", "b_2_6", "b_1_7","b_0_8", "around")
row.names(table)<-c(0,1,2,3,4,5,6,7,8)
return(table)}
	

Stable<-function(k)
{table<-outer(0:(nepis-1),0:(nmessages - 1), Vectorize(function(e,u){S(k,u,e)}))
colnames(table)<-c("Exactly4", "b_3_5", "b_2_6", "b_1_7","b_0_8", "around")
row.names(table)<-c("Exactly4","u_3_5", "u_2_6", "u_1_7", "u_0_8","p_3_5", "p_2_6", "p_1_7", "p_0_8")
return(table)}



 