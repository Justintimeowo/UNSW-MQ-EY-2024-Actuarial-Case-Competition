set.seed(11)

install.packages("markovchain")
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("matlib")
install.packages("moments")
library(markovchain)
library(ggplot2)
library(gridExtra)
library(matlib)
library(moments)

matrix_1<-matrix(data=c(0.793,0.207,
                          0.5,0.5), byrow = T, nrow=2)
matrix_2<-matrix(data=c(0.78015,0.21985,
                        0.46896,0.53104), byrow = T, nrow=2)
matrix_3<-matrix(data=c(0.77395,0.22605,
                        0.45398,0.54602), byrow = T, nrow=2)
matrix_4<-matrix(data=c(0.76792,0.23208,
                        0.43942,0.56058), byrow = T, nrow=2)


matrix_5<-matrix(data=c(0.65819,0.05551,0.0793,0.207,
                        0.41,0.4897,0.059,0.0413,
                        0.87,0,0,0.13,
                        0.5,0,0,0.5), byrow = T, nrow=4)
matrix_6<-matrix(data=c(0.65390,0.05123,0.07502,0.21985,
                        0.40914,0.48885,0.05815,0.04386,
                        0.86193,0,0,0.13807,
                        0.46896,0,0,0.53104), byrow = T, nrow=4)
matrix_7<-matrix(data=c(0.65184,0.04916,0.07295,0.22605,
                        0.40873,0.48843,0.05773,0.04511,
                        0.85803,0,0,0.14197,
                        0.45398,0,0,0.54602), byrow = T, nrow=4)
matrix_8<-matrix(data=c(0.64983,0.04715,0.07094,0.23208,
                        0.40833,0.48803,0.05733,0.04631,
                        0.85425,0,0,0.14575,
                        0.43942,0,0,0.56058), byrow = T, nrow=4)


(mc_1<-new("markovchain",states=c("Customer","Not Customer"),
                   transitionMatrix=matrix_1,
                   name="Base Control Model"))
(mc_2<-new("markovchain",states=c("Customer","Not Customer"),
           transitionMatrix=matrix_2,
           name="Year 1 Control Model"))
(mc_3<-new("markovchain",states=c("Customer","Not Customer"),
           transitionMatrix=matrix_3,
           name="Year 2 Control Model"))
(mc_4<-new("markovchain",states=c("Customer","Not Customer"),
           transitionMatrix=matrix_4,
           name="Year 3 Control Model"))

(mc_5<-new("markovchain",states=c("Customer","Discussion Forum", "Financial Coach", "Not Customer"),
           transitionMatrix=matrix_5,
           name="Base Solution Model"))
(mc_6<-new("markovchain",states=c("Customer","Discussion Forum", "Financial Coach", "Not Customer"),
           transitionMatrix=matrix_6,
           name="Year 1 Solution Model"))
(mc_7<-new("markovchain",states=c("Customer","Discussion Forum", "Financial Coach", "Not Customer"),
           transitionMatrix=matrix_7,
           name="Year 2 Solution Model"))
(mc_8<-new("markovchain",states=c("Customer","Discussion Forum", "Financial Coach", "Not Customer"),
           transitionMatrix=matrix_8,
           name="Year 3 Solution Model"))

(mc_1_3yrs<-mc_1^36)
(mc_2_3yrs<-mc_2^36)
(mc_3_3yrs<-mc_3^36)
(mc_4_3yrs<-mc_4^36)

(mc_5_3yrs<-mc_5^36)
(mc_6_3yrs<-mc_6^36)
(mc_7_3yrs<-mc_7^36)
(mc_8_3yrs<-mc_8^36)

initial_state_1<-c(1000,0)
initial_state_2<-c(1000,0)
initial_state_3<-c(1000,0)
initial_state_4<-c(1000,0)
initial_state_5<-c(1000,0,0,0)
initial_state_6<-c(1000,0,0,0)
initial_state_7<-c(1000,0,0,0)
initial_state_8<-c(1000,0,0,0)

n_1<-initial_state_1*mc_1_3yrs
n_2<-initial_state_2*mc_2_3yrs
n_3<-initial_state_3*mc_3_3yrs
n_4<-initial_state_4*mc_4_3yrs
n_5<-initial_state_5*mc_5_3yrs
n_6<-initial_state_6*mc_6_3yrs
n_7<-initial_state_7*mc_7_3yrs
n_8<-initial_state_8*mc_8_3yrs

rsim_customer<-function(MC,n,customers){
  sim_customer_pp<-rep(NA,customers) #Creates empty vector which stores whether they are customer
  for (t in 1:customers) {#t is for each customer
    out<-rmarkovchain(n,MC,t0="Customer") #Simulates the customer's state year
    sim_customer_pp[t]<-length(which(out=="Not Customer"))
    #Counts the number of times the customer was part of the bank
    #Repeats for every customer and puts it into the vector
  }
  return(sum(sim_customer_pp)) #Outputs the total times customer was part of the bank
}

rsim_cost_benefit<-function(MC,n,customers,benefit_pp){
  sim_benefit_pp<-rep(NA,customers) #Creates empty vector which stores whether they are customer
  for (t in 1:customers) {#t is for each customer
    out<-rmarkovchain(n,MC,t0="Customer") #Simulates the customer's state year
    sim_benefit_pp[t]<-benefit_pp*length(which(out!="Not Customer"))
    #Counts the number of times the customer was part of the bank
    #Repeats for every customer and puts it into the vector
  }
  return(sum(sim_benefit_pp)) #Outputs the total times customer was part of the bank
}

cost_benefit_1<-rep(NA,1000)
cost_benefit_2<-rep(NA,1000)


for (t in 1:1000) {
  cost_benefit_1[t]<-rsim_cost_benefit(mc_1,36,1000,26)
  cost_benefit_2[t]<-rsim_cost_benefit(mc_5,36,1000,26)
}

(mean(cost_benefit_1)-mean(cost_benefit_2))/mean(cost_benefit_1)

customer_1<-rep(NA,10000)
customer_2<-rep(NA,10000)
customer_3<-rep(NA,10000)
customer_4<-rep(NA,10000)
customer_5<-rep(NA,10000)
customer_6<-rep(NA,10000)
customer_7<-rep(NA,10000)
customer_8<-rep(NA,10000)

for (t in 1:10000) {
  customer_1[t]<-rsim_customer(mc_1,36,1000)
  customer_2[t]<-rsim_customer(mc_2,12,1000)
  customer_3[t]<-rsim_customer(mc_3,12,1000)
  customer_4[t]<-rsim_customer(mc_4,12,1000)
}

for (t in 1:10000) {
  customer_5[t]<-rsim_customer(mc_5,36,1000)
  customer_6[t]<-rsim_customer(mc_6,12,1000)
  customer_7[t]<-rsim_customer(mc_7,12,1000)
  customer_8[t]<-rsim_customer(mc_8,12,1000) 
}

customer_1_df<-data.frame(customer_1/36000)
customer_2_df<-data.frame(customer_2/12000)
customer_3_df<-data.frame(customer_3/12000)
customer_4_df<-data.frame(customer_4/12000)
customer_5_df<-data.frame(customer_5/36000)
customer_6_df<-data.frame(customer_6/12000)
customer_7_df<-data.frame(customer_7/12000)
customer_8_df<-data.frame(customer_8/12000)

ggplot(customer_1_df,aes(x=customer_1.36000))+
  geom_histogram(bins=25,color='white',fill='#222a35') + 
  labs(x = "Proportion of Customers who left bank",y="Simulation Count",
       title="Distribution of Customer Churn Control Model")

ggplot(customer_2_df,aes(x=customer_2.12000))+
  geom_histogram(bins=25,color='white',fill='#222a35') + 
  labs(x = "Proportion of Customers who left bank",y="Simulation Count",
       title="Distribution of Customer Churn Control Model Yr 1 Forecast")

ggplot(customer_3_df,aes(x=customer_3.12000))+
  geom_histogram(bins=25,color='white',fill='#222a35') + 
  labs(x = "Proportion of Customers who left bank",y="Simulation Count",
       title="Distribution of Customer Churn Control Model Yr 2 Forecast")

ggplot(customer_4_df,aes(x=customer_4.12000))+
  geom_histogram(bins=25,color='white',fill='#222a35') + 
  labs(x = "Proportion of Customers who left bank",y="Simulation Count",
       title="Distribution of Customer Churn Control Model Yr 3 Forecast")


ggplot(customer_5_df,aes(x=customer_5.36000))+
  geom_histogram(bins=25,color='white',fill='#8faadc') + 
  labs(x = "Proportion of Customers who left bank",y="Simulation Count",
       title="Distribution of Customer Churn with Solution Model")

ggplot(customer_6_df,aes(x=customer_6.12000))+
  geom_histogram(bins=25,color='white',fill='#8faadc') + 
  labs(x = "Proportion of Customers who left bank",y="Simulation Count",
       title="Distribution of Customer Churn with Solution Model Yr 1 Forecast")

ggplot(customer_7_df,aes(x=customer_7.12000))+
  geom_histogram(bins=25,color='white',fill='#8faadc') + 
  labs(x = "Proportion of Customers who left bank",y="Simulation Count",
       title="Distribution of Customer Churn with Solution Model Yr 2 Forecast")

ggplot(customer_8_df,aes(x=customer_8.12000))+
  geom_histogram(bins=25,color='white',fill='#8faadc') + 
  labs(x = "Proportion of Customers who left bank",y="Simulation Count",
       title="Distribution of Customer Churn with Solution Model Yr 3 Forecast")


mean(customer_1_df$customer_1.36000)
sd(customer_1_df$customer_1.36000)
min(customer_1_df$customer_1.36000)
max(customer_1_df$customer_1.36000)
skewness(customer_1_df$customer_1.36000)
kurtosis(customer_1_df$customer_1.36000)
mean(customer_2_df$customer_2.12000)
sd(customer_2_df$customer_2.12000)
min(customer_2_df$customer_2.12000)
max(customer_2_df$customer_2.12000)
skewness(customer_2_df$customer_2.12000)
kurtosis(customer_2_df$customer_2.12000)
mean(customer_3_df$customer_3.12000)
sd(customer_3_df$customer_3.12000)
min(customer_3_df$customer_3.12000)
max(customer_3_df$customer_3.12000)
skewness(customer_3_df$customer_3.12000)
kurtosis(customer_3_df$customer_3.12000)
mean(customer_4_df$customer_4.12000)
sd(customer_4_df$customer_4.12000)
min(customer_4_df$customer_4.12000)
max(customer_4_df$customer_4.12000)
skewness(customer_4_df$customer_4.12000)
kurtosis(customer_4_df$customer_4.12000)
mean(customer_5_df$customer_5.36000)
sd(customer_5_df$customer_5.36000)
min(customer_5_df$customer_5.36000)
max(customer_5_df$customer_5.36000)
skewness(customer_5_df$customer_5.36000)
kurtosis(customer_5_df$customer_5.36000)
mean(customer_6_df$customer_6.12000)
sd(customer_6_df$customer_6.12000)
min(customer_6_df$customer_6.12000)
max(customer_6_df$customer_6.12000)
skewness(customer_6_df$customer_6.12000)
kurtosis(customer_6_df$customer_6.12000)
mean(customer_7_df$customer_7.12000)
sd(customer_7_df$customer_7.12000)
min(customer_7_df$customer_7.12000)
max(customer_7_df$customer_7.12000)
skewness(customer_7_df$customer_7.12000)
kurtosis(customer_7_df$customer_7.12000)
mean(customer_8_df$customer_8.12000)
sd(customer_8_df$customer_8.12000)
min(customer_8_df$customer_8.12000)
max(customer_8_df$customer_8.12000)
skewness(customer_8_df$customer_8.12000)
kurtosis(customer_8_df$customer_8.12000)


