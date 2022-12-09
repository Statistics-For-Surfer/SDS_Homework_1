rm(list = ls())    # Clean the environment
library(VGAM)    
library(readr)  

##### Load the dataset #####
fitness <- read_csv("fitness.csv")   # Read the dataset
n <- nrow(fitness)                   # Number of observations

colnames(fitness) <- c("index" , "Frequences_per_mounth")   # Rename the column
fitness$index <- seq(from = 1 , to = n , by = 1)            # Indexing
##### Plotting & Summary statistics #####
# Historam 
par(mfrow = c(1,1))
hist(fitness$Frequences_per_mounth , col = "darkgoldenrod1" , 
     main = "Work out" , freq = F , xlab = "times work out",
     border = "white")
box()
summary(fitness$Frequences_per_mounth)
# Normalize the data  
maximum <- max(fitness$Frequences_per_mounth)
minimun <-  min(fitness$Frequences_per_mounth)
fitness$norm <-  round(( fitness$Frequences_per_mounth -  minimun ) / (maximum - minimun) , 2)
##### Build the privatized function #####
privatized_engine <- function(
    data = fitness$norm, m = 20 , h = 1 / m , eps = 0.1  )
  {
  n = length(data)        # Number of the observations
  bins <-  seq(0,1,h)     # Number of bins
  intervals <- cut( data, bins, include.lowest = T)   
  pj_hat <- table(intervals) / n   
  p_hat <- as.vector(pj_hat / h)
  nu <- rlaplace(m, 0, 2/eps)  # Perturbeb data
  Dj <- table(intervals) + nu 
  Dj[Dj < 0] = 0 
  qj_hat = Dj
  if (sum(qj_hat) != 0){qj_hat <- qj_hat / sum(qj_hat)} else {qj_hat <- rep(0, length(qj_hat))}
  

  q_absfre <- round(qj_hat * n , 0 )

 
  Z <- c()         #Build the new dataset
  i <- 0
  for ( x in q_absfre ){
    i <- i + 1
    Z <- c(Z, runif(x, bins[i],bins[i+1]))
  }
  
  private_dat <- data.frame(Z)
  
  private_dat$index <- seq(from = 1 , to = length(private_dat) , 1) 
  
  colnames(private_dat) <- c("privatized_times_norm" , "index")
  private_dat$privatized_times <- (private_dat$privatized_times_norm) * maximum
  return(private_dat)
  
}





##### Different set-up ####
m_vec   <- c(  5   ,  10 ,  15 , 20)    # Number of bins
eps_vec <- c( .001 , .01 , .1  ,  1)    # level of privacy
k_vec   <- c(  25  ,  50 ,  75 , 100 )  # (half -  3/4  - all) of the data 

##### Run over m let be fixed the other parameters #####
summary_stats_m <- matrix( NA , nrow = length(m_vec) , ncol = 6)
hist_m <- c()
colnames(summary_stats_m) <-  c("Min", "1st Qu" , "Median" , "Mean" , "3rd Qu" , "Max" )
rownames(summary_stats_m) <- m_vec
par(mfrow= c(2,2))
i <- 1
for (x in m_vec){
  dataset <- privatized_engine(data = fitness$norm , m = x)
  summary_stats_m[i,] <- summary(dataset$privatized_times) 
 
  hist(dataset$privatized_times, main = paste(" privatized dataset \n with m set to: " , x ) , xlab = 'times per month' , freq = F , col = "cyan4")
  box()
  i <- i + 1
}


summary(fitness$Frequences_per_mounth)
summary_stats_m
#####

#### Run over eps let be fixed the other parameters ####
summary_stats_eps <- matrix( NA , nrow = length(eps_vec) , ncol = 6)
hist_eps <- c()
colnames(summary_stats_eps) <-  c("Min", "1st Qu" , "Median" , "Mean" , "3rd Qu" , "Max" )
rownames(summary_stats_eps) <- eps_vec
par(mfrow= c(2,2))
i <- 1
for (x in eps_vec){
  dataset <- privatized_engine(data = fitness$norm , eps = x)
  summary_stats_eps[i,] <- summary(dataset$privatized_times) 
  
  hist(dataset$privatized_times, main = paste(" privatized dataset \n with eps set to: " , x ) , xlab = 'times per month' , freq = F , col = "cyan4")
  box()
  i <- i + 1
}
summary(fitness$Frequences_per_mounth)
summary_stats_eps
#####


##### Run over k let be fixed the other parameters #####
summary_stats_k <- matrix( NA , nrow = length(k_vec) , ncol = 6)
colnames(summary_stats_m) <-  c("Min", "1st Qu" , "Median" , "Mean" , "3rd Qu" , "Max" )
rownames(summary_stats_m) <- k_vec
par(mfrow= c(2,2))
i <- 1
for (x in k_vec){
  dataset <- privatized_engine(data = fitness$norm[1:x])
  summary_stats_k[i,] <- summary(dataset$privatized_times) 
  
  hist(dataset$privatized_times, main = paste(" privatized dataset \n with k set to: " , x ) , xlab = 'times per month' , freq = F , col = "cyan4")
  box()
  i <- i + 1
}


summary(fitness$Frequences_per_mounth)
summary_stats_k
#####

