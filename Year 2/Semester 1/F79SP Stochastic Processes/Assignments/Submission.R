# F79SP Coursework: Manufacturing Processess
################################################################################

################################################################################
# Initializing variables
################################################################################

# Set Seed to reproduce results
set.seed(6014)

# HWUM ID: H00456014
D = 4 # last digit of HWUM ID

# Define manufacturing processes' transition matrix
P <-matrix( c(0.5, 0.5, 0, 0, 0, 0,
              0.5, 0.2, 0.2, 0, 0, 0.1, 
              0, 0, 0.8, 0.2 * (0.6 + D/50), 0.2 * (0.2 - D/100), 0.2 * (0.2 - D/100), 
              0, 0, 0, 1, 0, 0, 
              0, 0, 0, 0, 1, 0, 
              0, 0, 0, 0, 0, 1),
              nrow=6,ncol=6,byrow= TRUE)

# All input items start from Machine A
Proposal_1 = 1

# All input items start from Machine B
Proposal_2 = 2

# All input items start from either Machine A or Machine B at random, with equal probability
# Proposal_3 = sample(x=2, size=1, prob= c(0.5, 0.5))
Proposal_3 = 4

# Costs per time-step
Material_Cost = 2
Machine_A_Cost = 0.1
Machine_B_Cost = 0.1
Machine_C_Cost = 0.2

# Selling price of products
Price_e = 9
Price_s = 6
Price_d = 0

################################################################################
# Simulations
################################################################################

# Main simulation function
Simulate_Manufacturing_Process = function(n, X_1){
  trials<-n
  
  # Initialising Counters
  totalsteps = 0
  totalABsteps = 0
  hitfour = 0
  hitfive = 0
  hitsix = 0
  Total_cost = 0
  Total_manu_cost = 0
  Total_revenue = 0
  Total_profit = 0
  
  for( j in 1:trials){
    
    # Initialise starting point
    X = X_1
    
    # For proposal 3
    if(X == 4){
      X = sample(x=2, size=1, prob= c(0.5, 0.5))
    }
    
    Total_cost = Total_cost + Material_Cost
    
    i = 1 # Number of steps
    j = 1 # Number of steps within Machine A and B
    while(X[i] < 4){
      
      # Checks if step is still in Machine A or B
      if(X[i] < 3){
        j = j + 1
      }
      
      p<-P[X[i],] # Calculate the p values
      X[i+1]<-sample(x=6, size=1, prob=p) # update the chain
      i = i + 1
      
      # Previous Machine Cost Counter
      if(X[i-1] == 1){
        Total_cost = Total_cost + Machine_A_Cost
        Total_manu_cost = Total_manu_cost + Machine_A_Cost
      }else if (X[i-1] == 2){
        Total_cost = Total_cost + Machine_B_Cost
        Total_manu_cost = Total_manu_cost + Machine_B_Cost
      }else if (X[i-1] == 3){
        Total_cost = Total_cost + Machine_C_Cost
        Total_manu_cost = Total_manu_cost + Machine_C_Cost
      }
    }
    
    # Time Counter
    totalsteps = totalsteps + (i-1)
    # Time Counter for Machine A and B only
    totalABsteps = totalABsteps + (j-1)
    
    # Hit Product & Revenue Counter
    if(X[i]==4){
      hitfour <- hitfour + 1
      Total_revenue = Total_revenue + Price_e
    }else if(X[i]==5){
      hitfive <- hitfive + 1
      Total_revenue = Total_revenue + Price_s
    }else if(X[i]==6){
      hitsix <- hitsix + 1
      Total_revenue = Total_revenue + Price_d
    }
    
  }
  
  # Expected Time
  time_est<-totalsteps/trials
  # Expected AB Time
  time_AB_est = totalABsteps/trials
  
  # Expected Proportions
  fourbest = hitfour/trials
  fivebest = hitfive/trials
  sixbest = hitsix/trials
  
  # Expected Costs
  T_cost_est = Total_cost/trials
  cost_est = Total_manu_cost/trials
  
  # Expected Profit
  Total_profit = Total_revenue - Total_cost
  profit_est = Total_profit/trials
  
  return(c(time_est, fourbest, fivebest, sixbest, time_AB_est, T_cost_est, profit_est, cost_est))
}

# Generating Information Vectors
Info_vec_1 = Simulate_Manufacturing_Process(n = 10000, X = Proposal_1)
Info_vec_2 = Simulate_Manufacturing_Process(n = 10000, X = Proposal_2)
Info_vec_3 = Simulate_Manufacturing_Process(n = 10000, X = Proposal_3)

################################################################################
# Plot of stationary distribution
################################################################################

simulate.chain<-function(trials,n,P,X0){
  m <-dim(P)[1]
  
  #Vectors to store proportion of simulations in each state
  levels<-matrix(0,nrow=m,ncol=n+1)
  for(j in 1:trials){
    if(X0 == 3){
      X = sample(x=2, size=1, prob= c(0.5, 0.5))
    } else {
      X<-X0 #initial state of the Markov Chain
    }
    
    levels[X, 1] =levels[X, 1] +1 #Record the initial state
    for(i in 1:n){
      p<-P[X[i],] #Get the p values
      X[i+1]<-sample(x=m,size=1,prob=p) #update the chain
      levels[X[i+1],i+1] =levels[X[i+1],i+1] +1
    }
  }
  levels<-levels/trials
}

m = 6
n = 40

levels <- simulate.chain(10000, n, P, 1)
levels2 <- simulate.chain(10000, n, P, 2)
levels3 <- simulate.chain(10000, n, P, 3)

plotting = function (m, n, levels, title){
  #Plot the evolution of the proportion at each state
  plot(levels[1,], type='l', xlim=c(0,n+1), ylim=c(0,1), xlab='Steps',
       ylab='Proportion', col=1,lwd=2, main = title)
  
  for (i in 2:m){
    lines(levels[i,], col=i, lwd=2)
  }
  
  labels <- sapply(1:m, function(x) { paste("State", x)})
  
  legend("topright", labels, col=1:m, lwd=2)
  
  #lines representing the stationary distribution
  exact <- c(0, 0, 0,  0.4533, 0.1067,  0.44)
  for (i in 1:m){
    lines(exact[i]*rep(1,n+1), col=i)
  }
}

plotting(m,n, levels, "Convergence of proportions for Proposal 1")
plotting(m,n, levels2, "Convergence of proportions for Proposal 2")
plotting(m,n, levels3, "Convergence of proportions for Proposal 3")

################################################################################
# Probability of Quality of Product, Given in Machine C
################################################################################

Start_Machine_C = 3
Info_vec_4 = Simulate_Manufacturing_Process(n = 10000, X = Start_Machine_C)

# Probability of excellent product
Quantity1 = Info_vec_4[2]
Prob_e = Quantity1

# Probability of satisfactory product
Prob_s = Info_vec_4[3]

# Probability of defective product
Quantity2 = Info_vec_4[4]
Prob_d = Quantity2

################################################################################
# Expected Manufacturing Time
################################################################################

# Proposal 1
Quantity3 = Info_vec_1[1]
Est_Time_1 = Quantity3

# Proposal 2
Quantity4 = Info_vec_2[1]
Est_Time_2 = Quantity4

# Proposal 3
Est_Time_3 = Info_vec_3[1]

################################################################################
# Expected Proportion of Quality of Products
################################################################################

### Proposal 1
# Excellent products
Est_Prob_1_e = Info_vec_1[2]
Quantity5 = Est_Prob_1_e

# Satisfactory products
Est_Prob_1_s = Info_vec_1[3]
Quantity6 = Est_Prob_1_s

# Defective products
Est_Prob_1_d = Info_vec_1[4]
Quantity7 = Est_Prob_1_d

### Proposal 2
# Excellent products
Est_Prob_2_e = Info_vec_2[2]
Quantity8 = Est_Prob_2_e

# Satisfactory products
Est_Prob_2_s = Info_vec_2[3]
Quantity9 = Est_Prob_2_s

# Defective products
Est_Prob_2_d = Info_vec_2[4]
Quantity10 = Est_Prob_2_d

### Proposal 3
# Excellent products
Est_Prob_3_e = Info_vec_3[2]

# Satisfactory products
Est_Prob_3_s = Info_vec_3[3]

# Defective products
Est_Prob_3_d = Info_vec_3[4]

################################################################################
# Expected Total Time Spent in Machine A and B
################################################################################

# Proposal 1
Est_Time_AB_1 = Info_vec_1[5]
Quantity11 = Est_Time_AB_1

# Proposal 2
Est_Time_AB_2 = Info_vec_2[5]
Quantity12 = Est_Time_AB_2

# Proposal 3
Est_Time_AB_3 = Info_vec_3[5]

################################################################################
# Expected Total Manufacturing Costs
################################################################################

# Proposal 1
Est_Cost_1 = Info_vec_1[6]
Quantity13 = Info_vec_1[8]

# Proposal 2
Est_Cost_2 = Info_vec_2[6]
Quantity14 = Info_vec_2[8]

# Proposal 3
Est_Cost_3 = Info_vec_3[6]

################################################################################
# Expected Profit of a Final Product
################################################################################

# Proposal 1
Est_Profit_1 = Info_vec_1[7]
Quantity15 = Est_Profit_1

# Proposal 2
Est_Profit_2 = Info_vec_2[7]

# Proposal 3
Est_Profit_3 = Info_vec_3[7]
Quantity16 = Est_Profit_3

################################################################################
# Print Results (4 d.p.)
################################################################################
#print(round(c(Est_Time_1, Est_Time_2, Est_Time_3, Est_Prob_1_e, Est_Prob_1_s, Est_Prob_1_d, 
#              Est_Prob_2_e, Est_Prob_2_s, Est_Prob_2_d, Est_Prob_3_e, Est_Prob_3_s, Est_Prob_3_d, 
#              Est_Cost_1, Est_Cost_2, Est_Cost_3, Est_Profit_1, Est_Profit_2, Est_Profit_3),4))
#######################################END######################################
