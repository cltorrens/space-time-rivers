//
// This Stan program defines a simple, unpooled, two-station [N] uptake model based on Heffernan & Cohen and Hall et al. 2016
//
// The first version uses streamMetabolizer light data and a 24-hour-long dataset
//

data { 
  int <lower = 1> T;      //number of hours each day is an integer
  int <lower = 1> D;      //number of total days is an integer
  real tau;               //travel time (in d) is a real number
  matrix[T,D] taulightMA; //light at each timestep is a matrix (hours, days)
  // vector[D] sumlight;     //total summed daily light is a vector
  real<lower=0> sumlight; // FOR 1-DAY MODEL ONLY
  matrix[T,D] zMA;        //depth /channel depth is a matrix
  matrix[T,D] concMA;     //nitrate concentration is a matrix
}

    
parameters { 
  real<lower = 0> sigma;     // standard deviation
  vector<lower=0>[D] K;      // Nitrate concentration's daily change ( d^-1)
  vector <lower=0> [D] N_e;  // Equilibrium nitrate concentration (umol L^-1)
  vector[D] U;               // assimilatory autotrophic nitrate uptake (mmol m^-2 d^-1)
}


model { 
  matrix[T,D] conc_hat;     // initialize modeled-N matrix
  
  for (d in 1:D) {
    conc_hat[1,d] = concMA[1,d]; 
    
      for (i in 2:T){
      conc_hat[i,d] = (N_e[d] - (U[d]*taulightMA[i,d])/(zMA[i,d]*sumlight) + K[d]*(N_e[d]/2)*tau)/(1+K[d]*tau/2);
      // CHANGE THE SUMLIGHT ^^ BACK TO DAILY NOT STATIC FOR THE MULTIDAY MODELS
      concMA[i,d] ~ normal (conc_hat[i,d], sigma); 
      }
    }
    
  // PRIORS  
  sigma~normal(0,1); 
  K~lognormal(1.1,0.5); // 3.00, 1.65
  N_e~normal(5, 1);
  U~normal(3, 1);
}


 generated quantities {
      matrix[T,D] conc_tilde; 
      matrix[T,D] conc_hat;             // does not carry through from model block. Maybe do that part in Transform Params and not in model block.
      for (d in 1:D) {
      conc_hat[1,d] = concMA[1,d];                     //initialize matrix
      conc_tilde[1,d] = normal_rng(concMA[1,d], sigma);
      
      for (i in 2:T){
        conc_hat[i,d]= (N_e[d] - (U[d]*taulightMA[i,d])/(zMA[i,d]*sumlight) + K[d]*(N_e[d]/2)*tau)/(1+K[d]*tau/2);
        // CHANGE THE SUMLIGHT ^^ BACK TO DAILY NOT STATIC FOR THE MULTIDAY MODELS
        conc_tilde[i,d] = normal_rng(conc_hat[i,d], sigma);     // each conc_tilde is 1) a new, faked dataset conditioned on the old dataset: posterior predictive distribution (does it look like the old dataset? IT SHOULD!)  and 2) a full probability distribution of each new datapoint
        }
      }
 }
