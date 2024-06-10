
    
    data {
    int <lower = 1> T;   //number of hours each day (24) is an integer
    int <lower = 1> D;   //number of total days (5) is an integer
    real deltat;     //time increment of sampling (in d) is a real number
    matrix[T,D] lightMA;    //light at each timestep is a matrix
    vector[D] sumlightIdeal; // total summed daily light from StreamMetabolizer is a vector
    vector[D] sumlightReal; // total summed daily light from satellite is a vector
    matrix[T,D] zMA; //depth /channel depth is a matrix
    matrix[T,D] concMA;//nitrate concentration is a matrix
    }
    
    parameters {
    real<lower = 0> sigma; //  standard deviation of NO3 variation relative to model
    vector<lower=0>[D] K;        // Nitrate turnover rate
    vector <lower=0> [D] N_b; // Background nitrate concentration
    vector[D] U;           // assimilatory autotrophic nitrate uptake 
    //real<lower = 0> mean_U; // mean U 
    real<lower = 0> sigma_U; //  standard deviation of mean assimilatory autotrophic nitrate uptake over entire study
    real b0; //intercept of linear relationship between uptake and sum daily light
    real<lower=0> b1; //slope of linear relationship between uptake and sum daily light
    }
    
  //transformed parameters {
    // N_b10 = N_b * 10  // so the mean and sd can be whole-# values?
    
    
  //}

   model {
    matrix[T,D] conc_hat;  
    
    for (d in 1:D) {
    U[d] ~ normal(b0 + b1*sumlightReal[d], sigma_U); //linear relationship between light and uptake - may need to scale as sumlightReal ~ 600
    //U[d] ~ normal(mean_U, sigma_U);   // simple pooling with mean U + sd
    conc_hat[1,d] = concMA[1,d];        // initialize model matrix *each day* using observed data matrix
    
    for (i in 2:T){
    //conc_hat[i,d]= concMA[i-1,d] -(U[d]*lightMA[i,d])/(zMA[i,d]*sumlight[d])+K[d]*(N_b[d]-concMA[i-1,d])*deltat;  //process error - carries forward; do need l38 in transformed params? 
    conc_hat[i,d]= conc_hat[i-1,d] -(U[d]*lightMA[i,d])/(zMA[i,d]*sumlightIdeal[d])+K[d]*(N_b[d]-conc_hat[i-1,d])*deltat; //observation error 
    concMA[i,d]~ normal (conc_hat[i,d], sigma);     // likelihood
            }
         }
   
    // Priors       // if the distribution is not given in the model, it must have a prior
    
    b0~normal(0,5); 
    b1~normal(0,10);  // 
    sigma~normal(0,1); // for no3
    K~lognormal(1,0.5);
    N_b~normal(4, 0.5);  // mean and sd of entire N dataset
    sigma_U~normal(0, 1);//half normal distribution - adjusted, was normal(0, 4.57)
    //mean_U~normal(0, 10);  // relax this for a dataset I've never seen - figure out how relaxed... 
    }
    
    generated quantities {
      matrix[T,D] conc_tilde; 
      matrix[T,D] conc_hat;             // does not carry through from model block. Maybe do that part in Transform Params and not in model block.
      for (d in 1:D) {
      conc_hat[1,d] = concMA[1,d];                     //initialize matrix
      conc_tilde[1,d] = normal_rng(concMA[1,d], sigma);
      
      for (i in 2:T){
    //conc_hat[i,d]= concMA[i-1,d] -(U[d]*lightMA[i,d])/(zMA[i,d]*sumlight[d])+K[d]*(N_b[d]-concMA[i-1,d])*deltat;  // predicted values
        conc_hat[i,d]= conc_hat[i-1,d] -(U[d]*lightMA[i,d])/(zMA[i,d]*sumlightIdeal[d])+K[d]*(N_b[d]-conc_hat[i-1,d])*deltat;
        conc_tilde[i,d] = normal_rng(conc_hat[i,d], sigma);     // each conc_tilde is 1) a new, faked dataset conditioned on the old dataset: posterior predictive diestribution (does it look like the old dataset? IT SHOULD!)  and 2) a full probability distribution of each new datapoint
        }
      }
    }
    
    //plot conc_hat vs concMA to test fit
    
