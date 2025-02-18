
    
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
    
    // process error and obs error equations (L36, 39, 40) can live up in the transformed params block. I still need the double loops in the model.
    // = are transfrmed params, ~ stay in the model block. 
            }
         }
   
    // Priors       // if the distribution is not given in the model, it must have a prior
    
    b0~normal(0,4.57); 
    b1~normal(0,10);  // 
    sigma~normal(0,1); // for no3
    K~lognormal(1,0.5);
    N_b~normal(52,3);  // mean and sd of entire N dataset  // use location and scale not mean and sd => general terms for center and breadth of the distribution
    sigma_U~normal(0, 1);//half normal distribution - adjusted, was normal(0, 4.57)
    //mean_U~normal(0, 10);  // relax this for a dataset I've never seen - figure out how relaxed... 
    // if any of these data points need to be changed by site, ** make them data ** - should be able to use the same stan code for all sites
   
    }
    
    generated quantities {
      matrix[T,D] conc_tilde; 
      matrix[T,D] conc_pred;             // does not carry through from model block. Maybe do that part in Transform Params and not in model block.
      for (d in 1:D) {
      conc_pred[1,d] = concMA[1,d];                     //initialize matrix
      conc_tilde[1,d] = normal_rng(concMA[1,d], sigma);
      
      for (i in 2:T){
    //conc_hat[i,d]= concMA[i-1,d] -(U[d]*lightMA[i,d])/(zMA[i,d]*sumlight[d])+K[d]*(N_b[d]-concMA[i-1,d])*deltat;  // predicted values - 1 step ahead.  
        conc_pred[i,d]= conc_pred[i-1,d] -(U[d]*lightMA[i,d])/(zMA[i,d]*sumlightIdeal[d])+K[d]*(N_b[d]-conc_pred[i-1,d])*deltat; //put conc_hat version in transformed parameters 
        conc_tilde[i,d] = normal_rng(conc_pred[i,d], sigma);     // each conc_tilde is 1) a new, faked dataset conditioned on the old dataset: posterior predictive diestribution (does it look like the old dataset? IT SHOULD!)  and 2) a full probability distribution of each new datapoint
        }
      }
    }
    
    //plot conc_hat vs concMA to test fit
    
