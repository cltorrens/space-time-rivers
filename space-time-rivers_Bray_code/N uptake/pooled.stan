
    
    data {
    int <lower = 1> T;   //number of hours each day (24) is an integer
    int <lower = 1> D;   //number of total days (5) is an integer
    real deltat;     //time increment of sampling (in d) is a real number
    matrix[T,D] lightMA;    //light at each timestep is a matrix
    vector[D] sumlight; // total summed daily light is a vector
    matrix[T,D] zMA;//depth /channel depth is a matrix
    matrix[T,D] concMA;//nitrate concentration is a matrix
    }
    
    parameters {
    real<lower = 0> sigma; //  standard deviation
    vector<lower=0>[D] K;        // Nitrate rate
    vector <lower=0> [D] N_b;// Background nitrate concentration
    vector[D] U;           // assimilatory autotrophic nitrate uptake
    real<lower = 0> sigma_U; //  standard deviation of mean assimilatory autotrophic nitrate uptake over entire study
    real b0;//intercept of linear relationship between uptake and sum daily light
    real<lower=0> b1;//slope of linear relationship between uptake and sum daily light
    }

   model {
    matrix[T,D] conc_hat;
    for (d in 1:D) {
    U[d] ~ normal(b0 + b1*sumlight[d]*1e-6, sigma_U);//linear relationship between light and uptake
    conc_hat[1,d] = concMA[1,d]; 
    for (i in 2:T){
    conc_hat[i,d]= concMA[i-1,d] -(U[d]*lightMA[i,d])/(zMA[i,d]*sumlight[d])+K[d]*(N_b[d]-concMA[i-1,d])*deltat;
    concMA[i,d]~ normal (conc_hat[i,d], sigma);
    }
    
    }
    b0~normal(0,4.57);
    b1~normal(0,10);
    K~lognormal(2.25,1.5);
    N_b~normal(57.16,11.98);
    sigma_U~normal(0,4.57);//half normal distribution
    }
    
