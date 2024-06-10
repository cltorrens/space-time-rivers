
    
    data {
    int <lower = 1> T;   //number of hours each day is an integer
    int <lower = 1> D;   //number of total days is an integer
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
    }

   model {
    matrix[T,D] conc_hat;
    for (d in 1:D) {
    conc_hat[1,d] = concMA[1,d]; 
    for (i in 2:T){
    conc_hat[i,d]= concMA[i-1,d] -(U[d]*lightMA[i,d])/(zMA[i,d]*sumlight[d])+K[d]*(N_b[d]-concMA[i-1,d])*deltat;
    concMA[i,d]~ normal (conc_hat[i,d], sigma);
    }
    
    }
    K~lognormal(2.25,1.5);
    N_b~normal(57.16,11.98);
    }
    
