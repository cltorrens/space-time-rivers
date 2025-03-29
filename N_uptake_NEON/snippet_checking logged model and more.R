##### RANDOM CODE SNIPPETS FOR NEON NO3

# SNIPPET - plotting model, checking the log/ unlogged method is working OK
plot(sumlight.real, U_mod_avg_bigc)
summary(lm(U_mod_avg_bigc~sumlight.real))
plot(exp(log(sumlight.real)), U_mod_avg_bigc, log="xy", main="U vs. real light, log-log axes; Big Creek, CA")
plot(sumlight.real, exp(0.06 + log(sumlight.real)*0.000033), col="red")

summary(lm(log(U_mod_avg_bigc)~log(sumlight.real)))

exp(-8.741055) # 0.0001598851
exp(0.836083 ) # 2.307312



# ANOTHER SNIPPET - which parameters to print

print(bigc19.fit.new02, pars=c("sigma", "sigma_U", "b0", "b1"))




# for (i in 2:T){
#   conc_hat[i,d] = concMA[i-1,d] -(U[d]*lightMA[i,d])/(zMA[i,d]*sumlightIdeal[d])+K[d]*(N_e[d]-concMA[i-1,d])*deltat;  //process error - carries forward; do need l38 in transformed params? 
#     //conc_hat[i,d] = conc_hat[i-1,d] -(U[d]*lightMA[i,d])/(zMA[i,d]*sumlightIdeal[d])+K[d]*(N_e[d]-conc_hat[i-1,d])*deltat; //observation error 
#     concMA[i,d]~ normal (conc_hat[i,d], sigma);     // likelihood
#     
    

summary