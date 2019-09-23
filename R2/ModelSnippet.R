
dObs <- Csnippet("
                 
                 if (R_FINITE(reports)) {
                 if ( (reports > 0) & (cases == 0) ) {
                 lik = (give_log) ? log(1e-16) : 1e-16;
                 } else {
                 lik = dpois(reports, cases * obsprob, give_log);
                 //printf(\"%f %f %f\\n \",lik, cases , obsprob);
                 }
                 } else {
                 lik = (give_log) ? 0 : 1;
                 }
                 //printf(\" %f \\n \",lik);
                 ")

rObs <- Csnippet("
                 reports = rpois(obsprob*cases);
                 ")

skel <- Csnippet("
                 double d_el = 0;
                 if (temperature>=8.4) {
                 d_el = -0.00001*pow(temperature,2) + 0.002*temperature - 0.019;
                 if (d_el<0) {d_el = 0;}
                 }
                 
                 double d_ln = 0;
                 if (temperature>=7.4) {
                 d_ln = 0.00003*pow(temperature,2) + 0.00073*temperature - 0.007;
                 if (d_ln<0) {d_ln = 0;}
                 }
                 
                 double d_na = 0;
                 if (temperature>=8.7) {
                 d_na = - 0.000008*pow(temperature,2) + 0.0019*temperature - 0.016;
                 if (d_na<0) {d_na = 0;}
                 }
                 
                 double d_pop = 0;
                 if (temperature>=4) {
                 d_pop = -0.00001867*pow(temperature,3) + 0.0008724*pow(temperature,2) - 0.006195*temperature + 0.01802;
                 if (d_pop<0) {d_pop = 0;}
                 }
                 
               double a_l = 0;
                 double pQL = 0;
                 if (temperature >= T_min_l)  {
                 pQL =  1;
                 if (pQL<0) { pQL = 0;}
                 }
                 a_l = pQL * lambda_l;
                 
                 double a_n = 0;
                 double pQN = 0;
                 if (temperature >= 7)  {
                 pQN = 1;
                 if (pQN<0) { pQN = 0;}
                 }
                 a_n = pQN * lambda_n;
                 
                 
                 double a_a = 0;
                 double pQA = 0;
                 if (temperature >= 7)  {
                 pQA = 1;
                 if (pQA<0) { pQA = 0;}
                 }
                 a_a = pQA * lambda_a;
                 
                 double lambda_hum = alpha * exp(0.058*temperature);
                 double beta_n = kappa * lambda_hum * pQN;
                 double beta_a = lambda_hum * pQA;
                 double d = 1 - pow((1-c),Tf*a_n*QN_i);
                 
                 DE = p*delta*d_pop*exp(-omega*delta*d_pop*EA)*EA - d_el*E - mu_e*E;
                 DQL = d_el*E - a_l*QL - mu_ql*QL;
                 DEL_s = (1-d)*((1-beta_hl)*H_i+(1-H_i))*f_l*a_l*QL - d_ln*EL_s - mu_el*EL_s; 
                 DEL_i = d*((1-beta_hl)*H_i+(1-H_i))*f_l*a_l*QL + beta_hl*f_l*a_l*QL*H_i - d_ln*EL_i - mu_el*EL_i; 
                 DQN_s = d_ln*EL_s - a_n*QN_s - mu_qn*QN_s;
                 DQN_i = d_ln*EL_i - a_n*QN_i - mu_qn*QN_i;
                 DEN_s = (1-d)*((1-beta_hn)*H_i+(1-H_i))*f_n*a_n*QN_s  - d_na*EN_s - mu_en*EN_s;
                 DEN_i = d*((1-beta_hn)*H_i+(1-H_i))*f_n*a_n*QN_s + beta_hn*f_n*a_n*QN_s*H_i + f_n*a_n*QN_i- d_na*EN_i - mu_en*EN_i;
                 DQA_s = d_na*EN_s - a_a*QA_s - mu_qa*QA_s;
                 DQA_i = d_na*EN_i - a_a*QA_i - mu_qa*QA_i;
                 DEA = f_a*a_a*(QA_s+QA_i) - d_pop*EA - mu_ea*EA;
                 DH_s = mu_h - beta_nh*a_n*QN_i*H_s - mu_h * H_s;
                 DH_i = beta_nh*a_n*QN_i*H_s - gamma*H_i - mu_h * H_i;
                 Dcases = beta_n*QN_i + beta_a*QA_i;
                 //if( t < 100){
                 //printf(\" t =%f; temp=%f  ;E= %f\\n \",t , temperature, E);}
                 ")

statenames <- c("E","QL","EL_s","EL_i","QN_s","QN_i","EN_s","EN_i","QA_s","QA_i","EA","H_s","H_i")

rInit <- Csnippet("
                  E = E0;
                  QL = QL0;
                  EL_s = EL_s0; 
                  EL_i = EL_i0; 
                  QN_s = QN_s0;
                  QN_i = QN_i0;
                  EN_s = EN_s0;
                  EN_i = EN_i0;
                  QA_s = QA_s0;
                  QA_i = QA_i0;
                  EA = EA0;
                  H_s = H_s0;
                  H_i = H_i0;
                  cases = 0;
                  ")
