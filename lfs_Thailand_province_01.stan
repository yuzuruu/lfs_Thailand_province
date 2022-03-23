// data
data {
  // length of ts
  int<lower=0> TT;
  // num of ts; rows of y
  int<lower=0> N; 
  // number of non-NA values in y
  int<lower=0> n_pos; 
  // col index of non-NA vals
  int<lower=0> col_indx_pos[n_pos]; 
  // row index of non-NA vals
  int<lower=0> row_indx_pos[n_pos]; 
  vector[n_pos] y; 
}
// parameter
parameters {
  // initial states
  vector[N] x0;
  // // mean of intrinsic growth rate
  // real u; 
  // // refed as pro_dev[TT,N]
  // vector[N] province_dev[TT]; 
  // // vector [N] season[TT]; 
  // real<lower=0> s_q[N];      //////////////////////////
  // real<lower=0> s_season[N]; //////////////////////////
  real<lower=0> s_x[N]; 
  real<lower=0> s_r[N]; 
  vector<lower = -pi()/2, upper = pi()/2> [N] mu_raw[TT-1];
}
transformed parameters {
  vector[N] x[TT]; 
  // Computing state space (x)
  for(i in 1:N){
    // initial state
    // x[1,i] = x0[i] + u + province_dev[1,i];
    x[1,i] = x0[i];
    // states after the 2nd. quarter
    // To detect changing points, we transform the x using cauchy distribution
    for(t in 2:TT) {
      // x[t,i] = x[t-1,i] + u + province_dev[t,i];
      // x[t,i] = x[t-1,i] + s_x[i]*tan(mu_raw[t-1,i]) + u + province_dev[t,i];
      x[t,i] = x[t-1,i] + s_x[i]*tan(mu_raw[t-1,i]);
    }
  }
}
// model
model {
  //prior of u
  // u ~ normal(0,100);
  // 
  for(i in 1:N){
    // prior of x0, s_r, s_q, s_season
    x0[i] ~ cauchy(y[i],1000000);
    // s_r[i] ~ student_t(3, 0, 10);
    // s_q[i] ~ student_t(3, 0, 5);
    // s_season[i] ~ student_t(3, 0, 5);
    // s_r[i] ~ normal(0, 100);
    // s_q[i] ~ normal(0, 100);
    // s_q[i] ~ cauchy(0,100);
    // s_season[i] ~ normal(5, 10);
    // estimating deviance by province
    // for(t in 1:TT){
    //   province_dev[t,i] ~ cauchy(0, s_q[i]);
    // }
    // // seasonal fluctuation by province
    // for(t in 4:TT){
    //   season[t, i] ~ normal(-sum(season[(t-3):(t-1),i]), s_season[i]);
    // }
  }
  // observation space
  for(i in 1:n_pos){
    // y[i] ~ normal(x[col_indx_pos[i], row_indx_pos[i]] + season[col_indx_pos[i], row_indx_pos[i]], s_r[row_indx_pos[i]]);
    y[i] ~ normal(x[col_indx_pos[i], row_indx_pos[i]], s_r[row_indx_pos[i]]);
  }
}
// computing likelihood
generated quantities {
  // 
  vector[n_pos] log_lik;
  vector[N] yhat[TT]; // refed as yhat[TT,N]。
  // likelyhood
  // for (n in 1:n_pos) log_lik[n] = normal_lpdf(y[n] | x[col_indx_pos[n], row_indx_pos[n]] + season[col_indx_pos[n], row_indx_pos[n]], s_r[row_indx_pos[n]]);
  for (n in 1:n_pos) log_lik[n] = normal_lpdf(y[n] | x[col_indx_pos[n], row_indx_pos[n]], s_r[row_indx_pos[n]]);
  // estimate Y using the estimated results above
  for(i in 1:N){
    for(t in 1:TT){
      // yhat[t,i] = normal_rng(x[t,i] + season[t,i], s_r[i]);
      yhat[t,i] = normal_rng(x[t,i], s_r[i]);
      }
      }
    }
