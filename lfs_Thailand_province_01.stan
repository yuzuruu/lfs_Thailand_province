// タイ都県別CLF推移から人的資源漁を推定する
// 24th. March 2022
// Yuzuru Utsunomiya, Ph. D.
// 
// もとにしたコードはココ↓
// 1.モデル全般
// https://atsa-es.github.io/atsa-labs/sec-marss-fitting-with-stan.html
// 2.stan神こと松浦先生による御作
// https://www.kyoritsu-pub.co.jp/bookdetail/9784320112421
// 
// 近頃、こういうメッセージが出る。Linuxは出ないがWSLは出る。
// 書法が変わるらしいけれど…。
// Compiling Stan program...
// Warning in '/tmp/RtmpN33yJu/model-1a0d55ec6cf1.stan', line 14, column 2: Declaration
//     of arrays by placing brackets after a variable name is deprecated and
//     will be removed in Stan 2.32.0. Instead use the array keyword before the
//     type. This can be changed automatically using the auto-format flag to
//     stanc

// data
data {
  // length of ts
  // 期間数
  int<lower=0> TT;
  // num of ts; rows of y
  // 都県数。77になるはず。
  int<lower=0> N; 
  // number of non-NA values in y
  // 欠損ではない値の数
  int<lower=0> n_pos; 
  // col index of non-NA vals
  // 欠損ではない値の列方向位置。
  int<lower=0> col_indx_pos[n_pos]; 
  // row index of non-NA vals
  // 欠損ではない値の行方向位置。
  // 行列としてデータを与えなくてもよいふうにする工夫。
  int<lower=0> row_indx_pos[n_pos]; 
  // 観測値そのもの。
  vector[n_pos] y; 
}
// parameter
parameters {
  // initial states
  vector<lower=0>[N] x0;
  // // mean of intrinsic growth rate
  // 全体の平均
  // 各年・都県毎にみられる差異は、transformed parameter部にて宣言。
  real<lower=0> u;
  // // 季節変動項
  // vector [N] season[TT];
  // // 季節変動項の標準偏差
  // real<lower=0> s_season[N];
  // 変化点検出に使う標準偏差。
  real<lower=0> s_x[N]; 
  // 状態空間の標準偏差。
  real<lower=0> s_r[N]; 
  // Cauchy分布を使って変化点検出するワザの一部。
  // Stan神が教えてくれた。
  vector<lower = -pi()/2, upper = pi()/2> [N] mu_raw[TT-1];
  // 四半期・都県別偏差。
  // 
  vector[N] pro_dev[TT]; // refed as pro_dev[TT,N]
  // 
  real<lower=0> sd_q;
}
transformed parameters {
  // 各年・都県毎にみられる差異。
  vector[N] x[TT]; 
  // Computing state space (x)
  // 状態空間を計算するよ。
  for(i in 1:N){
    // initial state
    // 初期値は別途に推定する。
    // x[1,i] = x0[i] + u + province_dev[1,i];
    x[1,i] = x0[i] + u + pro_dev[1,i];
    // states after the 2nd. quarter
    // 2期目以降の状態空間
    for(t in 2:TT) {
      // Cauchy分布を使って変化点検出するワザの一部。
      // Stan神が教えてくれた。
      // Page
      x[t,i] = x[t-1,i] + u + pro_dev[t,i] + s_x[i]*tan(mu_raw[t-1,i]);
    }
  }
}
// model
model {
  sd_q ~ cauchy(0,5);
  //prior of u
  // uは全体の平均。
  // Cauchy分布を使う。CLF@BKKは極端に大きいから。
  // 分散をごく大きくする。BKKとその他色々ではCLFが極端に異なることがあるから。
  u ~ cauchy(0,100);
  for(i in 1:N){
    // prior of x0, s_r, s_q, s_season
    // その他いろいろ変数事前分布。
    // xの初期値に事前分布を与える
    // ここまで大きな分散は必要ないかもしれない。
    // が、結果を眺める限りけっこう大きな分散がいる。
    // 大きな数を扱うときはなかなか気を遣うね。
    x0[i] ~ cauchy(y[i],1000000);
     for(t in 1:TT){
    pro_dev[t,i] ~ normal(0, sd_q);
    }
    // s_r[i] ~ student_t(3, 0, 10);
    // // s_season[i] ~ student_t(3, 0, 5);
    // // seasonal fluctuation by province
    // 季節調整項は、4期を考える。四半期データであるしなにより折れ線グラフが
    // 4期な変動を語っている気がする。
    // 期間を通じて、都県別標準偏差は不変にしましたよっと。
    // for(t in 4:TT){
    //   season[t, i] ~ normal(-sum(season[(t-3):(t-1),i]), s_season[i]);
    // }
  }
  // observation space
  // 観測空間。
  // ごく簡単なローカル線形トレンドモデル。
  // 季節変動やその他いろいろも組み込まないと。
  // でも季節変動を組み込むと、なかなか収束しないのよ。
  // イテレーションが400回（うちバーンイン200回）くらいで、Rhatが1.3より小さいくらい。
  // イテレーション回数を増やせば、収束するでしょう。
  // その他いろいろ叱られメッセージは、いろいろ設定変更すればなんとか。
  // ただ、時間がかかるのよ。
  for(i in 1:n_pos){
    // y[i] ~ normal(x[col_indx_pos[i], row_indx_pos[i]] + season[col_indx_pos[i], row_indx_pos[i]], s_r[row_indx_pos[i]]);
    y[i] ~ normal(x[col_indx_pos[i], row_indx_pos[i]], s_r[row_indx_pos[i]]);
  }
}

generated quantities {
  vector[n_pos] log_lik;
  vector[N] yhat[TT]; // refed as yhat[TT,N]。
  // likelyhood
  // for (n in 1:n_pos) log_lik[n] = normal_lpdf(y[n] | x[col_indx_pos[n], row_indx_pos[n]] + season[col_indx_pos[n], row_indx_pos[n]], s_r[row_indx_pos[n]]);
  for (n in 1:n_pos) log_lik[n] = normal_lpdf(y[n] | x[col_indx_pos[n], row_indx_pos[n]], s_r[row_indx_pos[n]]);
  // estimate Y using the estimated results above
  // 結果を使ってCLFを予測する。
  for(i in 1:N){
    for(t in 1:TT){
      yhat[t,i] = normal_rng(x[t,i], s_r[i]);
      // yhat[t,i] = normal_rng(x[t,i] + season[t, i], s_r[i]);
      }
      }
    }
