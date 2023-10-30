trade_dispute_data <- read.csv("C:\\Users\\User\\Documents\\Honours\\Thesis\\Datasets\\Ready to Go\\trade_dispute_data_dispute_form.csv")

#from 1995 to 2022
relevantdisputes <- head(trade_dispute_data, n=615)
disputeswherepr <- relevantdisputes[relevantdisputes$panel_requested == 'Yes',]

#number of unique complainants all disputes
complainants <- array(c(relevantdisputes$complainant_a,relevantdisputes$complainant_b,relevantdisputes$complainant_c,relevantdisputes$complainant_d,relevantdisputes$complainant_e,relevantdisputes$complainant_f,relevantdisputes$complainant_g,relevantdisputes$complainant_h,relevantdisputes$complainant_i),dim = c(615, 5, 1))
as.data.frame(table(complainants))

#number of unique respondents all disputes
respondents <- array(c(relevantdisputes$respondent_a,relevantdisputes$respondent_b,relevantdisputes$respondent_c,relevantdisputes$respondent_d,relevantdisputes$respondent_e),dim = c(615,5,1))
as.data.frame(table(respondents))

  #number of unique third parties all disputes
thirdparties <- array(c(relevantdisputes$thirdparty_a,relevantdisputes$thirdparty_b,relevantdisputes$thirdparty_c,relevantdisputes$thirdparty_d,relevantdisputes$thirdparty_e,relevantdisputes$thirdparty_f,relevantdisputes$thirdparty_g,relevantdisputes$thirdparty_h,relevantdisputes$thirdparty_i,relevantdisputes$thirdparty_j,relevantdisputes$thirdparty_k,relevantdisputes$thirdparty_l,relevantdisputes$thirdparty_m,relevantdisputes$thirdparty_n,relevantdisputes$thirdparty_o,relevantdisputes$thirdparty_p,relevantdisputes$thirdparty_q,relevantdisputes$thirdparty_r,relevantdisputes$thirdparty_s,relevantdisputes$thirdparty_t,relevantdisputes$thirdparty_u,relevantdisputes$thirdparty_v,relevantdisputes$thirdparty_w,relevantdisputes$thirdparty_x,relevantdisputes$thirdparty_y,relevantdisputes$thirdparty_z,relevantdisputes$thirdparty_aa,relevantdisputes$thirdparty_ab,relevantdisputes$thirdparty_ac,relevantdisputes$thirdparty_ad,relevantdisputes$thirdparty_ae,relevantdisputes$thirdparty_af,relevantdisputes$thirdparty_ag,relevantdisputes$thirdparty_ah,relevantdisputes$thirdparty_ai),dim = c(615,35,1))
as.data.frame(table(thirdparties))

#number of unique complainants where panel requested
prcomplainants <- array(c(disputeswherepr$complainant_a,disputeswherepr$complainant_b,disputeswherepr$complainant_c,disputeswherepr$complainant_d,disputeswherepr$complainant_e),dim = c(378, 5, 1))
as.data.frame(table(prcomplainants))

#number of unique respondents where panel requested
prrespondents <- array(c(disputeswherepr$respondent_a,disputeswherepr$respondent_b,disputeswherepr$respondent_c,disputeswherepr$respondent_d,disputeswherepr$respondent_e),dim = c(378,5,1))
as.data.frame(table(prrespondents))

#number of unique third parties where panel requested
prthirdparties <- array(c(disputeswherepr$thirdparty_a,disputeswherepr$thirdparty_b,disputeswherepr$thirdparty_c,disputeswherepr$thirdparty_d,disputeswherepr$thirdparty_e,disputeswherepr$thirdparty_f,disputeswherepr$thirdparty_g,disputeswherepr$thirdparty_h,disputeswherepr$thirdparty_i,disputeswherepr$thirdparty_j,disputeswherepr$thirdparty_k,disputeswherepr$thirdparty_l,disputeswherepr$thirdparty_m,disputeswherepr$thirdparty_n,disputeswherepr$thirdparty_o,disputeswherepr$thirdparty_p,disputeswherepr$thirdparty_q,disputeswherepr$thirdparty_r,disputeswherepr$thirdparty_s,disputeswherepr$thirdparty_t,disputeswherepr$thirdparty_u,disputeswherepr$thirdparty_v,disputeswherepr$thirdparty_w,disputeswherepr$thirdparty_x,disputeswherepr$thirdparty_y,disputeswherepr$thirdparty_z,disputeswherepr$thirdparty_aa,disputeswherepr$thirdparty_ab,disputeswherepr$thirdparty_ac,disputeswherepr$thirdparty_ad,disputeswherepr$thirdparty_ae,disputeswherepr$thirdparty_af,disputeswherepr$thirdparty_ag,disputeswherepr$thirdparty_ah,disputeswherepr$thirdparty_ai),dim = c(378,35,1))
as.data.frame(table(prthirdparties))
#disputes taken to the appellate panel
#table(trade_dispute_data$panel_requested)

