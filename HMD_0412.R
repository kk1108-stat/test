getwd()
setwd("/Users/kimkyum/Documents/공부/Yaqing/HMD")
setwd("/Users/kimkyum/Documents/공부/Yaqing/HMD/R2")
rm(list=ls())
load("HMD_11_30.RData")

save.image("HMD_1_26.RData")
load("HMD_8_21.RData")


M <- array(0,c(3,3,n))
for (i in 1:n){
  y0=rnorm(3)
  aux<-diag(3)+y0%*%t(y0)
  M[,,i]<-aux
}
M
help(approx)
Fmean=CovFMean(M=M,optns=list(metric="frobenius"))$Mout
Fmean

save.image("HMD_0424.RData")
load("HMD_0424.RData")
load("HMD_8_21.RData")
load("SPD_colab_simul1 (1).RData")
load("SPD_colab_simul2.RData")
mse1_colab[[1]]
mse2_colab[[1]]
setwd("/Users/kimkyum/Documents/공부/Yaqing/SPD simulation/")


library(purrr)
library(frechet)
library(pracma)
library(ggplot2)
library(reshape2)
library(cowplot)
library(colorspace)
install.packages("devtools")
library(devtools)
devtools::install_github("yqgchen/OPW", ref = "main")
library(OPW)
devtools::install_github("yqgchen/ODP")
library(ODP)
help("WassPW")

setwd("/Users/kimkyum/Documents/공부/Yaqing/HMD/R2")
List_using=lapply(List_RDS, readRDS)
List_using[[23]][1:2,]
# Generating dataset from [x,y]
data_gen=function(x,y){
  #setwd("/Users/kimkyum/Documents/2023 sp/Yaqing/HMD/R2")
  List_RDS=list.files("/Users/kimkyum/Documents/공부/Yaqing/HMD/R2")
  setwd("/Users/kimkyum/Documents/공부/Yaqing/HMD/R2")
  List_using=lapply(List_RDS, readRDS)
  male=array(0,c(35,y-x+1,100000))
  for (i in 1:35){
    L=List_using[[i]]
    for(j in 1:(y-x+1)){
      l=L[L$year==(x-1+j) & L$sex == "male" ,]$dx
      if(length(l) != 0){
        l[101]=max(100000-sum(as.numeric(l[1:100])),0)
        #print(paste0("contry ",i, "and This is iteration",j,",the value is",l[101]))
        l[1]=100000-sum(as.numeric(l[2:101]))
        male[i,j,] <- rep(c(0:100),as.numeric(l[1:101]))
      }
      else {male[i,j,]=rep(0,100000)}
    }
  }
  
  female=array(0,c(35,y-x+1,100000))
  for (i in 1:35){
    L=List_using[[i]]
    for(j in 1:(y-x+1)){
      l=L[L$year==(x-1+j) & L$sex == "female" ,]$dx
      if(length(l) != 0){
        l[101]=max(100000-sum(as.numeric(l[1:100])),0)
        #print(paste0("contry ",i, "and This is iteration",j,",the value is",l[101]))
        l[1]=100000-sum(as.numeric(l[2:101]))
        female[i,j,] <- rep(c(0:100),as.numeric(l[1:101]))
      }
      else {female[i,j,]=rep(0,100000)}
    }
  }
  
  # my_data = list(male, female)
  # my_data2=list(male,female)
  # 
  # 
  # for(i in c(1:8)){
  #   my_data2[[1]][i,,]=my_data[[1]][i,,]
  # }
  # for(i in c(9:10)){
  #   my_data2[[1]][i+25,,]=my_data[[1]][i,,]
  # }
  # for(i in c(11:35)){
  #   my_data2[[1]][i-2,,]=my_data[[1]][i,,]
  # }
  # for(i in c(1:8)){
  #   my_data2[[2]][i,,]=my_data[[2]][i,,]
  # }
  # for(i in c(9:10)){
  #   my_data2[[2]][i+25,,]=my_data[[2]][i,,]
  # }
  # for(i in c(11:35)){
  #   my_data2[[2]][i-2,,]=my_data[[2]][i,,]
  # }
  
  # my_data = list(male, female)
  # my_data2=list(male,female)
  #my_data=my_data2
  
  data = list(male, female)
  my_data=list(male,female)
  
  for(i in c(1:8)){
    my_data[[1]][i,,]=data[[1]][i,,]
  }
  for(i in c(9:10)){
    my_data[[1]][i+25,,]=data[[1]][i,,]
  }
  for(i in c(11:35)){
    my_data[[1]][i-2,,]=data[[1]][i,,]
  }
  for(i in c(1:8)){
    my_data[[2]][i,,]=data[[2]][i,,]
  }
  for(i in c(9:10)){
    my_data[[2]][i+25,,]=data[[2]][i,,]
  }
  for(i in c(11:35)){
    my_data[[2]][i-2,,]=data[[2]][i,,]
  }
  
  print(table(my_data[[2]][21,1,]))
  ### histogram list generate
  xin <- seq(1, y-x+1, 1)
  # Create a list to store histograms
  hist_list_m <- list()
  # Rows of interest
  rows_of_interest <- c(1:35)
  # Loop through the rows and xin values to create histograms
  for (row in rows_of_interest) {
    # Create a list to store histograms for the current row
    hist_row_list <- list()
    for (x in xin) {
      # Extract specific columns using xin
      data_value <- my_data[[1]][row, x, ]
      # Create histogram and store it in the list for the current x
      hist_row_list[[as.character(x)]] <- hist(data_value, plot = FALSE)
    }
    # Store the list of histograms for the current row in hist_list
    hist_list_m[[row]] <- hist_row_list
  } 
  
  for (x in xin) {
    # Extract specific columns using xin
    data_value1 <- my_data[[1]][34, x, ]
    data_value2 <- my_data[[1]][35, x, ]
    data_value <- c(data_value1,data_value2)
    # Create histogram and store it in the list for the current x
    hist_list_m[[34]][[as.character(x)]] <- hist(data_value, plot = FALSE)
  }
  # Create a list to store histograms
  hist_list_f <- list()
  # Loop through the rows and xin values to create histograms
  for (row in rows_of_interest) {
    # Create a list to store histograms for the current row
    hist_row_list <- list()
    for (x in xin) {
      # Extract specific columns using xin
      data_value <- my_data[[2]][row, x, ]
      # Create histogram and store it in the list for the current x
      hist_row_list[[as.character(x)]] <- hist(data_value, plot = FALSE)
    }
    # Store the list of histograms for the current row in hist_list
    hist_list_f[[row]] <- hist_row_list
  }
  ### Combining east and west of germen
  for (x in xin) {
    # Extract specific columns using xin
    data_value1 <- my_data[[2]][34, x, ]
    data_value2 <- my_data[[2]][35, x, ]
    data_value <- c(data_value1,data_value2)
    # Create histogram and store it in the list for the current x
    hist_list_f[[34]][[as.character(x)]] <- hist(data_value, plot = FALSE)
  }
  print(hist_list_f[[21]][[1]])
  
  # Create a list to store the results_female
  rows_of_interest<- c(1:34)
  xout=xin
  qSup=seq(0,1,0.005)
  res_list_f <- list()
  # Loop through the rows to apply LocDenReg
  for (row in rows_of_interest) {
    hist_row_list <- hist_list_f[[row]]
    # Apply LocDenReg for the current row
    res <- LocDenReg(xin = xin, hin = hist_row_list, xout = xout, optns = list(qSup = qSup))
    # Store the result in the list
    res_list_f[[row]] <- res
  }
  # Create a list to store the results_male
  res_list_m <- list()
  # Loop through the rows to apply LocDenReg
  for (row in rows_of_interest) {
    hist_row_list <- hist_list_m[[row]]
    # Apply LocDenReg for the current row
    res <- LocDenReg(xin = xin, hin = hist_row_list, xout = xout, optns = list(qSup = qSup))
    # Store the result in the list
    res_list_m[[row]] <- res
  }
  dSup=qSup*100
  ### This is nothing but Just changing the domain from qSup to dSup
  
  #### extract qf from res_list according to the component(sex)
  # res_list[[i]] is a result of applying LocDenReg to ith country
  Lq_m=list()
  for (row in rows_of_interest) {
    Lq_m[[row]] <- res_list_m[[row]]$qout
  }
  Lq_f=list()
  for (row in rows_of_interest) {
    Lq_f[[row]] <- res_list_f[[row]]$qout
  }


  return(list(Lq_m,Lq_f,hist_list_m,hist_list_f))
  
}



data_norm=function(c,length,Lq_m,Lq_f){
  qSup= seq(0,1,0.005)
  dSup=qSup*100
  rows_of_interest<- c(1:34)
  Lq_m_norm=list()
  id= matrix(rep(dSup, each =length), nrow =length, byrow = FALSE)
  for(row in rows_of_interest){
    Ahat=max(apply(Lq_m[[row]],1,function(t){sqrt(pracma::trapz(dSup, (t-dSup)^2))}))
    Lq_m_norm[[row]]=(Lq_m[[row]]-id)*c/Ahat+id
  }
  
  Lq_f_norm=list()
  for(row in rows_of_interest){
    Ahat=max(apply(Lq_f[[row]],1,function(t){sqrt(pracma::trapz(dSup, (t-dSup)^2))}))
    Lq_f_norm[[row]]=(Lq_f[[row]]-id)*c/Ahat+id
    print(Ahat)
  }
  return(list(Lq_m_norm,Lq_f_norm))
}

data_norm_delay=function(c,length,Lq_m,Lq_f,w){
  qSup= seq(0,1,0.005)
  dSup=qSup*100
  rows_of_interest<- c(1:34)
  Lq_m_norm=list()
  w0= matrix(rep(w, each =length), nrow =length, byrow = FALSE)
  for(row in rows_of_interest){
    Ahat=max(apply(Lq_m[[row]],1,function(t){sqrt(pracma::trapz(dSup, (t-w)^2))}))
    Lq_m_norm[[row]]=(Lq_m[[row]]-w0)*(c/Ahat)+w0
  }
  
  Lq_f_norm=list()
  for(row in rows_of_interest){
    Ahat=max(apply(Lq_f[[row]],1,function(t){sqrt(pracma::trapz(dSup, (t-w)^2))}))
    Lq_f_norm[[row]]=(Lq_f[[row]]-w0)*(c/Ahat)+w0
    print(Ahat)
  }
  return(list(Lq_m_norm,Lq_f_norm))
}


data_norm.A=function(p,length,Lq_m,Lq_f){
  qSup= seq(0,1,0.02)
  dSup=qSup*100*p
  rows_of_interest<- c(1:34)
  m_normA=c()
  id= matrix(rep(dSup, each =length), nrow =length, byrow = FALSE)
  for(row in rows_of_interest){
    Ahat=max(apply(Lq_m[[row]],1,function(t){sqrt(pracma::trapz(dSup, (t*p-dSup)^2))}))
    m_normA[row]=Ahat
  }
  
  f_normA=c()
  for(row in rows_of_interest){
    Ahat=max(apply(Lq_f[[row]],1,function(t){sqrt(pracma::trapz(dSup, (t*p-dSup)^2))}))
    f_normA[row]=Ahat
  }
  return(list(m_normA,f_normA))
}

data_norm.A_delay=function(p,length,Lq_m,Lq_f,w){
  qSup= seq(0,1,0.005)
  dSup=qSup*100*p
  rows_of_interest<- c(1:34)
  m_normA=c()
  id= matrix(rep(dSup, each =length), nrow =length, byrow = FALSE)
  for(row in rows_of_interest){
    Ahat=max(apply(Lq_m[[row]],1,function(t){sqrt(pracma::trapz(dSup, (t*p-w)^2))}))
    m_normA[row]=Ahat
  }
  
  f_normA=c()
  for(row in rows_of_interest){
    Ahat=max(apply(Lq_f[[row]],1,function(t){sqrt(pracma::trapz(dSup, (t*p-w)^2))}))
    f_normA[row]=Ahat
  }
  return(list(m_normA,f_normA))
}

dSup
Lq_m[[24]][6,]
PW_fmin$workGrid
apply(qm[[1]][[1]],1,function(t){sqrt(pracma::trapz(dSup, (t-dSup)^2))})
phase_rank=apply(Hhat_min,1,function(t){pracma::trapz(PW_fmin$workGrid, (t-PW_fmin$workGrid))})

### Get delay rank
apply(Hhat_min,1,sum)
cal_delay = function(Lq){
  delay=0
  for(i in 1:34){
    for(j in 1:2){
      delay = delay + pracma::trapz(qSup, (Lq-qm[[j]][[i]][1,]))
    }
  }
  return(delay)
}
cal_delay(qm[[1]][[24]][1,])
first_year=matrix(0,nrow=68,ncol=201)
for(i in 1:34){
  first_year[i,]=qm[[1]][[i]][1,]
  first_year[i+34,]=qm[[2]][[i]][1,]
}
first_year_m=matrix(0,nrow=34,ncol=201)
for(i in 1:34){
  first_year_m[i,]=qm[[1]][[i]][1,]
}
delay_rank=apply(first_year,1,cal_delay)



first_distmat=GetPairDist(data=first_year,distfun = l2metric, sup=qSup)
tprank=GetTpRank(first_distmat)
first_distmat_m=GetPairDist(data=first_year_m,distfun = l2metric, sup=qSup)
tprank_m=GetTpRank(first_distmat_m)
### Latvia(0.0093), Hungary(0.0098)
help(MakeObjMdsPlot)
MakeObjMdsPlot(distmat=first_distmat,color_by = tprank$rank,id=MDS_id)

MDS_id <- c("Australia_m", "Austria_m", "Belgium_m", "Bulgaria_m", "Belarus_m", "Canada_m",
                        "Switzerland_m", "Czech Republic_m", "Denmark_m", "Spain_m", "Estonia_m", "Finland_m",
                        "France_m", "United Kingdom_m", "Greece_m", "Hungary_m", "Ireland_m", "Iceland_m",
                        "Israel_m", "Italy_m", "Japan_m", "Lithuania_m", "Luxembourg_m", "Latvia_m", "Netherlands_m",
                        "Norway_m", "New Zealand_m", "Poland_m", "Portugal_m", "Slovakia_m", "Slovenia_m",
                        "Sweden_m", "United States_m", "Germany_m",
            "Australia_f", "Austria_f", "Belgium_f", "Bulgaria_f", "Belarus_f", "Canada_f",
            "Switzerland_f", "Czech Republic_f", "Denmark_f", "Spain_f", "Estonia_f", "Finland_f",
            "France_f", "United Kingdom_f", "Greece_f", "Hungary_f", "Ireland_f", "Iceland_f",
            "Israel_f", "Italy_f", "Japan_f", "Lithuania_f", "Luxembourg_f", "Latvia_f", "Netherlands_f",
            "Norway_f", "New Zealand_f", "Poland_f", "Portugal_f", "Slovakia_f", "Slovenia_f",
            "Sweden_f", "United States_f", "Germany_f")


MakeObjMdsPlot(distmat=first_distmat_m,color_by = tprank_m$rank,id=full_country_names)


plot(qSup,qm[[1]][[2]][2,]-qm[[1]][[24]][6,])
plot(qSup,(qm[[1]][[2]][2,]-qm[[1]][[24]][6,])*101/199.5+qm[[1]][[24]][6,])
plot(qSup,qm[[1]][[1]][1,]-dSup)
plot(qSup,qnorm_delayed[[1]][[2]][2,])
qnorm_delayed[[1]][[2]][2,]
nsubj <- 34
ntime <- 28
qSup <- seq(0,1,0.005)
dSup <- qSup*100
tVec <- seq_len(ntime)
Lt <- rep( list(tVec), nsubj )

qm_1=data_gen(1989,2016)
qm=data_gen(1989,2016)

qm[[1]][[1]][28,] ### male, first country, 2016 year quantile
qm[[3]][[2]][28] ### male, first country, 2016 year quantile
wasserstein(qm[[1]][[1]][28,] ,qm[[1]][[1]][28,] ,2)
dist4den(list(x=qSup,y=qm[[1]][[1]][28,]) ,list(x=qSup,y=qm[[1]][[1]][25,]) ,fctn_type = "quantile")
wasdistfun(qm[[1]][[1]][28,],qm[[1]][[1]][20,],qSup)


#### new omega_0 (frechet mean of 68 processes)
nw_q=qm[[1]][[24]][1,] ##previous w0: Latvia male 1994
omega=DenFMean(qin=eta_fdata,optns = list(qSup = qSup))
fyear=matrix(0,nrow=68,ncol=201) # first year quantile matrix
for(i in c(1:34)){
  fyear[i,]=qm[[1]][[i]][1,]
  fyear[i+34,]=qm[[2]][[i]][1,]
}
omega=DenFMean(qin=fyear,optns = list(qSup = qSup))
w0_q=omega$qout
w0_q
Aij_w0=data_norm.A_delay(1,28,qm[[1]],qm[[2]],w0_q)
w0_min=min(c(Aij_w0[[1]],Aij_w0[[2]]))
qnorm_w0=data_norm_delay(w0_min,28,qm[[1]],qm[[2]],w0_q)
PW_f_w0=WassPW(Lt=Lt,Lq=qnorm_w0[[2]],optns=list(ngrid=201,nknots=3,qSup=seq(0,1,0.005)))
PW_m_w0=WassPW(Lt=Lt,Lq=qnorm_w0[[1]],optns=list(ngrid=201,nknots=3,qSup=seq(0,1,0.005)))
Hhat_w0=Hhat_gen(PW_m_w0,PW_f_w0)[[1]]
Hhat_w0
Hhatinv_w0=Hhat_gen(PW_m_w0,PW_f_w0)[[2]]
Gamhat_w0=Gamhat_gen_w0(w0_min,28,Hhat_w0,Hhatinv_w0,qm[[3]],qm[[4]],w0_q)
eta_w=Gamhat_gen_wtest(w0_min,28,Hhat_w0,Hhatinv_w0,qm[[3]],qm[[4]],w0_q) ## for eta plot(not q, density)
set.seed(999)
qlist_w0=qlist_gen_w0(w0_min,qm,Gamhat_w0,w0_q)
save.image("HMD_8_21.RData")
nsubj2 <- 3
ntime2 <- 201
qSup2 <- seq(0,1,0.005)
tVec2 <- seq_len(ntime2)
Lt2 <- rep( list(tVec2), nsubj2 )
Psi_w0=Psigen(Lt2,qlist_w0,3) ###Psi_j plot checking
Psi_w0
save.image("HMD_8_21.RData")

### Omega_0: Frechet mean of selected first year male
Mens_pull1=c(22,5,28,30,11,24,16)
Mens_pull2=c(11,24,16)
Mens_pull3=c(24,16)

myear=matrix(0,nrow=7,ncol=201) # first year quantile matrix
for(i in c(1:7)){
  myear[i,]=qm[[1]][[Mens_pull1[i]]][1,]
}

omega_m=DenFMean(qin=myear,optns = list(qSup = qSup))
fmean=rep(0,201)
for(i in c(1:7)){
  fmean=fmean+myear[i,]/7
}

w0_m=omega_m$qout
Aij_w0m=data_norm.A_delay(1,28,qm[[1]],qm[[2]],w0_m)
w0m_min=min(c(Aij_w0m[[1]],Aij_w0m[[2]]))
qnorm_w0m=data_norm_delay(w0m_min,28,qm[[1]],qm[[2]],w0_m)
PW_f_w0m=WassPW(Lt=Lt,Lq=qnorm_w0m[[2]],optns=list(ngrid=201,nknots=3,qSup=seq(0,1,0.005)))
PW_m_w0m=WassPW(Lt=Lt,Lq=qnorm_w0m[[1]],optns=list(ngrid=201,nknots=3,qSup=seq(0,1,0.005)))
Hhat_w0m=Hhat_gen(PW_m_w0m,PW_f_w0m)[[1]]

Hhatinv_w0m=Hhat_gen(PW_m_w0m,PW_f_w0m)[[2]]
Gamhat_w0m=Gamhat_gen_w0(w0m_min,28,Hhat_w0m,Hhatinv_w0m,qm[[3]],qm[[4]],w0_m)
eta_wm=Gamhat_gen_wtest(w0m_min,28,Hhat_w0m,Hhatinv_w0m,qm[[3]],qm[[4]],w0_m) ## for eta plot(not q, density)
set.seed(999)
qlist_w0m=qlist_gen_w0(w0m_min,qm,Gamhat_w0m,w0_m)
save.image("HMD_11_30.RData")
nsubj2 <- 3
ntime2 <- 201
qSup2 <- seq(0,1,0.005)
tVec2 <- seq_len(ntime2)
Lt2 <- rep( list(tVec2), nsubj2 )
Psi_w0m=Psigen(Lt2,qlist_w0m,3) ###Psi_j plot checking
Psi_w0m
save.image("HMD_11_30.RData")

myear=matrix(0,nrow=3,ncol=201) # first year quantile matrix
for(i in c(1:3)){
  myear[i,]=qm[[1]][[Mens_pull2[i]]][1,]
}
omega_m2=DenFMean(qin=myear,optns = list(qSup = qSup))
w0_m2=omega_m2$qout
Aij_w0m2=data_norm.A_delay(1,28,qm[[1]],qm[[2]],w0_m2)
w0m_min2=min(c(Aij_w0m2[[1]],Aij_w0m2[[2]]))
qnorm_w0m2=data_norm_delay(w0m_min2,28,qm[[1]],qm[[2]],w0_m2)
PW_f_w0m2=WassPW(Lt=Lt,Lq=qnorm_w0m2[[2]],optns=list(ngrid=201,nknots=3,qSup=seq(0,1,0.005)))
PW_m_w0m2=WassPW(Lt=Lt,Lq=qnorm_w0m2[[1]],optns=list(ngrid=201,nknots=3,qSup=seq(0,1,0.005)))
Hhat_w0m2=Hhat_gen(PW_m_w0m2,PW_f_w0m2)[[1]]

Hhatinv_w0m2=Hhat_gen(PW_m_w0m2,PW_f_w0m2)[[2]]
Gamhat_w0m2=Gamhat_gen_w0(w0m_min2,28,Hhat_w0m2,Hhatinv_w0m2,qm[[3]],qm[[4]],w0_m2)
eta_wm2=Gamhat_gen_wtest(w0m_min2,28,Hhat_w0m2,Hhatinv_w0m2,qm[[3]],qm[[4]],w0_m2) ## for eta plot(not q, density)
set.seed(999)
qlist_w0m2=qlist_gen_w0(w0m_min2,qm,Gamhat_w0m2,w0_m2)
save.image("HMD_11_30.RData")
nsubj2 <- 3
ntime2 <- 201
qSup2 <- seq(0,1,0.005)
tVec2 <- seq_len(ntime2)
Lt2 <- rep( list(tVec2), nsubj2 )
Psi_w0m2=Psigen(Lt2,qlist_w0m2,3) ###Psi_j plot checking
Psi_w0m2
save.image("HMD_11_30.RData")

myear=matrix(0,nrow=2,ncol=201) # first year quantile matrix
for(i in c(1:2)){
  myear[i,]=qm[[1]][[Mens_pull3[i]]][1,]
}
omega_m3=DenFMean(qin=myear,optns = list(qSup = qSup))
w0_m3=omega_m3$qout
Aij_w0m3=data_norm.A_delay(1,28,qm[[1]],qm[[2]],w0_m3)
w0m_min3=min(c(Aij_w0m3[[1]],Aij_w0m3[[2]]))
qnorm_w0m3=data_norm_delay(w0m_min3,28,qm[[1]],qm[[2]],w0_m3)
PW_f_w0m3=WassPW(Lt=Lt,Lq=qnorm_w0m3[[2]],optns=list(ngrid=201,nknots=3,qSup=seq(0,1,0.005)))
PW_m_w0m3=WassPW(Lt=Lt,Lq=qnorm_w0m3[[1]],optns=list(ngrid=201,nknots=3,qSup=seq(0,1,0.005)))
Hhat_w0m3=Hhat_gen(PW_m_w0m3,PW_f_w0m3)[[1]]

Hhatinv_w0m3=Hhat_gen(PW_m_w0m3,PW_f_w0m3)[[2]]
Gamhat_w0m3=Gamhat_gen_w0(w0m_min3,28,Hhat_w0m3,Hhatinv_w0m3,qm[[3]],qm[[4]],w0_m3)
eta_wm3=Gamhat_gen_wtest(w0m_min3,28,Hhat_w0m3,Hhatinv_w0m3,qm[[3]],qm[[4]],w0_m3) ## for eta plot(not q, density)
set.seed(999)
qlist_w0m3=qlist_gen_w0(w0m_min3,qm,Gamhat_w0m3,w0_m3)
save.image("HMD_11_30.RData")
nsubj2 <- 3
ntime2 <- 201
qSup2 <- seq(0,1,0.005)
tVec2 <- seq_len(ntime2)
Lt2 <- rep( list(tVec2), nsubj2 )
Psi_w0m3=Psigen(Lt2,qlist_w0m3,3) ###Psi_j plot checking
Psi_w0m3
save.image("HMD_11_30.RData")


###### get the most delayed
first_year=matrix(0,nrow=68,ncol=201)
for(i in 1:34){
  first_year[i,]=qm[[1]][[i]][1,]
  first_year[i+34,]=qm[[2]][[i]][1,]
}
delay_rank=apply(first_year,1,cal_delay)
order(delay_rank)

delayed_index[,,1] ### transport rank 

qm[[1]][[16]][1,] ### new omega_0 hungary male 1989

####new omega0 (Most delayed country in the first year)
nw_q=qm[[1]][[16]][1,]
Aij_nw=data_norm.A_delay(1,28,qm[[1]],qm[[2]],nw_q)
nw_min=min(c(Aij_nw[[1]],Aij_nw[[2]]))
qnorm_nw=data_norm_delay(nw_min,28,qm[[1]],qm[[2]],nw_q)
PW_f_nw=WassPW(Lt=Lt,Lq=qnorm_nw[[2]],optns=list(ngrid=201,nknots=3,qSup=seq(0,1,0.005)))
PW_m_nw=WassPW(Lt=Lt,Lq=qnorm_nw[[1]],optns=list(ngrid=201,nknots=3,qSup=seq(0,1,0.005)))
Hhat_nw=Hhat_gen(PW_m_nw,PW_f_nw)[[1]]
Hhatinv_nw=Hhat_gen(PW_m_nw,PW_f_nw)[[2]]
eta_nw=Gamhat_gen_wtest(nw_min,28,Hhat_nw,Hhatinv_nw,qm[[3]],qm[[4]],nw_q) ## for eta plot(not q, density)
save.image("HMD_8_21.RData")
help("WassPW")
Gamhat_nw=Gamhat_gen_w0(nw_min,28,Hhat_nw,Hhatinv_nw,qm[[3]],qm[[4]],nw_q)
save.image("HMD_8_21.RData")
set.seed(999)
qlist_nw=qlist_gen_w0(nw_min,qm,Gamhat_nw,nw_q)
Psi_nw=Psigen(Lt2,qlist_nw,3)
save.image("HMD_8_21.RData")

############################



qm_t=data_gen(1983,2016)
Aij=data_norm.A(1,28,qm_1[[1]],qm_1[[2]])
Aij_delay=data_norm.A_delay(1,28,qm[[1]],qm[[2]])
Aij_w0=data_norm.A_delay(1,28,qm[[1]],qm[[2]],w0_q)






mean(Aij[[1]])
mean(Aij[[2]])
Aij_delay[[1]]
Const=mean(c(mean(Aij[[1]]),mean(Aij[[2]])))
Const1=max(c(Aij[[1]],Aij[[2]]))
Const2=min(c(Aij[[1]],Aij[[2]]))
Delayed_min=min(c(Aij_delay[[1]],Aij_delay[[2]]))
Delayed_mean=mean(c(Aij_delay[[1]],Aij_delay[[2]]))
w0_min=min(c(Aij_w0[[1]],Aij_w0[[2]]))

qnorm_c=data_norm(Const,28,qm[[1]],qm[[2]])
qnorm_min=data_norm(Const2,28,qm[[1]],qm[[2]])
qnorm_inf=data_norm2(Const,28,qm_1[[1]],qm_1[[2]])
qnorm_delayed=data_norm_delay(Delayed_min,28,qm[[1]],qm[[2]])
qnorm_delayed2=data_norm_delay(Delayed_mean,28,qm[[1]],qm[[2]])



Lq_ref=qm[[1]][[24]][6,]
Hhat_delayed=Hhat_gen(PW_mmin_de,PW_fmin_de)[[1]]
Hhatinv_delayed=Hhat_gen(PW_mmin_de,PW_fmin_de)[[2]]
Gamhat_delayed=Gamhat_gen_delayed(Delayed_min,28,Hhat_delayed,Hhatinv_delayed,qm[[3]],qm[[4]])
plot(qSup,Gamhat_delayed[[1]][201,])
qlist=qlist_gen_delayed(Delayed_min,qm,Gamhat_delayed)
qlist_de=qlist_gen_delayed(Delayed_min,qm,Gamhat_delayed)
qlist[[1]][100,]
qlist_de[[1]][100,]
nsubj2 <- 3
ntime2 <- 201
qSup2 <- seq(0,1,0.005)
tVec2 <- seq_len(ntime2)
Lt2 <- rep( list(tVec2), nsubj2 )
Psi=Psigen(Lt2,qlist_de,3)
Psi[[1]]-Psi[[2]]
save.image("HMD_0304.RData")
`# Now PW_flist and PW_mlist contain the results for each Const value (or an error message if WassPW failed).

length(qm_1[[4]][[34]])
dim(qnorm_1p[[1]][[1]])

PW_fmin=WassPW(Lt=Lt,Lq=qnorm_min[[2]],optns=list(ngrid=201,nknots=3,qSup=seq(0,1,0.005)))
PW_mmin=WassPW(Lt=Lt,Lq=qnorm_min[[1]],optns=list(ngrid=201,nknots=3,qSup=seq(0,1,0.005)))

qnorm_delayed=data_norm_delay(102,28,qm[[1]],qm[[2]])
PW_fmin_de=WassPW(Lt=Lt,Lq=qnorm_delayed[[2]],optns=list(ngrid=201,nknots=3,qSup=seq(0,1,0.005)))
PW_mmin_de=WassPW(Lt=Lt,Lq=qnorm_delayed[[1]],optns=list(ngrid=201,nknots=3,qSup=seq(0,1,0.005)))
save.image("HMD_0304.RData")
qnorm_delayed[[1]][[1]][1,]
PW_fc=WassPW(Lt=Lt,Lq=qnorm_c[[2]])
PW_mc=WassPW(Lt=Lt,Lq=qnorm_c[[1]])



Lt3 <- rep( list(seq_len(34)), 34 )

PW_mn=WassPW(Lt=Lt,Lq=qm_1[[1]])
PW_fn=WassPW(Lt=Lt,Lq=qm_1[[2]])

#### H_i,Psi_j

Hhat_gen=function(PW_m,PW_f){
  Hhat=(PW_m$hInv+PW_f$hInv)/2
  Hhat_inv=(PW_m$h+PW_f$h)/2
  return(list(Hhat,Hhat_inv))
}




Hhat_c=Hhat_gen(PW_mc,PW_fc)[[1]]
Hhatinv_c=Hhat_gen(PW_mc,PW_fc)[[2]]




Gamhat=Gamhat_gen(1,28,Hhat_1,Hhatinv_1,qm_1[[3]],qm_1[[4]])
Gamhat_c=Gamhat_gen(Const,28,Hhat_c,Hhatinv_c,qm_1[[3]],qm_1[[4]])

Gamhat_c2=Gamhat_gen(Const2,28,Hhat_c2,Hhatinv_c2,qm_1[[3]],qm_1[[4]])
save.image("HMD_1_26.RData")


qlist1=qlist_gen(qm_1,Gamhat_c16)
qlist16=qlist_gen(qm_1,Gamhat_c16)
nsubj2 <- 3
ntime2 <- 51
qSup2 <- seq(0,1,0.02)
tVec2 <- seq_len(ntime2)
Lt2 <- rep( list(tVec2), nsubj2 )
Psihat=Psigen(Lt2,qlist1)

save.image("HMD_1_26.RData")
# Psi_j plot using ggplot (Frechet mean)
# Create data frame for plotting
psidata <- data.frame(
  time = PW_fmin$workGrid + 1988,  # Adjust time by adding 1988
  blue_line = Psi_w0m[[1]] * 27 + 1989,  # Adjust blue line data
  red_line = Psi_w0m[[2]] * 27 + 1989   # Adjust red line data
)

# Psi_j plot  using ggplot2
ggplot(psidata, aes(x = time)) +
  geom_line(aes(y = blue_line, color = "Male"), size = 1) +  
  geom_line(aes(y = red_line, color = "Female"), size = 1) +  
  geom_line(aes(y = time), color = "gray", size = 0.8, linetype = "dashed", show.legend = FALSE) +
  labs(x = "Year", y = "Year", color = NULL) +
  scale_x_continuous(breaks = c(1989, 1998, 2007, 2016)) +
  scale_y_continuous(breaks = c(1989, 1998, 2007, 2016)) +
  scale_color_manual(values = c("Male" = "blue", "Female" = "red"), labels = c("Female", "Male")) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 16),
    legend.text = element_text(size = 17),
    axis.title = element_text(size = 17),
    legend.key.size = unit(2, "lines")
    #plot.margin = margin(0, 0, 0, 0)  # Remove all margins
  )
###### Amplitude rank plot
Adata <- data.frame(
  Country = c(full_country_names, full_country_names),
  Amp = c(Aij_w0[[1]] / w0_min, Aij_w0[[2]] / w0_min),
  sex = rep(c("Female", "Male"), each = 34)
)

# Order the data frame by country names
Adata <- Adata[order(Adata$Country), ]

# Create the bar plot
ggplot(data = Adata, aes(x = Country, y = Amp, fill = sex)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.8) +
  labs(x = "Country", y = "Amplitude", title = "Amplitude by Country") +
  scale_fill_manual(values = c("Female" = "red", "Male" = "blue")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  theme(axis.text.x = element_text(size = 10)) + # Adjust text size if needed
  scale_x_discrete(expand = c(0.02, 0.1)) 

#### seperatly at once
install.packages("patchwork")
library(patchwork)
# Create data frames for male and female amplitudes
Adata_total <- data.frame(
  Country = full_country_names,
  Amp_male = Aij[[1]] / Const2,
  Amp_female = Aij[[2]] / Const2
)
Adata_male <- data.frame(
  Country = full_country_names,
  Amp = Aij[[1]] / Const2
)
Adata_female <- data.frame(
  Country = full_country_names,
  Amp = Aij[[2]] / Const2
)

# Order the data frames by amplitude
Adata_male <- Adata_male[order(Adata_male$Amp), ]
Adata_female <- Adata_female[order(Adata_female$Amp), ]

# Convert Country to a factor with ordered levels
Adata_male$Country <- factor(Adata_male$Country, levels = Adata_male$Country)
Adata_female$Country <- factor(Adata_female$Country, levels = Adata_female$Country)

# Create plots for male and female amplitudes
plot_male <- ggplot(data = Adata_male, aes(x = Country, y = Amp)) +
  geom_bar(stat = "identity", width = 0.8, fill = "blue") +
  labs(y = "Amplitude", title = "Amplitude Rank of Male") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10)) + 
  scale_x_discrete(expand = c(0.02, 0.1)) +
  coord_cartesian(ylim=c(1,1.8))

plot_female <- ggplot(data = Adata_female, aes(x = Country, y = Amp)) +
  geom_bar(stat = "identity", width = 0.8, fill = "red") +
  labs(y = "Amplitude", title = "Amplitude Rank of Female") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10)) + 
  scale_x_discrete(expand = c(0.02, 0.1)) +
  coord_cartesian(ylim=c(1,1.8))

Adata_total <- data.frame(
  Country = full_country_names,
  Amp_male = Aij_w0m3[[1]] / w0m_min3,
  Amp_female = Aij_w0m3[[2]] / w0m_min3
)
Adata_total <- data.frame(
  Country = full_country_names,
  Amp_male = Aij_delay[[1]] / Delayed_min,
  Amp_female = Aij_delay[[2]] / Delayed_min
)
install.packages("ggrepel")
library(ggrepel)
# Create the scatter plot with the "y=x" text
plot_amp <- ggplot(data = Adata_total, aes(x = Amp_male, y = Amp_female, label = Country)) +
  geom_point(size=1.8) +  # Add points
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +  # Add y=x dashed line
  geom_text_repel(nudge_x = 0.005, nudge_y = 0.005, size = 4.5,max.overlaps = 20) +  # Add labels with slight adjustment
  labs(x = "Amplitude factors for males", y = "Amplitude factors for females") +  # Set axis labels
  theme_minimal() +
  coord_cartesian(ylim=c(2.5,5),xlim=c(1,4)) +
  annotate("text", x = 3.2, y = 3.2, label = "y = x", color = "gray", size = 14, angle = 40)+
  theme(
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 19)
  )
# Show the plot
print(plot_amp)

plot_delayamp <- ggplot(data = Adata_total, aes(x = Amp_male, y = Amp_female, label = Country)) +
  geom_point(size=1.8) +  # Add points
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +  # Add y=x dashed line
  geom_text_repel(nudge_x = 0.005, nudge_y = 0.005, size = 5) +  # Add labels with slight adjustment
  labs(x = "Amplitude of male", y = "Amplitude of female") +  # Set axis labels
  theme_minimal() +
  coord_cartesian(ylim=c(1.85,2.75),xlim=c(1,2.3)) +
  annotate("text", x = 2, y = 2, label = "y = x", color = "gray", size = 14, angle = 45)+
  theme(
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 19)
  )
# Show the plot
print(plot_amp)
# Combine plots vertically
combined_plot <- plot_male / plot_female
combined_plot





########### Phase rank
phase_rank=apply(Hhat_min,1,function(t){pracma::trapz(PW_fmin$workGrid, (t-PW_fmin$workGrid))})
phase_rankw=apply(Hhat_w0,1,function(t){pracma::trapz(PW_f_w0$workGrid, (t-PW_f_w0$workGrid))})
phase_ranknw=apply(Hhat_nw,1,function(t){pracma::trapz(PW_f_nw$workGrid, (t-PW_f_nw$workGrid))})

Pdata <- data.frame(
  Country = full_country_names,
  Phase = phase_ranknw
)
# Order the data frame by country names
Pdata <- Pdata[order(Pdata$Country), ]
# Order the data frame by phase values
Pdata2 <- Pdata[order(Pdata$Phase), ]
# Convert Country to factor with levels ordered by phase rank
Pdata2$Country <- factor(Pdata2$Country, levels = Pdata2$Country)

# Create the bar plot with custom color
ggplot(data = Pdata2, aes(x = Country, y = Phase)) +
  geom_bar(stat = "identity", width = 0.8, fill = "skyblue") +  # Set the fill color directly
  labs(y = "Integrated temporal advancement") +
  theme_minimal() +
  coord_cartesian(ylim=c(-220,150)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  theme(axis.text.x = element_text(size = 16)) + # Adjust text size if needed
  theme(axis.text.y = element_text(size = 14)) +
  theme(axis.title.y = element_text(size = 16)) + 
  theme(axis.title.x = element_text(size = 16)) +
  scale_x_discrete(expand = c(0.02, 0.1))
######

plot(PW_m1p$workGrid,apply(Hhat_0.3,2,mean))


plot(density(tauhat_p[1,]))
plot(density(tauhat_p[27,]))
plot(density(tauhat_p[51,]))

plot(density(Gamhat_p[[2]][1,]))
plot(density(Gamhat_p[[2]][27,]))
plot(density(Gamhat_p[[2]][51,],bw=10))


density_estimate <- density(Lq[[1]][1,] , from = 0, to = 105, n = 1000)

# Interpolate density values at dSup
interpolated_density <- approx(density_estimate$x, density_estimate$y, xout = dSup, method = "linear")

# Plot the results
plot(density_estimate, main = "Smoothed Density from Quantiles", xlab = "Value", ylab = "Density", col = "blue")
lines(dSup, interpolated_density$y, col = "red", lty = 2)


help(density)

#### Find the most delayed country
### ex. 1,1,1 (male, aus, 1989)
qSup=seq(0,1,0.005)
qSup
qm[[1]][[1]][28,]
rows_of_interest
years_of_interest=c(1:28)
delayed_index=array(0,dim=c(34,2,28))
for(i in rows_of_interest){
  for(j in c(1:2)){
    for(k in years_of_interest){
      delayed_index[i,j,k]=get_delay(i,j,k)
  }
  }
}
delayed_index[,,1]

min_index <- which(delayed_index == min(delayed_index), arr.ind = TRUE)
max_index <- which(delayed_index == max(delayed_index), arr.ind = TRUE)
min_index
max_index  ### Latvia,male, 1994(24,1,6)
full_country_names[24]
delayed_index[21,2,28]
get_delay=function(i,j,k){
  L_ij=matrix(0,nrow=34,ncol=2)
  for(a in rows_of_interest){
    for(b in c(1:2)){
      L_ij[a,b]=sum(apply(qm[[b]][[a]],1,function(t){pracma::trapz(qSup, (t-qm[[j]][[i]][k,]))}))/28
    }
  }
  return(sum(L_ij)/68)
}




#### Hinv, H check one more time

Gamhat_gen=function(Const,length,Hhat,Hhat_inv,hist_list_m,hist_list_f){
  qSup <- seq(0,1,0.005)
  xin <- seq(1,length,1)
  rows_of_interest<- c(1:34)
  
  res_list_f_hinv=list()
  for (row in rows_of_interest) {
    hist_row_list <- hist_list_f[[row]]
    # Apply LocDenReg for the current row
    res <- LocDenReg(xin = xin, hin = hist_row_list, xout = Hhat_inv[row,], optns = list(qSup = qSup))
    # Store the result in the list
    res_list_f_hinv[[row]] <- res
  }
  
  res_list_m_hinv=list()
  for (row in rows_of_interest) {
    hist_row_list <- hist_list_m[[row]]
    
    # Apply LocDenReg for the current row
    res <- LocDenReg(xin = xin, hin = hist_row_list, xout = Hhat_inv[row,], optns = list(qSup = qSup))
    
    # Store the result in the list
    res_list_m_hinv[[row]] <- res
  }
  ###extract qf
  Lq_m_hinv=list()
  for (row in rows_of_interest) {
    Lq_m_hinv[[row]] <- res_list_m_hinv[[row]]$qout   ### quantile만 저장
  }
  Lq_f_hinv=list()  #### note that each component is 51 by 51 matrix 
  for (row in rows_of_interest) {   ## each row is t(28를 51개로 쪼갬)
    Lq_f_hinv[[row]] <- res_list_f_hinv[[row]]$qout
  }
  ###normalize

  
  dSup=qSup*100
  Lq_mhinv_norm=list()
  id2= matrix(rep(dSup, each = 201), nrow = 201, byrow = FALSE)
  for(row in rows_of_interest){
    Ahat=max(apply(Lq_m_hinv[[row]],1,function(t){sqrt(pracma::trapz(dSup, (t-dSup)^2))}))
    Lq_mhinv_norm[[row]]=(Lq_m_hinv[[row]]-id2)*Const/Ahat+id2
  }
  
  Lq_fhinv_norm=list()
  for(row in rows_of_interest){
    Ahat=max(apply(Lq_f_hinv[[row]],1,function(t){sqrt(pracma::trapz(dSup, (t-dSup)^2))}))
    Lq_fhinv_norm[[row]]=(Lq_f_hinv[[row]]-id2)*Const/Ahat+id2
  }
  ### tau_j
  Gamj_fhat=0
  for(row in rows_of_interest){
    Gamj_fhat=Gamj_fhat+Lq_fhinv_norm[[row]]
  }
  Gamj_fhat=Gamj_fhat/34
  
  Gamj_mhat=0
  for(row in rows_of_interest){
    Gamj_mhat=Gamj_mhat+Lq_mhinv_norm[[row]]
  }
  Gamj_mhat=Gamj_mhat/34
  
  return(list(Gamj_mhat,Gamj_fhat))
}

Gamhat_gen_simple2=function(Const,length,Hhat,Hhat_inv,hist_list_1){
  qSup <- seq(0,1,0.005)
  xin <- seq(1,length,1)
  rows_of_interest<- c(1:34)
  
  res_list_1_hinv=list()
  for (row in rows_of_interest) {
    hist_row_list <- hist_list_1[[row]]
    # Apply LocDenReg for the current row
    res <- LocDenReg(xin = xin, hin = hist_row_list, xout = Hhat_inv[row,], optns = list(qSup = qSup))
    # Store the result in the list
    res_list_1_hinv[[row]] <- res
  }
  ###extract qf
  Lq_1_hinv=list()
  for (row in rows_of_interest) {
    Lq_1_hinv[[row]] <- res_list_1_hinv[[row]]$qout   ### quantile만 저장
  }
  ###normalize
  dSup=qSup*100
  Lq1_hinv_norm=list()
  id2= matrix(rep(dSup, each = 201), nrow = 201, byrow = FALSE)
  for(row in rows_of_interest){
    Ahat=max(apply(Lq_1_hinv[[row]],1,function(t){sqrt(pracma::trapz(dSup, (t-dSup)^2))}))
    Lq1_hinv_norm[[row]]=(Lq_1_hinv[[row]]-id2)*Const/Ahat+id2
  }
  ### tau_j
  Gamj_hat1=0
  for(row in rows_of_interest){
    Gamj_hat1=Gamj_hat1+Lq1_hinv_norm[[row]]
  }
  Gamj_hat1=Gamj_hat1/34
  
  return(Gamj_hat1)
}
help("LocDenReg")
###From delayed version
Gamhat_gen_delayed=function(Const,length,Hhat,Hhat_inv,hist_list_m,hist_list_f){
  qSup <- seq(0,1,0.005)
  xin <- seq(1,length,1)
  rows_of_interest<- c(1:34)
  
  res_list_f_hinv=list()
  for (row in rows_of_interest) {
    hist_row_list <- hist_list_f[[row]]
    # Apply LocDenReg for the current row
    res <- LocDenReg(xin = xin, hin = hist_row_list, xout = Hhat_inv[row,], optns = list(qSup = qSup))
    # Store the result in the list
    res_list_f_hinv[[row]] <- res
  }
  
  res_list_m_hinv=list()
  for (row in rows_of_interest) {
    hist_row_list <- hist_list_m[[row]]
    
    # Apply LocDenReg for the current row
    res <- LocDenReg(xin = xin, hin = hist_row_list, xout = Hhat_inv[row,], optns = list(qSup = qSup))
    
    # Store the result in the list
    res_list_m_hinv[[row]] <- res
  }
  ###extract qf
  Lq_m_hinv=list()
  for (row in rows_of_interest) {
    Lq_m_hinv[[row]] <- res_list_m_hinv[[row]]$qout   ### quantile만 저장
  }
  Lq_f_hinv=list()  #### note that each component is 51 by 51 matrix 
  for (row in rows_of_interest) {   ## each row is t(28를 51개로 쪼갬)
    Lq_f_hinv[[row]] <- res_list_f_hinv[[row]]$qout
  }
  ###normalize
  
  
  dSup=qSup*100
  Lq_mhinv_norm=list()
  Lq_ref=qm[[1]][[24]][6,]
  id2= matrix(rep(Lq_ref, each =201), nrow =201, byrow = FALSE)
  for(row in rows_of_interest){
    Ahat=max(apply(Lq_m_hinv[[row]],1,function(t){sqrt(pracma::trapz(dSup, (t-Lq_ref)^2))}))
    Lq_mhinv_norm[[row]]=(Lq_m_hinv[[row]]-id2)*Const/Ahat+id2
  }
  
  Lq_fhinv_norm=list()
  for(row in rows_of_interest){
    Ahat=max(apply(Lq_f_hinv[[row]],1,function(t){sqrt(pracma::trapz(dSup, (t-Lq_ref)^2))}))
    Lq_fhinv_norm[[row]]=(Lq_f_hinv[[row]]-id2)*Const/Ahat+id2
  }
  ### tau_j
  Gamj_fhat=0
  for(row in rows_of_interest){
    Gamj_fhat=Gamj_fhat+Lq_fhinv_norm[[row]]
  }
  Gamj_fhat=Gamj_fhat/34
  
  Gamj_mhat=0
  for(row in rows_of_interest){
    Gamj_mhat=Gamj_mhat+Lq_mhinv_norm[[row]]
  }
  Gamj_mhat=Gamj_mhat/34
  
  return(list(Gamj_mhat,Gamj_fhat))
}

Gamhat_gen_wtest=function(Const,length,Hhat,Hhat_inv,hist_list_m,hist_list_f,w){
  qSup <- seq(0,1,0.005)
  xin <- seq(1,length,1)
  rows_of_interest<- c(1:34)
  
  res_list_f_hinv=list()
  for (row in rows_of_interest) {
    hist_row_list <- hist_list_f[[row]]
    # Apply LocDenReg for the current row
    res <- LocDenReg(xin = xin, hin = hist_row_list, xout = Hhat_inv[row,], optns = list(qSup = qSup))
    # Store the result in the list
    res_list_f_hinv[[row]] <- res
  }
  
  res_list_m_hinv=list()
  for (row in rows_of_interest) {
    hist_row_list <- hist_list_m[[row]]
    
    # Apply LocDenReg for the current row
    res <- LocDenReg(xin = xin, hin = hist_row_list, xout = Hhat_inv[row,], optns = list(qSup = qSup))
    
    # Store the result in the list
    res_list_m_hinv[[row]] <- res
  }
  ###extract qf
  Lq_m_hinv=list()
  for (row in rows_of_interest) {
    Lq_m_hinv[[row]] <- res_list_m_hinv[[row]]$qout   ### quantile만 저장
  }
  Lq_f_hinv=list()  #### note that each component is 51 by 51 matrix 
  for (row in rows_of_interest) {   ## each row is t(28를 51개로 쪼갬)
    Lq_f_hinv[[row]] <- res_list_f_hinv[[row]]$qout
  }
  ###normalize
  
  
  dSup=qSup*100
  Lq_mhinv_norm=list()
  Lq_ref=w
  id2= matrix(rep(Lq_ref, each =201), nrow =201, byrow = FALSE)
  for(row in rows_of_interest){
    Ahat=max(apply(Lq_m_hinv[[row]],1,function(t){sqrt(pracma::trapz(dSup, (t-Lq_ref)^2))}))
    Lq_mhinv_norm[[row]]=(Lq_m_hinv[[row]]-id2)*Const/Ahat+id2
  }
  
  Lq_fhinv_norm=list()
  for(row in rows_of_interest){
    Ahat=max(apply(Lq_f_hinv[[row]],1,function(t){sqrt(pracma::trapz(dSup, (t-Lq_ref)^2))}))
    Lq_fhinv_norm[[row]]=(Lq_f_hinv[[row]]-id2)*Const/Ahat+id2
  }
  ### tau_j
  eta_male=matrix(0,nrow=51,ncol=201) ## row is time seq,col is qunatile on qSup
  
  for(t in seq(1,201,by=4)){
    eta_mdata=matrix(0,nrow=34,ncol=201)
    for(row in rows_of_interest){
      eta_mdata[row,]=Lq_mhinv_norm[[row]][t,]
    }
    eta_male[(t+3)/4,]=DenFMean(qin=eta_mdata,optns = list(qSup = qSup,dSup=dSup))$dout
  }
  
  eta_female=matrix(0,nrow=51,ncol=201) ## row is time seq,col is qunatile on qSup
  
  for(t in seq(1,201,by=4)){
    eta_fdata=matrix(0,nrow=34,ncol=201)
    for(row in rows_of_interest){
      eta_fdata[row,]=Lq_fhinv_norm[[row]][t,]
    }
    eta_female[(t+3)/4,]=DenFMean(qin=eta_fdata,optns = list(qSup = qSup,dSup=dSup))$dout
  }
  
  return(list(eta_male,eta_female))
  
}

Gamhat_gen_w0=function(Const,length,Hhat,Hhat_inv,hist_list_m,hist_list_f,w){
  qSup <- seq(0,1,0.005)
  xin <- seq(1,length,1)
  rows_of_interest<- c(1:34)
  
  res_list_f_hinv=list()
  for (row in rows_of_interest) {
    hist_row_list <- hist_list_f[[row]]
    # Apply LocDenReg for the current row
    res <- LocDenReg(xin = xin, hin = hist_row_list, xout = Hhat_inv[row,], optns = list(qSup = qSup))
    # Store the result in the list
    res_list_f_hinv[[row]] <- res
  }
  
  res_list_m_hinv=list()
  for (row in rows_of_interest) {
    hist_row_list <- hist_list_m[[row]]
    
    # Apply LocDenReg for the current row
    res <- LocDenReg(xin = xin, hin = hist_row_list, xout = Hhat_inv[row,], optns = list(qSup = qSup))
    
    # Store the result in the list
    res_list_m_hinv[[row]] <- res
  }
  ###extract qf
  Lq_m_hinv=list()
  for (row in rows_of_interest) {
    Lq_m_hinv[[row]] <- res_list_m_hinv[[row]]$qout   ### quantile만 저장
  }
  Lq_f_hinv=list()  #### note that each component is 51 by 51 matrix 
  for (row in rows_of_interest) {   ## each row is t(28를 51개로 쪼갬)
    Lq_f_hinv[[row]] <- res_list_f_hinv[[row]]$qout
  }
  ###normalize
  
  
  dSup=qSup*100
  Lq_mhinv_norm=list()
  Lq_ref=w
  id2= matrix(rep(Lq_ref, each =201), nrow =201, byrow = FALSE)
  for(row in rows_of_interest){
    Ahat=max(apply(Lq_m_hinv[[row]],1,function(t){sqrt(pracma::trapz(dSup, (t-Lq_ref)^2))}))
    Lq_mhinv_norm[[row]]=(Lq_m_hinv[[row]]-id2)*Const/Ahat+id2
  }
  
  Lq_fhinv_norm=list()
  for(row in rows_of_interest){
    Ahat=max(apply(Lq_f_hinv[[row]],1,function(t){sqrt(pracma::trapz(dSup, (t-Lq_ref)^2))}))
    Lq_fhinv_norm[[row]]=(Lq_f_hinv[[row]]-id2)*Const/Ahat+id2
  }
  ### tau_j
  eta_male=matrix(0,nrow=201,ncol=201) ## row is time seq,col is qunatile on qSup
  
  for(t in seq(1,201,by=1)){
    eta_mdata=matrix(0,nrow=34,ncol=201)
    for(row in rows_of_interest){
      eta_mdata[row,]=Lq_mhinv_norm[[row]][t,]
    }
    eta_male[t,]=DenFMean(qin=eta_mdata,optns = list(qSup = qSup))$qout
  }
 
  eta_female=matrix(0,nrow=201,ncol=201) ## row is time seq,col is qunatile on qSup
  
  for(t in seq(1,201,by=1)){
    eta_fdata=matrix(0,nrow=34,ncol=201)
    for(row in rows_of_interest){
      eta_fdata[row,]=Lq_fhinv_norm[[row]][t,]
    }
    eta_female[t,]=DenFMean(qin=eta_fdata,optns = list(qSup = qSup))$qout
  }
  
  return(list(eta_male,eta_female))
}

### temporary test
Gamhat_t=Gamhat_gen_delayed_test(Delayed_min,28,Hhat_delayed,Hhatinv_delayed,qm[[3]],qm[[4]])

eta_male=matrix(0,nrow=101,ncol=101) ## row is time seq,col is qunatile on qSup

for(t in seq(1,201,by=2)){
  eta_mdata=matrix(0,nrow=34,ncol=201)
  for(row in rows_of_interest){
    eta_mdata[row,]=Gamhat_t[[3]][[row]][t,]
  }
  eta_male[(t+1)/2,]=DenFMean(qin=eta_mdata,optns = list(qSup = qSup,dSup=dSup))$dout
}

eta_male

eta_female=matrix(0,nrow=101,ncol=101) ## row is time seq,col is qunatile on qSup

for(t in seq(1,201,by=2)){
  eta_fdata=matrix(0,nrow=34,ncol=201)
  for(row in rows_of_interest){
    eta_fdata[row,]=Gamhat_t[[4]][[row]][t,]
  }
  eta_female[(t+1)/2,]=DenFMean(qin=eta_fdata,optns = list(qSup = qSup,dSup=dSup))$dout
}

eta_w=Gamhat_gen_wtest((w0_min,28,Hhat_w0,Hhatinv_w0,qm[[3]],qm[[4]],w0_q))



##### eta plot using facet_wrap
eta_male2=eta_nw[[1]] ## hung
eta_female2=eta_nw[[2]] ## hung
eta_male2=eta_wm[[1]] 
eta_female2=eta_wm[[2]] 
# Combine male and female data into a single data frame
time <- seq(0, 1, 0.02)
dSup <- seq(0,100,by=0.5)
# Prepare male data
df_male <- as.data.frame(eta_male2)
colnames(df_male) <- paste0("Q", seq_along(dSup))
df_male$time <- time
df_male$group <- "Male"

# Prepare female data
df_female <- as.data.frame(eta_female2)
colnames(df_female) <- paste0("Q", seq_along(dSup))
df_female$time <- time
df_female$group <- "Female"

# Combine male and female data
df_combined <- rbind(df_male, df_female)

# Melt the data into long format
df_combined_long <- melt(df_combined, id.vars = c("time", "group"), variable.name = "Quantile", value.name = "Value")

# Add the year corresponding to each time point
df_combined_long$year <- 1989 + df_combined_long$time * (2016 - 1989)

# Convert Quantile from character to numeric to match dSup
df_combined_long$dSup <- as.numeric(gsub("Q", "", df_combined_long$Quantile)) - 1
df_combined_long$dSup <- dSup[df_combined_long$dSup + 1]

df_combined_long$group <- factor(df_combined_long$group, levels = c("Male", "Female"))
# Plotting using facet_wrap
library(ggplot2)
library(cowplot)
###define color palette
col51=colorRampPalette(c("purple","blue","green","yellow","red"))(51)


combined_plot <- ggplot(df_combined_long, aes(x = dSup, y = Value, group = year, color = year)) +
  geom_line(size = 1) +
  scale_color_gradientn(colors = rev(col51), name = "Year", breaks = seq(1989, 2016, by = 9)) +
  labs(x = "Age/Year", y = "Density") +
  facet_wrap(~ group) +  # Facet by group (Male and Female)
  theme_minimal() +
  theme(
    axis.text = element_text(size = 17),
    axis.title = element_text(size = 21),
    legend.title = element_text(size = 17),
    legend.text = element_text(size = 14),
    legend.position = "top",
    legend.key.size = unit(1.8, "lines"),
    strip.text = element_text(size = 20)  # Adjust the size of the facet labels
  )


# Display the combined plot
print(combined_plot)


##########

Gamhat_simple_delayed=function(Const,length,Hhat,Hhat_inv,hist_list_1){
  qSup <- seq(0,1,0.005)
  xin <- seq(1,length,1)
  rows_of_interest<- c(1:34)
  
  res_list_1_hinv=list()
  for (row in rows_of_interest) {
    hist_row_list <- hist_list_1[[row]]
    # Apply LocDenReg for the current row
    res <- LocDenReg(xin = xin, hin = hist_row_list, xout = Hhat_inv[row,], optns = list(qSup = qSup))
    # Store the result in the list
    res_list_1_hinv[[row]] <- res
  }
  ###extract qf
  Lq_1_hinv=list()
  for (row in rows_of_interest) {
    Lq_1_hinv[[row]] <- res_list_1_hinv[[row]]$qout   ### quantile만 저장
  }
  ###normalize
  dSup=qSup*100
  Lq1_hinv_norm=list()
  Lq_ref=qm[[1]][[24]][6,]
  id2= matrix(rep(Lq_ref, each =201), nrow =201, byrow = FALSE)
  for(row in rows_of_interest){
    Ahat=max(apply(Lq_1_hinv[[row]],1,function(t){sqrt(pracma::trapz(dSup, (t-Lq_ref)^2))}))
    Lq1_hinv_norm[[row]]=(Lq_1_hinv[[row]]-id2)*Const/Ahat+id2
  }
  ### tau_j
  Gamj_hat1=0
  for(row in rows_of_interest){
    Gamj_hat1=Gamj_hat1+Lq1_hinv_norm[[row]]
  }
  Gamj_hat1=Gamj_hat1/34
  
  return(Gamj_hat1)
}

Gamhat_simple_w0=function(Const,length,Hhat,Hhat_inv,hist_list_1,w){
  qSup <- seq(0,1,0.005)
  xin <- seq(1,length,1)
  rows_of_interest<- c(1:34)
  
  res_list_1_hinv=list()
  for (row in rows_of_interest) {
    hist_row_list <- hist_list_1[[row]]
    # Apply LocDenReg for the current row
    res <- LocDenReg(xin = xin, hin = hist_row_list, xout = Hhat_inv[row,], optns = list(qSup = qSup))
    # Store the result in the list
    res_list_1_hinv[[row]] <- res
  }
  ###extract qf
  Lq_1_hinv=list()
  for (row in rows_of_interest) {
    Lq_1_hinv[[row]] <- res_list_1_hinv[[row]]$qout   ### quantile만 저장
  }
  ###normalize
  dSup=qSup*100
  Lq1_hinv_norm=list()
  Lq_ref=w
  id2= matrix(rep(Lq_ref, each =201), nrow =201, byrow = FALSE)
  for(row in rows_of_interest){
    Ahat=max(apply(Lq_1_hinv[[row]],1,function(t){sqrt(pracma::trapz(dSup, (t-Lq_ref)^2))}))
    Lq1_hinv_norm[[row]]=(Lq_1_hinv[[row]]-id2)*Const/Ahat+id2
  }
  ### tau_j

  Gamj_hat1=matrix(0,nrow=201,ncol=201)
  for(t in seq(1,201,by=1)){
    eta_data=matrix(0,nrow=34,ncol=201)
    for(row in rows_of_interest){
      eta_data[row,]=Lq1_hinv_norm[[row]][t,]
    }
    Gamj_hat1[t,]=DenFMean(qin=eta_data,optns = list(qSup = qSup))$qout
  }
  
  return(Gamj_hat1)
}

w0= matrix(rep(Lq_m[[24]][6,], each =length), nrow =length, byrow = FALSE)

### tauhat
qlist_gen=function(Const,data,gamhat){
  nsubj <- 34
  ntime <- 28
  qSup <- seq(0,1,0.005)
  dSup <- qSup*100
  tVec <- seq_len(ntime)
  Lt <- rep( list(tVec), nsubj )
  set.seed(100)
  r=sample(x=1:2,size=34,replace = TRUE) # for each i, select j at random
  qm_z=list()
  for (i in 1:34){
    if(r[i]==1){
      qm_z[[i]]=data[[1]][[i]]
    }
    else{ qm_z[[i]]=data[[2]][[i]] }
  }
  hist_list_z=list()
  for (i in 1:34){
    if(r[i]==1){
      hist_list_z[[i]]=data[[3]][[i]]
    }
    else{ hist_list_z[[i]]=data[[4]][[i]] }
  }
  qnorm_zp=data_norm(Const,28,qm_z,data[[2]])[[1]]
  PW_zp=WassPW(Lt=Lt,Lq=qnorm_zp,optns=list(ngrid=201,nknots=3,qSup=seq(0,1,0.005)))
  Hhat_zp=PW_zp$hInv
  Hhatinv_zp=PW_zp$h
  tauhat=Gamhat_gen_simple2(Const,28,Hhat_zp,Hhatinv_zp,hist_list_z)
  qlist=list()
  qlist[[1]]=gamhat[[1]]
  qlist[[2]]=gamhat[[2]]
  qlist[[3]]=tauhat
  
  return(qlist)
}

qlist_gen_delayed=function(Const,data,gamhat){
  nsubj <- 34
  ntime <- 28
  qSup <- seq(0,1,0.005)
  dSup <- qSup*100
  tVec <- seq_len(ntime)
  Lt <- rep( list(tVec), nsubj )
  set.seed(100)
  r=sample(x=1:2,size=34,replace = TRUE) # for each i, select j at random
  qm_z=list()
  for (i in 1:34){
    if(r[i]==1){
      qm_z[[i]]=data[[1]][[i]]
    }
    else{ qm_z[[i]]=data[[2]][[i]] }
  }
  hist_list_z=list()
  for (i in 1:34){
    if(r[i]==1){
      hist_list_z[[i]]=data[[3]][[i]]
    }
    else{ hist_list_z[[i]]=data[[4]][[i]] }
  }
  qnorm_zp=data_norm_delay(Const,28,qm_z,data[[2]])[[1]]
  PW_zp=WassPW(Lt=Lt,Lq=qnorm_zp,optns=list(ngrid=201,nknots=3,qSup=seq(0,1,0.005)))
  Hhat_zp=PW_zp$hInv
  Hhatinv_zp=PW_zp$h
  tauhat=Gamhat_simple_delayed(Const,28,Hhat_zp,Hhatinv_zp,hist_list_z)
  qlist=list()
  qlist[[1]]=gamhat[[1]]
  qlist[[2]]=gamhat[[2]]
  qlist[[3]]=tauhat
  
  return(qlist)
}

qlist_gen_w0=function(Const,data,gamhat,w){
  nsubj <- 34
  ntime <- 28
  qSup <- seq(0,1,0.005)
  dSup <- qSup*100
  tVec <- seq_len(ntime)
  Lt <- rep( list(tVec), nsubj )
  set.seed(100)
  r=sample(x=1:2,size=34,replace = TRUE) # for each i, select j at random
  qm_z=list()
  for (i in 1:34){
    if(r[i]==1){
      qm_z[[i]]=data[[1]][[i]]
    }
    else{ qm_z[[i]]=data[[2]][[i]] }
  }
  hist_list_z=list()
  for (i in 1:34){
    if(r[i]==1){
      hist_list_z[[i]]=data[[3]][[i]]
    }
    else{ hist_list_z[[i]]=data[[4]][[i]] }
  }
  qnorm_zp=data_norm_delay(Const,28,qm_z,data[[2]],w)[[1]]
  PW_zp=WassPW(Lt=Lt,Lq=qnorm_zp,optns=list(ngrid=201,nknots=3,qSup=seq(0,1,0.005)))
  Hhat_zp=PW_zp$hInv
  Hhatinv_zp=PW_zp$h
  tauhat=Gamhat_simple_w0(Const,28,Hhat_zp,Hhatinv_zp,hist_list_z,w)
  qlist=list()
  qlist[[1]]=gamhat[[1]]
  qlist[[2]]=gamhat[[2]]
  qlist[[3]]=tauhat
  
  return(qlist)
}

nsubj2 <- 3
ntime2 <- 51
qSup2 <- seq(0,1,0.02)
tVec2 <- seq_len(ntime2)
Lt2 <- rep( list(tVec2), nsubj2 )
qlist=list()
qlist[[1]]=Gamhat_p[[1]]
qlist[[2]]=Gamhat_p[[2]]
qlist[[3]]=tauhat_p


Psigen=function(Lt2,qlist,nknots){
  nsubj2<-length(Lt2)
  tin <- unique( sort( unlist( Lt2 ) ) )
  trange <- range(tin)
  qSup <- seq(0,1,0.005)
  tin <- lapply( Lt2, function(t) ( t - trange[1] ) / diff(trange) )
  M <- 201
  workGrid = seq( 0, 1, length.out = M )
  qf <- lapply(1:nsubj2, function(i) {
    LocDenReg( xin = tin[[i]], qin = qlist[[i]], xout = workGrid, optns = list(qSup = qSup) )
  })
  qSup <- qf[[1]]$qSup
  qf <- lapply( 1:nsubj2, function(i) { 
    qf[[i]]$qout # each column corresponds to a time point
  } )
  
  tVec=workGrid
  
  trange <- range(tVec)
  workGrid <- ( tVec - trange[1] ) / diff(trange) # a grid on [0,1]
  M <- length(tVec)
  nsubj <- length(qf)
  qf <- lapply( qf, t ) # each column corresponds to a time point
  
  numOfKcurves = min(round(1 * (nsubj-1)))
  print(numOfKcurves)
  gijMat <- array(dim = c(numOfKcurves,M,nsubj) ) 
  distMat <- matrix( nrow = nsubj, ncol = numOfKcurves)
  hMat <- array(dim = c(nsubj,M) )
  hInvMat <- array(dim = c(nsubj,M) )
  qfAligned <- list()
  
  ## extract the closest subset to tj of workGrid (standardized presmoothing time grid)
  get_tJ <- function(tj=workGrid){
    workGrid[round(tj * (M-1)) + 1]
  }
  
  ## extract the quantile functions for the jth country on the closest subset to tj of workGrid (standardized presmoothing time grid)
  getQtJ <- function(j, tj = workGrid){
    qf[[j]][, round(tj * (M-1)) + 1]
  }
  
  ## evaluating the warping function on tGrid according to res 
  getSol <- function(res, tGrid){
    approx(x = seq(0,1, length.out = (2 + nknots)), y = c(0, sort(res), 1), xout = tGrid)$y
    #approx(x = seq(0,1, length.out = (2+ optns$nknots)), y = c(0, Rcppsort(res),1) ,n = M)$y
    #RcppPseudoApprox(X = seq(0,1, length.out = (2+ optns$nknots)), Y = c(0, Rcppsort(res),1), X_target = seq(0,1, length.out = M))
  }
  
  theCostOptim <- function(x, i, j, lambda, ti){
    tj = getSol(x, ti)
    pracma::trapz(ti, apply((getQtJ(j, tj) - getQtJ(i, ti))^2, 2, sum) + lambda * (get_tJ(tj)-ti)^2)
    #fdapace::trapzRcpp(ti, apply((getQtJ(tj, qtj) - qti)^2, 2, sum) + lambda * (tj-ti)^2)
  }
  
  getGijOptim <- function(i, j, lambda, minqaAvail ){
    s0 <- seq(0,1,length.out = (2+ nknots))[2:(1+nknots)]
    if( !minqaAvail ) { 
      optimRes <- optim( par = s0, fn = theCostOptim, method = 'L-BFGS-B', 
                         lower = rep(1e-6, nknots), upper = rep(1 - 1e-6, 2),
                         i = i, j = j, lambda = lambda, ti = workGrid)
    } else {
      optimRes <-  minqa::bobyqa( par = s0, fn = theCostOptim,  
                                  lower = rep(1e-6, nknots), upper = rep(1 - 1e-6, nknots),
                                  i = i, j = j, lambda = lambda, ti = workGrid)
    }
    bestSol <- getSol(optimRes$par, seq(0,1,length.out=M))  
    return( bestSol )
  }
  
  Vy = sqrt( sum( unlist(lapply(seq_along(qf), function(i) {
    qti = getQtJ(i)
    mu = rowMeans(qti)
    pracma::trapz(workGrid, apply((qti - mu)^2, 2, sum)) * diff(trange)
    #fdapace::trapzRcpp(workGrid, apply((qti - mu)^2, 2, sum)) * diff(trange)
  }) ) )/(nsubj-1) )
  lambda <- Vy*10^-4
  
  #for(i in seq_len(nsubj)){ # For each trajectory
    
   # cat('Computing pairwise warping for trajectory #', i, 'out of', nsubj, 'trajectories.\n')
    
  #  set.seed( i + 666 )
  #  candidateKcurves = sample(seq_len(nsubj)[-i], numOfKcurves)  
    
   # for(j in seq_len(numOfKcurves)){ # For each of the candidate curves 
  #    gijMat[j, ,i] = getGijOptim(i, candidateKcurves[j], lambda, TRUE)
  #    distMat[i,j] = sqrt(pracma::trapz(workGrid, apply((getQtJ(j=candidateKcurves[j], tj = gijMat[j, ,i]) - getQtJ(i))^2, 2, sum)))
  #    #fdapace::trapzRcpp(workGrid, apply((getQtJ(tj = gijMat[j, ,i], qtj = qf[[candidateKcurves[j]]]) - qti)^2, 2, sum))
  #  }
    
    ### 잠깐 생략
    
  #}
  
  #gijMat[1, ,3] #Psi_1
  #gijMat[2, ,3] #Psi_2

  Psi1=getGijOptim(1, 3, lambda, TRUE)
  Psi2=getGijOptim(2, 3, lambda, TRUE)
  return(list(Psi1,Psi2))
  
}


#density estimate
density_estimate <- density(qnorm_c[[2]][[33]][1,] , from = 0, to = 110, n = 1001, bw=10)
plot(density_estimate$x,density_estimate$y)
quantiles <- qnorm_c[[2]][[33]][1,]
density_estimates <- density(quantiles, from = min(quantiles), to = max(quantiles))
pdf=CreateDensity(histogram=qm_1[[3]][[33]][[1]],optns = list(outputGrid=dSup))
pdf$x
pdf$y
qSup
cum=cumtrapz(x=pdf$x,y=pdf$y)
help("CreateDensity")
qm_1[[3]][[33]][1]$bin
plot(x=pdf$x,y=pdf$y)
# Extract mids and density from the histogram
mids <- qm_1[[3]][[33]][[1]]$mids
density <- qm_1[[3]][[33]][[1]]$density

# Create the PDF using the density function
pdf <- approxfun(mids, density)
print(pdf)

# Plot the probability density function
plot(density_estimates, main = "Probability Density Function", 
     xlab = "Quantile Value", ylab = "Density", col = "blue", lwd = 2)


workGrid[35]
country_list
full_country_names <- c("Australia", "Austria", "Belgium", "Bulgaria", "Belarus", "Canada",
                        "Switzerland", "Czech Republic", "Denmark", "Spain", "Estonia", "Finland",
                        "France", "United Kingdom", "Greece", "Hungary", "Ireland", "Iceland",
                        "Israel", "Italy", "Japan", "Lithuania", "Luxembourg", "Latvia", "Netherlands",
                        "Norway", "New Zealand", "Poland", "Portugal", "Slovakia", "Slovenia",
                        "Sweden", "United States", "Germany")
full_country_names[22]
full_country_names[16]
states_list=c("Alabama","Arizona",    "Arkansas",    "Colorado"  , 
               "Connecticut", "Delaware","District of Columbia" , "Idaho"   ,
               "Illinois" ,"Iowa","Kansas"  ,  "Kentucky","Louisiana",
               "Maine","Massachusetts"   , "Michigan"  ,  "Missouri",
               "Montana","Nebraska" ,"New hampshire"   ,"North dakota" ,"Ohio"  , 
               "Oregon" , "Rhode island","South carolina","South dakota","Tennessee" ,
                "Texas" ,"Utah" ,"Virginia" ,"Wisconsin")
### Evolution Graph for each country
install.packages("gridExtra")
library(gridExtra)
install.packages("ggridges")
library(ggridges)
library(cowplot)
setwd("/Users/kimkyum/Documents/공부/Yaqing/NIBRS/")
library(reshape2)
library(ggplot2)
load("Crime_1_26.RData")
# Define the number of colors in the palette
num_colors <- 28

# Define the color ramp from blue to gray to red
color_palette <- colorRampPalette(c("purple","blue","green","yellow","red"))(num_colors)

# Display the color palette
color_palette
qnorm_c35=data_norm(350,28,qm_1[[1]],qm_1[[2]])
help(LocDenReg)


Lq <- crime_data_Lq[[1]]
Lq <- qm_1[[1]]
Lq <- qnorm_delayed2[[1]]
Lq <- qnorm_delayed2[[2]]
Lq <- qnorm_delayed[[1]]
Lq <- qnorm_delayed[[2]]

xin=seq(1,28,1)
xout=xin
Dq=LocDenReg(xin = xin, qin = Lq[[1]], xout = xout, optns = list(qSup = qSup))$dout
Dq

##### test above code with LocDenReg
xin=seq(1,28,1)
xout=xin
Dq=LocDenReg(xin = xin, qin = Lq[[1]], xout = xout, optns = list(qSup = qSup))$dout
Dq[[1]]$x

library(frechet)
install.packages("patchwork")
library(patchwork)
save.image("HMD_0304.RData")
load.image("HMD_0304.RData")
#####Origianal evolution
######## Using facet_grid
qm[[1]][[1]][1,]
# Prepare data for males
prepare_data2 <- function(Lq, gender_label) {
  data_list <- list()
  for (i in 1:length(Lq)) {
    country_data <- Lq[[i]]
    Ld <- matrix(0, nrow = 28, ncol = 101)
    for (j in 1:28) {
      density_estimate <- LocDenReg(qin = country_data[j,], optns = list(qSup=qSup,dSup=dSup))
      Ld[j, ] <- density_estimate$dout
    }
    Ld_data <- data.frame(x = density_estimate$x, t(Ld))
    time <- 1989:2016
    names(Ld_data) <- c("X", time)
    Ld_data_long <- melt(Ld_data, id.vars = "X")
    Ld_data_long$country <- full_country_names[i]
    Ld_data_long$gender <- gender_label
    data_list[[i]] <- Ld_data_long
  }
  return(do.call(rbind, data_list))
}

prepare_data <- function(Lq, gender_label) {
  data_list <- list()
  for (i in 1:(length(Lq)-1)) {
    country_data <- Lq[[i]]
    Ld <- matrix(0, nrow = 28, ncol = 107)
    for (j in 1:28) {
      density_estimate <- CreateDensity(histogram = country_data[[j]], optns = list(outputGrid = seq(0, 106, 1)))
      Ld[j, ] <- density_estimate$y
    }
    Ld_data <- data.frame(x = density_estimate$x, t(Ld))
    time <- 1989:2016
    names(Ld_data) <- c("X", time)
    Ld_data_long <- melt(Ld_data, id.vars = "X")
    Ld_data_long$country <- full_country_names[i]
    Ld_data_long$gender <- gender_label
    data_list[[i]] <- Ld_data_long
  }
  return(do.call(rbind, data_list))
}

save.image("HMD_8_21.RData")
length(qm[[]])
# Combine male and female data
data_male <- prepare_data(qm[[3]], "Male")
data_female <- prepare_data(qm[[4]], "Female")
combined_data <- rbind(data_male, data_female)

# Ensure the 'variable' column is numeric (representing years)
combined_data$variable <- as.integer(as.character(combined_data$variable))

col28 <- colorRampPalette(c("purple","blue","green","yellow","red"))(28)  # Reverse rainbow colors to have purple for 2016

# Create the male plot
plot_male <- ggplot(subset(combined_data, gender == "Male"), aes(x = X, y = value, color = variable, group = variable)) +
  geom_line(size = 0.4) +
  facet_wrap(~ country, ncol = 5) +
  scale_color_gradientn(colors = rev(col28), name = "Year", breaks = c(1989, 1996, 2003, 2010, 2016)) +
  labs(x = "Age/Year", y = "Density") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 11),  # Adjust size of facet labels
    axis.text = element_text(size = 9),
    axis.title = element_text(size = 14),
    legend.position = "none",  # Remove legend for individual plot
    panel.spacing = unit(0.2, "lines"),
    plot.margin = margin(1, 0.5, 0, 0, "cm")# Reduce spacing between plots
  )

# Create the female plot
plot_female <- ggplot(subset(combined_data, gender == "Female"), aes(x = X, y = value, color = variable, group = variable)) +
  geom_line(size = 0.4) +
  facet_wrap(~ country, ncol = 5) +
  scale_color_gradientn(colors = rev(col28), name = "Year", breaks = c(1989, 1996, 2003, 2010, 2016)) +
  labs(x = "Age/Year", y = "") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 11),  # Adjust size of facet labels
    axis.text = element_text(size = 9),
    axis.title = element_text(size = 14),
    legend.position = "none",  # Remove legend for individual plot
    panel.spacing = unit(0.2, "lines"),
    plot.margin = margin(1, 0, 0, 0.5, "cm")# Reduce spacing between plots
  )

# Extract the legend and modify it to be horizontal
legend <- get_legend(
  ggplot(combined_data, aes(x = X, y = value, color = variable, group = variable)) +
    geom_line(size = 0.8) +
    scale_color_gradientn(colors = rev(col28), name = "Year", breaks = c(1989, 1996, 2003, 2010, 2016)) +
    theme_minimal() +
    theme(
      legend.direction = "horizontal",
      legend.key.height = unit(0.5, 'cm'),
      legend.key.width = unit(1.5, 'cm'),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 13),
      legend.title.position = "top"
 )
)

combined_plot <- plot_grid(
  plot_male, plot_female,
  ncol = 2,
  rel_widths = c(1, 1)
)

# Manually adjust the positions of the labels
final_plot <- ggdraw() +
  draw_plot(legend, x = 0.12, y = 0.85, width = 0.8, height = 0.1) +  # Adjust the legend position
  draw_plot(combined_plot, y = 0, height = 0.9) +  # Adjust the height to make room for the legend
  draw_label("Males", x = 0.225, y = 0.95, fontface = "bold", size = 18, hjust = 0) +
  draw_label("Females", x = 0.82, y = 0.95, fontface = "bold", size = 18, hjust = 1)

# Display the final plot
print(final_plot)





##########
Hhat_gen(PW_mlist[[4]],PW_flist[[4]])[[1]][1:12,1:9]
Hhat_gen(PW_mlist[[1]],PW_flist[[1]])[[1]][1:12,1:9]
Hhat_test=Hhat_gen(PW_mlist[[4]],PW_flist[[4]])[[1]]
Hhat_min=Hhat_gen(PW_mmin,PW_fmin)[[1]]
save.image("HMD_0222.RData")
Hhatinv_test=Hhat_gen(PW_mlist[[1]],PW_mlist[[1]])[[2]]
Hhatinv_min=Hhat_gen(PW_mmin,PW_fmin)[[2]]
order_index
fwarp_list_t=list()
rows_of_interest=seq(1,34,1)
colors <- rainbow(34)
for(row in rows_of_interest){
  fwarp_list_t[[row]]=Hhat_test[row,]
}
par(mfrow=c(6,6))
for(j in order_index[1:34]){
  plot(NULL, xlim = c(0, 29), ylim = c(0, 29), 
       xlab = "time", ylab = "time", main = paste0(full_country_names[j]))
  
  # Loop through the list and plot each density function
  for (i in seq_along(fwarp_list_t)) {
    lines(PW_flist[[1]]$workGrid, fwarp_list_t[[i]], col = "gray", lwd = 2)
  }
  lines(PW_flist[[1]]$workGrid, fwarp_list_t[[j]], col = colors[1], lwd = 2)
}
### H warping ggplot version (new purple color)
library(ggplot2)
library(cowplot)
Hhat_delayed=Hhat_gen(PW_mmin_de,PW_fmin_de)[[1]]
dfp2 = data.frame(matrix(rnorm(201*34),nrow=201))
#for (i in 1:nrow(Hhat_test)) {
#  dfp2[, i] <- Hhat_delayed[i, ]+1988
#}
dfp2 = data.frame(matrix(rnorm(201*34),nrow=201))
for (i in 1:nrow(Hhat_w0)) {
  dfp2[, i] <- Hhat_w0m3[i, ]+1988
}


plots <- lapply(order_index, function(i) {
  p <- ggplot(data = dfp2, aes(x = PW_fmin$workGrid + 1988)) +
    xlim(1988, 2017) + ylim(1988, 2017) +
    labs(title = full_country_names[i]) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14),  # Adjust title font size
      axis.title = element_blank(),
      axis.text = element_text(size = 10)# Remove axis titles
    ) +
    scale_y_continuous(breaks = c(1989, 1998, 2007, 2016)) +
    scale_x_continuous(breaks = c(1989, 1998, 2007, 2016)) 
  
  for (j in 1:34) {
    p <- p + geom_line(aes_string(y = names(dfp2)[j]), color = "gray", size = 0.5)
  }
  
  p <- p +
    geom_line(aes_string(y = names(dfp2)[i]), color = "red", size = 0.7) +
    geom_line(aes_string(y = PW_flist[[1]]$workGrid + 1988), color = "black", size = 0.5, linetype = "dashed") +
    theme(legend.position = "none")
  
  return(p)
})

# Combine all plots into one grid
final_plot <- plot_grid(plotlist = plots, nrow = 6, ncol = 6)

# Add common x-axis and y-axis labels
final_plot2 <- ggdraw(final_plot) +
  theme(plot.margin = margin(10, 10, 20, 20)) +
  draw_label("Year", x = 0.5, y = -0.02, hjust = 0.5, vjust = 0, size = 16, fontface = "bold") +  # Adjust y for x-axis label
  draw_label("Year", x = -0.01, y = 0.5, hjust = 0, vjust = 0.5, angle = 90, size = 16, fontface = "bold") 

print(final_plot2)
####
# Function to create base plots
create_base_plot <- function(i) {
  p <- ggplot(data = dfp2, aes(x = PW_fmin$workGrid + 1988)) +
    xlim(1988, 2017) + ylim(1988, 2017) +
    labs(title = full_country_names[i]) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14),  # Adjust title font size
      axis.title = element_blank(),
      axis.text = element_text(size = 10)
    ) +
    scale_y_continuous(breaks = c(1989, 1998, 2007, 2016)) +
    scale_x_continuous(breaks = c(1989, 1998, 2007, 2016))
  
  for (j in 1:34) {
    p <- p + geom_line(aes_string(y = names(dfp2)[j]), color = "gray", size = 0.5)
  }
  
  p <- p +
    geom_line(aes_string(y = names(dfp2)[i]), color = "#4B0082", size = 0.7) +
    geom_line(aes_string(y = PW_flist[[1]]$workGrid + 1988), color = "black", size = 0.5, linetype = "dashed") +
    theme(legend.position = "none")
  
  return(p)
}

# Create all base plots
plots <- lapply(order_index, create_base_plot)
adjust_axis_text_and_margin <- function(plot, is_bottom, is_left) {
  plot + theme(
    axis.text.x = if (is_bottom) element_text(size = 10) else element_blank(),
    axis.text.y = if (is_left) element_text(size = 10) else element_blank(),
    plot.margin = margin(1, 0.5, 1, 1)  # Reduce margins around plots
  )
}
# Define which plots are on the bottom and left
nrow <- 6
ncol <- 6

plots_adjusted <- lapply(seq_along(plots), function(i) {
  row_pos <- ceiling(i / ncol)
  col_pos <- i %% ncol
  if (col_pos == 0) col_pos <- ncol
  is_bottom <- row_pos == nrow
  is_left <- col_pos == 1
  adjust_axis_text_and_margin(plots[[i]], is_bottom, is_left)
})
final_plot <- plot_grid(plotlist = plots_adjusted, nrow = 6, ncol = 6, align = 'v', 
                        rel_heights = rep(1, nrow), rel_widths = c(1,1,1,1,1,1))

# Add common axis labels
final_plot2 <- ggdraw(final_plot) +
  draw_label("Year", x = 0.5, y = -0.03, hjust = 0.5, vjust = 0, size = 16, fontface = "bold") +
  draw_label("Year", x = -0.02, y = 0.5, hjust = 0, vjust = 0.5, angle = 90, size = 16, fontface = "bold") +
  theme(plot.margin = margin(10, 10, 21, 21))

print(final_plot2)

# Combine dfp2 into a single long format dataframe for ggplot
library(tidyr)
library(ggplot2)
library(dplyr)
# Ensure the columns are named correctly in dfp2 (e.g., "V1", "V2", ..., "V34")

# create sample data
dfp <- data.frame(matrix(rnorm(147), nrow=21))
dfp2 = data.frame(matrix(rnorm(201*34),nrow=201))
for (i in 1:nrow(Hhat_test)) {
  dfp2[, i] <- Hhat_min[i, ]+1988
}
# original plot (removed unnecessary call to y)
p <- ggplot(data=dfp2, aes(x=PW_flist[[1]]$workGrid+1988)) +
  xlim(1988, 2017) + ylim(1988, 2017) +
  labs(x = "year", y = "year", title = full_country_names[1]) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks = c(1989, 1998, 2007 ,2016)) +
  scale_x_continuous(breaks = c(1989, 1998, 2007 ,2016))
# loop
for (i in 1:34) {
  # use aes_string with names of the data.frame
  p <- p + geom_line(aes_string(y = names(dfp2)[i]),color = "gray", size = 0.5)
}
p=p+geom_line(aes_string(y = names(dfp2)[1]), color = "red", size = 0.5)
p=p+geom_line(aes_string(y = PW_flist[[1]]$workGrid+1988), color = "black", size = 0.7, linetype="dashed")
p <- p + theme(legend.position = "none")
# print the result
print(p)

dfp <- data.frame(matrix(rnorm(147), nrow=21))

p <- ggplot() +
  xlim(0, 29) + ylim(0, 29) +
  labs(x = "time", y = "time", title = full_country_names[1]) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Initialize an empty list to accumulate geom_line layers
lines <- list()

# Add gray lines for all countries
for (j in 1:34) {
  lines[[j]] <- geom_line(aes(x = PW_flist[[1]]$workGrid, y = fwarp_list_t[[j]]), color = "gray", linetype = "dashed", size = 0.5)
}

# Add red line for the current country
lines[[35]] <- geom_line(aes(x = PW_flist[[1]]$workGrid, y = fwarp_list_t[[i]]), color = "red", size = 0.5)

# Add all accumulated lines to the plot
p <- p + lines

# Remove legend
p <- p + theme(legend.position = "none")


## Individual H_i for male with p=1

mwarp_list_t=list()
rows_of_interest=seq(1,34,1)
colors <- rainbow(34)
for(row in rows_of_interest){ 
  mwarp_list_t[[row]]=PW_mlist[[14]]$hInv[row,]
}
par(mfrow=c(4,5))
for(j in order_index[1:20]){
  plot(NULL, xlim = c(0, 29), ylim = c(0, 29), 
       xlab = "time", ylab = "time", main = "Warping for male")
  
  # Loop through the list and plot each density function
  for (i in seq_along(mwarp_list_t)) {
    lines(PW_m2p$workGrid, mwarp_list_t[[i]], col = "gray", lwd = 2)
  }
  lines(PW_m2p$workGrid, mwarp_list_t[[j]], col = colors[1], lwd = 2)
  legend("bottomright", legend = paste0(full_country_names[j]), col = colors, lty = 1, lwd = 2)
  
}
par(mfrow=c(3,5))
for(j in order_index[21:34]){
  plot(NULL, xlim = c(0, 29), ylim = c(0, 29), 
       xlab = "time", ylab = "time", main = "Warping for male")
  
  # Loop through the list and plot each density function
  for (i in seq_along(mwarp_list_t)) {
    lines(PW_m2p$workGrid, mwarp_list_t[[i]], col = "gray", lwd = 2)
  }
  lines(PW_m2p$workGrid, mwarp_list_t[[j]], col = colors[1], lwd = 2)
  legend("bottomright", legend = paste0(full_country_names[j]), col = colors, lty = 1, lwd = 2)
  
}

plot(PW_m1p$workGrid,Hhat_0.3[21,])

# Full H_i plot p=1
hwarp_list_t=list()
rows_of_interest=seq(1,34,1)
colors <- rainbow(34)
for(row in rows_of_interest){
  hwarp_list_t[[row]]=Hhat_1[row,]
}
par(mfrow=c(4,5))
for (j in c(1:20)) {
  # Set up an empty plot with specified axes and labels
  plot(NULL, xlim = c(0, 29), ylim = c(0, 29), 
       xlab = "time", ylab = "time", main =  paste("Warping for", country_list[j]))
  
  # Loop through the list and plot each density function in gray
  for (i in seq_along(hwarp_list_t)) {
    lines(PW_m1p$workGrid, hwarp_list_t[[i]], col = "gray", lwd = 2)
  }
  
  # Plot the warping function for the current component in a different color
  lines(PW_m1p$workGrid, hwarp_list_t[[j]], col = colors[1], lwd = 2)
  
  # Add a legend indicating the country for the current component
  legend("bottomright", legend = paste(country_list[j]), col = colors[1], lty = 1, lwd = 2)
}

par(mfrow=c(3,5))
for (j in c(21:34)) {
  # Set up an empty plot with specified axes and labels
  plot(NULL, xlim = c(0, 29), ylim = c(0, 29), 
       xlab = "time", ylab = "time", main =  paste("Warping for", country_list[j]))
  
  # Loop through the list and plot each density function in gray
  for (i in seq_along(hwarp_list_t)) {
    lines(PW_m1p$workGrid, hwarp_list_t[[i]], col = "gray", lwd = 2)
  }
  
  # Plot the warping function for the current component in a different color
  lines(PW_m1p$workGrid, hwarp_list_t[[j]], col = colors[1], lwd = 2)
  
  # Add a legend indicating the country for the current component
  legend("bottomright", legend = paste(country_list[j]), col = colors[1], lty = 1, lwd = 2)
}


# Hhat plot for p=1
fwarp_list_2=list()
rows_of_interest=seq(1,34,1)
colors <- rainbow(34)
for(row in rows_of_interest){
  fwarp_list_2[[row]]=PW_f2p$hInv[row,]
}
par(mfrow=c(4,5))
for(j in c(1:20)){
  plot(NULL, xlim = c(0, 29), ylim = c(0, 29), 
       xlab = "time", ylab = "time", main = "Warping for female")
  
  # Loop through the list and plot each density function
  for (i in seq_along(fwarp_list_2)) {
    lines(PW_f2p$workGrid, fwarp_list_2[[i]], col = "gray", lwd = 2)
  }
  lines(PW_f2p$workGrid, fwarp_list_2[[j]], col = colors[1], lwd = 2)
  legend("bottomright", legend = paste0(country_list[j]), col = colors, lty = 1, lwd = 2)
  
}
par(mfrow=c(3,5))
for(j in c(21:34)){
  plot(NULL, xlim = c(0, 29), ylim = c(0, 29), 
       xlab = "time", ylab = "time", main = "Warping for female")
  
  # Loop through the list and plot each density function
  for (i in seq_along(fwarp_list_2)) {
    lines(PW_f2p$workGrid, fwarp_list_2[[i]], col = "gray", lwd = 2)
  }
  lines(PW_f2p$workGrid, fwarp_list_2[[j]], col = colors[1], lwd = 2)
  legend("bottomright", legend = paste0(country_list[j]), col = colors, lty = 1, lwd = 2)
  
}



install.packages("Polychrome")
library(Polychrome)
P36 = createPalette(36,  c("#ff0000", "#00ff00", "#0000ff"))
swatch(P36)

#### Hhat
fwarp_list_t=list()
rows_of_interest=seq(1,34,1)
colors <- rainbow(34)
for(row in rows_of_interest){
  fwarp_list_t[[row]]=PW_flist[[11]]$hInv[row,]
}
par(mfrow=c(1,2))
plot(NULL, xlim = c(0, 29), ylim = c(0, 29), 
     xlab = "time", ylab = "time", main = "Subject Warping Functions")
for (i in seq_along(hwarp_list_t)) {
  lines(PW_m2p$workGrid, fwarp_list_t[[i]], col = col[i], lwd = 2)
}
plot(NULL, xlim = c(0, 29), ylim = c(0, 29), 
     xlab = "time", ylab = "time", main = "Subject Warping Functions")
for (i in seq_along(hwarp_list_t)) {
  lines(PW_m2p$workGrid, fwarp_list_t[[i]], col = col[i], lwd = 2)
}
# Add a legend
legend("bottomright", legend = paste0(country_list), col = P36, lty = 1, lwd = 2)

country_list=c("aus",    "aut",    "bel"  , 
               "bgr"  ,  "blr"  ,  "can"   ,
               "che"  ,  "cze","dnk"  ,  "esp",
               "est"   , "fin"  ,  "fratnp",
               "gbr" ,"grc"   , "hun"  , 
               "irl"  ,  "isl"   , "isr"   ,
               "ita"  ,  "jpn"   , "ltu"  , 
               "lux"  ,  "lva"   , "nld"  , 
               "nor"  ,  "nzl", "pol"  , 
               "prt"  ,  "svk"  ,  "svn" ,  
               "swe"  ,  "usa", "ger"   )

### Pairwise time warping for ditribution valued data
nsubj <- 34
ntime <- 28
qSup <- seq(0,1,0.02)
tVec <- seq_len(ntime)
Lt <- rep( list(tVec), nsubj )

PW_m1=WassPW(Lt=Lt,Lq=qnorm_1[[1]])
PW_f1=WassPW(Lt=Lt,Lq=qnorm_1[[2]])

plot(PW_m1$workGrid,PW_m1$hInv[25,])
plot(PW_f1$workGrid,PW_f1$hInv[25,])



dim(Lq_m[[1]])
dim(id)

List_RDS=list.files("/Users/kimkyum/Documents/공부/Yaqing/HMD/R2")
setwd("/Users/kimkyum/Documents/공부/Yaqing/HMD/R2")
List_using=lapply(List_RDS, readRDS)
List_RDS

male=array(0,c(35,34,100000))
for (i in 1:35){
  L=List_using[[i]]
  for(j in 1:34){
    l=L[L$year==1982+j & L$sex == "male" ,]$dx
    if(length(l) != 0){
      l[101]=max(100000-sum(as.numeric(l[1:100])),0)
      #print(paste0("contry ",i, "and This is iteration",j,",the value is",l[101]))
      l[1]=100000-sum(as.numeric(l[2:101]))
      male[i,j,] <- rep(c(0:100),as.numeric(l[1:101]))
    }
    else {male[i,j,]=rep(0,100000)}
  }
}

female=array(0,c(35,34,100000))
for (i in 1:35){
  L=List_using[[i]]
  for(j in 1:34){
    l=L[L$year==1982+j & L$sex == "female" ,]$dx
    if(length(l) != 0){
      l[101]=max(100000-sum(as.numeric(l[1:100])),0)
      #print(paste0("contry ",i, "and This is iteration",j,",the value is",l[101]))
      l[1]=100000-sum(as.numeric(l[2:101]))
      female[i,j,] <- rep(c(0:100),as.numeric(l[1:101]))
    }
    else {female[i,j,]=rep(0,100000)}
  }
}

my_data = list(male, female)
my_data[[1]]
my_data2=list(male,female)
for(i in c(1:8)){
  my_data2[[1]][i,,]=my_data[[1]][i,,]
}
for(i in c(9:10)){
  my_data2[[1]][i+25,,]=my_data[[1]][i,,]
}
for(i in c(11:35)){
  my_data2[[1]][i-2,,]=my_data[[1]][i,,]
}
for(i in c(1:8)){
  my_data2[[2]][i,,]=my_data[[2]][i,,]
}
for(i in c(9:10)){
  my_data2[[2]][i+25,,]=my_data[[2]][i,,]
}
for(i in c(11:35)){
  my_data2[[2]][i-2,,]=my_data[[2]][i,,]
}

my_data=my_data2  

for (x in xin) {
  # Extract specific columns using xin
  data_value1 <- my_data[[2]][9, x, ]
  data_value2 <- my_data[[2]][10, x, ]
  data_value <- c(data_value1,data_value2)
  # Create histogram and store it in the list for the current x
  hist_list_f[[34]][[as.character(x)]] <- hist(data_value, plot = FALSE)
}



###### Local frechet density estimation
Fvar
##### my_data는 순서좀 달라.(동독서독을 뒤로빼면서 순서가 꼬였는데 수정함,34,35가 동독서독)
median(my_data[[2]][21,7,])
median(my_data[[2]][21,16,])
median(my_data[[2]][21,27,])
median(my_data[[2]][21,34,])

median(my_data[[2]][1,7,])
median(my_data[[2]][1,16,])
median(my_data[[2]][1,27,])
median(my_data[[2]][1,34,])

mean(my_data[[1]][8,7,])
mean(my_data[[1]][8,26,])
mean(my_data[[1]][8,34,])

mean(my_data[[2]][31,7,])
mean(my_data[[2]][31,26,])
mean(my_data[[2]][31,34,])

table(my_data[[1]][26,7,])
plot(res_list_f[[1]]$dout[[7]]$x,res_list_f[[1]]$dout[[7]]$y)
plot(res_list_f[[21]])
res_list_f[[1]]$dout[[1]]
country_list=c("aus",    "aut",    "bel"  , 
               "bgr"  ,  "blr"  ,  "can"   ,
               "che"  ,  "cze","dnk"  ,  "esp",
               "est"   , "fin"  ,  "fratnp",
               "gbr" ,"grc"   , "hun"  , 
               "irl"  ,  "isl"   , "isr"   ,
               "ita"  ,  "jpn"   , "ltu"  , 
               "lux"  ,  "lva"   , "nld"  , 
               "nor"  ,  "nzl", "pol"  , 
               "prt"  ,  "svk"  ,  "svn" ,  
               "swe"  ,  "usa", "ger"   )


hist_list_f[[33]]

### histogram list generate

xin <- seq(1, 34, 1)

# Create a list to store histograms
hist_list <- list()

# Rows of interest
rows_of_interest <- c(1:35)

# Loop through the rows and xin values to create histograms
for (row in rows_of_interest) {

  # Create a list to store histograms for the current row
  hist_row_list <- list()
  
  for (x in xin) {
    # Extract specific columns using xin
    data_value <- my_data[[1]][row, x, ]
    
    # Create histogram and store it in the list for the current x
    hist_row_list[[as.character(x)]] <- hist(data_value, plot = FALSE)
  }
  
  # Store the list of histograms for the current row in hist_list
  hist_list[[row]] <- hist_row_list
}

hist_list_m=hist_list

for(i in c(1:33)){
  hist_list_m[[i]]=hist_list[[i]]
}
for (x in xin) {
  # Extract specific columns using xin
  data_value1 <- my_data[[1]][34, x, ]
  data_value2 <- my_data[[1]][35, x, ]
  data_value <- c(data_value1,data_value2)
  # Create histogram and store it in the list for the current x
  hist_list_m[[34]][[as.character(x)]] <- hist(data_value, plot = FALSE)
}


# Create a list to store histograms
hist_list_f <- list()


# Loop through the rows and xin values to create histograms
for (row in rows_of_interest) {
  
  # Create a list to store histograms for the current row
  hist_row_list <- list()
  
  for (x in xin) {
    # Extract specific columns using xin
    data_value <- my_data[[2]][row, x, ]
    
    # Create histogram and store it in the list for the current x
    hist_row_list[[as.character(x)]] <- hist(data_value, plot = FALSE)
  }
  
  # Store the list of histograms for the current row in hist_list
  hist_list_f[[row]] <- hist_row_list
}


### Combining east and west of germen

for (x in xin) {
  # Extract specific columns using xin
  data_value1 <- my_data[[2]][34, x, ]
  data_value2 <- my_data[[2]][35, x, ]
  data_value <- c(data_value1,data_value2)
  # Create histogram and store it in the list for the current x
  hist_list_f[[34]][[as.character(x)]] <- hist(data_value, plot = FALSE)
}


# Create a list to store the results_female
rows_of_interest<- c(1:34)
xout=xin
xin
qSup=seq(0,1,0.02)

res_list_f <- list()

# Loop through the rows to apply LocDenReg
for (row in rows_of_interest) {
  hist_row_list <- hist_list_f[[row]]
  
  # Apply LocDenReg for the current row
  res <- LocDenReg(xin = xin, hin = hist_row_list, xout = xout, optns = list(qSup = qSup))
  
  # Store the result in the list
  res_list_f[[row]] <- res
}

res_list_ft <- list()

# Loop through the rows to apply LocDenReg
for (row in rows_of_interest) {
  hist_row_list <- hist_list_f[[row]]
  
  # Apply LocDenReg for the current row
  res <- LocDenReg(xin = xin, hin = hist_row_list, xout = xout, optns = list(qSup = qSup))
  
  # Store the result in the list
  res_list_ft[[row]] <- res
}
res_list_ft[[34]]$qout[34,]
# Create a list to store the results_male
res_list_m <- list()

# Loop through the rows to apply LocDenReg
for (row in rows_of_interest) {
  hist_row_list <- hist_list_m[[row]]
  
  # Apply LocDenReg for the current row
  res <- LocDenReg(xin = xin, hin = hist_row_list, xout = xout, optns = list(qSup = qSup))
  
  # Store the result in the list
  res_list_m[[row]] <- res
}


# Access the histograms

plot(res_list_m[[11]],obj=1)
plot(res_list_f[[33]],obj=1)
res_list_m[[1]]$qSup
seq(0,1,0.05)
dim(res_list_m[[1]]$qout)
res_list_m[[1]]$qout[1,51]
# Transform distribution to Transport data
dSup=qSup*100
plot(dSup,res_list_m[[1]]$qout[1,])
That_list=list()
### This is nothing but Just changing the domain from qSup to dSup

#### extract qf from res_list according to the component(sex)
# res_list[[i]] is a result of applying LocDenReg to ith country
Lq_m=list()
for (row in rows_of_interest) {
  Lq_m[[row]] <- res_list_m[[row]]$qout
}
Lq_f=list()
for (row in rows_of_interest) {
  Lq_f[[row]] <- res_list_f[[row]]$qout
}


#### Normalize the T_ij over t
par(mfrow=c(1,1))
Lq_m_norm=list()
dSup
id= matrix(rep(dSup, each = 34), nrow = 34, byrow = FALSE)
Ahat_m=c()
for(row in rows_of_interest){
  Ahat=max(apply(Lq_m[[row]],1,function(t){sqrt(pracma::trapz(dSup, (t-dSup)^2))}))
  Ahat_m[row]=Ahat
  Lq_m_norm[[row]]=(Lq_m[[row]]-id)/Ahat+id
}
round(Ahat_m,2)

Lq_f_norm=list()
Ahat_f=c()
for(row in rows_of_interest){
  Ahat=max(apply(Lq_f[[row]],1,function(t){sqrt(pracma::trapz(dSup, (t-dSup)^2))}))
  Ahat_f[row]=Ahat
  Lq_f_norm[[row]]=(Lq_f[[row]]-id)/Ahat+id
}
round(Ahat_f,2)


Lq_m_norm[[1]][1,]
dim(Lq_m[[1]])


plot(dSup,Lq_f_norm[[21]][1,])


##### Optimal sub period searching
calculate_Fvar <- function(Lq_m_norm, Lq_f_norm, index) {
  Fvar_value = 0
  rows_of_interest = c(1:34)
  qt=list()
    for (row in rows_of_interest) {
      qt[[row]] = list(x=qSup,y=Lq_m_norm[[row]][index,])
      qt[[row+34]] = list(x=qSup,y=Lq_f_norm[[row]][index,])
    }
    Fmean = DenFMean(qin = qt, optns = list(qSup = seq(0, 1, 0.02)))  
    Fmean_list = list(x = Fmean$qSup, y = Fmean$qout)
    
    for (i in c(1:34)) {
      
      list_m = list(x = qSup, y = Lq_m_norm[[i]][index,])
      list_f = list(x = qSup, y = Lq_f_norm[[i]][index,])
      Fvar_value = Fvar_value + dist4den(Fmean_list, list_m,fctn_type="quantile") + dist4den(Fmean_list, list_f,fctn_type="quantile")
    }
  print(Fvar_value)
  return(Fvar_value)
}
Fvar=c()
for (i in c(1,28)){
  Fvar[i]=calculate_Fvar(qnorm_c15[[1]], qnorm_c15[[2]], i)
}
c(Fvar[1],Fvar[28])

qnorm_c15 <- data_norm(150, 28, qm_1[[1]], qm_1[[2]])


plot(x = qSup, y = Lq_f_norm[[1]][1,])
plot(x = Fmean$qSup, y = Fmean$qout)
qSup
help("DenFVar")
resF <- DenFVar(qin = Ly, supin = qSup)
### Pairwise time warping for ditribution valued data
nsubj <- 34
ntime <- 34
qSup <- seq(0,1,0.02)
tVec <- seq_len(ntime)
Lt <- rep( list(tVec), nsubj )

PW_m=WassPW(Lt=Lt,Lq=Lq_m)
PW_f=WassPW(Lt=Lt,Lq=Lq_f)

plot(PW_m$workGrid,PW_m$hInv[25,])
plot(PW_f$workGrid,PW_f$hInv[25,])

PW_m_norm=WassPW(Lt=Lt,Lq=Lq_m_norm)
PW_f_norm=WassPW(Lt=Lt,Lq=Lq_f_norm)

plot(PW_m$workGrid,PW_f_norm$hInv[25,])

Hhat=(PW_m_norm$hInv+PW_f_norm$hInv)/2
Hhat_inv=(PW_m_norm$h+PW_f_norm$h)/2
plot(PW_m_norm$workGrid,Hhat[21,])

hist_list_f[[1]]

##### \tau(\Psi_j)=\gamma_j hat estimation
###Hinv -> locdenreg(xout=Hinv) -> take average over i
# Loop through the rows to apply LocDenReg
res_list_f_hinv=list()
for (row in rows_of_interest) {
  hist_row_list <- hist_list_f[[row]]
  
  # Apply LocDenReg for the current row
  res <- LocDenReg(xin = xin, hin = hist_row_list, xout = Hhat_inv[row,], optns = list(qSup = qSup))
  
  # Store the result in the list
  res_list_f_hinv[[row]] <- res
}

res_list_m_hinv=list()
for (row in rows_of_interest) {
  hist_row_list <- hist_list_m[[row]]
  
  # Apply LocDenReg for the current row
  res <- LocDenReg(xin = xin, hin = hist_row_list, xout = Hhat_inv[row,], optns = list(qSup = qSup))
  
  # Store the result in the list
  res_list_m_hinv[[row]] <- res
}
###extract qf
Lq_m_hinv=list()
for (row in rows_of_interest) {
  Lq_m_hinv[[row]] <- res_list_m_hinv[[row]]$qout   ### quantile만 저장
}
Lq_f_hinv=list()  #### note that each component is 51 by 51 matrix 
for (row in rows_of_interest) {   ## each row is t(34를 51개로 쪼갬)
  Lq_f_hinv[[row]] <- res_list_f_hinv[[row]]$qout
}
plot(dSup,Lq_m[[1]][2,])
plot(dSup,Lq_mhinv_norm[[1]][2,])

###normalize
Lq_mhinv_norm=list()
id2= matrix(rep(dSup, each = 51), nrow = 51, byrow = FALSE)
for(row in rows_of_interest){
  Ahat=max(apply(Lq_m_hinv[[row]],1,function(t){sqrt(pracma::trapz(dSup, (t-dSup)^2))}))
  Lq_mhinv_norm[[row]]=(Lq_m_hinv[[row]]-id2)/Ahat+id2
}

Lq_fhinv_norm=list()
for(row in rows_of_interest){
  Ahat=max(apply(Lq_f_hinv[[row]],1,function(t){sqrt(pracma::trapz(dSup, (t-dSup)^2))}))
  Lq_fhinv_norm[[row]]=(Lq_f_hinv[[row]]-id2)/Ahat+id2
}

### averaging
tauj_fhat=0
for(row in rows_of_interest){
 tauj_fhat=tauj_fhat+Lq_fhinv_norm[[row]]
}
tauj_fhat=tauj_fhat/35
dim(tauj_fhat)

tauj_mhat=0
for(row in rows_of_interest){
  tauj_mhat=tauj_mhat+Lq_mhinv_norm[[row]]
}
tauj_mhat=tauj_mhat/35
dim(tauj_mhat)
plot(dSup,tauj_fhat[2,])

#### Hhat plot
# Assuming your list of density functions is called "density_list"
# density_list <- list(density_function_1, density_function_2, ..., density_function_35)

# Create a vector of colors
mwarp_list=list()
for(row in rows_of_interest){
  mwarp_list[[row]]=PW_m_norm$hInv[row,]
}

mwarp_list_h=list()
for(row in rows_of_interest){
  mwarp_list_h[[row]]=PW_m$hInv[row,]
}

fwarp_list=list()
for(row in rows_of_interest){
  fwarp_list[[row]]=PW_f_norm$hInv[row,]
}
par(mfrow=c(1,1))
colors <- rainbow(34)
length(mwarp_list)
# Create an empty plot
plot(NULL, xlim = c(0, 35), ylim = c(0, 35), 
     xlab = "time", ylab = "time", main = "Warping Functions for male")


plot(NULL, xlim = c(0, 35), ylim = c(0, 35), 
     xlab = "time", ylab = "time", main = "Warping Functions for female")
# Loop through the list and plot each density function
for (i in seq_along(mwarp_list)) {
  lines(PW_m_norm$workGrid, mwarp_list[[i]], col = colors[i], lwd = 2)
}
lines(PW_m_norm$workGrid, mwarp_list[[24]], col = colors[24], lwd = 2)

lines(PW_m_norm$workGrid, mwarp_list[[25]], col = colors[25], lwd = 2)
lines(PW_m_norm$workGrid, mwarp_list[[23]], col = colors[23], lwd = 2)
  # Loop through the list and plot each density function
for (i in seq_along(mdensity_list)) {
  lines(PW_m_norm$workGrid, fwarp_list[[i]], col = colors[i], lwd = 2)
}

for (i in seq_along(mdensity_list)) {
  lines(PW_m_norm$workGrid, mwarp_list_h[[i]], col = colors[i], lwd = 2)
}
lines(PW_m_norm$workGrid, mwarp_list_h[[11]], col = colors[11], lwd = 2)
country_list=c("aus",    "aut",    "bel"  , 
               "bgr"  ,  "blr"  ,  "can"   ,
               "che"  ,  "cze","dnk"  ,  "esp",
               "est"   , "fin"  ,  "fratnp",
               "gbr" ,"grc"   , "hun"  , 
               "irl"  ,  "isl"   , "isr"   ,
               "ita"  ,  "jpn"   , "ltu"  , 
               "lux"  ,  "lva"   , "nld"  , 
               "nor"  ,  "nzl", "pol"  , 
               "prt"  ,  "svk"  ,  "svn" ,  
               "swe"  ,  "usa", "ger"   )


# Add a legend
legend("bottomright", legend = paste0(country_list), col = colors, lty = 1, lwd = 2)



###### Highlight plot
mwarp_list_1=list()
for(row in rows_of_interest){
  mwarp_list_1[[row]]=PW_m1$hInv[row,]
}
fwarp_list_1=list()
for(row in rows_of_interest){
  fwarp_list_1[[row]]=PW_f1$hInv[row,]
}
par(mfrow=c(4,5))
for(j in c(21:34)){
  plot(NULL, xlim = c(0, 29), ylim = c(0, 29), 
       xlab = "time", ylab = "time", main = "Warping for female")
  
  # Loop through the list and plot each density function
  for (i in seq_along(fwarp_list_1)) {
    lines(PW_m1$workGrid, fwarp_list_1[[i]], col = "gray", lwd = 2)
  }
  lines(PW_m1$workGrid, fwarp_list_1[[j]], col = colors[1], lwd = 2)
  legend("bottomright", legend = paste0(country_list[j]), col = colors, lty = 1, lwd = 2)
  
}




###### Pairwise warping function(WassPWdense) study
trange <- range(tVec)
trange
workGrid <- ( tVec - trange[1] ) / diff(trange) # a grid on [0,1]
workGrid
M <- length(tVec)
nsubj <- length(qf)
nsubj
qf <- lapply( Lq_m, t ) # each column corresponds to a time point

dim(qf[[1]])
qf[[1]][,1]
## extract the quantile functions for the jth country on the closest subset to tj of workGrid (standardized presmoothing time grid)


get_tJ <- function(tj=workGrid){
  workGrid[round(tj * (M-1)) + 1]
}
get_tJ(getSol(s0, workGrid))-workGrid

getQtJ <- function(j, tj = workGrid){
  qf[[j]][, round(tj * (M-1)) + 1]
}

getSol <- function(res, tGrid){
  approx(x = seq(0,1, length.out = (2 + nknots)), y = c(0, sort(res), 1), xout = tGrid)$y
  #approx(x = seq(0,1, length.out = (2+ optns$nknots)), y = c(0, Rcppsort(res),1) ,n = M)$y
  #RcppPseudoApprox(X = seq(0,1, length.out = (2+ optns$nknots)), Y = c(0, Rcppsort(res),1), X_target = seq(0,1, length.out = M))
}
getSol(s0,workGrid)
workGrid
theCostOptim <- function(x, i, j, lambda, ti){
  tj = getSol(x, ti)
  pracma::trapz(ti, apply((getQtJ(j, tj) - getQtJ(i, ti))^2, 2, sum) + lambda * (get_tJ(tj)-ti)^2)
  #fdapace::trapzRcpp(ti, apply((getQtJ(tj, qtj) - qti)^2, 2, sum) + lambda * (tj-ti)^2)
}
theCostOptim(s0,i=1,j=3,lambda=lambda,ti=workGrid)
theCostOptim(s0,i=1,j=3,lambda=100,ti=workGrid)
s0 <- seq(0,1,length.out = (2+ 2))[2:(1+2)]
s0
nknots=2
getGijOptim <- function(i, j, lambda, minqaAvail ){
  s0 <- seq(0,1,length.out = (2+ optns$nknots))[2:(1+optns$nknots)]
  if( !minqaAvail ) { 
    optimRes <- optim( par = s0, fn = theCostOptim, method = 'L-BFGS-B', 
                       lower = rep(1e-6, optns$nknots), upper = rep(1 - 1e-6, optns$nknots),
                       i = i, j = j, lambda = lambda, ti = workGrid)
  } else {
    optimRes <-  minqa::bobyqa( par = s0, fn = theCostOptim,  
                                lower = rep(1e-6, optns$nknots), upper = rep(1 - 1e-6, optns$nknots),
                                i = i, j = j, lambda = lambda, ti = workGrid)
  }
  bestSol <- getSol(optimRes$par, seq(0,1,length.out=M))  
  return( bestSol )
}

Vy = sqrt( sum( unlist(lapply(seq_along(qf), function(i) {
  qti = getQtJ(i)
  mu = rowMeans(qti)
  pracma::trapz(workGrid, apply((qti - mu)^2, 2, sum)) * diff(trange)
  #fdapace::trapzRcpp(workGrid, apply((qti - mu)^2, 2, sum)) * diff(trange)
}) ) )/(nsubj-1) )
lambda <- Vy*10^-4

optimRes <- optim( par = s0, fn = theCostOptim, method = 'L-BFGS-B', 
                   lower = rep(1e-6, 2), upper = rep(1 - 1e-6, 2),
                   i = 2, j = 34, lambda = lambda, ti = workGrid)
optimRes
pracma::trapz(c(1/3,2/3,1), c(1,4,9))*3
sqrt(pracma::trapz(qSup, (Lq_m[[1]][34,]/100-qSup)^2))


max(apply(Lq_m[[25]],1,function(t){sqrt(pracma::trapz(dSup, (t-dSup)^2))}))
apply(Lq_m[[33]],1,function(t){sqrt(pracma::trapz(dSup, (t-dSup)^2))})
par(mfrow=c(1,2))
plot(dSup,Lq_m[[18]][34,])
lines(dSup,dSup)
plot(dSup,Lq_m[[25]][34,])
lines(dSup,dSup)
Lq_m_norm=list()
for(row in rows_of_interest){
  sup=max(apply(Lq_m[[row]],1,function(t){sqrt(pracma::trapz(dSup, (t-dSup)^2))}))
  Lq_m_norm[[row]]=Lq_m[[row]]/sup
}
Lq_m_norm[[1]][1,]
Lq_m[[1]][1,]
### Qunatile, F assign
### male
x0 <-seq(0,100,length.out=10001)
for(i in 1:17){
  for(j in 1){
    for(k in c(1983:2016)){
      hy=hist(my_data[[j]][i,k-1982,])
      pdf=CreateDensity(histogram=hy,optns = list(outputGrid=x0))
      cum=cumtrapz(x=pdf$x,y=pdf$y)
      print(i)
      print(k)
      assign(paste0("qf_",i,"_",j,"_",k),approxfun(x=cum,y=pdf$x))
    }
  }
}
for(i in 18:33){
  for(j in 1){
    for(k in c(1983:2016)){
      hy=hist(my_data[[j]][i,k-1982,])
      pdf=CreateDensity(histogram=hy,optns = list(outputGrid=x0))
      cum=cumtrapz(x=pdf$x,y=pdf$y)
      print(i)
      print(k)
      assign(paste0("qf_",i,"_",j,"_",k),approxfun(x=cum,y=pdf$x))
    }
  }
}
### female
for(i in 1:17){
  for(j in 2){
    for(k in c(1983:2016)){
      hy=hist(my_data[[j]][i,k-1982,])
      pdf=CreateDensity(histogram=hy,optns = list(outputGrid=x0))
      cum=cumtrapz(x=pdf$x,y=pdf$y)
      print(i)
      print(k)
      assign(paste0("qf_",i,"_",j,"_",k),approxfun(x=cum,y=pdf$x))
    }
  }
}

for(i in 18:33){
  for(j in 2){
    for(k in c(1983:2016)){
      hy=hist(my_data[[j]][i,k-1982,])
      pdf=CreateDensity(histogram=hy,optns = list(outputGrid=x0))
      cum=cumtrapz(x=pdf$x,y=pdf$y)
      print(i)
      print(k)
      assign(paste0("qf_",i,"_",j,"_",k),approxfun(x=cum,y=pdf$x))
    }
  }
}
### For Germany(sum Eest,West)
for(j in 1:2){
  for(k in c(1983:2016)){
    Ger=c(my_data[[j]][34,k-1982,],my_data[[j]][35,k-1982,])
    hy=hist(Ger)
    pdf=CreateDensity(histogram=hy,optns = list(outputGrid=x0))
    cum=cumtrapz(x=pdf$x,y=pdf$y)
    assign(paste0("qf_34","_",j,"_",k),approxfun(x=cum,y=pdf$x))
  }
}
help("CreateDensity")

# Quantile function over i
Q_over_i=function(sex,time,q){
  if(q==1){return(100)}
  quan=0
  for (i in 1:34){
    quan=quan+get(paste0("qf_",i,"_",sex,"_",time))(q)
  }
  return(quan/34)
}
Q_over_i(1,1985,0.3)
# F over i
s0=seq(0,1,length.out=5001)
for(j in 1:2){
  for(k in c(1983:2016)){
    Y=c()
    for(i in 1:length(s0)){
      Y[i]=Q_over_i(j,k,s0[i])
    }
    assign(paste0("F_over_i","_",j,"_",k),approxfun(x=Y,y=s0))
  }
}
F_over_i_1_1985(64.50604)
#############################################

##############################################




# Quantile function over selected i,sex

Q_over_ia=function(time,q){
  quan=Q_over_i(1,time,q)+Q_over_i(2,time,q)
  return(quan/2)
}
Q_over_ia(1985,0.1)

# F over i,j
s0=seq(0,1,length.out=5001)
for(k in c(1983:2016)){
    Y=c()
    for(i in 1:length(s0)){
      Y[i]=Q_over_ia(k,s0[i])
    }
    assign(paste0("F_over_ij_",k),approxfun(x=Y,y=s0))
  }


F_over_ij_1985(100)

# Overall Quantile function(for selected i and common time domain) using Riemann sum
Q_overall=function(q){
  quan=0
  for (j in c(1983:2016)){
    quan=quan+Q_over_ia(j,q)
  }
  return(quan/34)
}
Q_overall(0.23)

# Overall F
so=seq(0,1,length.out=5001)
Yo=c()
for(i in 1:length(so)){ 
    Yo[i]=Q_overall(so[i])
  }
F_overall=approxfun(x=Yo,y=so)
F_overall(68.1289)

#T_t
T_t=function(time,x){
  if(x==100){return(100)}
  a=F_overall(x)
  Tt=Q_over_ia(time,a)
  return(Tt)
}


#T_a
T_a=function(sex,x){
  if(x==100){return(100)}
  Ta=0
  for(j in c(1983:2016)){
    Ta=Ta+Q_over_i(sex,j,get(paste0("F_over_ij_",j))(x))
  }
  return(Ta/34)
}
sum(T_a(1,15),T_a(2,15))/2
T_a(1,78)
#T_a2
T_a2=function(sex,x){
  if(x==100){return(100)}
  Ta=0
  for(j in c(1983:2016)){
    Ta=Ta+Q_over_i(sex,j,F_overall(x))
  }
  return(Ta/34)
}
sum(T_a2(1,15),T_a2(2,15))/2
T_a2(1,78)
#plot T_a
ss=seq(0,100,0.1)
T_male=c()
T_female=c()
for(i in 1:length(ss)){
  T_male[i]=T_a(1,ss[i])
  T_female[i]=T_a(2,ss[i])
}
par(mfrow=c(1,1))

plot(ss,T_male,type="l",col="blue",lty=2,lwd=2,xlab='h/year',ylab='h/year')
lines(ss,ss,lwd=2)
lines(ss,T_female,type="l",col="red",lty=3,lwd=2)
legend('topleft', 
       c("male", "female"), 
       col = c("blue","red"),
       lty = c(2, 3))

Plot_Tt=function(s){
  Tt=array(0,c(34,length(s)))
  for (i in 1983:2016){
    for (j in 1:length(s)){
      Tt[i-1982,j]=T_t(i,s[j]) 
    }
  }
  plot(s,Tt[1,],type='l',col=1,lty=1,lwd=1.5,xlab='domain',ylab='range')
  for(i in 2:34){
    points(s,Tt[i,],type='l',col=i,lty=i,lwd=1.5)
  }
  legend('topleft',c(1983:2016),col=1:34,lty=1:34,lwd=3,bty='n')
}

s1=seq(0,100,1)
Tt=array(0,c(34,length(s1)))
for (i in 1983:2016){
  for (j in 1:length(s1)){
    Tt[i-1982,j]=T_t(i,s1[j]) 
  }
}

sum(Tt[,10]^2)/

Tt_data=data.frame(x=s1,T_1983=Tt[1,],T_1984=Tt[2,],T_1985=Tt[3,],T_1986=Tt[4,],T_1987=Tt[5,],
                   T_1988=Tt[6,],T_1989=Tt[7,],T_1990=Tt[8,],T_1991=Tt[9,],T_1992=Tt[10,],T_1993=Tt[11,],
                   T_1994=Tt[12,],T_1995=Tt[13,],T_1996=Tt[14,],T_1997=Tt[15,],T_1998=Tt[16,],T_1999=Tt[17,],
                   T_2000=Tt[18,],T_2001=Tt[19,],T_2002=Tt[20,],T_2003=Tt[21,],T_2004=Tt[22,],T_2005=Tt[23,],
                   T_2006=Tt[24,],T_2007=Tt[25,],T_2008=Tt[26,],T_2009=Tt[27,],T_2010=Tt[28,],T_2011=Tt[29,],
                   T_2012=Tt[30,],T_2013=Tt[31,],T_2014=Tt[32,],T_2015=Tt[33,],T_2016=Tt[34,])
names(Tt_data)=c("X",time)
data_long=melt(Tt_data, id="X")
Tt_data
Ta_data=data.frame(x=ss,T_f=T_female,T_s=ss,T_m=T_male)
names(Ta_data)=c("X","female","id","male")
data_long2=melt(Ta_data, id="X")
length(Tt[6,])

# +scale_colour_gradientn(name='year', colours = c('blue','red'))
# scale_colour_gradient(name='year', low=col[1],high=col[34])
# geom_ribbon(data=data_long,aes(ymin=data_long$value, ymax=data_long$value, fill= "blue", alpha=0.5))
Ttplot=ggplot(data_long,aes(x=X, y=value, color=as.integer(variable), 
       group=variable)) + xlab("age/year") +ylab("age/year")+xlim(c(0,100))+ylim(c(0,100))+
        geom_line() +scale_colour_gradientn(name='year', colours=rev(col), breaks=c(1,9,17,25,34),labels=c("1983","1991","1999","2007","2016"))+
      theme(legend.key.height= unit(2.65, 'cm'),
        legend.key.width= unit(0.7, 'cm'),legend.text = element_text(size=18),
        legend.title = element_text(size=15),axis.text = element_text(size=13),
        axis.title = element_text(size=15))
setwd("/Users/kimkyum/Documents/공부/Yaqing/HMD")
load("HMD_326_3.Rdata")
  

Ttplot
ggsave("Tt.png", dpi=300, dev='png', height=15, width=28, units="cm")


Ta_data=data.frame(x=ss,T_f=T_female,T_s=ss,T_m=T_male)
names(Ta_data)=c("X","female","id","male")
data_long2=melt(Ta_data, id="X")
Taplot=ggplot(data_long2,aes(x=X, y=value,group=variable,colour=variable,linetype=variable)) + xlab("age/year") +ylab("age/year")+xlim(c(0,100))+ylim(c(0,100))+
  geom_line() + 
  theme(legend.key.height= unit(2, 'cm'),
        legend.key.width= unit(0.5, 'cm'),legend.text = element_text(size=11),
        legend.title = element_text(size=15))+
  scale_color_manual(name='sex',values=c("red","black","blue"),breaks=c("female","male"),labels=c("Female","Male"))+
  scale_linetype_manual(values=c("longdash","solid","longdash"),guide=FALSE)
Taplot
ggsave("Ta.png", dpi=300, dev='png', height=8, width=15, units="cm")




#ot explanation
x=seq(-3,6,0.01)
n1=dnorm(x,0,1)
n2=dnorm(x,3,0.5)
dfo=data.frame(x=x,y1=n1,y2=n2)
ggplot(dfo) +
  geom_line(aes(x=x, y=y1,colour="Y1")) +
  geom_line(aes(x=x, y=y2,colour="Y2")) +
  geom_ribbon( aes(x=x, ymin=0, ymax=y1), fill = "#FFFFBF", alpha = .7) +
  geom_ribbon( aes(x=x, ymin=0, ymax=y2), fill = "#F76D5E", alpha = .8) +
  xlab("x") +ylab("f(x)") +
  scale_color_manual(values = c("Y1" = "#FFFFBF", "Y2" = "#F76D5E"),labels=c("Source : N(0,1)",expression("Target: N(3,"~0.5^2~")")))+
  theme(legend.key.height= unit(0.8, 'cm'),legend.position = c(0.185,0.88),
        legend.key.width= unit(1, 'cm'),legend.text = element_text(size=12),
        axis.text = element_text(size=13),
        axis.title = element_text(size=15),legend.title=element_blank())+
annotate("text", x=3, y=0.2, label= expression(mu[2]) ,size=5) +
  annotate("text", x=0.1, y=0.2, label= expression(mu[1]),size=5)
ggsave("OTex.png", dpi=300, dev='png', height=8, width=15, units="cm")

x=seq(-4,4,0.01)
n1=dnorm(x,0,1)
dfo2=data.frame(x=x,y1=n1)
pdf=ggplot(dfo2) +
  geom_line(aes(x=x, y=y1)) +
  theme(legend.key.height= unit(0.8, 'cm'),legend.position = c(0.185,0.88),
        legend.key.width= unit(1, 'cm'),legend.text = element_text(size=12),
        axis.text.y = element_blank(),
        axis.title = element_blank(),legend.title=element_blank())+
  geom_segment(aes(x=0, y=0.2, xend=0.5, yend=0.2), arrow = arrow(length=unit(.2, 'cm')),
               color='red', lwd=0.8)+
  geom_segment(aes(x=0, y=0.2, xend=-0.5, yend=0.2), arrow = arrow(length=unit(.2, 'cm')),
               color='blue', lwd=0.8)+
  annotate("segment", x = 0, xend = 0, y = 0, yend = 0.4,
           colour = "gray50")
    

dfo4=data.frame(age=male[16,1,])
pdf2=ggplot(dfo4,aes(x=age))+
  geom_density(alpha=0.5,aes())+
  theme(legend.key.height= unit(0.8, 'cm'),legend.position = c(0.185,0.88),
        legend.key.width= unit(1, 'cm'),legend.text = element_text(size=12),
        axis.text.y = element_blank(),axis.text = element_text(size=18),
        axis.title = element_blank(),legend.title=element_blank())+
  geom_segment(aes(x=62.5, y=0.01, xend=67.5, yend=0.01), arrow = arrow(length=unit(.2, 'cm')),
               color='red', lwd=0.8)+
  geom_segment(aes(x=62.5, y=0.01, xend=57.5, yend=0.01), arrow = arrow(length=unit(.2, 'cm')),
               color='blue', lwd=0.8)+
  annotate("segment", x = 62.5, xend = 62.5, y = 0, yend = 0.0185,
           colour = "gray50")


sx=seq(0,100,1)
sy1=sx*sx/100
sy2=2*sx-sy1
dfo3=data.frame(x=sx,y1=sy1,y2=sy2)
ot=ggplot(dfo3) +
  geom_line(aes(x=x, y=y1)) +
  geom_line(aes(x=x, y=y2)) + geom_line(aes(x=x, y=x))+
  theme(legend.key.height= unit(0.8, 'cm'),legend.position = c(0.185,0.88),
        legend.key.width= unit(1, 'cm'),legend.text = element_text(size=12),
        axis.text = element_text(size=18),
        axis.title = element_blank(),legend.title=element_blank())+
  geom_segment(aes(x=62.5, y=62.5, xend=62.5, yend=85), arrow = arrow(length=unit(.2, 'cm')),
               color='red', lwd=0.8)+
  geom_segment(aes(x=62.5, y=62.5, xend=62.5, yend=40), arrow = arrow(length=unit(.2, 'cm')),
               color='blue', lwd=0.8)+
  annotate("text", x=63.5, y=94, label= expression("T"[mu[1] %->%mu[2]]) ,size=10)+
  annotate("text", x=47.5, y=13, label= expression("T"[mu[2] %->%mu[1]]) ,size=10)
ot
d1<-arrangeGrob(ot,pdf2,ncol=2)
grid.draw(d1)
ggsave("OT_int.png", dpi=300, dev='png', height=24, width=30, units="cm",d1)

# optimal transport map visualization
t1 = seq(0,1,0.0005)
plot(t1,dnorm(t1,0,1),type="l")
x1 <-seq(-2,2,length.out=5001)
ft <- CreateDensity(y=tt(dnorm(t1,0,1),0),optns = list(outputGrid=x1))
tt=function(x,t){
  return(t*qnorm(pnorm(x,0,1),3,0.5)+(1-t)*x)
}
get_para=function(t){
  t1 = seq(0,1,0.0005)
  mu=tt(qnorm(t1,0,1),t)[1001]
  sigma=tt(qnorm(t1,0,1),t)[1684]-tt(qnorm(t1,0,1),t)[1001]
  return(c(mu,sigma))
}
get_para(0.8)
tt(qnorm(t1,0,1),0)[1684]-
  # 1001번째가 데이터의 50%인 평균, 1684번째가 대략 데이터의 84.16%로 평균 플러스 1 시그마
  # 따라서 시그마는 tt(qnorm,t)[1684]-tt(qnorm,t)[1001]
  
  plot(ft$x,ft$y,type='l',col=2,lty=2,lwd=2,
       xlim=c(-3,3),ylim=c(0,4),xlab='domain',ylab='density')
install.packages("ggridges")
library("ggridges")
x <- c(rnorm(1000000, mean = 0, 1),
       rnorm(1000000, mean = 0.6, sd = 0.9),
       rnorm(1000000, mean = 1.2, 0.8),
       rnorm(1000000, mean = 1.8, 0.7),
       rnorm(1000000, mean = 2.4, 0.6),
       rnorm(1000000, mean = 3, 0.5))
group <- c(rep("0", 1000000), rep("0.2", 1000000), rep("0.4", 1000000), 
           rep("0.6", 1000000), rep("0.8", 1000000), rep("1", 1000000))
df <- data.frame(x, group)



cols <- c("#F76D5E", "#FFFFBF", "#72D8FF", "#78FFBF","#F29FBF","#D25F3F")
ggplot(df, aes(x = x, fill = group)) +
  geom_density(alpha = 0.8, color = NA) + 
  scale_fill_manual(values = cols)

ridge_density_plot <-                                # Draw densities in ridgeline plot
  ggplot(df,aes(x = x , y = group,  fill = group)) +
  ggridges::geom_density_ridges(alpha=0.8,scale = 4) +        # Overlap among histograms
  scale_fill_brewer(name='t',palette = "YlOrRd",labels=c("t=0: N(0,1)","t=0.2","t=0.4","t=0.6","t=0.8","t=1: N(3,0.5)")) +
  xlab("x") +ylab("f(x)") +
  theme(legend.key.height= unit(2.1, 'cm'),
        legend.key.width= unit(0.7, 'cm'),legend.text = element_text(size=16),
        legend.title = element_text(size=15),axis.text = element_text(size=13),
        axis.title = element_text(size=15))
  


ridge_density_plot
ggsave("OT.png", dpi=300, dev='png', height=15, width=28, units="cm")

ridge_density_plot2 <-                                # Draw densities in ridgeline plot
  ggplot(df,aes(x = x , y = group,  fill = group)) +
  ggridges::geom_density_ridges(alpha=0.2,scale = 4) +        # Overlap among histograms
  scale_fill_brewer(name="t",palette = "YlOrRd",labels=c("1","0.8","0.6","0.4","0.2","0"),direction =-1) +
  theme_ridges() 
ridge_density_plot2 
heat_hcl(34)[1:34]

#female/male histogram 
#make long format of data frame
library(gtable)
library(grid)
library(gridExtra)
age_f<-c(female[11,1,],female[11,9,],female[11,17,],female[11,25,],female[11,34,],
         female[16,1,],female[16,9,],female[16,17,],female[16,25,],female[16,34,],
         female[18,1,],female[18,9,],female[18,17,],female[18,25,],female[18,34,],
         female[32,1,],female[32,9,],female[32,17,],female[32,25,],female[32,34,],
         female[33,1,],female[33,9,],female[33,17,],female[33,25,],female[33,34,])
age_m<-c(male[11,1,],male[11,9,],male[11,17,],male[11,25,],male[11,34,],
         male[16,1,],male[16,9,],male[16,17,],male[16,25,],male[16,34,],
         male[18,1,],male[18,9,],male[18,17,],male[18,25,],male[18,34,],
         male[32,1,],male[32,9,],male[32,17,],male[32,25,],male[32,34,],
         male[33,1,],male[33,9,],male[33,17,],male[33,25,],male[33,34,])
country<- c(rep("FRA", 500000), rep("ISR", 500000), rep("JPN", 500000), 
            rep("UK", 500000), rep("USA", 500000))
year<-c(rep(rep(c("1983","1991","1999","2007","2016"),each=100000),5))
df_female=data.frame(age=age_f,country=country,year=year)
df_male=data.frame(age=age_m,country=country,year=year)


data_format1<- ggplot(df_female,aes(x=age))+
              geom_histogram(aes(y=..density..),color="gray50",fill="gray",alpha=0.5)+
              geom_density(color="red",fill="red",alpha=0.4)+facet_grid(country~year,switch='y')+
              theme(axis.title.y = element_blank(),axis.ticks = element_blank(),
                    axis.text.y = element_blank(),axis.title.x.bottom =element_text(size=15,face="bold"),
                    axis.title.x.top =element_text(size=25,face="bold"),
              strip.text.y.left = element_text(angle=360,size=12,face="bold"),
              strip.text.x = element_text(size=12,face="bold"))+xlab("age/year")+
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Female", breaks = NULL, labels = NULL))
data_format1

data_format2<- ggplot(df_male,aes(x=age))+
  geom_histogram(aes(y=..density..),color="gray50",fill="gray",alpha=0.5)+
  geom_density(color="blue",fill="blue",alpha=0.2)+facet_grid(country~year,switch='y')+
  theme(axis.title.y = element_blank(),axis.ticks = element_blank(),
        axis.text.y = element_blank(),axis.title.x.bottom =element_text(size=15,face="bold"),
        axis.title.x.top =element_text(size=25,face="bold"),
        strip.text.y.left = element_text(angle=360,size=12,face="bold"),
        strip.text.x = element_text(size=12,face="bold"))+xlab("age/year")+
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Male", breaks = NULL, labels = NULL))

data_format2
d<-arrangeGrob(data_format1,data_format2,ncol=2)
grid.draw(d)
ggsave("MTD_data.png", dpi=300, dev='png', height=24, width=30, units="cm",d)

#OT interpretation
sx=seq(0,100,1)
sy1=sx*sx/100
sy2=2*sx-sy1
plot(sx,sy1,type="l")
lines(sx)



Ta_data=data.frame(x=ss,T_f=T_female,T_s=ss,T_m=T_male)
names(Ta_data)=c("X","female","id","male")
data_long2=melt(Ta_data, id="X")
Taplot=ggplot(data_long2,aes(x=X, y=value,group=variable,colour=variable,linetype=variable)) + xlab("h/year") +ylab("h/year")+xlim(c(0,100))+ylim(c(0,100))+
  geom_line() + 
  theme(legend.key.height= unit(2, 'cm'),
        legend.key.width= unit(0.5, 'cm'),legend.text = element_text(size=11),
        legend.title = element_text(size=15))+
  scale_color_manual(name='sex',values=c("red","black","blue"),breaks=c("female","male"),labels=c("Female","Male"))+
  scale_linetype_manual(values=c("longdash","solid","longdash"),guide=FALSE)
Taplot


par(mfrow=c(1,2))
hist(female[1,1,])
hist(male[1,1,])
df1=data.frame(
  sex=factor(rep(c("Female","Male"),each=100000)),
  age=c(female[1,1,],male[1,1,])
)
df2=data.frame(
  sex=factor(rep(c("Female","Male"),each=100000)),
  age=c(female[1,2,],male[1,2,])
)
df3=data.frame(
  sex=factor(rep(c("Female","Male"),each=100000)),
  age=c(female[1,34,],male[1,34,])
)
head(df1)
ggplot(df1,aes(x=age,fill=sex,color=sex))+
  geom_histogram(aes(y=..density..),color="black",fill="gray",alpha=0.2)+
  geom_density(alpha=0.5,aes(fill=sex))+
  facet_grid(~sex)+
  scale_fill_brewer(palette="Set1")+
  theme(legend.position = "none")
ggsave("1983_aus.png", dpi=300, dev='png', height=8, width=15, units="cm")

ggplot(df2,aes(x=age,fill=sex,color=sex))+
  geom_histogram(aes(y=..density..),color="black",fill="gray",alpha=0.2)+
  geom_density(alpha=0.5,aes(fill=sex))+
  facet_grid(~sex)+
  scale_fill_brewer(palette="Set1")+
  theme(legend.position = "none")
ggsave("1984_aus.png", dpi=300, dev='png', height=8, width=15, units="cm")

ggplot(df3,aes(x=age,fill=sex,color=sex))+
  geom_histogram(aes(y=..density..),color="black",fill="gray",alpha=0.2)+
  geom_density(alpha=0.5,aes(fill=sex))+
  facet_grid(~sex)+
  scale_fill_brewer(palette="Set1")+
  theme(legend.position = "none")
ggsave("2016_aus.png", dpi=300, dev='png', height=8, width=15, units="cm")
c
plot(s1,Tt[1,],type='l',col=colors[1],lty=1,lwd=1,xlab='domain',ylab='range')
for(i in 2:34){
  points(s1,Tt[i,],type='l',col=colors[i],lty=1,lwd=1)
}
legend('topleft',c(1983:2016),col=1:34,lty=1:34,lwd=3,bty='n')

ggplot(economics, aes(x=s1)) + 
  geom_line(aes(y = Tt[1,]), color = "darkred") + 
  geom_line(aes(y = Tt[15,]), color="steelblue", linetype="twodash") 

Plot_Tt(s1)

plot(f1$x,f1$y,type='l',col=2,lty=2,lwd=2,
     xlim=c(0,1),ylim=c(0,2),xlab='domain',ylab='density')
points(f2$x,f2$y,type='l',col=3,lty=3,lwd=2)
points(f3$x,f3$y,type='l',col=4,lty=4,lwd=2)
points(x0,dbeta(x0,3,2),type='l',lwd=2)
legend('topleft',
       c('true','raw sample','histogram','frequency table (unequal bin)'),
       col=1:4,lty=1:4,lwd=3,bty='n')




s1=seq(0,100,1)
T_2016=c()
T_1983=c()
T_2000=c()
for(i in 1:length(s1)){
  T_1983[i]=T_t(1983,s1[i])
  T_2016[i]=T_t(2016,s1[i])
  T_2000[i]=T_t(2000,s1[i])
}
par(mfrow=c(1,1))

plot(s1,T_1983,type="l",col="blue")
lines(s1,T_2000)
lines(s1,T_2016,type="l",col="red")
legend(x = 0, y = 80, 
       c("male", "female"), 
       col = c("blue","red"),
       pch = c(20, 2))




setwd("/Users/kimkyum/Documents/공부/Yaqing/HMD/")
save.image("HMD_0206.RData")














###
qf=function(i,sex,time,q){
  x0 <-seq(0,100,length.out=2001)
  pdf=CreateDensity(histogram=hist(my_data[[sex]][i,time-1982,]),optns = list(outputGrid=x0))
  cum=cumtrapz(x=pdf$x,y=pdf$y)
  quan=approxfun(x=cum,y=pdf$x)
  return(quan(q))
}

cdf=function(i,sex,time,x){
  x0 <-seq(0,100,length.out=2001)
  pdf=CreateDensity(histogram=hist(my_data[[sex]][i,time-1982,]),optns = list(outputGrid=x0))
  cum=cumtrapz(x=pdf$x,y=pdf$y)
  cf=approxfun(x=pdf$x,y=cum)
  return(cf(x))
}



#Fn over all country(i)
for (i in c(1983:2019)){
  s1=seq(0,0.05,0.0025)
  s2=seq(0.06,0.2,0.02)
  s3=seq(0.22,1,0.02)
  s=c(s1,s2,s3)
  out=c()
  for(j in 1:length(s)){
    out[j]<-Q_over_ia3(country,i,s[j])
  }
  setwd("/Users/kimkyum/Documents/2023 sp/Yaqing/HMD/Fn")
  saveRDS(out,paste0("Fn_",i,".rds"))
}




cdf_23_1_2016(98)
paste0("cdf_",i,"_",j,"_",k)
hist(my_data[[1]][27,1,])
df=CreateDensity(histogram=hy,optns = list(outputGrid=x0))
cdft=cumtrapz(x=df$x,y=df$y)
qtf=approxfun(x=cdft,y=df$x)
ctf=approxfun(x=df$x,y=cdft)

set.seed(100)
n <- 100
x0 <-seq(0,1,length.out=51)
Y <- rbeta(n,3,2)
# input: histogram
histY <- hist(Y)
plot(histY)
f2 <- CreateDensity(histogram=histY,optns = list(outputGrid=x0))
hy=hist(my_data2[[2]][1,1,])
xt <-seq(0,100,length.out=2001)
ft=CreateDensity(histogram=hy,optns = list(outputGrid=xt))
cdft=cumtrapz(x=ft$x,y=ft$y)
plot(ft$x,cdft,type='l')

qt=approx(x=cdft,y=ft$x)
qtf=approxfun(x=cdft,y=ft$x)
ctf=approxfun(x=ft$x,y=cdft)



par(mfrow=c(1,2))







# Quantile function over selected i// 아마도 domain 벗어난 값에 대해 NA 배출하는 코드 추가해야할듯?



Q_over_i3=function(i,sex,time,q){
  quan=0
  for (j in i){
    quan=quan+quantile(my_data2[[sex]][j,time-1932,1:100000],probs=q,type=1)
  }
  return(quan/length(i))
}

# Quantile function over selected i,sex

Q_over_ia3=function(i,time,q){
  quan=Q_over_i3(i,1,time,q)+Q_over_i3(i,2,time,q)
  return(quan/2)
}
# Overall Quantile function(for selected i and common time domain) using Riemann sum
Q_overall3=function(i,domain,q){
  quan=0
  for (j in domain){
    quan=quan+Q_over_ia3(i,j,q)
  }
  return(quan/length(domain))
}
Q_overall3(country,c(1980:2019),0)

#T_t
T_t=function(time,i,x){
  q=predict(loess_test,x)
  if(is.na(q)==TRUE){q=1}
  Tt=Q_over_ia3(i,time,q)
  return(Tt)
}

#T_a
T_a1=function(i,sex,domain,x){
  Ta=0
  for(j in domain){
    q=predict(get(paste0("loess_",j)),x)
    if(is.na(q)==TRUE){q=1}
    q=min(q,1)
    Ta=Ta+Q_over_i3(i,sex,j,q)
  }
  return(Ta/length(domain))
}




#Ploting 
plotT_t=function(time,i,Fn){
  s=seq(0,100,1)
  out=c()
  for(j in 2:100){
    out[j]<-T_t(time,i,Fn,s[j])
  }
  out[1]=0
  out[101]=100
  plot(s,out,type="l")
}

plotT_ta1=function(i,sex,domain){
  s=seq(0,100,1)
  out=c()
  for(j in 2:100){
    out[j]<-T_ta1(i,sex,domain,s[j])
  }
  out[1]=0
  out[101]=100
  plot(s,out,type="l")
  lines(s,100-out)
}



plotQ_i=function(i,sex,time){
  s=seq(0,1,0.01)
  out=c()
  for(j in 1:101){
    out[j]<-Q_over_i3(i,sex,time,s[j])
  }
  plot(s,out)
}
plotQ_i(country,1,1980)

par(mfrow=c(1,2))


# Plot optimal transport map
p1 <- ggplot(data = data.frame(x = c(-5, 5)), aes(x)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 1)) + 
  stat_function(fun = dnorm, n = 101, args = list(mean = 0.6, sd = 0.9)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = 0.5, sd = 0.8)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = 0.75, sd = 0.7)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = 1, sd = 0.6)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = 1, sd = 0.6)) +
  ylab("") + scale_y_continuous(breaks = NULL)
p1


# Smoothing
set.seed(100)
n <- 100
x0 <-seq(0,1,length.out=51)
Y <- rbeta(n,3,2)
f1 <- CreateDensity(y=Y,optns = list(outputGrid=x0))
# input: histogram
histY <- hist(Y)
plot(histY)
f2 <- CreateDensity(histogram=histY,optns = list(outputGrid=x0))

# input: frequency table with unequally spaced (random) bins
binY <- c(0,sort(runif(9)),1)
freqY <- c()
for (i in 1:(length(binY)-1)) {
  freqY[i] <- length(which(Y>binY[i] & Y<=binY[i+1]))
}
f3 <- CreateDensity(freq=freqY, bin=binY,optns = list(outputGrid=x0))

# plot
plot(f1$x,f1$y,type='l',col=2,lty=2,lwd=2,
     xlim=c(0,1),ylim=c(0,2),xlab='domain',ylab='density')
points(f2$x,f2$y,type='l',col=3,lty=3,lwd=2)
points(f3$x,f3$y,type='l',col=4,lty=4,lwd=2)
points(x0,dbeta(x0,3,2),type='l',lwd=2)
legend('topleft',
       c('true','raw sample','histogram','frequency table (unequal bin)'),
       col=1:4,lty=1:4,lwd=3,bty='n')



#Preparing the data set
f.data <- read.delim("lt_female/fltper_1x1/USA.fltper_1x1.txt",skip=1,header = TRUE,sep="")
m.data <- read.delim("lt_male/mltper_1x1/USA.mltper_1x1.txt",skip=1,header = TRUE,sep="")

#female data
female.dxs <- f.data[,c("Year","Age","dx")]
female.dxs[female.dxs$Age=="110+",][2] <- "110"
female.dxs$age <- as.integer(female.dxs$Age)
colnames(female.dxs)[1] <- "year"
female.dxs$sex <- factor("female",levels=c("female","male"))
female.dxs <- female.dxs[,c("year","sex","age","dx")]
str(female.dxs)

rm(f.data)
#male data

male.dxs <- m.data[,c("Year","Age","dx")]
male.dxs[male.dxs$Age=="110+",][2] <- "110"
male.dxs$age <- as.integer(male.dxs$Age)
colnames(male.dxs)[1] <- "year"
male.dxs$sex <- factor("male",levels=c("female","male"))
male.dxs <- male.dxs[,c("year","sex","age","dx")]
str(male.dxs)

rm(m.data)

#combine data
dx <- rbind(female.dxs,male.dxs)
View(dx)
rm(list=c("female.dxs","male.dxs"))
saveRDS(dx,"usa_dx.rds")

#Read data
aus.dx <- readRDS("aus_dx.rds")
aut.dx <- readRDS("aut_dx.rds")
bel.dx <- readRDS("bel_dx.rds")
bgr.dx <- readRDS("bgr_dx.rds")
blr.dx <- readRDS("blr_dx.rds")
can.dx <- readRDS("can_dx.rds")
che.dx <- readRDS("che_dx.rds")
chl.dx <- readRDS("chl_dx.rds")
cze.dx <- readRDS("cze_dx.rds")
deutnp.dx <- readRDS("deutnp_dx.rds")
dnk.dx <- readRDS("dnk_dx.rds")
esp.dx <- readRDS("esp_dx.rds")
est.dx <- readRDS("est_dx.rds")
fin.dx <- readRDS("fin_dx.rds")
fratnp.dx <- readRDS("fratnp_dx.rds")
gbr_np.dx <- readRDS("gbr_np_dx.rds")
grc.dx <- readRDS("grc_dx.rds")
hkg.dx <- readRDS("hkg_dx.rds")
hrv.dx <- readRDS("hrv_dx.rds")
hun.dx <- readRDS("hun_dx.rds")
irl.dx <- readRDS("irl_dx.rds")
isl.dx <- readRDS("isl_dx.rds")
isr.dx <- readRDS("isr_dx.rds")
ita.dx <- readRDS("ita_dx.rds")
jpn.dx <- readRDS("jpn_dx.rds")
kor.dx <- readRDS("kor_dx.rds")
ltu.dx <- readRDS("ltu_dx.rds")
lux.dx <- readRDS("lux_dx.rds")
lva.dx <- readRDS("lva_dx.rds")
nld.dx <- readRDS("nld_dx.rds")
nor.dx <- readRDS("nor_dx.rds")
nzl_np.dx <- readRDS("nzl_np_dx.rds")
pol.dx <- readRDS("pol_dx.rds")
prt.dx <- readRDS("prt_dx.rds")
rus.dx <- readRDS("rus_dx.rds")
svk.dx <- readRDS("svk_dx.rds")
svn.dx <- readRDS("svn_dx.rds")
swe.dx <- readRDS("swe_dx.rds")
twn.dx <- readRDS("twn_dx.rds")
ukr.dx <- readRDS("ukr_dx.rds")
usa.dx <- readRDS("usa_dx.rds")



# plot
plot(f1$x,f1$y,type='l',col=2,lty=2,lwd=2,
     xlim=c(0,1),ylim=c(0,2),xlab='domain',ylab='density')
points(f2$x,f2$y,type='l',col=3,lty=3,lwd=2)
points(f3$x,f3$y,type='l',col=4,lty=4,lwd=2)
points(x0,dbeta(x0,3,2),type='l',lwd=2)
legend('topleft',
       c('true','raw sample','histogram','frequency table (unequal bin)'),
       col=1:4,lty=1:4,lwd=3,bty='n')

hy=hist(my_data[[2]][4,3,])
xt <-seq(0,100,length.out=50001)
ft=CreateDensity(histogram=hy,optns = list(outputGrid=xt))
cdft=cumtrapz(x=ft$x,y=ft$y)

plot(ft$x,cdft,type='l')

qq=approx(x=cdft,y=ft$x,n=50001)
qq$x[5000]
qq$y[5000]
qtf=approxfun(x=cdft,y=ft$x,n=50001)
ctf=approxfun(x=ft$x,y=cdft,n=50001)
ctf(56.4898)
qtf(0.09997945)
0.3-ctf(qtf(0.3))
qt$y
qt$x
plot(qt$x,qt$y,type='l')
plot(ft$x,ft$y,type='l',col=2,lty=2,lwd=2,
     xlim=c(0,100),ylim=c(0,0.04),xlab='domain',ylab='density')
