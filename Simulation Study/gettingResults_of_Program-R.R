#locat <- c(seq(12.5, 42.5, length=4))
# coor1 <- c(22.5, 22.5, 27.5, 27.5, 32.5, 32.5, 37.5, 37.5,
#            42.5, 42.5, 47.5, 47.5, 52.5, 52.5, 57.5, 57.5)
# coor2 <- c(22.5, 57.5, 27.5, 52.5, 32.5, 47.5, 37.5, 42.5,
#            37.5, 42.5, 32.5, 47.5, 27.5, 52.5, 22.5, 57.5)
# coordinate_new <- cbind(coor1,coor2)

coor <- c(5,15,25,35,45,55,65,75,85,95)
coordinate_new <- expand.grid(X=coor, Y=coor)
dim(coordinate_new)

beta1_Bias<-beta0_Bias<-sig2x_Bias<-gamma_Bias<-alpha_Bias<-nu_Bias<-mux_Bias<-theta_Bias<-c(0)
beta1_Bar<-beta0_Bar<-sig2x_Bar<-gamma_Bar<-alpha_Bar<-nu_Bar<-mux_Bar<-theta_Bar<-c(0)
var_beta1_E<-var_beta0_E<-var_sig2x_E<-var_gamma_E<-var_alpha_E<-var_nu_E<-var_mux_E<-var_theta_E<-c(0)
beta1_mse<-beta0_mse<-sig2x_mse<-gamma_mse<-alpha_mse<-nu_mse<-mux_mse<-theta_mse<-matrix(c(0),25,40)
Y_16 <- Y_pre <- Mean_Sq_Y_pred <- matrix(c(0),1000,16)
my_task<-read.table("/home/tadayon/Simulation/mytask")
for(f in my_task[,1]){
  path<-paste("~/Simulation/Code/",f,"/",sep="")
  load(file=paste(path,"output.RData",sep=""))
  beta1_Bias[f] <- sum(beta1_est-2)
  beta0_Bias[f] <- sum(beta0_est-0.1)
  sig2x_Bias[f] <- sum(sig2x_est-1)
  gamma_Bias[f] <- sum(gamma_est-1.2)
  alpha_Bias[f] <- sum(alpha_est+2.1)
  nu_Bias[f] <- sum(nu_est-0.6)
  mux_Bias[f] <- sum(mux_est-0)
  theta_Bias[f] <- sum(theta_est-1.6)
  var_beta1_E[f] <- mean(var_beta1_est)
  var_beta0_E[f] <- mean(var_beta0_est)
  var_sig2x_E[f] <- mean(var_sig2x_est)
  var_gamma_E[f] <- mean(var_gamma_est)
  var_alpha_E[f] <- mean(var_alpha_est)
  var_nu_E[f] <- mean(var_nu_est)
  var_mux_E[f] <- mean(var_mux_est)
  var_theta_E[f] <- mean(var_theta_est)
  beta1_mse[f,] <- beta1_est
  beta0_mse[f,] <- beta0_est
  sig2x_mse[f,] <- sig2x_est
  gamma_mse[f,] <- gamma_est
  alpha_mse[f,] <- alpha_est
  nu_mse[f,] <- nu_est
  mux_mse[f,] <- mux_est
  theta_mse[f,] <- theta_est
  Y_16[(((f-1)*40)+1):(f*40),] <- y16[1:40,]
  Y_pre[(((f-1)*40)+1):(f*40),] <- y_predict16[1:40,]
  Mean_Sq_Y_pred[(((f-1)*40)+1):(f*40),] <- Mean_Sq_Ypred[1:40,]}
beta1_Bar <- mean(beta1_mse)
beta0_Bar <- mean(beta0_mse)
sig2x_Bar <- mean(sig2x_mse)
gamma_Bar <- mean(gamma_mse)
alpha_Bar <- mean(alpha_mse)
nu_Bar <- mean(nu_mse)
mux_Bar <- mean(mux_mse)
theta_Bar <- mean(theta_mse)
print("beta1_Bias =");formatC(sum(beta1_Bias)/1000,digits=9, format="f")
print("beta0_Bias =");formatC(sum(beta0_Bias)/1000,digits=9, format="f")
print("sig2x_Bias =");formatC(sum(sig2x_Bias)/1000,digits=9, format="f")
print("gamma_Bias =");formatC(sum(gamma_Bias)/1000,digits=9, format="f")
print("alpha_Bias =");formatC(sum(alpha_Bias)/1000,digits=9, format="f")
print("nu_Bias =");formatC(sum(nu_Bias)/1000,digits=9, format="f")
print("mux_Bias =");formatC(sum(mux_Bias)/1000,digits=9, format="f")
print("theta_Bias =");formatC(sum(theta_Bias)/1000,digits=9, format="f")
print("var_beta1_E =");formatC(mean(var_beta1_E),digits=9, format="f")
print("var_beta0_E =");formatC(mean(var_beta0_E),digits=9, format="f")
print("var_sig2x_E =");formatC(mean(var_sig2x_E),digits=9, format="f")
print("var_gamma_E =");formatC(mean(var_gamma_E),digits=9, format="f")
print("var_alpha_E =");formatC(mean(var_alpha_E),digits=9, format="f")
print("var_nu_E =");formatC(mean(var_nu_E),digits=9, format="f")
print("var_mux_E =");formatC(mean(var_mux_E),digits=9, format="f")
print("var_theta_E =");formatC(mean(var_theta_E),digits=9, format="f")
print("var_beta1_Empri =");formatC(mean((beta1_mse-beta1_Bar)^2),digits=9, format="f")
print("var_beta0_Empri =");formatC(mean((beta1_mse-beta1_Bar)^2),digits=9, format="f")
print("var_sig2x_Empri =");formatC(mean((sig2x_mse-sig2x_Bar)^2),digits=9, format="f")
print("var_gamma_Empri =");formatC(mean((gamma_mse-gamma_Bar)^2),digits=9, format="f")
print("var_alpha_Empri =");formatC(mean((alpha_mse-alpha_Bar)^2),digits=9, format="f")
print("var_nu_Empri =");formatC(mean((nu_mse-nu_Bar)^2),digits=9, format="f")
print("var_mux_Empri =");formatC(mean((mux_mse-mux_Bar)^2),digits=9, format="f")
print("var_theta_Empri =");formatC(mean((theta_mse-theta_Bar)^2),digits=9, format="f")
for(i in 1:16){print(c(coordinate_new[i,],mean(Mean_Sq_Y_pred[,i])))}




