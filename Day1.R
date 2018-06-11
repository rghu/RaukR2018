##################
#Name: Raghu Chandramohan
#Title: RaukR Lab (Day 1)
##################


#############
#Q1.2)

myIterAtoR.max <- 5
second_iterator.max<-7
col.NUM= 10
row.cnt =10
fwzy45 <- matrix(rep(1, col.NUM*row.cnt),nrow=row.cnt)
for(haystack in (2-1):col.NUM){
  for(needle in 1:row.cnt) {
    if(haystack>=myIterAtoR.max){
      fwzy45[haystack, needle]<-NA}
  }}

#A1.2)


i_max <- 5
num_col= 10
num_row =10
mat <- matrix(rep(1, num_row*num_col),nrow=num_row)
for(i in 1:num_col){
  for(j in 1:num_row) {
    if(i>=i_max){
      mat[i, j]<-NA}
  }}

#############
#Q1.3)
simulate_genotype <- function( q, N=100 ) {
  if( length(q)==1 ){
    p <- (1 - q)
    f_gt <- c(p^2, 2*p*q, q^2) # AA, AB, BB
  }else{
    f_gt<-q
  }
  tmp <- sample( c('AA','AB','BB'), size =N, prob=f_gt, replace=T )
  return(tmp)
}

#A1.3)
simulate_genotype <- function( q, N=100 ) {
  if( length(q)==1 ){
    p <- (1 - q)
    f_gt <- c(p^2, 2*p*q, q^2) # AA, AB, BB
  }else{
    f_gt<-q
  }
  tmp <- sample( c('AA','AB','BB'), 
                 size =N, 
                 prob=f_gt, 
                 replace=T )
  return(tmp)
}

#############
#Q1.4) Assign a vector of three last months (abbreviated in English) in a year to a hidden variable my_months.

#A1.4)
.my_months = c("Oct", "Nov", "Dec")
.my_months = month.abb[10:12]

#############
#Q1.5) Modify the function below so that it works with pipes:
my_filter <- function(threshold = 1, data, scalar = 5) {
  data[data >= threshold] <- NA 
  data <- data * scalar
  return(data)
}

#A1.5)
my_filter <- function(data, threshold = 1, scalar = 5) {
  data[data >= threshold] <- NA 
  data <- data * scalar
  return(data)
}

#############
#Q1.6) Is the code below correct? Can it be improved?
simulate_phenotype <- function(pop_params, gp_map, gtype) {
  pop_mean <- pop_params[1]
  pop_var <- pop_params[2]
  pheno <- rnorm(n = N, mean = pop_mean, sd = sqrt(pop_var))
  effect <- rep(0, times = length(N))
  for (gt_iter in c('AA', 'AB', 'BB')) {
    effect[gtype == gt_iter] <- rnorm(n = sum(gtype == gt_iter), 
                                      mean = gp_map[gt_iter, 'mean_eff'], 
                                      sd = sqrt(gp_map[gt_iter, 'var_eff']))
  }
  dat <- data.frame(gt = gtype, raw_pheno = pheno, effect = effect, pheno = pheno + effect)
  return(dat)
}

#A1.6)
simulate_phenotype <- function(pop_params, gp_map, gtype) {
  pop_mean <- pop_params[1]
  pop_var <- pop_params[2]
  pheno <- rnorm(n = N, mean = pop_mean, sd = sqrt(pop_var))
  effect <- rep(0, times = length(N))
  for (gt_iter in c('AA', 'AB', 'BB')) {
    effect[gtype == gt_iter] <- rnorm(n = sum(gtype == gt_iter), 
                                      mean = gp_map[gt_iter, 'mean_eff'], 
                                      sd = sqrt(gp_map[gt_iter, 'var_eff']))
  }
  dat <- data.frame(gt = gtype, raw_pheno = pheno, effect = effect, pheno = pheno + effect)
  return(dat)
}

#############
#Q2.1 Task: Computing Variance.
#A2.1 
calculate_mean = function(data){
  dat = sum(data) / length(data)
  return(dat)
}

calculate_sum_std_dev = function(data){
  dat = sum((data - calculate_mean(data))^2)
  return(dat)
}

calculate_sd = function(data){
  dat = sqrt(calculate_sum_std_dev(data)/(length(data) - 1))
  return(dat)
}

calculate_var = function(data){
  dat = (calculate_var(data))^2
  return(dat)
}

dat = calculate_sd(c(1,2,3,4,5))


################
#2.2 Task: Writing a Wrapper Function.
randomSampleInt <- function(x, verbose, length, seed = 42) {
  if (verbose) {
    print(paste0('Generating random sample of ', length, ' integers using seed ', seed))
  }
  set.seed(seed)
  sampleInt <- sample(x = x, size = length, replace = TRUE)
  return(sampleInt)
} 

randomSampleLetter <- function(N, silent=T, lett) {
  if (!silent) {
    print(paste0('Generating random sample of ', N, ' letters.'))
  }
  sample <- sample(x = lett, size = N, replace = TRUE)
  return(sample)
}

randomSampleLetter_wrapper = function(x, verbose, length, seed = 42){
  set.seed(seed)
  randomSampleLetter(N = length, silent = !verbose, lett = x)
}

######################
#2.3 Task: Customizing

plot_wrapper = function(data, color = "red", shape = 4, ...){
  graphics::plot(x = data, pch = shape, col = color, ...)
}

######################
#2.4 Task: Adding Arguments to a Function.

red_plot <- function(x, y) { 
  plot(x, y, las=1, cex.axis=.8, ...)
}

formals(red_plot) = (c(formals(red_plot), alist(... = )))
red_plot(1, 1, col='red', pch=19)


############################
#3.1 
input <- sample(1:1000, size = 1000, replace = T)
currmin <- 1000
for (i in input) {
  if (i < currmin) {
    currmin <- i
    print(paste0("The new minimum is: ", currmin))
  }
}

t1 =  proc.time()
by_col_test = matrix(sample(x = 1:42, size = 100000000, replace = T), ncol = 10000, nrow = 10000, byrow = T)
t2 = proc.time()
(t2-t1)

init_time = proc.time()
by_row_test = matrix(sample(x = 1:42, size = 100000000, replace = T), ncol = 10000, nrow = 10000, byrow = F)
t2 = proc.time()
(t2-t1)

################
#4.2 Task: Timing Reliability.
boxplot(unlist(lapply(1:100, function(x) system.time(rnorm(n = 10e6))[3])))
replicate(100, system.time(rnorm(n = 10e6))[3])

##############################
#4.3 Task microbenchmarking
#Q
timing <- double(100)
for (i in 1:100) {
  st <- system.time(rnorm(n = 10e6))
  timing[i] <- st[3]
}
boxplot(timing)

#A
timing <- double(100)
for (i in 1:100) {
  t1 <- get_nanotime()
  rnorm(n = 10e6)
  t2 <- get_nanotime()
  timing[i] <- t2 - t1
}
boxplot(timing)

#A
microtiming_precision()
mean(microtiming_precision())
var(microtiming_precision())

microtiming_precision()
x = microtiming_precision()
