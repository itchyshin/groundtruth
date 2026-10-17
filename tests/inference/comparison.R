# Deterministic paired accounting: no fitting, generation or optional engine.
if (file.exists('R/inference_comparison.R')) source('R/inference_comparison.R')
stopifnot(exists('gt_compare_inference', mode='function'))
make <- function(id, usable, covered) data.frame(scenario='control',rep=1:4,
 adapter=id,target='alpha',level=.95,truth=0,estimate=c(0,0,0,NA_real_),
 lower=ifelse(usable,ifelse(covered,-1,1),NA_real_),upper=ifelse(usable,ifelse(covered,1,2),NA_real_),
 usable_interval=usable,covered=ifelse(usable,covered,NA), interval_expected=TRUE)
a <- make('a',c(TRUE,TRUE,FALSE,FALSE),c(TRUE,FALSE,FALSE,FALSE))
b <- make('b',c(TRUE,FALSE,TRUE,FALSE),c(TRUE,FALSE,TRUE,FALSE))
s <- list(targets=rbind(a,b))
z <- gt_compare_inference(s,baseline='a',comparison='b')
stopifnot(nrow(z)==1,z$attempted==4,z$baseline_usable==2,z$comparison_usable==2,
 z$baseline_coverage==.5,z$comparison_coverage==1,z$coverage_difference==.5,
 z$finite_yield_difference==.25,
 abs(z$finite_yield_difference_mcse-stats::sd(c(0,0,1,0))/2)<1e-12,
 abs(z$coverage_difference_mcse-stats::sd(c(-1,1,0,0))/2)<1e-12)
reject <- function(x) inherits(try(gt_compare_inference(x,baseline='a',comparison='b'),silent=TRUE),'try-error')
stopifnot(reject(list(targets=rbind(a,b,b[1,]))),reject(list(targets=rbind(a,b[-1,]))))
swapped<-gt_compare_inference(s,baseline='b',comparison='a')
stopifnot(swapped$coverage_difference==-.5)
permuted<-gt_compare_inference(list(targets=s$targets[c(8,1,6,3,5,2,4,7),]),baseline='a',comparison='b')
stopifnot(identical(z,permuted))
zero<-a;zero$adapter<-'c';zero$usable_interval<-FALSE;zero$covered<-NA;zero$lower<-zero$upper<-NA_real_
zz<-gt_compare_inference(list(targets=rbind(a,zero)),baseline='a',comparison='c')
stopifnot(is.na(zz$comparison_coverage),is.na(zz$coverage_difference_mcse))
bad<-s;bad$targets$truth[5]<-2
stopifnot(reject(bad))
bad<-s;bad$targets$covered[1]<-FALSE
stopifnot(reject(bad))
bad<-s;bad$targets$lower[1]<-Inf
stopifnot(reject(bad))
# Four-attempt denominator check remains per method, not joint-finite only.
stopifnot(z$joint_usable==1,z$baseline_finite_yield==.25,z$comparison_finite_yield==.5)
# Removing the same scheduled replication from both methods must not shrink N.
lost<-list(targets=s$targets[s$targets$rep!=4,],reps=4L)
stopifnot(reject(lost))
stopifnot(exists('gt_inference_details',mode='function'))

stopifnot(nrow(gt_inference_details(s))==0,z$baseline_finite_hits==1,z$comparison_finite_hits==2)
stopifnot(z$baseline_state_unknown==4,!z$schedule_verified)
details<-s$targets[c('scenario','rep','adapter','target','level')]
details$state<-ifelse(s$targets$usable_interval,'finite',ifelse(s$targets$rep==4,'error','incomplete'))
typed<-s;typed$inference_details<-details
zt<-gt_compare_inference(typed,'a','b')
stopifnot(zt$baseline_state_finite==2,zt$baseline_state_error==1,zt$baseline_state_incomplete==1,zt$baseline_state_unknown==0)
bad<-typed;bad$inference_details<-rbind(details,details[1,]);stopifnot(reject(bad))
bad<-typed;bad$inference_details$state[1]<-'timeout';stopifnot(reject(bad))
cat('INFERENCE_ACCOUNTING_OK\n')
