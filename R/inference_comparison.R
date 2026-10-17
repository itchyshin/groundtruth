#' Compare two methods on paired study attempts
#'
#' @param study A study returned by [gt_runstudy()].
#' @param baseline,comparison Distinct adapter IDs to compare explicitly.
#' @details Differences are comparison minus baseline, separately for each
#' scenario, fixed-effect target and interval level. Every replication must
#' occur exactly once for each method. Conditional coverage uses usable finite
#' intervals. Finite covered-interval yield uses all scheduled attempts; a zero
#' contribution means no usable finite covered interval, not that an unbounded
#' confidence set excludes truth. Conditional-coverage MCSE uses paired ratio
#' influence values. Width differences use jointly usable finite intervals.
#' MCSEs describe asymptotic Monte Carlo uncertainty, not calibration. Matching
#' replication keys require a study that gave methods the same data; combining
#' unrelated studies by matching seed numbers is unsupported.
#' @return A data frame of paired counts, coverage and finite-yield differences,
#' their Monte Carlo standard errors, and joint-finite width differences.
#' @examples
#' s <- gt_runstudy(gt_scenario('paired'),
#'   adapters = list(gt_adapter('a'), gt_adapter('b')), reps = 2L)
#' gt_compare_inference(s, 'a', 'b')
#' @export
gt_compare_inference <- function(study, baseline, comparison) {
  if (length(baseline) != 1L || length(comparison) != 1L ||
      !is.character(baseline) || !is.character(comparison) ||
      is.na(baseline) || is.na(comparison) || identical(baseline, comparison))
    stop('Select two distinct adapter IDs.', call. = FALSE)
  details <- gt_inference_details(study)
  rows <- study$targets
  required <- c('scenario','rep','adapter','target','level','truth','estimate',
                'lower','upper','usable_interval','covered')
  if (!is.data.frame(rows) || !all(required %in% names(rows)))
    stop('Study target rows are missing required columns.', call. = FALSE)
  rows <- rows[rows$adapter %in% c(baseline, comparison) &
                 rows$target %in% c('alpha','beta') & !is.na(rows$level), , drop=FALSE]
  if (!nrow(rows) || !all(c(baseline, comparison) %in% rows$adapter))
    stop('Both adapters need fixed-effect interval rows.', call. = FALSE)
  keycols <- c('scenario','target','level')
  keys <- unique(rows[keycols]); keys <- keys[do.call(order, keys),,drop=FALSE]
  mcse <- function(v) if(length(v)>1L) stats::sd(v)/sqrt(length(v)) else NA_real_
  out <- lapply(seq_len(nrow(keys)), function(i) {
    z <- rows[rows$scenario==keys$scenario[i] & rows$target==keys$target[i] &
                rows$level==keys$level[i],,drop=FALSE]
    a <- z[z$adapter==baseline,,drop=FALSE]; b <- z[z$adapter==comparison,,drop=FALSE]
    if (anyNA(a$rep) || anyNA(b$rep) || anyDuplicated(a$rep) || anyDuplicated(b$rep) ||
        !length(a$rep) || !setequal(a$rep,b$rep))
      stop('Pairing requires unique matching replication keys.',call.=FALSE)
    a <- a[order(a$rep),,drop=FALSE]; b <- b[match(a$rep,b$rep),,drop=FALSE]
    schedule_verified <- FALSE
    if (!is.null(study$reps)) {
      if (length(study$reps)!=1L || !is.finite(study$reps) || study$reps<1 || study$reps!=floor(study$reps) ||
          !setequal(a$rep,seq_len(study$reps)))
        stop('Scheduled replications are missing from target rows.',call.=FALSE)
      schedule_verified <- TRUE
    }
    if (!is.null(study$ledger)) {
      for (id in c(baseline,comparison)) {
        l <- study$ledger[study$ledger$scenario==keys$scenario[i] & study$ledger$adapter==id,,drop=FALSE]
        if (anyNA(l$rep) || anyDuplicated(l$rep) || !setequal(a$rep,l$rep))
          stop('Attempt ledger and paired target schedule disagree.',call.=FALSE)
      }
      schedule_verified <- TRUE
    }
    if(any(!is.finite(a$truth)) || !identical(a$truth,b$truth))
      stop('Paired targets must have identical finite truth.',call.=FALSE)
    validate <- function(d) {
      u <- d$usable_interval %in% TRUE
      finite <- is.finite(d$lower) & is.finite(d$upper) & d$lower<=d$upper
      if (any(u & (!finite | !is.finite(d$estimate))) ||
          any(u & (is.na(d$covered) | d$covered != (d$lower<=d$truth & d$truth<=d$upper))))
        stop('Usable intervals or coverage flags are inconsistent.',call.=FALSE)
      u
    }
    ua<-validate(a); ub<-validate(b)
    ca<-as.numeric(ua & a$covered %in% TRUE); cb<-as.numeric(ub & b$covered %in% TRUE)
    qa<-if(any(ua)) sum(ca)/sum(ua) else NA_real_
    qb<-if(any(ub)) sum(cb)/sum(ub) else NA_real_
    ratio_mcse<-if(any(ua)&&any(ub)) mcse((cb-qb*ub)/mean(ub)-(ca-qa*ua)/mean(ua)) else NA_real_
    joint<-ua&ub
    widths<-(b$upper-b$lower)[joint]-(a$upper-a$lower)[joint]
    result <- data.frame(scenario=keys$scenario[i],target=keys$target[i],level=keys$level[i],
      baseline=baseline,comparison=comparison,attempted=nrow(a),schedule_verified=schedule_verified,
      baseline_usable=sum(ua),comparison_usable=sum(ub),joint_usable=sum(joint),
      baseline_finite_hits=sum(ca),comparison_finite_hits=sum(cb),
      baseline_coverage=qa,comparison_coverage=qb,coverage_difference=qb-qa,
      coverage_difference_mcse=ratio_mcse,
      baseline_finite_yield=mean(ca),comparison_finite_yield=mean(cb),
      finite_yield_difference=mean(cb-ca),finite_yield_difference_mcse=mcse(cb-ca),
      joint_width_difference=if(length(widths))mean(widths) else NA_real_,
      joint_width_difference_mcse=mcse(widths),stringsAsFactors=FALSE)
    states <- function(id,u) {
      d <- details[details$scenario==keys$scenario[i] & details$adapter==id &
                     details$target==keys$target[i] & details$level==keys$level[i],,drop=FALSE]
      state <- rep('unknown',nrow(a))
      k <- match(a$rep,d$rep); known<-!is.na(k); state[known]<-d$state[k[known]]
      if(any((state=='finite' & !u) | (known & u & state!='finite')))
        stop('Companion interval states disagree with finite usable rows.',call.=FALSE)
      state
    }
    sa<-states(baseline,ua);sb<-states(comparison,ub)
    for(state in c('finite','unbounded','incomplete','error','timeout','invalid','unknown')) {
      result[[paste0('baseline_state_',state)]]<-sum(sa==state)
      result[[paste0('comparison_state_',state)]]<-sum(sb==state)
    }
    result
  })
  result<-do.call(rbind,out); rownames(result)<-NULL; result
}

#' Inspect typed inference outcomes without changing target schemas
#'
#' @param study A study returned by [gt_runstudy()].
#' @return A keyed data frame of inference details. Legacy studies without the
#'   companion return an empty data frame; their interval states are unknown.
#' @examples
#' gt_inference_details(gt_runstudy(gt_scenario('ordinary'), reps = 1L))
#' @export
gt_inference_details <- function(study) {
  d<-study$inference_details
  if(is.null(d)) return(data.frame(scenario=character(),rep=integer(),adapter=character(),
    target=character(),level=numeric(),state=character(),criterion=character(),
    interval_method=character(),lower=numeric(),upper=numeric(),root_lr_residual=numeric(),
    endpoint_distance=numeric(),elapsed_seconds=numeric(),message=character(),stringsAsFactors=FALSE))
  keys<-c('scenario','rep','adapter','target','level')
  if(!is.data.frame(d)||!all(c(keys,'state')%in%names(d))||anyNA(d[keys])||
     anyDuplicated(d[keys])||anyNA(d$state)||
     !all(d$state%in%c('finite','unbounded','incomplete','error','timeout','invalid')))
    stop('Invalid or duplicate inference-detail keys/states.',call.=FALSE)
  d
}
