.ec_time_name_score <- function(name) {
  n <- tolower(trimws(name))
  if (grepl("^(date|time|year|annee|année|period|periode|période|quarter|trimestre|month|mois)$", n)) return(4)
  if (grepl("(^|_)(date|time|year|annee|period|quarter|month)(_|$)", n)) return(3)
  if (grepl("date|year|annee|année|period|periode|période|quarter|trimestre|month|mois", n)) return(2)
  0
}

.ec_parse_time_vector <- function(x, name = "") {
  n <- length(x); nonmiss <- !is.na(x)
  empty <- function() list(valid=FALSE, kind="unknown", parsed=rep(NA_real_,n), display=rep(NA_character_,n), dates=as.Date(rep(NA_character_,n)), valid_share=0, unique_n=0L, score=0)
  if (!n || !any(nonmiss)) return(empty())
  score <- .ec_time_name_score(name); parsed <- rep(NA_real_,n); display <- rep(NA_character_,n); kind <- "unknown"; dates <- as.Date(rep(NA_character_,n))

  if (inherits(x,"Date")) {
    dates <- as.Date(x); parsed <- as.numeric(dates); display <- as.character(dates); kind <- "date"; score <- score + 8
  } else if (inherits(x,c("POSIXct","POSIXlt"))) {
    xx <- as.POSIXct(x); dates <- as.Date(xx); parsed <- as.numeric(xx); display <- format(xx,"%Y-%m-%d %H:%M:%S"); kind <- "datetime"; score <- score + 8
  } else if (is.numeric(x) || is.integer(x)) {
    z <- suppressWarnings(as.numeric(x)); zn <- z[is.finite(z)]
    if (length(zn)) {
      whole <- all(abs(zn-round(zn)) < 1e-8); yrmax <- as.integer(format(Sys.Date(),"%Y"))+25L
      if (whole && min(zn)>=1000 && max(zn)<=yrmax) { parsed <- z; display <- ifelse(is.na(z),NA_character_,as.character(as.integer(z))); kind <- "year"; score <- score+5 }
    }
  } else if (is.character(x) || is.factor(x)) {
    z <- trimws(as.character(x)); z[is.na(x)|z==""] <- NA_character_; zn <- z[!is.na(z)]
    if (length(zn) && all(grepl("^[12][0-9]{3}$",zn))) {
      yy <- suppressWarnings(as.integer(z)); yrmax <- as.integer(format(Sys.Date(),"%Y"))+25L
      if (all(yy[!is.na(yy)]>=1000 & yy[!is.na(yy)]<=yrmax)) { parsed<-as.numeric(yy); display<-z; kind<-"year"; score<-score+5 }
    }
    if (length(zn) && identical(kind,"unknown") && all(grepl("^[12][0-9]{3}[-/](0[1-9]|1[0-2])$",zn))) {
      zz <- gsub("/","-",z); yy <- suppressWarnings(as.integer(substr(zz,1,4))); mm <- suppressWarnings(as.integer(substr(zz,6,7)))
      parsed <- yy*12 + mm; display <- zz; kind <- "year_month"; score <- score+6
    }
    if (length(zn) && identical(kind,"unknown") && all(grepl("^[12][0-9]{3}[- ]?[Qq][1-4]$",zn))) {
      yy <- suppressWarnings(as.integer(substr(z,1,4))); qq <- suppressWarnings(as.integer(sub(".*[Qq]","",z)))
      parsed <- yy*4+qq; display <- ifelse(is.na(z),NA_character_,paste0(yy,"-Q",qq)); kind <- "year_quarter"; score <- score+6
    }
    if (length(zn) && identical(kind,"unknown") && all(grepl("^[12][0-9]{3}[-/](0[1-9]|1[0-2])[-/](0[1-9]|[12][0-9]|3[01])$",zn))) {
      iso <- gsub("/","-",z); dd <- suppressWarnings(as.Date(iso,format="%Y-%m-%d"))
      if (!any(!is.na(iso)&is.na(dd))) { dates<-dd; parsed<-as.numeric(dd); display<-as.character(dd); kind<-"date"; score<-score+7 }
    }
  }
  good <- is.finite(parsed); share <- if(sum(nonmiss)) sum(good & nonmiss)/sum(nonmiss) else 0; un <- length(unique(parsed[good]))
  valid <- !identical(kind,"unknown") && share>=.95 && un>=4L
  if(!valid) score <- min(score,2)
  list(valid=valid,kind=kind,parsed=parsed,display=display,dates=dates,valid_share=share,unique_n=un,score=score)
}

.ec_date_grid <- function(dates) {
  d <- sort(unique(as.Date(dates[!is.na(dates)])))
  if(length(d)<2L) return(list(frequency="unknown",regular=NA,gaps=NA_integer_,grid=as.numeric(d)))
  y <- as.integer(format(d,"%Y")); m <- as.integer(format(d,"%m")); day <- as.integer(format(d,"%d")); ym <- y*12L + m
  dd <- diff(as.numeric(d)); dym <- diff(ym)
  month_end <- format(d + 1, "%d") == "01"
  same_anchor <- length(unique(day)) == 1L || all(month_end)
  # Prefer calendar rules before day-distance rules. This handles February, leap years and month-end indices.
  if(same_anchor && all(dym %% 12L == 0L) && stats::median(dym)==12L) {
    idx <- y; di <- diff(idx); return(list(frequency="annual",regular=all(di==1L),gaps=sum(pmax(di-1L,0L)),grid=idx))
  }
  if(same_anchor && all(dym %% 3L == 0L) && stats::median(dym)==3L) {
    idx <- y*4L + ((m-1L)%/%3L + 1L); di<-diff(idx); return(list(frequency="quarterly",regular=all(di==1L),gaps=sum(pmax(di-1L,0L)),grid=idx))
  }
  if(same_anchor && stats::median(dym)==1L) {
    idx <- ym; di<-diff(idx); return(list(frequency="monthly",regular=all(di==1L),gaps=sum(pmax(di-1L,0L)),grid=idx))
  }
  if(all(dd %% 7 == 0) && stats::median(dd)==7) { idx <- as.integer((as.numeric(d)-as.numeric(d[1L]))/7); di<-diff(idx); return(list(frequency="weekly",regular=all(di==1L),gaps=sum(pmax(di-1L,0L)),grid=idx)) }
  if(stats::median(dd)==1) { idx <- as.numeric(d); di<-diff(idx); return(list(frequency="daily",regular=all(di==1),gaps=sum(pmax(di-1,0)),grid=idx)) }
  list(frequency="irregular_or_custom",regular=FALSE,gaps=NA_integer_,grid=as.numeric(d))
}

.ec_time_grid_info <- function(p) {
  z <- p$parsed[is.finite(p$parsed)]
  if(length(z)<2L) return(list(frequency="unknown",regular=NA,gaps=NA_integer_,expected=NA_real_))
  if(p$kind=="year") {u<-sort(unique(z)); di<-diff(u); return(list(frequency="annual",regular=all(di==1),gaps=sum(pmax(di-1,0)),expected=1))}
  if(p$kind=="year_month") {u<-sort(unique(z));di<-diff(u);return(list(frequency="monthly",regular=all(di==1),gaps=sum(pmax(di-1,0)),expected=1))}
  if(p$kind=="year_quarter") {u<-sort(unique(z));di<-diff(u);return(list(frequency="quarterly",regular=all(di==1),gaps=sum(pmax(di-1,0)),expected=1))}
  if(p$kind=="date") {g<-.ec_date_grid(p$dates); return(list(frequency=g$frequency,regular=g$regular,gaps=g$gaps,expected=1))}
  if(p$kind=="datetime") {u<-sort(unique(z));di<-diff(u);md<-stats::median(di);return(list(frequency="custom_datetime",regular=all(abs(di-md)<=max(1e-8,abs(md)*1e-8)),gaps=if(all(abs(di-md)<=max(1e-8,abs(md)*1e-8))) 0L else NA_integer_,expected=md))}
  list(frequency="irregular_or_custom",regular=FALSE,gaps=NA_integer_,expected=NA_real_)
}

.ec_time_step <- function(parsed, kind) {
  z<-sort(unique(parsed[is.finite(parsed)])); if(length(z)<2L)return(list(regular=NA,expected=NA_real_,gaps=NA_integer_,step=NA_real_))
  d<-diff(z); expected<-if(kind%in%c("year","year_month","year_quarter"))1 else stats::median(d); tol<-max(1e-8,abs(expected)*.03)
  list(regular=all(abs(d-expected)<=tol),expected=expected,gaps=if(kind%in%c("year","year_month","year_quarter"))as.integer(sum(pmax(round(d)-1L,0L))) else NA_integer_,step=stats::median(d))
}

.ec_time_frequency_label <- function(kind, parsed, p=NULL) {
  if(!is.null(p)) return(.ec_time_grid_info(p)$frequency)
  if(kind=="year")return("annual");if(kind=="year_quarter")return("quarterly");if(kind=="year_month")return("monthly");"irregular_or_custom"
}

.ec_time_candidates <- function(data) {
  rows<-lapply(names(data),function(nm){p<-.ec_parse_time_vector(data[[nm]],nm);if(!p$valid)return(NULL); g<-.ec_time_grid_info(p); z<-p$parsed[is.finite(p$parsed)]; data.frame(variable=nm,kind=p$kind,frequency=g$frequency,score=p$score,valid_share=p$valid_share,unique_n=p$unique_n,duplicated_time=anyDuplicated(z)>0L,regular_spacing=isTRUE(g$regular),missing_periods=g$gaps,stringsAsFactors=FALSE)})
  out<-do.call(rbind,rows);if(is.null(out))return(data.frame(variable=character(),kind=character(),frequency=character(),score=numeric(),valid_share=numeric(),unique_n=integer(),duplicated_time=logical(),regular_spacing=logical(),missing_periods=integer(),stringsAsFactors=FALSE));out<-out[order(-out$score,-out$unique_n,out$variable),,drop=FALSE];rownames(out)<-NULL;out
}

#' Detect the likely observational structure of a dataset
#' @param data A data.frame.
#' @return A conservative structure suggestion and temporal candidates.
#' @export
eco_data_structure <- function(data) {
  if(!is.data.frame(data)).ec_stop("`data` must be a data.frame.");.ec_validate_data_columns(data);cand<-.ec_time_candidates(data)
  pc <- .ec_panel_candidates(data)
  if (nrow(pc)) return(list(structure="panel_candidate", confidence="medium", time_variable=NULL,
    reason=paste0("Repeated individuals with unique individual-period pairs are plausible: ", paste(paste(pc$id, pc$time, sep=" + "), collapse="; "), ". Confirm both indexes explicitly in Panel mode."),
    candidates=cand, panel_candidates=pc))
  if(!nrow(cand))return(list(structure="cross_section",confidence="high",time_variable=NULL,reason="No sufficiently reliable temporal index candidate was detected.",candidates=cand))
  top<-cand[1L,,drop=FALSE]; second<-if(nrow(cand)>1L)cand$score[2L] else -Inf; unique_index<-!isTRUE(top$duplicated_time);clear<-is.infinite(second)||top$score>=second+2;strong<-top$score>=7;regular<-isTRUE(top$regular_spacing)
  if(strong&&unique_index&&regular&&clear)return(list(structure="time_series",confidence="high",time_variable=top$variable,reason=paste0("Reliable temporal index detected in `",top$variable,"` (",top$frequency,")."),candidates=cand))
  list(structure="ambiguous_temporal",confidence=if(strong)"medium" else "low",time_variable=top$variable,reason=if(isTRUE(top$duplicated_time))paste0("A temporal variable was detected in `",top$variable,"`, but time values repeat. This may be panel/longitudinal data.") else if(!regular) paste0("A temporal variable was detected in `",top$variable,"`, but its calendar grid is irregular or contains gaps. Confirm the mode explicitly.") else "More than one plausible temporal index exists, or the evidence is not strong enough for automatic classification.",candidates=cand)
}

#' Audit a time index before time-series econometric modelling
#' @param data A data.frame.
#' @param time Name of the time-index variable.
#' @return A one-row audit data.frame.
#' @export
eco_time_audit <- function(data,time){
  if(!is.data.frame(data)).ec_stop("`data` must be a data.frame.");.ec_validate_data_columns(data);if(length(time)!=1L||!is.character(time)||!time%in%names(data)).ec_stop("`time` must name one column in `data`.")
  p<-.ec_parse_time_vector(data[[time]],time);if(!p$valid).ec_stop("`",time,"` is not a sufficiently reliable temporal index. Use a supported unambiguous representation or apply an explicit type override first.")
  good<-is.finite(p$parsed);z<-p$parsed[good];g<-.ec_time_grid_info(p);ordok<-length(z)<2L||all(diff(z)>=0)
  data.frame(time_variable=time,time_kind=p$kind,frequency=g$frequency,n_rows=nrow(data),n_time_nonmissing=sum(good),missing_time=sum(!good),unique_time=length(unique(z)),duplicate_time=sum(duplicated(z)),sorted_ascending=ordok,regular_spacing=g$regular,missing_periods=g$gaps,first_time=if(any(good))p$display[which.min(replace(p$parsed,!good,Inf))] else NA_character_,last_time=if(any(good))p$display[which.max(replace(p$parsed,!good,-Inf))] else NA_character_,stringsAsFactors=FALSE)
}

.ec_prepare_time_data <- function(data,time){
  aud<-eco_time_audit(data,time);if(aud$missing_time>0L).ec_stop("Time-series modelling requires a non-missing time index. `",time,"` contains ",aud$missing_time," missing time value(s).")
  if(aud$duplicate_time>0L).ec_stop("Single-series time econometrics requires one observation per time point. `",time,"` contains duplicated time values. Repeated dates may indicate panel/longitudinal data.")
  p<-.ec_parse_time_vector(data[[time]],time);ord<-order(p$parsed);original_ids<-rownames(data); if(is.null(original_ids)) original_ids<-as.character(seq_len(nrow(data))); out<-as.data.frame(data)[ord,,drop=FALSE]; rownames(out)<-original_ids[ord]
  list(data=out,parsed=p$parsed[ord],display=p$display[ord],dates=p$dates[ord],kind=p$kind,audit=aud,reordered=!identical(ord,seq_len(nrow(data))))
}
