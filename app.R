# =============================================================================
# $HYPE SIGNAL TERMINAL  v4
# Signal-first. Trade plan. Light theme. Actionable.
# =============================================================================

options(stringsAsFactors = FALSE, warn = -1, scipen = 999)

`%+%` <- paste0

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(httr)
  library(jsonlite)
  library(dplyr)
  library(zoo)
})

# =============================================================================
# CONFIG
# =============================================================================

HL_API       <- "https://api.hyperliquid.xyz/info"
BINANCE_SPOT <- "https://api.binance.com/api/v3"
BINANCE_PERP <- "https://fapi.binance.com/fapi/v1"
BYBIT_BASE   <- "https://api.bybit.com/v5"

HL_COIN     <- "HYPE"
BN_SPOT_SYM <- "HYPEUSDT"
BN_PERP_SYM <- "HYPEUSDT"
BYBIT_SYM   <- "HYPEUSDT"

PRICE_MS   <- 15000
OB_MS      <- 15000
FUNDING_MS <- 60000
CANDLE_MS  <- 120000

API_TIMEOUT  <- 8
CEX_TIMEOUT  <- 3
STALE_WARN_S <- 30
STALE_ERR_S  <- 120
CEX_CACHE    <- "data/cex_cache.rds"
CEX_CACHE_MAX_AGE <- 86400

# Account & fee model
ACCOUNT_SIZE <- 100000
MAX_RISK_PCT <- 0.02
FEE_MAKER    <- 0.0005
FEE_TAKER    <- 0.0006
SLIPPAGE_EST <- 0.0003
ROUND_TRIP   <- FEE_MAKER + FEE_TAKER + 2*SLIPPAGE_EST
MAX_POS_PCT  <- 0.20
TRADE_LOG_FILE <- "data/trade_log.rds"

# =============================================================================
# HTTP HELPERS
# =============================================================================

now_ms <- function() round(as.numeric(Sys.time()) * 1000)
now_s  <- function() as.numeric(Sys.time())

fetch_wrap <- function(expr) {
  t0 <- now_s()
  tryCatch({
    val <- expr
    list(ok=!is.null(val), data=val, fetched_at=t0,
         error_msg=if(is.null(val)) "no data" else NA_character_)
  }, error=function(e) list(ok=FALSE, data=NULL, fetched_at=t0, error_msg=conditionMessage(e)))
}

ON_SHINYAPPS <- Sys.getenv("R_CONFIG_ACTIVE","") == "shinyapps" || file.exists("/srv/connect/apps")
.cex_mem <- list(data=NULL, at=0)
read_cex_cache <- function() {
  now <- as.numeric(Sys.time())
  if(!is.null(.cex_mem$data) && (now - .cex_mem$at) < 60) return(.cex_mem$data)
  cache <- tryCatch({
    tmp <- tempfile(fileext=".rds")
    r <- GET("https://raw.githubusercontent.com/ChiraagNAX/hype-signal/main/data/cex_cache.rds",
             write_disk(tmp,overwrite=TRUE), timeout(5))
    if(status_code(r)==200) readRDS(tmp) else NULL
  }, error=function(e) NULL)
  if(is.null(cache)) cache <- tryCatch({
    if(file.exists(CEX_CACHE)) readRDS(CEX_CACHE) else NULL
  }, error=function(e) NULL)
  if(!is.null(cache)) { .cex_mem$data <<- cache; .cex_mem$at <<- now }
  cache
}

safe_get <- function(url, query=NULL, tout=API_TIMEOUT) {
  tryCatch({
    r <- GET(url, query=query, timeout(tout))
    if(status_code(r)==200) fromJSON(content(r,"text",encoding="UTF-8"),simplifyVector=FALSE) else NULL
  }, error=function(e) NULL)
}

safe_post <- function(url, body) {
  tryCatch({
    r <- POST(url, body=toJSON(body,auto_unbox=TRUE),
              add_headers("Content-Type"="application/json"), timeout(API_TIMEOUT))
    if(status_code(r)==200) fromJSON(content(r,"text",encoding="UTF-8"),simplifyVector=FALSE) else NULL
  }, error=function(e) NULL)
}

# =============================================================================
# FORMATTERS
# =============================================================================

fp   <- function(x,d=4) { x<-suppressWarnings(as.numeric(x)); if(is.na(x))"—" else formatC(x,format="f",digits=d,big.mark=",") }
fpct <- function(x,d=4) { x<-suppressWarnings(as.numeric(x)); if(is.na(x))"—" else paste0(formatC(x*100,format="f",digits=d),"%") }
fbig <- function(x) {
  x<-suppressWarnings(as.numeric(x)); if(is.na(x)) return("—")
  if(abs(x)>=1e9) paste0("$",round(x/1e9,2),"B") else if(abs(x)>=1e6) paste0("$",round(x/1e6,2),"M")
  else if(abs(x)>=1e3) paste0("$",round(x/1e3,1),"K") else paste0("$",round(x,0))
}

age_str <- function(fat) {
  if(is.null(fat)||is.na(fat)) return("—")
  s<-round(now_s()-fat)
  if(s<60) paste0(s,"s") else paste0(round(s/60,0),"m")
}

# =============================================================================
# FETCHERS — HYPERLIQUID
# =============================================================================

fetch_hl_perp_ctx <- function() {
  fetch_wrap({
    res<-safe_post(HL_API,list(type="metaAndAssetCtxs"))
    if(is.null(res)) return(NULL)
    meta<-res[[1]]$universe; ctxs<-res[[2]]
    idx<-which(sapply(meta,function(x) x$name==HL_COIN))
    if(!length(idx)) return(NULL)
    ctx<-ctxs[[idx[1]]]
    list(mark_px=as.numeric(ctx$markPx), oracle_px=as.numeric(ctx$oraclePx),
         mid_px=suppressWarnings(as.numeric(ctx$midPx)),
         funding=as.numeric(ctx$funding), oi=as.numeric(ctx$openInterest),
         premium=as.numeric(ctx$premium), day_vol=as.numeric(ctx$dayNtlVlm))
  })
}

fetch_hl_spot_price <- function() {
  fetch_wrap({
    res<-safe_post(HL_API,list(type="spotMetaAndAssetCtxs"))
    if(is.null(res)) return(NULL)
    pairs<-res[[1]]$universe; ctxs<-res[[2]]
    idx<-which(sapply(pairs,function(x) grepl("^HYPE",x$name,ignore.case=TRUE)))
    if(!length(idx)) return(NULL)
    ctx<-ctxs[[idx[1]]]
    list(mark_px=suppressWarnings(as.numeric(ctx$markPx)),
         day_vol=suppressWarnings(as.numeric(ctx$dayNtlVlm)))
  })
}

fetch_hl_orderbook <- function(n=30) {
  fetch_wrap({
    res<-safe_post(HL_API,list(type="l2Book",coin=HL_COIN,nSigFigs=5))
    if(is.null(res)||is.null(res$levels)) return(NULL)
    lvl<-res$levels
    mk<-function(raw,s) data.frame(price=as.numeric(sapply(raw,`[[`,"px")),
                                    size=as.numeric(sapply(raw,`[[`,"sz")),
                                    side=s,stringsAsFactors=FALSE)
    list(bids=mk(lvl[[1]],"bid"),asks=mk(lvl[[2]],"ask"),
         n_bid_levels=length(lvl[[1]]),n_ask_levels=length(lvl[[2]]))
  })
}

fetch_hl_candles <- function(interval="5m", n=300) {
  fetch_wrap({
    iv_ms<-switch(interval,"1m"=60e3,"5m"=3e5,"15m"=9e5,"1h"=36e5,"4h"=144e5,60e3)
    end<-now_ms(); start<-end-n*iv_ms
    res<-safe_post(HL_API,list(type="candleSnapshot",
                                req=list(coin=HL_COIN,interval=interval,startTime=start,endTime=end)))
    if(is.null(res)||!length(res)) return(NULL)
    data.frame(time=as.POSIXct(sapply(res,function(x)as.numeric(x$t))/1000,origin="1970-01-01",tz="UTC"),
               open=as.numeric(sapply(res,function(x)x$o)), high=as.numeric(sapply(res,function(x)x$h)),
               low=as.numeric(sapply(res,function(x)x$l)), close=as.numeric(sapply(res,function(x)x$c)),
               vol=as.numeric(sapply(res,function(x)x$v)), stringsAsFactors=FALSE)
  })
}

fetch_hl_funding_hist <- function(n=60) {
  fetch_wrap({
    end<-now_ms(); start<-end-n*8*3600e3
    res<-safe_post(HL_API,list(type="fundingHistory",coin=HL_COIN,startTime=start,endTime=end))
    if(is.null(res)||!length(res)) return(NULL)
    data.frame(time=as.POSIXct(sapply(res,function(x)as.numeric(x$time))/1000,origin="1970-01-01",tz="UTC"),
               funding=as.numeric(sapply(res,function(x)x$fundingRate)),
               premium=as.numeric(sapply(res,function(x)x$premium)), stringsAsFactors=FALSE)
  })
}

fetch_hl_predicted_fundings <- function() {
  fetch_wrap({
    res<-safe_post(HL_API,list(type="predictedFundings"))
    if(is.null(res)) return(NULL)
    hype_row<-NULL
    for(item in res) if(length(item)>=1&&identical(item[[1]],HL_COIN)){hype_row<-item[[2]];break}
    if(is.null(hype_row)) return(NULL)
    lapply(hype_row,function(x) list(venue=x[[1]],rate=suppressWarnings(as.numeric(x[[2]]))))
  })
}

fetch_hl_trades <- function() {
  fetch_wrap({
    res<-safe_post(HL_API,list(type="recentTrades",coin=HL_COIN))
    if(is.null(res)||!length(res)) return(NULL)
    data.frame(time=as.POSIXct(sapply(res,function(x)as.numeric(x$time))/1000,origin="1970-01-01",tz="UTC"),
               price=as.numeric(sapply(res,function(x)x$px)), size=as.numeric(sapply(res,function(x)x$sz)),
               side=sapply(res,function(x)x$side), stringsAsFactors=FALSE)
  })
}

# =============================================================================
# FETCHERS — BINANCE
# =============================================================================

cex_cache_wrap <- function(live_fn, cache_key) {
  if(ON_SHINYAPPS) {
    cache<-read_cex_cache()
    if(!is.null(cache)&&!is.null(cache[[cache_key]]))
      return(list(ok=TRUE,data=cache[[cache_key]],fetched_at=cache$fetched_at,error_msg=NA_character_))
    return(list(ok=FALSE,data=NULL,fetched_at=as.numeric(Sys.time()),error_msg="no cache"))
  }
  r<-tryCatch(live_fn(), error=function(e) list(ok=FALSE,data=NULL,fetched_at=as.numeric(Sys.time()),error_msg=conditionMessage(e)))
  if(!isTRUE(r$ok)) { cache<-read_cex_cache(); if(!is.null(cache)&&!is.null(cache[[cache_key]]))
    r<-list(ok=TRUE,data=cache[[cache_key]],fetched_at=cache$fetched_at,error_msg=NA_character_) }
  r
}

fetch_bn_spot <- function() cex_cache_wrap(function() fetch_wrap({
    res<-safe_get(paste0(BINANCE_SPOT,"/ticker/24hr"),list(symbol=BN_SPOT_SYM),tout=CEX_TIMEOUT)
    if(is.null(res)) return(NULL)
    list(price=as.numeric(res$lastPrice),change_pct=as.numeric(res$priceChangePercent),
         volume=as.numeric(res$quoteVolume))
  }), "bn_spot")

fetch_bn_premium_index <- function() cex_cache_wrap(function() fetch_wrap({
    res<-safe_get(paste0(BINANCE_PERP,"/premiumIndex"),list(symbol=BN_PERP_SYM),tout=CEX_TIMEOUT)
    if(is.null(res)) return(NULL)
    list(mark_px=as.numeric(res$markPrice), index_px=as.numeric(res$indexPrice),
         funding_rate=as.numeric(res$lastFundingRate), next_fund_time=as.numeric(res$nextFundingTime))
  }), "bn_prem")

fetch_bn_perp <- function() {
  if(ON_SHINYAPPS) {
    cache<-read_cex_cache()
    if(!is.null(cache)&&!is.null(cache$bn_perp)) {
      d<-cache$bn_perp; d$oi_base<-if(!is.null(cache$bn_oi_base)) cache$bn_oi_base else NA_real_
      return(list(ok=TRUE,data=d,fetched_at=cache$fetched_at,error_msg=NA_character_))
    }
    return(list(ok=FALSE,data=NULL,fetched_at=as.numeric(Sys.time()),error_msg="no cache"))
  }
  r <- fetch_wrap({
    t<-safe_get(paste0(BINANCE_PERP,"/ticker/24hr"),list(symbol=BN_PERP_SYM),tout=CEX_TIMEOUT)
    oi<-safe_get(paste0(BINANCE_PERP,"/openInterest"),list(symbol=BN_PERP_SYM),tout=CEX_TIMEOUT)
    if(is.null(t)&&is.null(oi)) return(NULL)
    list(price=if(!is.null(t)) as.numeric(t$lastPrice) else NA_real_,
         change_pct=if(!is.null(t)) as.numeric(t$priceChangePercent) else NA_real_,
         oi_base=if(!is.null(oi)) as.numeric(oi$openInterest) else NA_real_)
  })
  if(!r$ok) { cache<-read_cex_cache(); if(!is.null(cache)&&!is.null(cache$bn_perp)) {
    d<-cache$bn_perp; d$oi_base<-if(!is.null(cache$bn_oi_base)) cache$bn_oi_base else NA_real_
    r<-list(ok=TRUE,data=d,fetched_at=cache$fetched_at,error_msg=NA_character_) }}
  r
}

fetch_bn_perp_book <- function(limit=25) cex_cache_wrap(function() fetch_wrap({
    res<-safe_get(paste0(BINANCE_PERP,"/depth"),list(symbol=BN_PERP_SYM,limit=limit),tout=CEX_TIMEOUT)
    if(is.null(res)) return(NULL)
    list(bids=data.frame(price=as.numeric(sapply(res$bids,`[[`,1)),size=as.numeric(sapply(res$bids,`[[`,2)),side="bid",stringsAsFactors=FALSE),
         asks=data.frame(price=as.numeric(sapply(res$asks,`[[`,1)),size=as.numeric(sapply(res$asks,`[[`,2)),side="ask",stringsAsFactors=FALSE),
         n_bid_levels=length(res$bids),n_ask_levels=length(res$asks))
  }), "bn_ob")

fetch_bn_perp_candles <- function(interval="5m",limit=300) {
  fetch_wrap({
    res<-safe_get(paste0(BINANCE_PERP,"/klines"),list(symbol=BN_PERP_SYM,interval=interval,limit=limit),tout=CEX_TIMEOUT)
    if(is.null(res)) return(NULL)
    data.frame(time=as.POSIXct(sapply(res,function(x)as.numeric(x[[1]]))/1000,origin="1970-01-01",tz="UTC"),
               open=as.numeric(sapply(res,function(x)x[[2]])),high=as.numeric(sapply(res,function(x)x[[3]])),
               low=as.numeric(sapply(res,function(x)x[[4]])),close=as.numeric(sapply(res,function(x)x[[5]])),
               vol=as.numeric(sapply(res,function(x)x[[8]])),stringsAsFactors=FALSE)
  })
}

# =============================================================================
# FETCHERS — BYBIT
# =============================================================================

fetch_bybit_perp <- function() cex_cache_wrap(function() fetch_wrap({
    res<-safe_get(paste0(BYBIT_BASE,"/market/tickers"),list(category="linear",symbol=BYBIT_SYM),tout=CEX_TIMEOUT)
    if(is.null(res)||!length(res$result$list)) return(NULL)
    it<-res$result$list[[1]]
    list(price=suppressWarnings(as.numeric(it$lastPrice)),
         change_pct=suppressWarnings(as.numeric(it$price24hPcnt)),
         oi_usd=suppressWarnings(as.numeric(it$openInterestValue)),
         funding_rate=suppressWarnings(as.numeric(it$fundingRate)))
  }), "bybit_perp")

fetch_bybit_perp_book <- function(limit=25) cex_cache_wrap(function() fetch_wrap({
    res<-safe_get(paste0(BYBIT_BASE,"/market/orderbook"),list(category="linear",symbol=BYBIT_SYM,limit=limit),tout=CEX_TIMEOUT)
    if(is.null(res)||is.null(res$result)) return(NULL)
    b<-res$result$b; a<-res$result$a
    if(!length(b)||!length(a)) return(NULL)
    list(bids=data.frame(price=as.numeric(sapply(b,`[[`,1)),size=as.numeric(sapply(b,`[[`,2)),side="bid",stringsAsFactors=FALSE),
         asks=data.frame(price=as.numeric(sapply(a,`[[`,1)),size=as.numeric(sapply(a,`[[`,2)),side="ask",stringsAsFactors=FALSE),
         n_bid_levels=length(b),n_ask_levels=length(a))
  }), "bybit_ob")

# =============================================================================
# ANALYTICS
# =============================================================================

calc_twap <- function(df, period) {
  if(is.null(df)||nrow(df)<period) return(list(last=NA_real_,n_bars=0,period=period,series=NULL))
  tp<-(df$high+df$low+df$close)/3
  series<-as.numeric(rollmean(tp,k=period,fill=NA,align="right"))
  last<-tail(series[!is.na(series)],1)
  list(last=if(length(last)) last else NA_real_, n_bars=sum(!is.na(series)), period=period, series=series)
}

calc_ob_imbalance <- function(books, pct=0.005) {
  all_bids<-bind_rows(lapply(books,function(b) if(!is.null(b$data)) b$data$bids else NULL))
  all_asks<-bind_rows(lapply(books,function(b) if(!is.null(b$data)) b$data$asks else NULL))
  if(nrow(all_bids)==0||nrow(all_asks)==0) return(list(imb=NA_real_,bid_notl=NA_real_,ask_notl=NA_real_,mid=NA_real_,n_venues=0))
  all_bids$pb<-round(all_bids$price,2); all_asks$pb<-round(all_asks$price,2)
  agg_b<-all_bids%>%group_by(pb)%>%summarise(size=sum(size,na.rm=TRUE),.groups="drop")
  agg_a<-all_asks%>%group_by(pb)%>%summarise(size=sum(size,na.rm=TRUE),.groups="drop")
  mid<-(max(agg_b$pb)+min(agg_a$pb))/2
  bv<-sum(agg_b$pb[agg_b$pb>=mid*(1-pct)]*agg_b$size[agg_b$pb>=mid*(1-pct)],na.rm=TRUE)
  av<-sum(agg_a$pb[agg_a$pb<=mid*(1+pct)]*agg_a$size[agg_a$pb<=mid*(1+pct)],na.rm=TRUE)
  tot<-bv+av; n_ok<-sum(sapply(books,function(b)!is.null(b$data)))
  list(imb=if(tot>0) bv/tot else 0.5, bid_notl=bv, ask_notl=av, mid=mid, n_venues=n_ok)
}

detect_candle_gaps <- function(df,interval) {
  if(is.null(df)||nrow(df)<2) return(list(gap_pct=NA,n_gaps=0))
  iv_s<-switch(interval,"1m"=60,"5m"=300,"15m"=900,"1h"=3600,"4h"=14400,60)
  diffs<-as.numeric(diff(df$time))
  gap_mask<-diffs>iv_s*1.5
  list(gap_pct=round(sum(gap_mask)/length(diffs)*100,2), n_gaps=sum(gap_mask))
}

calc_trade_flow <- function(trades_data) {
  if(is.null(trades_data)||nrow(trades_data)<5) return(list(buy_vol=NA,sell_vol=NA,ratio=NA,n_trades=0))
  buys<-trades_data[trades_data$side=="B",]; sells<-trades_data[trades_data$side!="B",]
  bv<-sum(buys$price*buys$size,na.rm=TRUE); sv<-sum(sells$price*sells$size,na.rm=TRUE)
  tot<-bv+sv
  list(buy_vol=bv, sell_vol=sv, ratio=if(tot>0) bv/tot else 0.5, n_trades=nrow(trades_data))
}

# =============================================================================
# SIGNAL ENGINE
# =============================================================================

build_signal <- function(hl_ctx, hl_spot, bn_perp, bn_prem, bybit_perp,
                         ob_result, twaps, fund_hist, pred_fundings,
                         candle_gaps, trade_flow, prev_oi) {

  scores<-numeric(0); labels<-character(0); weights<-numeric(0)
  sources<-character(0); raw_vals<-character(0); warns<-character(0)
  conf_hits<-0; conf_total<-0

  push<-function(score,label,source,raw,w=1) {
    if(is.na(score)||is.nan(score)) score<-0
    scores<<-c(scores,max(-100,min(100,score))); labels<<-c(labels,label)
    weights<<-c(weights,w); sources<<-c(sources,source); raw_vals<<-c(raw_vals,raw)
    conf_hits<<-conf_hits+w; conf_total<<-conf_total+w
  }
  skip<-function(label,reason,w=1) {
    warns<<-c(warns,paste0(label,": ",reason)); conf_total<<-conf_total+w
  }

  cur_px<-if(!is.null(hl_ctx$data)&&!is.na(hl_ctx$data$mark_px)) hl_ctx$data$mark_px else NA_real_

  # 1. HL Funding
  if(!is.null(hl_ctx$data)&&!is.na(hl_ctx$data$funding)) {
    fr<-hl_ctx$data$funding
    push(-tanh(fr/0.001)*100,"HL Funding (8h)","Hyperliquid",fpct(fr),w=3)
  } else skip("HL Funding","unavailable",w=3)

  # 2. HL Premium
  if(!is.null(hl_ctx$data)&&!is.na(hl_ctx$data$premium)) {
    p<-hl_ctx$data$premium
    push(-tanh(p/0.002)*80,"HL Perp Premium","Hyperliquid",fpct(p),w=2)
  } else skip("HL Premium","unavailable",w=2)

  # 3. Perp/Spot basis
  hl_px<-if(!is.null(hl_ctx$data)) hl_ctx$data$mark_px else NA_real_
  sp_px<-if(!is.null(hl_spot$data)) hl_spot$data$mark_px else NA_real_
  bn_px<-if(!is.null(bn_perp$data)) bn_perp$data$price else NA_real_

  if(!is.na(hl_px)&&!is.na(sp_px)&&sp_px>0) {
    basis<-(hl_px-sp_px)/sp_px
    push(-tanh(basis/0.003)*60,"Perp/Spot Basis","HL perp vs spot",fpct(basis),w=1.5)
  } else skip("Perp/Spot Basis","unavailable",w=1.5)

  # 4. Cross-venue
  if(!is.na(hl_px)&&!is.na(bn_px)&&bn_px>0) {
    xv<-(hl_px-bn_px)/bn_px
    push(-tanh(xv/0.002)*50,"HL vs Binance","Cross-venue",fpct(xv),w=1)
  } else skip("HL vs Binance","unavailable",w=1)

  # 5. CEX Funding consensus
  bn_fr<-if(!is.null(bn_prem$data)&&!is.na(bn_prem$data$funding_rate)) bn_prem$data$funding_rate else NA_real_
  by_fr<-if(!is.null(bybit_perp$data)&&!is.na(bybit_perp$data$funding_rate)) bybit_perp$data$funding_rate else NA_real_
  cex<-c(bn_fr,by_fr); cex<-cex[!is.na(cex)]
  if(length(cex)>0) {
    med<-median(cex)
    push(-tanh(med/0.001)*80,paste0("CEX Funding (",length(cex),"/2)"),"BN+Bybit",fpct(med),w=1.5)
  } else skip("CEX Funding","unavailable",w=1.5)

  # 6. OB imbalance
  if(!is.na(ob_result$imb)) {
    push((ob_result$imb-0.5)*200,paste0("Orderbook (",ob_result$n_venues,"/3)"),
         "Aggregate OB",paste0(round(ob_result$imb*100,1),"% bids"),w=2)
    if(ob_result$n_venues<3) warns<-c(warns,paste0("OB: only ",ob_result$n_venues,"/3 venues"))
  } else skip("Orderbook","unavailable",w=2)

  # 7. TWAP deviations
  twap_cfg<-list(t5=list(lbl="5m TWAP",w=0.8),t15=list(lbl="15m TWAP",w=1.2),
                 t60=list(lbl="1h TWAP",w=1.8),t240=list(lbl="4h TWAP",w=2.5))
  if(!is.na(cur_px)&&!is.null(twaps)) {
    for(k in names(twap_cfg)) {
      cfg<-twap_cfg[[k]]; tw<-twaps[[k]]
      if(!is.null(tw)&&!is.na(tw$last)) {
        dev<-(cur_px-tw$last)/tw$last
        push(-tanh(dev/0.01)*70,cfg$lbl,paste0(tw$n_bars," bars"),
             paste0(ifelse(dev>=0,"+",""),round(dev*100,3),"%"),w=cfg$w)
      } else skip(cfg$lbl,"no candles",w=cfg$w)
    }
  } else for(k in names(twap_cfg)) skip(twap_cfg[[k]]$lbl,"no data",w=twap_cfg[[k]]$w)

  # 8. Candle gap penalty
  if(!is.null(candle_gaps)&&!is.na(candle_gaps$gap_pct)&&candle_gaps$gap_pct>5) {
    push(-abs(candle_gaps$gap_pct)*0.5,"Candle Gaps","continuity",
         paste0(candle_gaps$gap_pct,"% missing"),w=0.5)
  }

  # 9. Funding trend
  if(!is.null(fund_hist$data)&&nrow(fund_hist$data)>=5) {
    fh<-tail(fund_hist$data,12)
    slope<-coef(lm(funding~seq_along(funding),data=fh))[2]
    push(-tanh(slope/0.00005)*60,"Funding Trend","HL hist",
         paste0("slope=",formatC(slope*1e4,format="f",digits=2),"bp/period"),w=1.2)
  } else skip("Funding Trend","insufficient history",w=1.2)

  # 10. Predicted funding divergence
  if(!is.null(pred_fundings$data)&&length(pred_fundings$data)>=2) {
    rates<-sapply(pred_fundings$data,function(x) suppressWarnings(as.numeric(x$rate)))
    is_hl<-sapply(pred_fundings$data,function(x)grepl("HL|Hyperliquid",x$venue,ignore.case=TRUE))
    hl_p<-rates[is_hl]; hl_p<-hl_p[!is.na(hl_p)]
    cx_p<-rates[!is_hl]; cx_p<-cx_p[!is.na(cx_p)]
    if(length(hl_p)&&length(cx_p)) {
      dv<-mean(cx_p)-mean(hl_p)
      push(tanh(dv/0.0005)*70,"Funding Divergence","HL predicted",
           paste0("CEX-HL=",fpct(dv)),w=1.5)
    }
  } else skip("Funding Divergence","unavailable",w=1.5)

  # 11. Trade flow
  if(!is.null(trade_flow)&&!is.na(trade_flow$ratio)) {
    push((trade_flow$ratio-0.5)*160,paste0("Trade Flow (",trade_flow$n_trades,")"),
         "HL trades",paste0(round(trade_flow$ratio*100,1),"% buys"),w=1.5)
  } else skip("Trade Flow","no data",w=1.5)

  # 12. OI delta
  cur_oi<-if(!is.null(hl_ctx$data)&&!is.na(hl_ctx$data$oi)) hl_ctx$data$oi else NA_real_
  if(!is.na(prev_oi)&&!is.na(cur_oi)&&prev_oi>0) {
    oi_chg<-(cur_oi-prev_oi)/prev_oi
    hl_fr<-if(!is.null(hl_ctx$data)&&!is.na(hl_ctx$data$funding)) hl_ctx$data$funding else 0
    if(abs(oi_chg)>0.005) {
      push(-tanh(oi_chg*sign(hl_fr)/0.02)*60,
           paste0("OI Delta (",ifelse(oi_chg>0,"+",""),round(oi_chg*100,2),"%)"),
           "HL OI",paste0(fbig(prev_oi)," -> ",fbig(cur_oi)),w=1.5)
    }
  }

  if(!length(scores)) return(list(score=NA_real_,confidence=0,components=data.frame(),warnings=warns,
                                   twap_levels=list(),cur_px=cur_px))

  composite<-max(-100,min(100,sum(scores*weights)/sum(weights)))
  confidence<-if(conf_total>0) round(conf_hits/conf_total*100,0) else 0

  twap_levels<-list()
  if(!is.null(twaps)) {
    for(k in names(twaps)) if(!is.null(twaps[[k]])&&!is.na(twaps[[k]]$last)) twap_levels[[k]]<-twaps[[k]]$last
  }

  list(score=round(composite,1), confidence=confidence,
       components=data.frame(Signal=labels, Score=round(scores,1), Weight=weights,
                             Source=sources, Value=raw_vals, stringsAsFactors=FALSE),
       warnings=warns, twap_levels=twap_levels, cur_px=cur_px)
}

# =============================================================================
# TRADE PLAN BUILDER
# =============================================================================

build_trade_plan <- function(sig_result, hl_ctx, ob_result) {
  sc <- sig_result$score
  conf <- sig_result$confidence
  px <- sig_result$cur_px
  twaps <- sig_result$twap_levels

  if(is.na(sc)||is.na(px)) return(list(
    action="WAIT", action_col="#94a3b8", reason="Insufficient data to form a view.",
    entry="—", stop="—", tp1="—", tp2="—", invalidation="—",
    horizon="—", sizing="—", conviction="None", edge_notes=list()
  ))

  # Determine key levels from TWAPs
  tw_vals <- unlist(twaps)
  tw_below <- tw_vals[tw_vals < px]
  tw_above <- tw_vals[tw_vals > px]
  nearest_support <- if(length(tw_below)) max(tw_below) else px*0.99
  nearest_resist  <- if(length(tw_above)) min(tw_above) else px*1.01

  # OB midpoint as secondary reference
  ob_mid <- if(!is.na(ob_result$mid)) ob_result$mid else px

  # Funding
  fr <- if(!is.null(hl_ctx$data)&&!is.na(hl_ctx$data$funding)) hl_ctx$data$funding else 0
  fr_cost_8h <- abs(fr)*100

  # Conviction level
  conviction <- if(abs(sc)>60&&conf>=80) "High"
    else if(abs(sc)>30&&conf>=60) "Medium"
    else if(abs(sc)>15&&conf>=50) "Low"
    else "None"

  # Position sizing suggestion
  sizing <- if(conviction=="High") "Full size (1x base)"
    else if(conviction=="Medium") "Half size (0.5x base)"
    else if(conviction=="Low") "Quarter size (0.25x base)"
    else "No position — edge too thin"

  # Horizon based on which TWAPs are driving
  comps <- sig_result$components
  twap_scores <- comps[grepl("TWAP",comps$Signal),]
  if(nrow(twap_scores)>0) {
    dominant <- twap_scores$Signal[which.max(abs(twap_scores$Score))]
    horizon <- if(grepl("4h",dominant)) "4-12 hours"
      else if(grepl("1h",dominant)) "1-4 hours"
      else if(grepl("15m",dominant)) "15-60 minutes"
      else "5-15 minutes"
  } else horizon <- "Unknown — no TWAP data"

  # Edge notes — specific observations
  notes <- list()
  if(abs(fr)>0.0005) notes<-c(notes, paste0("Funding is ", if(fr>0)"positive (shorts get paid)" else "negative (longs get paid)", " at ", round(fr*100,4), "% per 8h"))
  if(!is.na(ob_result$imb)) {
    if(ob_result$imb>0.65) notes<-c(notes, "Strong bid-side orderbook support — sellers face a wall")
    else if(ob_result$imb<0.35) notes<-c(notes, "Heavy ask-side pressure — bids are thin")
  }
  ob_skew <- if(!is.na(ob_result$imb)) ob_result$imb else 0.5

  if(sc > 20) {
    # BULLISH
    entry_zone <- round(max(nearest_support, px*0.997), 4)
    stop_loss  <- round(nearest_support*0.99, 4)
    tp1        <- round(nearest_resist, 4)
    tp2        <- round(px*1.02, 4)
    invalidation <- paste0("Close below ", fp(stop_loss,2), " or funding flips heavily positive")
    action <- if(sc>60) "STRONG LONG" else "LEAN LONG"
    action_col <- if(sc>60) "#0a8754" else "#2d9d6a"
    reason <- if(sc>60) "Multiple signals aligned bullish — TWAPs, flow, and structure all favour upside."
      else "Modest bullish lean. Some supporting factors but conviction is mixed."
  } else if(sc < -20) {
    # BEARISH
    entry_zone <- round(min(nearest_resist, px*1.003), 4)
    stop_loss  <- round(nearest_resist*1.01, 4)
    tp1        <- round(nearest_support, 4)
    tp2        <- round(px*0.98, 4)
    invalidation <- paste0("Close above ", fp(stop_loss,2), " or funding flips heavily negative")
    action <- if(sc< -60) "STRONG SHORT" else "LEAN SHORT"
    action_col <- if(sc< -60) "#b5291a" else "#c0522e"
    reason <- if(sc< -60) "Multiple signals aligned bearish — TWAPs, flow, and structure all favour downside."
      else "Modest bearish lean. Some factors supportive but not overwhelming."
  } else {
    # NEUTRAL — still show key levels as reference
    entry_zone <- round(px, 4)
    stop_loss  <- if(sc>=0) round(nearest_support*0.99, 4) else round(nearest_resist*1.01, 4)
    tp1 <- round(nearest_resist, 4)
    tp2 <- round(nearest_support, 4)
    invalidation <- "Score moves beyond +/-20 for a directional entry"
    action <- "NO TRADE"
    action_col <- "#64748b"
    reason <- "No clear directional edge. Signals are conflicting or flat. Wait for alignment."
    sizing <- "No position — no edge"
  }

  is_neutral <- (action=="NO TRADE")
  list(action=action, action_col=action_col, reason=reason,
       entry=if(is.numeric(entry_zone)) fp(entry_zone) else entry_zone,
       stop=if(is.numeric(stop_loss)) fp(stop_loss) else stop_loss,
       tp1=if(is.numeric(tp1)) fp(tp1) else tp1,
       tp2=if(is.numeric(tp2)) fp(tp2) else tp2,
       lbl_entry=if(is_neutral)"Current Price" else "Entry Zone",
       lbl_stop=if(is_neutral)"Key Support" else "Stop Loss",
       lbl_tp1=if(is_neutral)"Resistance" else "Target 1",
       lbl_tp2=if(is_neutral) "Support" else "Target 2",
       invalidation=invalidation,
       horizon=horizon, sizing=sizing, conviction=conviction,
       edge_notes=notes, fr_cost_8h=round(fr_cost_8h,4),
       entry_num=if(is.numeric(entry_zone)) entry_zone else NA_real_,
       stop_num=if(is.numeric(stop_loss)) stop_loss else NA_real_,
       tp1_num=if(is.numeric(tp1)) tp1 else NA_real_,
       tp2_num=if(is.numeric(tp2)) tp2 else NA_real_,
       direction=if(sc>20) "LONG" else if(sc< -20) "SHORT" else "FLAT",
       score=sc)
}

# =============================================================================
# POSITION MANAGER
# =============================================================================

new_position <- function() {
  saved <- load_trade_log()
  realized <- if(length(saved)) sum(sapply(saved, function(t) t$pnl_usd)) else 0
  list(state="FLAT", direction=NA, entry_px=NA, size_usd=NA, size_coins=NA,
       stop_px=NA, tp1_px=NA, tp2_px=NA, opened_at=NA, horizon_end=NA,
       pending_entry=NA, pending_dir=NA, pending_stop=NA, pending_tp1=NA,
       pending_tp2=NA, pending_size_usd=NA, pending_size_coins=NA,
       pending_score=NA, pending_at=NA, pending_horizon_s=NA,
       hedge_active=FALSE, hedge_dir=NA, hedge_entry=NA, hedge_size_usd=NA,
       hedge_size_coins=NA, hedge_stop=NA, hedge_opened_at=NA,
       realized_pnl=realized, trade_log=saved)
}

calc_position_size <- function(conviction, px, stop_px) {
  mult <- switch(conviction, "High"=1.0, "Medium"=0.5, "Low"=0.25, 0)
  max_usd <- ACCOUNT_SIZE * MAX_POS_PCT * mult
  if(!is.na(stop_px) && !is.na(px) && px > 0 && stop_px > 0) {
    risk_per_coin <- abs(px - stop_px)
    if(risk_per_coin > 0) {
      risk_budget <- ACCOUNT_SIZE * MAX_RISK_PCT * mult
      risk_coins <- risk_budget / risk_per_coin
      risk_usd <- risk_coins * px
      max_usd <- min(max_usd, risk_usd)
    }
  }
  coins <- if(px > 0) max_usd / px else 0
  list(usd=round(max_usd, 2), coins=round(coins, 3))
}

horizon_to_seconds <- function(h) {
  if(grepl("4-12", h)) 8*3600
  else if(grepl("1-4", h)) 2.5*3600
  else if(grepl("15-60", h)) 37*60
  else if(grepl("5-15", h)) 10*60
  else 4*3600
}

calc_pnl <- function(pos, cur_px) {
  if(is.na(pos$entry_px) || is.na(cur_px)) return(list(usd=0, pct=0, net_usd=0, net_pct=0))
  raw <- if(pos$direction=="LONG") (cur_px - pos$entry_px) / pos$entry_px
         else (pos$entry_px - cur_px) / pos$entry_px
  gross_usd <- raw * pos$size_usd
  fees_usd <- pos$size_usd * ROUND_TRIP
  net_usd <- gross_usd - fees_usd
  net_pct <- if(pos$size_usd > 0) net_usd / pos$size_usd * 100 else 0
  list(usd=round(gross_usd,2), pct=round(raw*100,2), net_usd=round(net_usd,2), net_pct=round(net_pct,2),
       fees=round(fees_usd,2))
}

update_position <- function(pos, sig_result, tplan, cur_px, twaps=NULL) {
  if(is.na(cur_px)) return(pos)
  now <- as.numeric(Sys.time())

  # Check hedge status first
  if(isTRUE(pos$hedge_active)) {
    hedge_pnl_raw <- if(pos$hedge_dir=="SHORT") (pos$hedge_entry - cur_px)/pos$hedge_entry
                     else (cur_px - pos$hedge_entry)/pos$hedge_entry
    hedge_exit <- FALSE; hedge_reason <- NULL

    # Close hedge if: signal realigns with primary, or hedge stop hit, or hedge profitable enough
    if(pos$direction=="LONG" && sig_result$score > 10) { hedge_exit<-TRUE; hedge_reason<-"Signal realigned bullish" }
    if(pos$direction=="SHORT" && sig_result$score < -10) { hedge_exit<-TRUE; hedge_reason<-"Signal realigned bearish" }
    if(pos$hedge_dir=="SHORT" && cur_px >= pos$hedge_stop) { hedge_exit<-TRUE; hedge_reason<-"Hedge stop hit" }
    if(pos$hedge_dir=="LONG" && cur_px <= pos$hedge_stop) { hedge_exit<-TRUE; hedge_reason<-"Hedge stop hit" }
    if(hedge_pnl_raw > 0.01) { hedge_exit<-TRUE; hedge_reason<-"Hedge profit taken (1%+)" }
    if((now - pos$hedge_opened_at) > 3600) { hedge_exit<-TRUE; hedge_reason<-"Hedge time limit (1h)" }

    if(hedge_exit) {
      hedge_gross <- hedge_pnl_raw * pos$hedge_size_usd
      hedge_fees <- pos$hedge_size_usd * ROUND_TRIP
      hedge_net <- hedge_gross - hedge_fees
      trade <- list(dir=paste0("HEDGE-",pos$hedge_dir), entry=pos$hedge_entry, exit=cur_px,
                    size_usd=pos$hedge_size_usd, pnl_usd=round(hedge_net,2),
                    pnl_pct=round((hedge_net/pos$hedge_size_usd)*100,2),
                    fees=round(hedge_fees,2), reason=hedge_reason,
                    opened=pos$hedge_opened_at, closed=now,
                    duration_m=round((now - pos$hedge_opened_at)/60,1))
      pos$trade_log <- c(pos$trade_log, list(trade))
      save_trade_log(pos$trade_log)
      pos$realized_pnl <- pos$realized_pnl + hedge_net
      pos$hedge_active <- FALSE; pos$hedge_dir <- NA; pos$hedge_entry <- NA
      pos$hedge_size_usd <- NA; pos$hedge_size_coins <- NA
      pos$hedge_stop <- NA; pos$hedge_opened_at <- NA
    }
  }

  if(pos$state == "FLAT") {
    if(tplan$direction != "FLAT" && !is.na(tplan$entry_num) && !is.na(tplan$stop_num)) {
      sz <- calc_position_size(tplan$conviction, tplan$entry_num, tplan$stop_num)
      if(sz$usd > 0) {
        pos$state <- "PENDING"
        pos$pending_dir <- tplan$direction
        pos$pending_entry <- tplan$entry_num
        pos$pending_stop <- tplan$stop_num
        pos$pending_tp1 <- tplan$tp1_num
        pos$pending_tp2 <- tplan$tp2_num
        pos$pending_size_usd <- sz$usd
        pos$pending_size_coins <- sz$coins
        pos$pending_score <- tplan$score
        pos$pending_at <- now
        pos$pending_horizon_s <- horizon_to_seconds(tplan$horizon)
      }
    }
    return(pos)
  }

  if(pos$state == "PENDING") {
    age <- now - pos$pending_at
    if(age > 300) {
      pos$state <- "FLAT"; return(pos)
    }
    cur_score <- sig_result$score
    if(pos$pending_dir == "LONG" && cur_score < 10) {
      pos$state <- "FLAT"; return(pos)
    }
    if(pos$pending_dir == "SHORT" && cur_score > -10) {
      pos$state <- "FLAT"; return(pos)
    }
    filled <- FALSE
    if(pos$pending_dir == "LONG" && cur_px <= pos$pending_entry * 1.001) filled <- TRUE
    if(pos$pending_dir == "SHORT" && cur_px >= pos$pending_entry * 0.999) filled <- TRUE
    if(pos$pending_dir == "LONG" && cur_score > 40) filled <- TRUE
    if(pos$pending_dir == "SHORT" && cur_score < -40) filled <- TRUE
    if(filled) {
      pos$state <- "OPEN"
      pos$direction <- pos$pending_dir
      pos$entry_px <- cur_px
      pos$size_usd <- pos$pending_size_usd
      pos$size_coins <- pos$pending_size_coins
      pos$stop_px <- pos$pending_stop
      pos$tp1_px <- pos$pending_tp1
      pos$tp2_px <- pos$pending_tp2
      pos$opened_at <- now
      pos$horizon_end <- now + pos$pending_horizon_s
    }
    return(pos)
  }

  if(pos$state == "OPEN") {
    pnl <- calc_pnl(pos, cur_px)
    exit_reason <- NULL

    # Check 4h TWAP for longer-term structure
    lt_bullish <- !is.null(twaps) && !is.na(twaps$t240) && twaps$t240 > 0 && cur_px < twaps$t240
    lt_bearish <- !is.null(twaps) && !is.na(twaps$t240) && twaps$t240 > 0 && cur_px > twaps$t240

    if(pos$direction == "LONG") {
      if(cur_px <= pos$stop_px) exit_reason <- "STOP LOSS"
      if(!is.na(pos$tp2_px) && cur_px >= pos$tp2_px) exit_reason <- "TARGET 2 HIT"
      if(sig_result$score < -20) {
        if(lt_bullish && !isTRUE(pos$hedge_active) && is.null(exit_reason)) {
          hedge_sz <- pos$size_usd * 0.5
          pos$hedge_active <- TRUE; pos$hedge_dir <- "SHORT"
          pos$hedge_entry <- cur_px; pos$hedge_size_usd <- hedge_sz
          pos$hedge_size_coins <- round(hedge_sz/cur_px, 3)
          pos$hedge_stop <- cur_px * 1.008; pos$hedge_opened_at <- now
        } else if(!isTRUE(pos$hedge_active)) {
          exit_reason <- "SIGNAL FLIPPED BEARISH"
        }
      }
    } else {
      if(cur_px >= pos$stop_px) exit_reason <- "STOP LOSS"
      if(!is.na(pos$tp2_px) && cur_px <= pos$tp2_px) exit_reason <- "TARGET 2 HIT"
      if(sig_result$score > 20) {
        if(lt_bearish && !isTRUE(pos$hedge_active) && is.null(exit_reason)) {
          hedge_sz <- pos$size_usd * 0.5
          pos$hedge_active <- TRUE; pos$hedge_dir <- "LONG"
          pos$hedge_entry <- cur_px; pos$hedge_size_usd <- hedge_sz
          pos$hedge_size_coins <- round(hedge_sz/cur_px, 3)
          pos$hedge_stop <- cur_px * 0.992; pos$hedge_opened_at <- now
        } else if(!isTRUE(pos$hedge_active)) {
          exit_reason <- "SIGNAL FLIPPED BULLISH"
        }
      }
    }

    if(now > pos$horizon_end) exit_reason <- "HORIZON EXPIRED"
    if(pnl$net_pct < -3) exit_reason <- "MAX LOSS BREACHED"

    if(!is.null(exit_reason)) {
      trade <- list(dir=pos$direction, entry=pos$entry_px, exit=cur_px,
                    size_usd=pos$size_usd, pnl_usd=pnl$net_usd, pnl_pct=pnl$net_pct,
                    fees=pnl$fees, reason=exit_reason,
                    opened=pos$opened_at, closed=now,
                    duration_m=round((now - pos$opened_at)/60,1))
      pos$trade_log <- c(pos$trade_log, list(trade))
      save_trade_log(pos$trade_log)
      pos$realized_pnl <- pos$realized_pnl + pnl$net_usd
      pos$state <- "FLAT"
      pos$direction <- NA; pos$entry_px <- NA; pos$size_usd <- NA
      pos$size_coins <- NA; pos$stop_px <- NA; pos$tp1_px <- NA
      pos$tp2_px <- NA; pos$opened_at <- NA; pos$horizon_end <- NA
    }
    return(pos)
  }
  pos
}

load_trade_log <- function() {
  tryCatch({
    if(file.exists(TRADE_LOG_FILE)) readRDS(TRADE_LOG_FILE) else list()
  }, error=function(e) list())
}

save_trade_log <- function(log) {
  tryCatch(saveRDS(log, TRADE_LOG_FILE), error=function(e) NULL)
}

calc_performance <- function(trades, hours=NULL) {
  now <- as.numeric(Sys.time())
  if(!is.null(hours)) trades <- Filter(function(t) (now - t$closed) <= hours*3600, trades)
  n <- length(trades)
  if(n == 0) return(list(n=0, pnl=0, fees=0, wins=0, losses=0, win_rate=0,
                          avg_pnl=0, best=0, worst=0, avg_duration=0, expectancy=0))
  pnls <- sapply(trades, function(t) t$pnl_usd)
  fees <- sapply(trades, function(t) t$fees)
  durations <- sapply(trades, function(t) t$duration_m)
  wins <- sum(pnls > 0); losses <- sum(pnls <= 0)
  avg_win <- if(wins>0) mean(pnls[pnls>0]) else 0
  avg_loss <- if(losses>0) mean(abs(pnls[pnls<=0])) else 0
  win_rate <- if(n>0) wins/n else 0
  expectancy <- win_rate * avg_win - (1-win_rate) * avg_loss
  list(n=n, pnl=sum(pnls), fees=sum(fees), wins=wins, losses=losses,
       win_rate=round(win_rate*100,1), avg_pnl=round(mean(pnls),2),
       best=round(max(pnls),2), worst=round(min(pnls),2),
       avg_duration=round(mean(durations),1), expectancy=round(expectancy,2),
       pnl_pct=round(sum(pnls)/ACCOUNT_SIZE*100,3))
}

# =============================================================================
# SIGNAL DISPLAY HELPERS
# =============================================================================

sig_label<-function(s) {
  if(is.na(s)||is.null(s)) return("NO DATA")
  if(s>60)"STRONG BUY" else if(s>20)"BUY" else if(s>-20)"NEUTRAL" else if(s>-60)"SELL" else "STRONG SELL"
}

sig_bg<-function(s) {
  if(is.na(s)||is.null(s)) return("#f5f5f5")
  if(s>60)"#e8f8f0" else if(s>20)"#f0f8f4" else if(s>-20)"#f8f8f8"
  else if(s>-60)"#fdf4f0" else "#fdf0f0"
}

sig_col<-function(s) {
  if(is.na(s)||is.null(s)) return("#999")
  if(s>60)"#0a8754" else if(s>20)"#2d9d6a" else if(s>-20)"#666"
  else if(s>-60)"#c0522e" else "#b5291a"
}

conf_col<-function(c) if(c>=80)"#2d9d6a" else if(c>=50)"#c89620" else "#c0522e"

# =============================================================================
# THEME
# =============================================================================

light_theme <- bs_theme(
  version=5, bg="#ffffff", fg="#1a1a2e",
  primary="#2563eb", secondary="#f1f5f9", success="#16a34a",
  warning="#d97706", danger="#dc2626",
  "font-size-base"="0.875rem",
  "enable-rounded"=TRUE
)

CSS <- "
body { background:#f8f9fb !important; font-family: -apple-system, BlinkMacSystemFont, 'Inter', 'Segoe UI', sans-serif !important; color:#1a1a2e; }
.card,.bslib-card { background:#fff !important; border:1px solid #e5e7eb !important; border-radius:12px !important; box-shadow:0 1px 3px rgba(0,0,0,0.04); }

/* Signal hero */
.signal-hero { border-radius:16px 16px 0 0; padding:24px 24px 20px; text-align:center; transition:background 0.5s ease; }
.signal-score { font-size:64px; font-weight:800; line-height:1; letter-spacing:-3px; }
.signal-label { font-size:13px; font-weight:700; letter-spacing:2px; margin-top:4px; text-transform:uppercase; }

/* Score scale */
.score-scale { display:flex; justify-content:space-between; font-size:9px; color:#94a3b8; padding:0 4px; margin-top:2px; }

/* Gauge bar */
.gauge-track { height:8px; background:#e5e7eb; border-radius:4px; overflow:hidden; position:relative; margin-top:12px; }
.gauge-mid   { position:absolute; left:50%; top:0; bottom:0; width:1px; background:#cbd5e1; z-index:1; }
.gauge-fill  { height:100%; border-radius:4px; transition:width 0.5s ease; }

.signal-conf  { display:inline-block; padding:4px 14px; border-radius:20px; font-size:11px; font-weight:600; margin-top:10px; }

/* Component bars */
.comp-row { display:flex; align-items:center; padding:7px 12px; border-bottom:1px solid #f8f9fb; gap:10px; }
.comp-row:last-child { border-bottom:none; }
.comp-bar { width:70px; height:6px; background:#f1f5f9; border-radius:3px; overflow:hidden; flex-shrink:0; position:relative; }
.comp-bar-mid { position:absolute; left:50%; top:0; bottom:0; width:1px; background:#e5e7eb; }
.comp-bar-fill { height:100%; border-radius:3px; position:absolute; top:0; }
.comp-name { font-size:11px; color:#334155; flex:1; min-width:0; }
.comp-score { font-size:12px; font-weight:700; width:44px; text-align:right; flex-shrink:0; }
.comp-val { font-size:10px; color:#94a3b8; width:100px; text-align:right; flex-shrink:0; }

/* Trade plan */
.tp-action { font-size:28px; font-weight:800; letter-spacing:1px; line-height:1; }
.tp-reason { font-size:12px; color:#64748b; margin-top:6px; line-height:1.5; }
.tp-grid { display:grid; grid-template-columns:1fr 1fr; gap:12px; margin-top:16px; }
.tp-box { background:#f8f9fb; border-radius:8px; padding:12px 14px; }
.tp-box-lbl { font-size:10px; color:#94a3b8; text-transform:uppercase; letter-spacing:0.5px; font-weight:600; }
.tp-box-val { font-size:15px; font-weight:700; color:#1a1a2e; margin-top:2px; }
.tp-meta { display:flex; gap:16px; margin-top:14px; flex-wrap:wrap; }
.tp-meta-item { flex:1; min-width:100px; }
.tp-meta-lbl { font-size:10px; color:#94a3b8; text-transform:uppercase; letter-spacing:0.5px; }
.tp-meta-val { font-size:13px; font-weight:600; color:#1a1a2e; margin-top:2px; }
.tp-note { font-size:11px; color:#475569; padding:6px 0; border-bottom:1px solid #f1f5f9; line-height:1.4; }
.tp-note:last-child { border-bottom:none; }
.tp-invalidation { font-size:11px; color:#94a3b8; margin-top:12px; padding:10px 14px; background:#fef9f0; border-radius:8px; border-left:3px solid #d97706; }

/* Data rows */
.d-row { display:flex; justify-content:space-between; align-items:center; padding:6px 0; }
.d-lbl { font-size:11px; color:#94a3b8; text-transform:uppercase; letter-spacing:0.5px; }
.d-val { font-size:13px; font-weight:600; color:#1a1a2e; }

/* Section header */
.sh { font-size:10px; color:#94a3b8; font-weight:600; text-transform:uppercase; letter-spacing:1px;
      padding-bottom:6px; border-bottom:1px solid #f1f5f9; margin-bottom:8px; }

/* Status indicators */
.status-dot { width:6px; height:6px; border-radius:50%; display:inline-block; margin-right:4px; flex-shrink:0; }
.status-ok  { background:#16a34a; }
.status-warn{ background:#d97706; }
.status-err { background:#dc2626; }

/* Warning pills */
.warn-pill { display:inline-block; padding:3px 10px; background:#fef3cd; color:#856404; font-size:10px;
             border-radius:12px; margin:2px 4px 2px 0; font-weight:500; }
.gap-pill  { background:#f8d7da; color:#842029; }
"

# =============================================================================
# UI
# =============================================================================

ui <- page_fluid(
  theme = light_theme,
  tags$head(tags$style(HTML(CSS))),

  # ---- Header ----
  div(style="display:flex;align-items:baseline;justify-content:space-between;padding:16px 4px 8px;",
    div(style="display:flex;align-items:baseline;gap:10px;",
      tags$span(style="font-size:20px;font-weight:800;color:#1a1a2e;","$HYPE"),
      tags$span(style="font-size:13px;color:#94a3b8;font-weight:500;","Signal Terminal")
    ),
    div(style="display:flex;align-items:center;gap:12px;",
      div(style="font-size:11px;color:#94a3b8;", textOutput("ts",inline=TRUE)),
      actionButton("btn_refresh","Refresh",
                   style="font-size:11px;background:#f1f5f9;color:#64748b;border:1px solid #e5e7eb;padding:4px 12px;border-radius:6px;font-weight:500;")
    )
  ),

  # ---- Row 1: Signal + Trade Plan + Market Context ----
  fluidRow(
    # LEFT — signal score + prices
    column(3,
      div(class="card", style="padding:0; overflow:hidden;",
        uiOutput("signal_hero"),
        div(style="padding:0 16px 14px;",
          div(class="sh", style="margin-top:10px;", "Key Prices"),
          uiOutput("key_prices")
        )
      )
    ),

    # MIDDLE — trade plan + position
    column(5,
      div(class="card", style="padding:20px;",
        div(class="sh", "Trade Plan"),
        uiOutput("trade_plan")
      ),
      div(class="card", style="padding:16px; margin-top:8px;",
        div(class="sh", "Position Tracker ($100K Account)"),
        uiOutput("position_tracker")
      ),
      div(class="card", style="padding:16px; margin-top:8px;",
        div(class="sh", "Performance"),
        uiOutput("performance_panel")
      )
    ),

    # RIGHT — market context
    column(4,
      div(class="card", style="padding:16px;",
        div(class="sh", "Funding Rates"),
        uiOutput("funding_panel"),
        div(class="sh", style="margin-top:12px;", "Open Interest"),
        uiOutput("oi_panel"),
        div(class="sh", style="margin-top:12px;", "Orderbook"),
        uiOutput("ob_panel"),
        div(class="sh", style="margin-top:12px;", "Data Health"),
        uiOutput("health_summary")
      )
    )
  ),

  tags$div(style="height:8px;"),

  # ---- Row 2: Signal breakdown ----
  fluidRow(
    column(12,
      div(class="card", style="padding:16px;",
        div(class="sh", "Signal Breakdown — what's driving the score"),
        uiOutput("comp_list"),
        uiOutput("warn_pills")
      )
    )
  ),

  tags$div(style="font-size:10px;color:#cbd5e1;padding:8px 4px;","Score ranges from -100 (strong sell) to +100 (strong buy) · Public APIs · Refreshes every 15s")
)

# =============================================================================
# SERVER
# =============================================================================

server <- function(input, output, session) {

  rv <- reactiveValues(
    r_hl_ctx=list(ok=FALSE,data=NULL,fetched_at=NA,error_msg="init"),
    r_hl_spot=list(ok=FALSE,data=NULL,fetched_at=NA,error_msg="init"),
    r_bn_spot=list(ok=FALSE,data=NULL,fetched_at=NA,error_msg="init"),
    r_bn_perp=list(ok=FALSE,data=NULL,fetched_at=NA,error_msg="init"),
    r_bn_prem=list(ok=FALSE,data=NULL,fetched_at=NA,error_msg="init"),
    r_bybit=list(ok=FALSE,data=NULL,fetched_at=NA,error_msg="init"),
    r_hl_ob=list(ok=FALSE,data=NULL,fetched_at=NA,error_msg="init"),
    r_bn_ob=list(ok=FALSE,data=NULL,fetched_at=NA,error_msg="init"),
    r_bybit_ob=list(ok=FALSE,data=NULL,fetched_at=NA,error_msg="init"),
    r_hl_fund=list(ok=FALSE,data=NULL,fetched_at=NA,error_msg="init"),
    r_pred_fund=list(ok=FALSE,data=NULL,fetched_at=NA,error_msg="init"),
    r_hl_trades=list(ok=FALSE,data=NULL,fetched_at=NA,error_msg="init"),
    c_hl_5m=NULL, c_hl_1h=NULL, c_hl_4h=NULL,
    c_bn_5m=NULL, c_bn_1h=NULL,
    c_at=NA,
    prev_oi=NA_real_,
    last_upd=Sys.time(),
    pos=new_position()
  )

  t_price<-reactiveTimer(PRICE_MS)
  t_ob<-reactiveTimer(OB_MS)
  t_funding<-reactiveTimer(FUNDING_MS)
  t_candles<-reactiveTimer(CANDLE_MS)

  do_prices<-function() {
    old<-rv$r_hl_ctx
    if(old$ok&&!is.null(old$data)&&!is.na(old$data$oi)) rv$prev_oi<-old$data$oi
    rv$r_hl_ctx<-fetch_hl_perp_ctx(); rv$r_hl_spot<-fetch_hl_spot_price()
    rv$r_bn_spot<-fetch_bn_spot(); rv$r_bn_perp<-fetch_bn_perp()
    rv$r_bn_prem<-fetch_bn_premium_index(); rv$r_bybit<-fetch_bybit_perp()
    rv$r_hl_trades<-fetch_hl_trades(); rv$last_upd<-Sys.time()
  }
  do_ob<-function() {
    rv$r_hl_ob<-fetch_hl_orderbook(30); rv$r_bn_ob<-fetch_bn_perp_book(25)
    rv$r_bybit_ob<-fetch_bybit_perp_book(25)
  }
  do_funding<-function() {
    rv$r_hl_fund<-fetch_hl_funding_hist(60); rv$r_pred_fund<-fetch_hl_predicted_fundings()
  }
  do_candles<-function() {
    rv$c_hl_5m<-fetch_hl_candles("5m",300); rv$c_hl_1h<-fetch_hl_candles("1h",200)
    rv$c_hl_4h<-fetch_hl_candles("4h",200)
    rv$c_bn_5m<-fetch_bn_perp_candles("5m",300); rv$c_bn_1h<-fetch_bn_perp_candles("1h",200)
    rv$c_at<-now_s()
  }

  observeEvent(TRUE,{do_prices();do_ob();do_funding();do_candles()},once=TRUE)
  observe({t_price();isolate(do_prices())})
  observe({t_ob();isolate(do_ob())})
  observe({t_funding();isolate(do_funding())})
  observe({t_candles();isolate(do_candles())})
  observeEvent(input$btn_refresh,{do_prices();do_ob()})

  # ---- Derived ----
  signal_twaps<-reactive({
    df5<-if(!is.null(rv$c_hl_5m))rv$c_hl_5m$data else NULL
    df1h<-if(!is.null(rv$c_hl_1h))rv$c_hl_1h$data else NULL
    df4h<-if(!is.null(rv$c_hl_4h))rv$c_hl_4h$data else NULL
    list(t5=calc_twap(df5,5),t15=calc_twap(df5,15),t60=calc_twap(df1h,60),t240=calc_twap(df4h,60))
  })

  ob_result<-reactive(calc_ob_imbalance(list(hl=rv$r_hl_ob,bn=rv$r_bn_ob,bybit=rv$r_bybit_ob)))

  candle_gaps<-reactive({
    df<-if(!is.null(rv$c_hl_5m))rv$c_hl_5m$data else NULL
    detect_candle_gaps(df,"5m")
  })

  trade_flow<-reactive(calc_trade_flow(rv$r_hl_trades$data))

  sig<-reactive({
    build_signal(hl_ctx=rv$r_hl_ctx,hl_spot=rv$r_hl_spot,bn_perp=rv$r_bn_perp,
                 bn_prem=rv$r_bn_prem,bybit_perp=rv$r_bybit,
                 ob_result=ob_result(),twaps=signal_twaps(),fund_hist=rv$r_hl_fund,
                 pred_fundings=rv$r_pred_fund,candle_gaps=candle_gaps(),
                 trade_flow=trade_flow(),prev_oi=rv$prev_oi)
  })

  tplan<-reactive(build_trade_plan(sig(), rv$r_hl_ctx, ob_result()))

  observe({
    s <- sig(); tp <- tplan()
    px <- s$cur_px
    if(!is.na(px)) rv$pos <- update_position(rv$pos, s, tp, px, signal_twaps())
  })

  # =============================================================================
  # OUTPUTS
  # =============================================================================

  output$ts<-renderText(format(rv$last_upd,"Updated %H:%M:%S"))

  # ---- Signal hero ----
  output$signal_hero<-renderUI({
    s<-sig(); sc<-if(!is.null(s)) s$score else NA
    conf<-if(!is.null(s)) s$confidence else 0
    lb<-sig_label(sc); col<-sig_col(sc); bg<-sig_bg(sc)
    ccol<-conf_col(conf)
    pct<-(if(!is.na(sc)) sc+100 else 100)/2

    div(class="signal-hero", style=paste0("background:",bg,";"),
      div(class="signal-score",style=paste0("color:",col,";"),if(!is.na(sc)) sc else "—"),
      div(class="signal-label",style=paste0("color:",col,";"),lb),
      div(class="gauge-track",
        div(class="gauge-mid"),
        div(class="gauge-fill",style=paste0("width:",pct,"%;background:",col,";"))),
      div(class="score-scale", span("-100 Sell"), span("0"), span("+100 Buy")),
      div(class="signal-conf",style=paste0("background:",ccol,"18;color:",ccol,";border:1px solid ",ccol,"33;"),
          paste0("Confidence: ",conf,"%"))
    )
  })

  # ---- Key prices ----
  output$key_prices<-renderUI({
    ctx<-rv$r_hl_ctx$data; sp<-rv$r_hl_spot$data
    bn<-rv$r_bn_perp$data; by<-rv$r_bybit$data

    pr<-function(lbl,val,sub=NULL) {
      div(class="d-row",
        div(class="d-lbl",lbl),
        div(style="text-align:right;",
          div(class="d-val",val),
          if(!is.null(sub)) div(style="font-size:10px;color:#94a3b8;",sub)))
    }

    basis_str <- "—"
    if(!is.null(ctx)&&!is.null(sp)&&!is.na(ctx$mark_px)&&!is.na(sp$mark_px)&&sp$mark_px>0) {
      b<-(ctx$mark_px-sp$mark_px)/sp$mark_px
      basis_str<-paste0(ifelse(b>=0,"+",""),round(b*100,4),"%")
    }

    div(
      pr("HL Perp",  if(!is.null(ctx)) fp(ctx$mark_px) else "—"),
      pr("HL Spot",  if(!is.null(sp)) fp(sp$mark_px) else "—"),
      pr("BN Perp",  if(!is.null(bn)&&!is.na(bn$price)) fp(bn$price) else "—"),
      pr("Bybit",    if(!is.null(by)&&!is.na(by$price)) fp(by$price) else "—"),
      pr("Basis",    basis_str, "perp/spot")
    )
  })

  # ---- Trade Plan ----
  output$trade_plan<-renderUI({
    tp<-tplan()

    notes_ui <- if(length(tp$edge_notes)) {
      div(style="margin-top:14px;",
        div(style="font-size:10px;color:#94a3b8;font-weight:600;text-transform:uppercase;letter-spacing:0.5px;margin-bottom:6px;","Edge Notes"),
        lapply(tp$edge_notes, function(n) div(class="tp-note", n))
      )
    } else NULL

    div(
      div(class="tp-action", style=paste0("color:",tp$action_col,";"), tp$action),
      div(class="tp-reason", tp$reason),

      div(class="tp-grid",
        div(class="tp-box",
          div(class="tp-box-lbl", tp$lbl_entry),
          div(class="tp-box-val", tp$entry)),
        div(class="tp-box",
          div(class="tp-box-lbl", tp$lbl_stop),
          div(class="tp-box-val", style="color:#dc2626;", tp$stop)),
        div(class="tp-box",
          div(class="tp-box-lbl", tp$lbl_tp1),
          div(class="tp-box-val", style="color:#16a34a;", tp$tp1)),
        div(class="tp-box",
          div(class="tp-box-lbl", tp$lbl_tp2),
          div(class="tp-box-val", style="color:#16a34a;", tp$tp2))
      ),

      {
        sz <- if(!is.na(tp$entry_num) && !is.na(tp$stop_num) && tp$direction != "FLAT")
          calc_position_size(tp$conviction, tp$entry_num, tp$stop_num) else list(usd=0,coins=0)
        cost_rt <- round(sz$usd * ROUND_TRIP, 2)
        div(class="tp-meta",
          div(class="tp-meta-item",
            div(class="tp-meta-lbl","Horizon"),
            div(class="tp-meta-val", tp$horizon)),
          div(class="tp-meta-item",
            div(class="tp-meta-lbl","Conviction"),
            div(class="tp-meta-val", tp$conviction)),
          div(class="tp-meta-item",
            div(class="tp-meta-lbl","Size"),
            div(class="tp-meta-val", if(sz$usd>0) paste0("$",formatC(sz$usd,format="f",digits=0,big.mark=",")," / ",sz$coins," coins") else "No position")),
          div(class="tp-meta-item",
            div(class="tp-meta-lbl","Fees+Slip (RT)"),
            div(class="tp-meta-val", if(cost_rt>0) paste0("$",formatC(cost_rt,format="f",digits=2)," (",round(ROUND_TRIP*100,2),"%)") else "—"))
        )
      },

      notes_ui,

      if(tp$action!="WAIT"&&tp$action!="NO TRADE")
        div(class="tp-invalidation", paste0("Invalidation: ", tp$invalidation))
      else NULL
    )
  })

  # ---- Position tracker ----
  output$position_tracker<-renderUI({
    pos <- rv$pos
    cur_px <- sig()$cur_px

    state_col <- switch(pos$state, "FLAT"="#94a3b8", "PENDING"="#f59e0b", "OPEN"="#3b82f6", "#94a3b8")
    state_icon <- switch(pos$state, "FLAT"="—", "PENDING"="⏳", "OPEN"="▶", "—")

    header <- div(style="display:flex;align-items:center;justify-content:space-between;margin-bottom:10px;",
      div(style=paste0("font-size:13px;font-weight:700;color:",state_col,";"),
          paste(state_icon, pos$state, if(!is.na(pos$direction)) pos$direction else "")),
      div(style="font-size:11px;color:#94a3b8;",
          paste0("Account: $",formatC(ACCOUNT_SIZE + pos$realized_pnl, format="f", digits=2, big.mark=",")))
    )

    body <- if(pos$state == "PENDING") {
      ttl <- max(0, round(300 - (as.numeric(Sys.time()) - pos$pending_at)))
      div(
        div(style="font-size:11px;color:#64748b;margin-bottom:6px;",
            paste0("Waiting for fill — ", pos$pending_dir, " @ $", fp(pos$pending_entry))),
        div(class="tp-grid",
          div(class="tp-box", div(class="tp-box-lbl","Entry"), div(class="tp-box-val", fp(pos$pending_entry))),
          div(class="tp-box", div(class="tp-box-lbl","Size"), div(class="tp-box-val", paste0("$",pos$pending_size_usd))),
          div(class="tp-box", div(class="tp-box-lbl","Stop"), div(class="tp-box-val",style="color:#dc2626;", fp(pos$pending_stop))),
          div(class="tp-box", div(class="tp-box-lbl","Expires"), div(class="tp-box-val", paste0(ttl,"s")))
        ),
        div(style="font-size:10px;color:#94a3b8;margin-top:4px;",
            paste0("Score at signal: ", round(pos$pending_score,1), " | Will cancel if signal flips or 5min expires"))
      )
    } else if(pos$state == "OPEN") {
      pnl <- calc_pnl(pos, cur_px)
      elapsed <- round((as.numeric(Sys.time()) - pos$opened_at) / 60, 0)
      remaining <- max(0, round((pos$horizon_end - as.numeric(Sys.time())) / 60, 0))
      pnl_col <- if(pnl$net_usd >= 0) "#16a34a" else "#dc2626"
      tp1_hit <- if(pos$direction=="LONG") cur_px >= pos$tp1_px else cur_px <= pos$tp1_px
      div(
        div(style="display:flex;justify-content:space-between;align-items:baseline;",
          div(style="font-size:12px;font-weight:600;color:#334155;",
              paste0(pos$direction, " ", pos$size_coins, " HYPE @ $", fp(pos$entry_px))),
          div(style=paste0("font-size:16px;font-weight:700;color:",pnl_col,";"),
              paste0(ifelse(pnl$net_usd>=0,"+",""), "$", formatC(abs(pnl$net_usd),format="f",digits=2),
                     " (", ifelse(pnl$net_pct>=0,"+",""), pnl$net_pct, "%)"))
        ),
        div(class="tp-grid", style="margin-top:8px;",
          div(class="tp-box", div(class="tp-box-lbl","Current"), div(class="tp-box-val", fp(cur_px))),
          div(class="tp-box", div(class="tp-box-lbl","Stop"), div(class="tp-box-val",style="color:#dc2626;", fp(pos$stop_px))),
          div(class="tp-box", div(class="tp-box-lbl", if(tp1_hit)"T1 ✓" else "Target 1"),
              div(class="tp-box-val",style=paste0("color:",if(tp1_hit)"#16a34a" else "#64748b",";"), fp(pos$tp1_px))),
          div(class="tp-box", div(class="tp-box-lbl","Target 2"), div(class="tp-box-val",style="color:#16a34a;", fp(pos$tp2_px)))
        ),
        div(style="display:flex;justify-content:space-between;margin-top:6px;font-size:10px;color:#94a3b8;",
          span(paste0("Fees: $", pnl$fees)),
          span(paste0(elapsed, "min in | ", remaining, "min left")),
          span(paste0("Gross: $", formatC(pnl$usd,format="f",digits=2)))
        ),
        if(isTRUE(pos$hedge_active)) {
          h_raw <- if(pos$hedge_dir=="SHORT") (pos$hedge_entry-cur_px)/pos$hedge_entry
                   else (cur_px-pos$hedge_entry)/pos$hedge_entry
          h_net <- round(h_raw*pos$hedge_size_usd - pos$hedge_size_usd*ROUND_TRIP, 2)
          h_col <- if(h_net>=0) "#16a34a" else "#dc2626"
          h_age <- round((as.numeric(Sys.time()) - pos$hedge_opened_at)/60, 0)
          div(style="margin-top:8px;padding:8px;background:#fef3c7;border-radius:6px;border:1px solid #fbbf24;",
            div(style="font-size:10px;font-weight:700;color:#92400e;margin-bottom:4px;",
                paste0("⚡ HEDGE ACTIVE — ", pos$hedge_dir, " ", pos$hedge_size_coins, " HYPE @ $", fp(pos$hedge_entry))),
            div(style="display:flex;justify-content:space-between;font-size:10px;",
              span(style=paste0("font-weight:600;color:",h_col,";"),
                   paste0("P&L: ",ifelse(h_net>=0,"+",""),"$",formatC(abs(h_net),format="f",digits=2))),
              span(style="color:#92400e;", paste0("Stop: $",fp(pos$hedge_stop))),
              span(style="color:#92400e;", paste0(h_age,"min | max 60min"))
            ),
            div(style="font-size:9px;color:#a16207;margin-top:3px;",
                "Temporary hedge — closes when signal realigns with primary position")
          )
        } else NULL
      )
    } else {
      div(style="font-size:11px;color:#94a3b8;", "No active position — waiting for signal alignment")
    }

    trade_log_ui <- if(length(pos$trade_log) > 0) {
      recent <- rev(pos$trade_log)
      if(length(recent) > 5) recent <- recent[1:5]
      rows <- lapply(recent, function(t) {
        col <- if(t$pnl_usd >= 0) "#16a34a" else "#dc2626"
        div(style="display:flex;justify-content:space-between;align-items:center;padding:4px 0;border-bottom:1px solid #f1f5f9;font-size:10px;",
          span(style="color:#64748b;width:50px;", t$dir),
          span(style="color:#334155;", paste0("$",fp(t$entry)," → $",fp(t$exit))),
          span(style=paste0("font-weight:600;color:",col,";"),
               paste0(ifelse(t$pnl_usd>=0,"+",""),"$",formatC(abs(t$pnl_usd),format="f",digits=2))),
          span(style="color:#94a3b8;width:60px;text-align:right;", paste0(t$duration_m,"m | ",t$reason))
        )
      })
      div(style="margin-top:10px;border-top:1px solid #e2e8f0;padding-top:8px;",
        div(style="font-size:10px;color:#94a3b8;font-weight:600;text-transform:uppercase;letter-spacing:0.5px;margin-bottom:4px;",
            paste0("Trade Log (P&L: $", formatC(pos$realized_pnl,format="f",digits=2), ")")),
        do.call(div, rows)
      )
    } else NULL

    tagList(header, body, trade_log_ui)
  })

  # ---- Performance panel ----
  output$performance_panel<-renderUI({
    pos <- rv$pos
    trades <- pos$trade_log
    if(length(trades) == 0) return(div(style="font-size:11px;color:#94a3b8;padding:8px 0;",
      "No completed trades yet — performance will appear here"))

    horizons <- list(
      list(label="1H", hours=1), list(label="4H", hours=4),
      list(label="24H", hours=24), list(label="7D", hours=168),
      list(label="ALL", hours=NULL)
    )

    perf_row <- function(h) {
      p <- calc_performance(trades, h$hours)
      if(p$n == 0) return(NULL)
      pnl_col <- if(p$pnl >= 0) "#16a34a" else "#dc2626"
      div(style="display:flex;align-items:center;justify-content:space-between;padding:5px 0;border-bottom:1px solid #f1f5f9;font-size:11px;",
        span(style="font-weight:600;color:#334155;width:35px;", h$label),
        span(style="color:#64748b;", paste0(p$n," trades")),
        span(style="color:#64748b;", paste0(p$win_rate,"% win")),
        span(style=paste0("font-weight:600;color:",pnl_col,";"),
             paste0(ifelse(p$pnl>=0,"+",""),"$",formatC(abs(p$pnl),format="f",digits=2,big.mark=","))),
        span(style=paste0("color:",pnl_col,";"), paste0(ifelse(p$pnl_pct>=0,"+",""),p$pnl_pct,"%")),
        span(style="color:#94a3b8;", paste0("avg ",p$avg_duration,"m"))
      )
    }

    rows <- Filter(Negate(is.null), lapply(horizons, perf_row))
    if(length(rows) == 0) return(div(style="font-size:11px;color:#94a3b8;", "No trades in selected horizons"))

    all_perf <- calc_performance(trades, NULL)
    summary <- div(style="margin-top:8px;padding-top:8px;border-top:1px solid #e2e8f0;font-size:10px;color:#64748b;display:flex;justify-content:space-between;",
      span(paste0("Best: +$",all_perf$best)),
      span(paste0("Worst: $",all_perf$worst)),
      span(paste0("Fees paid: $",formatC(all_perf$fees,format="f",digits=2))),
      span(paste0("Expectancy: $",all_perf$expectancy,"/trade"))
    )

    equity <- ACCOUNT_SIZE + pos$realized_pnl
    equity_col <- if(pos$realized_pnl >= 0) "#16a34a" else "#dc2626"
    eq_bar <- div(style="display:flex;justify-content:space-between;align-items:baseline;margin-bottom:8px;",
      div(style="font-size:12px;color:#334155;font-weight:600;",
          paste0("Equity: $",formatC(equity,format="f",digits=2,big.mark=","))),
      div(style=paste0("font-size:11px;font-weight:600;color:",equity_col,";"),
          paste0(ifelse(pos$realized_pnl>=0,"+",""),"$",formatC(abs(pos$realized_pnl),format="f",digits=2),
                 " (",round(pos$realized_pnl/ACCOUNT_SIZE*100,3),"%)"))
    )

    tagList(eq_bar, do.call(div, rows), summary)
  })

  # ---- Signal component list ----
  output$comp_list<-renderUI({
    s<-sig()
    if(is.null(s)||nrow(s$components)==0)
      return(div(style="color:#94a3b8;padding:20px;text-align:center;","Waiting for data..."))

    df<-s$components
    rows<-lapply(seq_len(nrow(df)), function(i) {
      sc<-df$Score[i]; if(is.na(sc)) sc<-0
      col<-if(sc>=10)"#16a34a" else if(sc<=-10)"#dc2626" else "#64748b"
      bar_pct<-max(0,min(100,(sc+100)/2))
      bar_left<-if(sc>=0) "50%" else paste0(bar_pct,"%")
      bar_width<-if(sc>=0) paste0(bar_pct-50,"%") else paste0(50-bar_pct,"%")

      div(class="comp-row",
        div(class="comp-bar",
          div(class="comp-bar-mid"),
          div(class="comp-bar-fill",style=paste0("background:",col,";left:",bar_left,";width:",bar_width,";"))),
        div(class="comp-name",df$Signal[i]),
        div(class="comp-val",df$Value[i]),
        div(class="comp-score",style=paste0("color:",col,";"),
            paste0(ifelse(sc>=0,"+",""),sc))
      )
    })
    do.call(div,rows)
  })

  output$warn_pills<-renderUI({
    s<-sig()
    if(is.null(s)||!length(s$warnings)) return(NULL)
    tags$div(style="margin-top:10px;padding-top:8px;border-top:1px solid #f1f5f9;",
      lapply(s$warnings, function(w) {
        cl<-if(grepl("GAP|gap",w))"gap-pill" else "warn-pill"
        tags$span(class=cl,w)
      })
    )
  })

  # ---- Funding panel ----
  output$funding_panel<-renderUI({
    ctx<-rv$r_hl_ctx$data; bn<-rv$r_bn_prem$data; by<-rv$r_bybit$data

    fr_row<-function(lbl,fr,note=NULL) {
      if(is.null(fr)||is.na(fr)) return(div(class="d-row",div(class="d-lbl",lbl),div(class="d-val","—")))
      col<-if(fr>0.0001)"#dc2626" else if(fr< -0.0001)"#16a34a" else "#64748b"
      apr<-round(fr*3*365*100,1)
      div(class="d-row",
        div(class="d-lbl",lbl),
        div(style="text-align:right;",
          div(class="d-val",style=paste0("color:",col,";"),
              paste0(ifelse(fr>0,"+",""),round(fr*100,4),"%")),
          div(style="font-size:10px;color:#94a3b8;",paste0(apr,"% APR",if(!is.null(note)) paste0(" · ",note) else ""))))
    }

    div(
      fr_row("Hyperliquid", if(!is.null(ctx)) ctx$funding else NULL),
      fr_row("Binance",     if(!is.null(bn)) bn$funding_rate else NULL, "indicative"),
      fr_row("Bybit",       if(!is.null(by)) by$funding_rate else NULL)
    )
  })

  # ---- OI panel ----
  output$oi_panel<-renderUI({
    ctx<-rv$r_hl_ctx$data; bn<-rv$r_bn_perp$data; by<-rv$r_bybit$data

    oi_row<-function(lbl,val) div(class="d-row",div(class="d-lbl",lbl),div(class="d-val",val))

    hl_usd<-if(!is.null(ctx)&&!is.na(ctx$oi)&&!is.na(ctx$mark_px)) fbig(ctx$oi*ctx$mark_px) else "—"
    bn_usd<-if(!is.null(bn)&&!is.na(bn$oi_base)&&!is.na(bn$price)) fbig(bn$oi_base*bn$price) else "—"
    by_usd<-if(!is.null(by)&&!is.na(by$oi_usd)) fbig(by$oi_usd) else "—"

    div(oi_row("Hyperliquid",hl_usd), oi_row("Binance",bn_usd), oi_row("Bybit",by_usd))
  })

  # ---- OB panel ----
  output$ob_panel<-renderUI({
    ob<-ob_result()
    if(is.na(ob$imb)) return(div(class="d-row",div(class="d-lbl","Status"),div(class="d-val","Unavailable")))
    bid_pct<-round(ob$imb*100,1); ask_pct<-round((1-ob$imb)*100,1)
    col<-if(ob$imb>0.55)"#16a34a" else if(ob$imb<0.45)"#dc2626" else "#64748b"

    div(
      div(class="d-row",div(class="d-lbl","Venues"),div(class="d-val",paste0(ob$n_venues,"/3"))),
      div(class="d-row",
        div(class="d-lbl","Imbalance"),
        div(class="d-val",style=paste0("color:",col,";"),paste0(bid_pct,"% bids"))),
      div(style="margin-top:4px;",
        div(style="display:flex;justify-content:space-between;font-size:9px;color:#94a3b8;margin-bottom:2px;",
            span(paste0("Bid ",bid_pct,"%")), span(paste0("Ask ",ask_pct,"%"))),
        div(style="height:5px;background:#f1f5f9;border-radius:3px;overflow:hidden;",
          div(style=paste0("width:",bid_pct,"%;height:100%;background:",col,";border-radius:3px;"))))
    )
  })

  # ---- Health summary ----
  output$health_summary<-renderUI({
    feeds<-list(
      list(lbl="HL Perp",r=rv$r_hl_ctx), list(lbl="HL Spot",r=rv$r_hl_spot),
      list(lbl="BN Perp",r=rv$r_bn_perp), list(lbl="BN PremIdx",r=rv$r_bn_prem),
      list(lbl="Bybit",r=rv$r_bybit), list(lbl="HL OB",r=rv$r_hl_ob),
      list(lbl="BN OB",r=rv$r_bn_ob), list(lbl="Bybit OB",r=rv$r_bybit_ob),
      list(lbl="HL Fund",r=rv$r_hl_fund), list(lbl="Pred Fund",r=rv$r_pred_fund),
      list(lbl="HL Trades",r=rv$r_hl_trades)
    )

    n_ok<-sum(sapply(feeds,function(f)isTRUE(f$r$ok)))
    n_total<-length(feeds)

    dots<-lapply(feeds, function(f) {
      ok<-isTRUE(f$r$ok); fat<-f$r$fetched_at
      cls<-if(!ok)"status-err" else { s<-now_s()-fat; if(s<STALE_WARN_S)"status-ok" else if(s<STALE_ERR_S)"status-warn" else "status-err" }
      tags$span(title=paste0(f$lbl,": ",if(ok)age_str(fat) else f$r$error_msg),
                class=paste("status-dot",cls), style="cursor:help;")
    })

    div(
      div(style="display:flex;align-items:center;gap:4px;flex-wrap:wrap;", dots),
      div(style="font-size:10px;color:#94a3b8;margin-top:4px;",
          paste0(n_ok,"/",n_total," feeds live · ",age_str(as.numeric(rv$last_upd))))
    )
  })
}

shinyApp(ui=ui,server=server)
