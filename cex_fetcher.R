# CEX Data Fetcher — runs via GitHub Actions every 5 min
# Fetches Binance + Bybit data and saves as .rds cache

library(httr)
library(jsonlite)

options(warn = -1, scipen = 999)

safe_get <- function(url, query = NULL) {
  tryCatch({
    r <- GET(url, query = query, timeout(10))
    if (status_code(r) == 200) fromJSON(content(r, "text", encoding = "UTF-8"), simplifyVector = FALSE)
    else { cat("  HTTP", status_code(r), "from", url, "\n"); NULL }
  }, error = function(e) { cat("  Error:", conditionMessage(e), "\n"); NULL })
}

cat("=== CEX Fetcher starting at", format(Sys.time(), "%Y-%m-%d %H:%M:%S UTC"), "===\n")

result <- list(fetched_at = as.numeric(Sys.time()))

# --- Binance ---
cat("Fetching Binance perp ticker...\n")
bn_ticker <- safe_get("https://fapi.binance.com/fapi/v1/ticker/24hr", list(symbol = "HYPEUSDT"))
if (!is.null(bn_ticker)) {
  result$bn_perp <- list(
    price = as.numeric(bn_ticker$lastPrice),
    change_pct = as.numeric(bn_ticker$priceChangePercent),
    volume = as.numeric(bn_ticker$quoteVolume)
  )
  cat("  BN perp price:", result$bn_perp$price, "\n")
}

cat("Fetching Binance OI...\n")
bn_oi <- safe_get("https://fapi.binance.com/fapi/v1/openInterest", list(symbol = "HYPEUSDT"))
if (!is.null(bn_oi)) {
  result$bn_oi_base <- as.numeric(bn_oi$openInterest)
  cat("  BN OI (base):", result$bn_oi_base, "\n")
}

cat("Fetching Binance premium index...\n")
bn_prem <- safe_get("https://fapi.binance.com/fapi/v1/premiumIndex", list(symbol = "HYPEUSDT"))
if (!is.null(bn_prem)) {
  result$bn_prem <- list(
    mark_px = as.numeric(bn_prem$markPrice),
    index_px = as.numeric(bn_prem$indexPrice),
    funding_rate = as.numeric(bn_prem$lastFundingRate),
    next_fund_time = as.numeric(bn_prem$nextFundingTime)
  )
  cat("  BN funding rate:", result$bn_prem$funding_rate, "\n")
}

cat("Fetching Binance orderbook...\n")
bn_ob <- safe_get("https://fapi.binance.com/fapi/v1/depth", list(symbol = "HYPEUSDT", limit = 20))
if (!is.null(bn_ob)) {
  result$bn_ob <- list(
    bids = data.frame(
      price = as.numeric(sapply(bn_ob$bids, `[[`, 1)),
      size = as.numeric(sapply(bn_ob$bids, `[[`, 2)),
      side = "bid", stringsAsFactors = FALSE),
    asks = data.frame(
      price = as.numeric(sapply(bn_ob$asks, `[[`, 1)),
      size = as.numeric(sapply(bn_ob$asks, `[[`, 2)),
      side = "ask", stringsAsFactors = FALSE),
    n_bid_levels = length(bn_ob$bids),
    n_ask_levels = length(bn_ob$asks)
  )
  cat("  BN OB levels:", length(bn_ob$bids), "bids,", length(bn_ob$asks), "asks\n")
}

cat("Skipping Binance spot — HYPE not listed on BN spot\n")

# --- Bybit ---
cat("Fetching Bybit perp ticker...\n")
by_ticker <- safe_get("https://api.bybit.com/v5/market/tickers", list(category = "linear", symbol = "HYPEUSDT"))
if (!is.null(by_ticker) && length(by_ticker$result$list)) {
  it <- by_ticker$result$list[[1]]
  result$bybit_perp <- list(
    price = suppressWarnings(as.numeric(it$lastPrice)),
    change_pct = suppressWarnings(as.numeric(it$price24hPcnt)),
    oi_usd = suppressWarnings(as.numeric(it$openInterestValue)),
    funding_rate = suppressWarnings(as.numeric(it$fundingRate))
  )
  cat("  Bybit price:", result$bybit_perp$price, "funding:", result$bybit_perp$funding_rate, "\n")
}

cat("Fetching Bybit orderbook...\n")
by_ob <- safe_get("https://api.bybit.com/v5/market/orderbook", list(category = "linear", symbol = "HYPEUSDT", limit = 25))
if (!is.null(by_ob) && !is.null(by_ob$result)) {
  b <- by_ob$result$b; a <- by_ob$result$a
  if (length(b) && length(a)) {
    result$bybit_ob <- list(
      bids = data.frame(
        price = as.numeric(sapply(b, `[[`, 1)),
        size = as.numeric(sapply(b, `[[`, 2)),
        side = "bid", stringsAsFactors = FALSE),
      asks = data.frame(
        price = as.numeric(sapply(a, `[[`, 1)),
        size = as.numeric(sapply(a, `[[`, 2)),
        side = "ask", stringsAsFactors = FALSE),
      n_bid_levels = length(b),
      n_ask_levels = length(a)
    )
    cat("  Bybit OB levels:", length(b), "bids,", length(a), "asks\n")
  }
}

# --- Save ---
out_path <- file.path("data", "cex_cache.rds")
saveRDS(result, out_path)
cat("\nSaved to", out_path, "- size:", file.size(out_path), "bytes\n")
cat("Fields:", paste(names(result), collapse = ", "), "\n")
cat("=== Done ===\n")
