# Binocular Telemetry, Logging & Notifications

## Architecture: Self-Contained Backend

```
┌──────────────────────────────────────────────┐
│  Binocular Daemon                            │
│                                              │
│  otel4s metrics → in-memory ring buffer      │
│                        ↓                     │
│                   HTTP JSON API (/metrics)    │
│                        ↓                     │
│  Notifications → Discord/Telegram/Email      │
└──────────────────┬───────────────────────────┘
                   │
                   ▼
         Grafana (Infinity plugin)
         queries your JSON API directly
```

No Prometheus, no Mimir, no external TSDB. The app owns everything.

## Logging: Scribe + scribe-cats

Replace Logback + scala-logging with Scribe for Scala-native, cats-effect compatible logging.

| Feature         | Scribe                        | Logback                   |
| --------------- | ----------------------------- | ------------------------- |
| Colored console | Built-in, beautiful           | Via `%highlight` patterns |
| File rotation   | Built-in `FileWriter`         | `RollingFileAppender`     |
| Config          | Programmatic Scala            | XML (logback.xml)         |
| Cats Effect     | `scribe-cats` module          | `log4cats-slf4j` adapter  |
| Performance     | Fastest (compile-time macros) | Good                      |

Keep `Console.scala` for CLI UX (progress bars, in-place updates). Layer Scribe underneath for
structured file logging. Daemon mode (`run` command) gets both: pretty console + rotated log files.

```scala
"com.outr" %% "scribe" % "3.19.0"
"com.outr" %% "scribe-cats" % "3.19.0"
```

Remove: `logback-classic`, `scala-logging`.

## Metrics: otel4s → Ring Buffer → JSON API → Grafana

### How It Works

1. **otel4s** instruments the code (counters, gauges, histograms)
2. A custom `MetricExporter` writes to an **in-memory ring buffer** (last N hours, or SQLite for
   persistence across restarts)
3. The app exposes `GET /api/metrics?metric=X&from=T1&to=T2` returning JSON time-series
4. **Grafana Infinity plugin** queries the API directly — no Prometheus needed
5. Grafana provides dashboards + optional alerting

### Key Metrics

| Metric                                   | Type      | Description                                      |
| ---------------------------------------- | --------- | ------------------------------------------------ |
| `binocular_oracle_confirmed_height`      | Gauge     | Current confirmed Bitcoin block height            |
| `binocular_oracle_forks_tree_size`       | Gauge     | Number of blocks in forks tree                    |
| `binocular_oracle_last_update_epoch`     | Gauge     | Unix timestamp of last successful update          |
| `binocular_oracle_tx_submitted_total`    | Counter   | Total transactions submitted                      |
| `binocular_oracle_tx_failed_total`       | Counter   | Failed transaction count                          |
| `binocular_oracle_blocks_promoted_total` | Counter   | Total blocks promoted to confirmed                |
| `binocular_oracle_headers_added_total`   | Counter   | Total headers added to forks tree                 |
| `binocular_oracle_bitcoin_tip_height`    | Gauge     | Latest block height from Bitcoin RPC              |
| `binocular_oracle_lag_blocks`            | Gauge     | Bitcoin tip height minus oracle confirmed height  |
| `binocular_oracle_loop_duration_seconds` | Histogram | Duration of each run loop iteration               |

### Where Metrics Are Updated

- **`confirmed_height`, `forks_tree_size`, `last_update_epoch`** — after each successful tx
- **`tx_submitted_total`** — incremented on every tx submission attempt
- **`tx_failed_total`** — incremented on tx failure (before retry sleep)
- **`blocks_promoted_total`** — incremented by count of promoted blocks per tx
- **`headers_added_total`** — incremented by count of headers added per tx
- **`bitcoin_tip_height`** — updated on every poll of Bitcoin RPC
- **`lag_blocks`** — recomputed as `bitcoin_tip_height - confirmed_height` after each poll
- **`loop_duration_seconds`** — observed once per main loop iteration

### Dependencies

```scala
"org.typelevel" %% "otel4s-oteljava" % "0.11.2"
```

### Grafana Setup

Deploy Grafana as a container, install the Infinity datasource plugin. Configure it to query the
app's JSON API endpoint. No Prometheus, no Loki, no collectors needed.

### Grafana Dashboard (Suggested Panels)

1. **Oracle Height vs Bitcoin Tip** — time series of `confirmed_height` and `bitcoin_tip_height`
2. **Block Lag** — time series of `lag_blocks`
3. **Tx Success/Failure Rate** — stacked rate graph of `tx_submitted_total` vs `tx_failed_total`
4. **Blocks Promoted** — rate of `blocks_promoted_total`
5. **Headers Added** — rate of `headers_added_total`
6. **Loop Duration** — histogram quantiles of `loop_duration_seconds`
7. **Last Update Age** — `time() - last_update_epoch` as single stat

### Docker Health Check

The metrics HTTP server doubles as a health check endpoint:

```dockerfile
HEALTHCHECK --interval=30s --timeout=5s --retries=3 \
  CMD wget -q --spider http://localhost:9090/metrics || exit 1
```

## Notifications

### App-Level vs Grafana Alerting

|                      | App-Level                                  | Grafana Alerting               |
| -------------------- | ------------------------------------------ | ------------------------------ |
| **Latency**          | Instant (event-driven)                     | 1min+ eval intervals           |
| **Rich context**     | Full app context (tx hashes, fork details) | Only what's in metrics         |
| **Reliability**      | Works if Grafana is down                   | Requires Grafana healthy       |
| **Config**           | Version-controlled in code                 | Grafana UI (harder to version) |
| **Threshold alerts** | Must implement yourself                    | Built-in, visual config        |
| **Deduplication**    | Must implement yourself                    | Built-in                       |
| **Setup**            | ~100 lines of code                         | Zero code, UI config           |

**Decision: App-level notifications**, with Grafana alerting as optional addition later.

For a single daemon, app-level is more reliable and gives richer context. Critical alerts (reorg
detected, submission failed, oracle stalled) are **events**, not metric thresholds — the app knows
about them immediately.

### Implementation: HTTP Webhooks

No special library needed — all three channels are simple HTTP POSTs:

| Channel      | How                                               | Dependency                      |
| ------------ | ------------------------------------------------- | ------------------------------- |
| **Discord**  | POST JSON to webhook URL                          | Existing HTTP client            |
| **Telegram** | POST to `api.telegram.org/bot{token}/sendMessage` | Existing HTTP client            |
| **Email**    | `emil` library (Cats Effect native SMTP)          | `com.github.eikek::emil-common` |

Discord + Telegram = ~50 lines total using http4s-client or sttp (existing HTTP clients from
Bitcoin RPC). No bot frameworks needed for one-way alerts.

If interactive Telegram commands are needed later (`/status`, `/forcecheck`), upgrade to
**telegramium** (Scala 3, Cats Effect native).

### Notification Service Interface

```scala
trait NotificationService[F[_]]:
  def notify(message: String, severity: Severity): F[Unit]

enum Severity:
  case Info, Warning, Critical
```

### Alerting Examples

Example alert conditions for production monitoring.

#### Oracle Falling Behind

- **Condition:** `lag_blocks > 200` for 30 minutes
- **Severity:** Warning
- **Message:** "Binocular oracle is {lag} blocks behind Bitcoin tip"

#### Oracle Stuck (No Updates)

- **Condition:** `time() - last_update_epoch > 3600` for 5 minutes
- **Severity:** Critical
- **Message:** "Binocular oracle has not updated in {duration}"

#### Sustained Transaction Failures

- **Condition:** tx failure rate above threshold for 15 minutes
- **Severity:** Warning
- **Message:** "Binocular oracle experiencing sustained tx failures"

#### Reorg Detected

- **Condition:** App detects Bitcoin reorg event
- **Severity:** Critical
- **Message:** "Bitcoin reorg detected: depth={depth}, old tip={hash}, new tip={hash}"

### Email Dependency (optional)

```scala
"com.github.eikek" %% "emil-common" % "0.17.0"
```

## Full Dependency Summary

```scala
// Logging
"com.outr" %% "scribe" % "3.19.0"
"com.outr" %% "scribe-cats" % "3.19.0"

// Metrics
"org.typelevel" %% "otel4s-oteljava" % "0.11.2"

// Email (optional)
"com.github.eikek" %% "emil-common" % "0.17.0"

// Remove:
// "ch.qos.logback" % "logback-classic"
// "com.typesafe.scala-logging" %% "scala-logging"
```

## External Services

Just Grafana (one container). No Prometheus, no Loki, no collectors.

## Implementation Order

1. Swap logging: replace logback/scala-logging with Scribe
2. Add metrics ring buffer + JSON API endpoint
3. Add notification service (Discord/Telegram webhooks)
4. Deploy Grafana with Infinity plugin, build dashboards
5. (Optional) Add Grafana alerting for threshold-based alerts
6. (Optional) Add email notifications via emil
7. (Optional) Add interactive Telegram bot via telegramium
