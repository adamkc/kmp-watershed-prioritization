# Capture screenshots for the one-minute workflow tour (docs/plots/workflow_tour*).
# Starts the app locally, drives it in headless Chrome (chromote), and saves one
# screenshot per step plus element boxes to <frames_dir>/frames.json.
# Run from the project root: Rscript scripts/tour_capture.R <frames_dir>
# Then build the media with scripts/tour_compose.R.
suppressMessages({ library(chromote); library(jsonlite) })

proj <- normalizePath(getwd(), winslash = "/")   # run from the project root
out  <- commandArgs(TRUE)[1]
dir.create(out, showWarnings = FALSE, recursive = TRUE)
port <- 8766L; W <- 1440L; H <- 900L

app <- processx::process$new(
  file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"),
  c("-e", sprintf("shiny::runApp('.', port=%d, launch.browser=FALSE)", port)),
  wd = proj, stdout = file.path(out, "app_stdout.log"),
  stderr = file.path(out, "app_stderr.log"))
on.exit(try(app$kill(), silent = TRUE), add = TRUE)
url <- sprintf("http://127.0.0.1:%d", port)
for (i in 1:90) {
  ok <- tryCatch({ suppressWarnings(readLines(url, n = 1)); TRUE }, error = function(e) FALSE)
  if (ok) break; Sys.sleep(1)
}
if (!ok) stop("app did not start")

b <- ChromoteSession$new(width = W, height = H)
b$Emulation$setDeviceMetricsOverride(width = W, height = H, deviceScaleFactor = 1, mobile = FALSE)
js <- function(code) b$Runtime$evaluate(expression = code, returnByValue = TRUE,
                                        awaitPromise = TRUE)$result$value
q <- function(s) toJSON(s, auto_unbox = TRUE)
poll <- function(expr, timeout = 30, every = 0.25) {
  t0 <- Sys.time()
  repeat {
    if (isTRUE(tryCatch(js(expr), error = function(e) FALSE))) return(TRUE)
    if (as.numeric(Sys.time() - t0, units = "secs") > timeout) {
      message("  poll timed out: ", substr(gsub("\\s+", " ", expr), 1, 70)); return(FALSE) }
    Sys.sleep(every)
  }
}
# Only count outputs the user can see; suspended outputs in hidden tabs keep
# the .recalculating class indefinitely.
wait_idle <- function(extra = 0.8, timeout = 45) {
  Sys.sleep(0.5)
  poll("!document.documentElement.classList.contains('shiny-busy') &&
        Array.from(document.querySelectorAll('.recalculating')).filter(function(e){return e.offsetParent!==null;}).length===0",
       timeout = timeout)
  Sys.sleep(extra)
}
wait_map <- function(timeout = 15) {
  poll("document.querySelectorAll('#map path.leaflet-interactive').length > 0 &&
        document.querySelectorAll('#map img.leaflet-tile:not(.leaflet-tile-loaded)').length === 0",
       timeout = timeout)
  Sys.sleep(0.7)
}
click <- function(sel) js(sprintf(
  "(function(){var e=document.querySelector(%s); if(!e) return 'missing'; e.click(); return 'ok';})()", q(sel)))
box_expr <- function(expr) {
  v <- js(sprintf("(function(){var e=(%s); if(!e) return null; var r=e.getBoundingClientRect();
                   return JSON.stringify({x:r.x,y:r.y,w:r.width,h:r.height});})()", expr))
  if (is.null(v)) NULL else fromJSON(v)
}
box <- function(sel) box_expr(sprintf("document.querySelector(%s)", q(sel)))
is_open <- function(step) isTRUE(js(sprintf(
  "!!document.querySelector('.accordion-item[data-value=\"%s\"] .accordion-collapse.show')", step)))

frames <- list(); meta <- list()
shot <- function(name, boxes = list()) {
  r <- b$Page$captureScreenshot(format = "png")
  writeBin(base64_dec(r$data), file.path(out, paste0(name, ".png")))
  frames[[name]] <<- list(file = paste0(name, ".png"), boxes = boxes)
  message("shot ", name)
}
hdr <- function(step) box(sprintf('.accordion-item[data-value="%s"] .accordion-button', step))
badge1 <- "Array.from(document.querySelectorAll('#map .leaflet-tooltip')).find(function(t){return t.textContent.trim()==='#1';})"
tab <- function(v) box(sprintf('.nav-link[data-value="%s"]', v))

# ---- landing ---------------------------------------------------------------
b$Page$navigate(url)
poll("typeof Shiny!=='undefined' && !!document.querySelector('#map.leaflet-container, #map .leaflet-container')", 60)
js("window.dispatchEvent(new Event('resize'))")
wait_idle(1); wait_map(); Sys.sleep(1)
meta$step1_open_on_load <- is_open("step1")
shot("s01_landing", list(step1_header = hdr("step1"), map = box('#map')))

# ---- Step 1: expand it, open the sub-zone dropdown, pick Klamath ----------
# Expand Step 1 first if the app ever lands with it collapsed (recorded in meta).
if (!meta$step1_open_on_load) {
  click('.accordion-item[data-value="step1"] .accordion-button'); Sys.sleep(1.1)
}
shot("s01b_step1", list(subzone = box('.accordion-item[data-value="step1"] .selectize-input'),
                        step1_header = hdr("step1")))
js("var s=$('#subzone')[0].selectize; s.focus(); s.open(); 'ok';"); Sys.sleep(0.8)
shot("s02_dropdown", list(
  option = box('.selectize-dropdown .option[data-value="region:Klamath"]'),
  dropdown = box('.selectize-dropdown')))
js("var s=$('#subzone')[0].selectize; s.setValue('region:Klamath'); s.close(); s.blur(); 'ok';")
wait_idle(1); wait_map(); Sys.sleep(1.2)
meta$step2_auto_open <- is_open("step2")
shot("s03_klamath", list(map = box('#map'), step2_header = hdr("step2"),
                         subzone = box('.accordion-item[data-value="step1"] .selectize-input')))
if (!meta$step2_auto_open) { click('.accordion-item[data-value="step2"] .accordion-button'); Sys.sleep(1) }
shot("s04_step2", list(scenario_btn = box('#pick_scenario_postfire_meadows'), step2_header = hdr("step2")))

# ---- Step 2: pick the scenario ----------------------------------------------
click('#pick_scenario_postfire_meadows')
wait_idle(1); wait_map(); Sys.sleep(1.2)
js("var r=document.querySelector('.slider-row'); if(r) r.scrollIntoView({block:'center'});"); Sys.sleep(0.6)
sid <- js("document.querySelector('.slider-row input.js-range-slider').id")
handle <- sprintf("document.getElementById(%s).parentElement.querySelector('.irs-handle')", q(sid))
shot("s05_scored", list(map = box('#map'), badge1 = box_expr(badge1), goal = box('#scenario_goal'),
                        handle = box_expr(handle), slider_row = box('.slider-row'),
                        step3_header = hdr("step3"), step4_header = hdr("step4")))

# ---- Step 4: drag the recent-fire weight 1 -> 5 ----------------------------
vals <- seq(1.5, 5, by = 0.5)
for (k in seq_along(vals)) {
  v <- vals[k]
  js(sprintf("$('#%s').data('ionRangeSlider').update({from:%s}); Shiny.setInputValue('%s', %s);", sid, v, sid, v))
  wait_idle(0.6); wait_map(); Sys.sleep(0.3)
  shot(sprintf("s06_slider_%02d", k), list(handle = box_expr(handle), map = box('#map'),
                                            badge1 = box_expr(badge1)))
}

# ---- Ranked HUCs ------------------------------------------------------------
click('.nav-link[data-value="Ranked HUCs"]'); wait_idle(1)
poll("document.querySelectorAll('#ranking_table tbody tr').length > 1", 30); Sys.sleep(0.8)
shot("s07_table", list(tab = tab("Ranked HUCs"), table = box('#ranking_table'),
                       download = box('#download_ranking')))

# ---- Sensitivity ------------------------------------------------------------
click('.nav-link[data-value="Sensitivity"]'); wait_idle(0.8)
shot("s08_sens_empty", list(tab = tab("Sensitivity"), run = box('#run_sensitivity')))
click('#run_sensitivity')
poll("document.querySelector('#sens_plot img') !== null", 90)
wait_idle(1.5); poll("document.querySelectorAll('.shiny-notification').length === 0", 15); Sys.sleep(0.5)
shot("s09_sens_plot", list(plot = box('#sens_plot img'), run = box('#run_sensitivity')))

# ---- Report -----------------------------------------------------------------
click('.nav-link[data-value="Report"]')
poll("['#report_chart_map_out','#report_chart_ranking_out','#report_chart_facets_out','#report_chart_ws_1_out']
      .every(function(s){return document.querySelector(s+' img')!==null;})", 150)
wait_idle(1.5)
shot("s10_report_top", list(tab = tab("Report"), download_html = box('#download_report_html'),
                            download_md = box('#download_report_md')))

# Find the element that scrolls the report, then capture eased scroll steps.
js("window.__sc=(function(){var e=document.getElementById('report_rendered');
     while(e && e!==document.body){var s=getComputedStyle(e);
       if((s.overflowY==='auto'||s.overflowY==='scroll') && e.scrollHeight>e.clientHeight) return e;
       e=e.parentElement;} return document.scrollingElement;})(); 0;
    window.__topOf=function(el){return __sc.scrollTop+el.getBoundingClientRect().top-__sc.getBoundingClientRect().top;}; 'ok';")
y_map <- js("Math.max(0, __topOf(document.querySelector('#report_chart_map_out')) - 95)")
y_ws  <- js("(function(){var h=Array.from(document.querySelectorAll('#report_rendered h2')).find(function(e){return e.textContent.indexOf('Individual watershed')===0;});
             var h3=h; while(h3 && h3.tagName!=='H3') h3=h3.nextElementSibling; return Math.max(0, __topOf(h3||h) - 25);})()")
meta$scroll <- list(y_map = y_map, y_ws = y_ws)
ease <- function(n) (1 - cos(pi * seq(0, 1, length.out = n + 1)[-1])) / 2
ys <- c(y_map * ease(18), y_map + (y_ws - y_map) * ease(30))
for (k in seq_along(ys)) {
  js(sprintf("__sc.scrollTop=%f;", ys[k])); Sys.sleep(0.15)
  bx <- list()
  if (k == 18)         bx$map_img <- box('#report_chart_map_out img')
  if (k == length(ys)) { bx$ws_img <- box('#report_chart_ws_1_out img')
                         bx$ws_table <- box_expr("document.querySelector('#report_chart_ws_1_out').previousElementSibling") }
  shot(sprintf("s11_scroll_%02d", k), bx)
}

writeLines(toJSON(list(frames = frames, meta = meta), auto_unbox = TRUE, pretty = TRUE, null = "null"),
           file.path(out, "frames.json"))
b$close()
message("done: ", length(frames), " frames; step2 auto-opened: ", meta$step2_auto_open)
