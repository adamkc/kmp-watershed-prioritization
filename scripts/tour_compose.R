# Build the one-minute tour from scripts/tour_capture.R output: animated cursor, click
# rings, crossfades, faded-in highlights, caption bar (second version), and
# title/end cards. Writes GIF (gifski) and MP4 (av), with and without captions.
# Run from the project root:
#   Rscript scripts/tour_compose.R <frames_dir> <out_dir> [gifski_lib_dir]
# Needs magick, av, and gifski (install.packages("gifski"), or pass a library path).
suppressMessages({ library(magick); library(jsonlite); library(av) })
a <- commandArgs(TRUE)
fdir <- a[1]; odir <- a[2]; LOGO <- "www/kmp_logo.png"
if (length(a) >= 3) library(gifski, lib.loc = a[3]) else library(gifski)
pdir <- file.path(odir, "png"); dir.create(pdir, recursive = TRUE, showWarnings = FALSE)
unlink(list.files(pdir, full.names = TRUE))

FPS <- 12.5; S <- 2 / 3; OW <- 960; OH <- 600; CAPH <- 64
ORANGE <- "#f97316"; FONT <- "Segoe UI"
fj <- fromJSON(file.path(fdir, "frames.json"), simplifyVector = FALSE)
FR <- fj$frames; META <- fj$meta

cache <- new.env()
scr <- function(n) {
  if (is.null(cache[[n]])) cache[[n]] <- image_scale(
    image_read(file.path(fdir, FR[[n]]$file)), sprintf("%dx%d!", OW, OH))
  cache[[n]]
}
B <- function(n, k) {
  b <- FR[[n]]$boxes[[k]]; if (is.null(b)) stop("missing box ", n, "$", k)
  list(x = b$x * S, y = b$y * S, w = b$w * S, h = b$h * S)
}
ctr <- function(b, fx = 0.5, fy = 0.5) c(b$x + b$w * fx, b$y + b$h * fy)
union_box <- function(p, q) {
  x0 <- min(p$x, q$x); y0 <- min(p$y, q$y)
  list(x = x0, y = y0, w = max(p$x + p$w, q$x + q$w) - x0, h = max(p$y + p$h, q$y + q$h) - y0)
}
nf <- function(secs) max(1L, as.integer(round(secs * FPS)))
ease <- function(t) (1 - cos(pi * t)) / 2

# ---- cards -----------------------------------------------------------------
logo_disc <- function(d) {
  lg <- image_scale(image_read(LOGO), sprintf("%dx%d!", d, d))
  mask <- image_draw(image_blank(d, d, "none"))
  symbols(d / 2, d / 2, circles = d / 2 - 1, inches = FALSE, add = TRUE, bg = "white", fg = "white")
  invisible(dev.off())
  image_composite(lg, mask, operator = "DstIn")
}
card <- function(bg, title, sub, d = 150, ty = 300) {
  img <- image_colorize(image_blur(scr(bg), 0, 8), 72, "#0b1220")
  img <- image_composite(img, logo_disc(d), offset = sprintf("+%d+%d", (OW - d) %/% 2, ty - d - 40))
  img <- image_annotate(img, title, gravity = "north", location = sprintf("+0+%d", ty),
                        size = 42, color = "white", font = FONT, weight = 700)
  image_annotate(img, sub, gravity = "north", location = sprintf("+0+%d", ty + 60),
                 size = 24, color = "#cbd5e1", font = FONT)
}
last_scroll <- tail(sort(grep("^s11_scroll_", names(FR), value = TRUE)), 1)
CARD <- list(
  intro = card("s01_landing", "KMP Watershed Prioritization", "A one-minute tour of the tool"),
  outro = card(last_scroll, "Klamath Meadows Partnership", "Learn more at www.klamathmeadows.org", d = 120, ty = 290))

# ---- timeline state --------------------------------------------------------
TL <- list()
st <- list(base = list(type = "card", which = "intro"), cursor = NULL, cap = "", hl = list(), ramp = 0L)
push <- function(n = 1L, ring = NULL, base = NULL) {
  hl <- st$hl
  for (i in seq_len(n)) {
    alpha <- if (st$ramp > 0) c(0.35, 0.7)[3L - st$ramp] else 1
    if (st$ramp > 0) st$ramp <<- st$ramp - 1L
    TL[[length(TL) + 1]] <<- list(base = base %||% st$base, cursor = st$cursor, ring = ring,
                                  cap = st$cap, hl = lapply(hl, function(b) list(b = b, a = alpha)))
  }
}
`%||%` <- function(x, y) if (is.null(x)) y else x
screen <- function(n) st$base <<- list(type = "screen", name = n)
hold <- function(secs) push(nf(secs))
cap <- function(text) st$cap <<- text
hl <- function(...) { st$hl <<- list(...); st$ramp <<- if (length(st$hl)) 2L else 0L }
move_to <- function(p, secs) {
  p0 <- st$cursor %||% p; n <- nf(secs)
  for (i in seq_len(n)) { st$cursor <<- p0 + (p - p0) * ease(i / n); push() }
}
click <- function() for (k in 1:4) push(ring = list(r = c(5, 11, 17, 23)[k], a = c(0.9, 0.7, 0.45, 0.2)[k]))
blend_base <- function(from, to, t) list(type = "blend", a = from, b = to, t = round(t, 3))
fade_to <- function(target, secs, path = NULL) {
  from <- st$base; to <- if (is.character(target)) list(type = "screen", name = target) else target
  n <- nf(secs); p0 <- st$cursor
  for (i in seq_len(n)) {
    if (!is.null(path)) st$cursor <<- p0 + (path - p0) * ease(i / n)
    push(base = blend_base(from, to, i / n))
  }
  st$base <<- to
}

# ---- storyboard ------------------------------------------------------------
hold(3.0)                                                     # intro card
fade_to("s01_landing", 0.7)
cap("The sidebar walks through the steps; the map shows the results.")
st$cursor <- ctr(B("s01_landing", "map"), 0.55, 0.55); hold(2.6)

cap("Step 1: choose the area. Here, the Klamath region.")
if (!isTRUE(META$step1_open_on_load)) {
  move_to(ctr(B("s01_landing", "step1_header"), 0.35), 0.8); click()
  fade_to("s01b_step1", 0.45); hold(0.5)
} else screen("s01b_step1")
move_to(ctr(B("s01b_step1", "subzone"), 0.4), 0.7); click()
fade_to("s02_dropdown", 0.4); hold(1.3)
move_to(ctr(B("s02_dropdown", "option"), 0.25), 0.7); click()
fade_to("s03_klamath", 0.6); hl(B("s03_klamath", "map")); hold(2.0); hl()

cap("Step 2: pick a scenario. Darker watersheds rank higher.")
if (!isTRUE(META$step2_auto_open)) {
  move_to(ctr(B("s03_klamath", "step2_header"), 0.35), 0.7); click()
  fade_to("s04_step2", 0.4); hold(0.5)
} else screen("s04_step2")
move_to(ctr(B("s04_step2", "scenario_btn"), 0.35), 0.8); click()
fade_to("s05_scored", 0.6); hl(B("s05_scored", "map")); hold(3.0); hl()

cap("Step 3: the goal is prefilled from the scenario and goes in the report.")
move_to(ctr(B("s05_scored", "goal"), 0.75, 0.6), 0.8)
hl(B("s05_scored", "goal")); hold(2.4); hl()

cap("Step 4: drag a weight. The ranking updates as you go.")
move_to(ctr(B("s05_scored", "handle")), 0.8)
push(2, ring = list(r = 6, a = 0.8))
sl <- sort(grep("^s06_slider_", names(FR), value = TRUE))
for (k in seq_along(sl)) fade_to(sl[k], 0.32, path = ctr(B(sl[k], "handle")))
last_sl <- tail(sl, 1)
hl(B(last_sl, "map")); hold(2.6); hl()

cap("Ranked HUCs: every watershed in order. Sort it or download a CSV.")
move_to(ctr(B("s07_table", "tab")), 0.9); click()
fade_to("s07_table", 0.5); hl(B("s07_table", "table")); hold(2.6)
move_to(ctr(B("s07_table", "download"), 0.3), 0.6); hl(B("s07_table", "download")); hold(1.3); hl()

cap("Sensitivity: jitter the weights 1,000 times and see which ranks hold.")
move_to(ctr(B("s08_sens_empty", "tab")), 0.8); click()
fade_to("s08_sens_empty", 0.5); hold(0.8)
move_to(ctr(B("s08_sens_empty", "run"), 0.3), 0.6); click()
fade_to("s09_sens_plot", 0.6); hl(B("s09_sens_plot", "plot")); hold(3.0); hl()

cap("Report: a written summary, downloadable as HTML or Markdown.")
move_to(ctr(B("s10_report_top", "tab")), 0.8); click()
fade_to("s10_report_top", 0.5); hold(0.9)
move_to(ctr(B("s10_report_top", "download_html"), 0.3), 0.6)
hl(union_box(B("s10_report_top", "download_md"), B("s10_report_top", "download_html"))); hold(1.8); hl()

cap("The report includes the ranking map and charts.")
move_to(c(OW - 40, OH * 0.55), 0.5)
sc <- sort(grep("^s11_scroll_", names(FR), value = TRUE))
map_k <- which(vapply(sc, function(n) !is.null(FR[[n]]$boxes$map_img), logical(1)))
for (n in sc[seq_len(map_k)]) { screen(n); push() }
hl(B(sc[map_k], "map_img")); hold(2.0); hl()
cap("Step 5 adds a profile of each top watershed, metric by metric.")
for (n in sc[(map_k + 1):length(sc)]) { screen(n); push() }
hl(B(last_scroll, "ws_img")); hold(3.4); hl()

st$cursor <- NULL; cap("")
fade_to(list(type = "card", which = "outro"), 0.8); hold(3.2)

message(sprintf("timeline: %d frames = %.1f s", length(TL), length(TL) / FPS))

# ---- rendering -------------------------------------------------------------
base_img <- function(bs) switch(bs$type,
  screen = scr(bs$name), card = CARD[[bs$which]],
  blend  = image_composite(base_img(bs$a), base_img(bs$b), operator = "blend",
                           compose_args = as.character(round(100 * bs$t))))
col_a <- function(col, a) grDevices::adjustcolor(col, alpha.f = a)
draw_box <- function(b, a, pad = 4) {
  rect(b$x - pad, b$y - pad, b$x + b$w + pad, b$y + b$h + pad, border = col_a("white", 0.9 * a), lwd = 6)
  rect(b$x - pad, b$y - pad, b$x + b$w + pad, b$y + b$h + pad, border = col_a(ORANGE, a), lwd = 3.5)
}
draw_cursor <- function(p, pressed) {
  s <- if (pressed) 0.9 else 1
  px <- c(0, 0, 5, 9, 12, 8, 15) * s * 1.15; py <- c(0, 21, 16, 24, 22.5, 14.5, 14.5) * s * 1.15
  polygon(p[1] + px + 2, p[2] + py + 2, col = col_a("black", 0.35), border = NA)
  polygon(p[1] + px, p[2] + py, col = "white", border = "black", lwd = 1.6)
}
cap_cache <- new.env()
cap_bar <- function(text) {
  k <- if (nzchar(text)) text else "<blank>"
  if (is.null(cap_cache[[k]])) {
    bar <- image_blank(OW, CAPH, "#111827")
    if (nzchar(text)) bar <- image_annotate(bar, text, gravity = "center", size = 23,
                                            color = "white", font = FONT, weight = 600)
    cap_cache[[k]] <- bar
  }
  cap_cache[[k]]
}

keys <- vapply(TL, function(s) paste(deparse(s, control = NULL), collapse = ""), character(1))
ukeys <- unique(keys); idx <- match(keys, ukeys)
plain <- file.path(pdir, sprintf("p%04d.png", seq_along(ukeys)))
capd  <- file.path(pdir, sprintf("c%04d.png", seq_along(ukeys)))
first <- match(seq_along(ukeys), idx)
message(sprintf("rendering %d unique frames", length(ukeys)))
for (u in seq_along(ukeys)) {
  s <- TL[[first[u]]]
  img <- image_draw(base_img(s$base))
  for (h in s$hl) draw_box(h$b, h$a)
  if (!is.null(s$ring)) symbols(s$cursor[1], s$cursor[2], circles = s$ring$r, inches = FALSE, add = TRUE,
                                fg = col_a(ORANGE, s$ring$a), lwd = 3)
  if (!is.null(s$cursor)) draw_cursor(s$cursor, !is.null(s$ring))
  dev.off()
  image_write(img, plain[u], format = "png")
  image_write(image_append(c(img, cap_bar(s$cap)), stack = TRUE), capd[u], format = "png")
  if (u %% 50 == 0) message("  ", u)
}

seqp <- plain[idx]; seqc <- capd[idx]
gifski(seqp, file.path(odir, "workflow_tour.gif"), width = OW, height = OH, delay = 1 / FPS, progress = FALSE)
gifski(seqc, file.path(odir, "workflow_tour_captioned.gif"), width = OW, height = OH + CAPH, delay = 1 / FPS, progress = FALSE)
av_encode_video(seqp, file.path(odir, "workflow_tour.mp4"), framerate = FPS, verbose = FALSE)
av_encode_video(seqc, file.path(odir, "workflow_tour_captioned.mp4"), framerate = FPS, verbose = FALSE)
for (f in c("workflow_tour.gif", "workflow_tour_captioned.gif", "workflow_tour.mp4", "workflow_tour_captioned.mp4"))
  message(sprintf("%-30s %5.1f MB", f, file.info(file.path(odir, f))$size / 1e6))
