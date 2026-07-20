# =============================================================================
# Cancer R&D against total NTIS R&D.
#
# Indexes both annual project counts to their 2021 value and plots them on a
# common scale, with 2023-2024 dashed to mark the years most affected by
# registration lag.
#
# Input :  ntis2_yearly_total_stats_*
# Output:  figureS1_cancer_vs_total_*  -> Supplementary Figure S1
# =============================================================================

if (!exists("PROJ_ROOT")) source("config.R")

suppressMessages(library(data.table))
d <- fread(latest_file(OUT_TABLES, "ntis2_yearly_total_stats_.*\\.csv$"))
setnames(d, c("total_projects", "total_cancer_projects"), c("all_n", "cancer_n"))
d <- d[Year >= 2007]
b21 <- function(v) 100 * v / v[d$Year == 2021]
tot <- b21(d$all_n); can <- b21(d$cancer_n); yr <- d$Year
sol <- yr <= 2023
seg <- function(y, col, lw) { lines(yr[sol], y[sol], col = col, lwd = lw)
                              lines(yr[yr >= 2023], y[yr >= 2023], col = col, lwd = lw, lty = 2) }
stamp <- format(Sys.Date(), "%y%m%d")
png(file.path(OUT_FIGURES, sprintf("figureS1_cancer_vs_total_%s.png", stamp)),
    width = 7.0, height = 4.1, units = "in", res = 200)
par(mar = c(4.2, 4.5, 2.8, 4.2), mgp = c(2.7, 0.8, 0), family = "sans")
plot(yr, tot, type = "n", xlim = c(2007, 2024.5), ylim = c(45, 108), xlab = "Year",
     ylab = "Project count (2021 value = 100)", cex.lab = 1.12, cex.axis = 1.02,
     main = "Cancer R&D was relatively protected in the 2024 R&D reduction",
     cex.main = 1.05, font.main = 1)
abline(v = c(2021, 2023), col = "grey85", lty = 3)
seg(tot, "grey45", 3); seg(can, "#C0392B", 3.4)
pts <- function(y, col) points(2024, y[yr == 2024], pch = 19, col = col, cex = 1.15)
pts(tot, "grey45"); pts(can, "#C0392B")
lab <- function(y, txt, col, cx = 1.02) text(2024.15, y, txt, col = col, font = 2, cex = cx, pos = 4, xpd = NA)
lab(can[yr == 2024] + 1.5, "cancer  -9.8%", "#C0392B")
lab(tot[yr == 2024] - 1.5, "total  -15.5%", "grey40")
text(2023, 46.5, "2023", col = "grey55", cex = 0.85, pos = 1, xpd = NA)
legend("topleft", c("Cancer R&D", "Total NTIS", "2023-2024 (dashed)"),
       col = c("#C0392B", "grey45", "grey45"), lwd = c(3.4, 3, 1.5),
       lty = c(1, 1, 2), bty = "n", cex = 1.0, seg.len = 2.4)
dev.off()
cat(sprintf("saved figureS1: 2024 cancer=%.1f total=%.1f (2021=100); 2023->2024 cancer -9.8%%, total -15.5%%\n",
            can[yr == 2024], tot[yr == 2024]))
