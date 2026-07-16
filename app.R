# =============================================================================
# CAO 2026 -- Exploratory Data Analysis (Shiny)
#
# A scrollable, report-style EDA app for data/clean/cao-2026-clean.csv, built to
# respect the measurement level of each item (almost nothing here is truly
# continuous -- most items are ordinal Likert scales):
#
#   continuous  : yob (age), job_tenure, time_use_1..6 (% of time), plus the
#                 0-10 ideology / anchor scales treated as interval.
#   ordinal     : the 4-5 point Likert items (issue_*, incivility_*, ...).
#   categorical : coded nominal factors (gender, race, prov_terr, which-credential,
#                 which-sector, multi-selects).
#
# Every section is a card with a left sidebar holding that card's controls
# (and a short note) beside its plot/table.
#
# Sections:
#   1. Spearman correlation heatmap  -- rank correlations across numeric items
#   2. Most correlated pairs         -- the heatmap's strongest cells, ranked
#                                       live; Spearman/Pearson, adjustable N, and
#                                       an option to hide within-battery pairs
#   3. Bivariate comparison          -- three pickers: X, Y, and plot type. The
#                                       plot type defaults to "Auto" (chosen from
#                                       the two variables' measurement levels) but
#                                       can be forced. The association statistic
#                                       always follows the measurement levels:
#                                         cont x cont  -> scatter + lm/loess (Pearson r)
#                                         cont x ord   -> box+violin by level  (Spearman rho)
#                                         ord  x ord   -> cross-tab heatmap    (Spearman rho)
#                                         num  x cat   -> box+violin           (eta^2, ANOVA)
#                                         cat  x cat   -> 100% stacked bars     (Cramer's V)
#                                       lm + loess lines appear only on a Scatter
#                                       of two continuous / interval items.
#   4. Single-variable view          -- histogram (continuous, bins slider) or
#                                       bar chart (ordinal / categorical)
#   5. Variable dictionary           -- question wording + type for every item
#
# Run from the project root:
#   shiny::runApp("analysis/eda-shiny")
# =============================================================================

suppressMessages({
  library(shiny)
  library(bslib)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(forcats)
  library(scales)
  library(patchwork)
  library(DT)
  library(googlesheets4)
  library(readr)
})

# ----------------------------------------------------------------------------
# LOCATE + LOAD DATA (works whether launched from project root or app dir)
# ----------------------------------------------------------------------------
data_url <- "https://docs.google.com/spreadsheets/d/1ABKnu0gYexNZwGMyfQC97WDlgZkA2EhIDblKA7YheoQ/edit?gid=410479067#gid=410479067"
labels_path <- "labels.csv"

gs4_deauth()
schema <- readRDS("schema.rds")

raw <- read_sheet(data_url) |>
  # type_convert only applies to character type columns, so force character
  mutate(across(everything(), as.character)) |>
  type_convert(col_types = schema)

labels_tbl <- if (file.exists(labels_path)) {
  readr::read_csv(labels_path, show_col_types = FALSE)
} else {
  tibble(variable = names(raw), label = "")
}

# Qualtrics stores each matrix sub-question as "<shared stem> - <sub-item>", so
# the same long stem repeats on every row of a matrix (e.g. all time_use_* begin
# "In a typical week, what percentage..."). For display, drop that stem: take the
# text before the first " - " as the stem, and where a stem is shared by 2+
# variables (i.e. it really is a matrix), keep only the sub-item. Unique prefixes
# (gender's "... - Selected Choice", the _TEXT columns) are left untouched.
has_dash <- grepl(" - ", labels_tbl$label, fixed = TRUE)
stem <- ifelse(has_dash, sub(" - .*$", "", labels_tbl$label), NA_character_)
is_matrix_stem <- !is.na(stem) & stem %in% stem[duplicated(stem)]
labels_tbl$label[is_matrix_stem] <- trimws(
  sub("^.*? - ", "", labels_tbl$label[is_matrix_stem], perl = TRUE)
)

label_of <- function(v) {
  l <- labels_tbl$label[match(v, labels_tbl$variable)]
  if (length(l) == 0 || is.na(l) || l == "") v else l
}

# ----------------------------------------------------------------------------
# CLASSIFY VARIABLES BY MEASUREMENT LEVEL
#
#   continuous_vars  -- genuinely continuous / quasi-continuous, plus the 0-10
#                       scales treated as interval. The ONLY items that get a
#                       scatterplot with lm + loess lines of best fit.
#   categorical_vars -- coded nominal factors (incl. numeric-coded ones like
#                       educ_valuable = which credential, job_longest = which
#                       sector, and the comma-coded multi-selects).
#   ordinal_vars     -- everything else numeric: the 4-5 point Likert items.
# ----------------------------------------------------------------------------
admin_drop <- c(
  "source",
  "StartDate",
  "EndDate",
  "RecordedDate",
  "ResponseId",
  "UserLanguage",
  "Status",
  "total_duration",
  "n_answered",
  "last_answered",
  "timing",
  "results",
  "interview",
  "strongmayor_open",
  "job_background_16_TEXT",
  "gender_9_TEXT",
  "race_13_TEXT"
)

continuous_vars <- c(
  "yob",
  "job_tenure",
  "time_use_1",
  "time_use_2",
  "time_use_3",
  "time_use_4",
  "time_use_5",
  "time_use_6",
  "ideology",
  "anchor_ideo_pm",
  "anchor_ideo_opp_leader",
  "anchor_ideo_premier",
  "anchor_ideo_mayor",
  "anchor_ideo_community"
)
continuous_vars <- intersect(continuous_vars, names(raw))

categorical_vars <- c(
  "gender",
  "race",
  "housing",
  "education",
  "prov_terr",
  "strong_mayor",
  "job_environment",
  "job_background",
  "educ_specific",
  "indig_champions",
  "indig_initiatives",
  "educ_valuable",
  "job_longest"
)
categorical_vars <- intersect(categorical_vars, names(raw))

numeric_all <- names(raw)[vapply(raw, is.numeric, logical(1))]
ordinal_vars <- setdiff(
  numeric_all,
  c(admin_drop, categorical_vars, continuous_vars)
)

# Items that live on a numeric axis (rank-correlatable): continuous + ordinal.
numeric_like <- c(continuous_vars, ordinal_vars)

# Coerce the categorical set to factors (many are semicolon-coded multi-selects).
dat <- raw
for (v in categorical_vars) {
  dat[[v]] <- as.factor(dat[[v]])
}

all_pickable <- sort(c(continuous_vars, ordinal_vars, categorical_vars))

var_type <- function(v) {
  if (v %in% continuous_vars) {
    "continuous"
  } else if (v %in% ordinal_vars) {
    "ordinal"
  } else {
    "categorical"
  }
}
is_cat <- function(v) var_type(v) == "categorical"
is_num <- function(v) v %in% numeric_like

# ----------------------------------------------------------------------------
# VALUE LABELS (answer text for numeric codes)
#
# value_labels.csv (question, value, label) is produced by fetch-labels.R from
# the Qualtrics survey definition. Used to show answer text instead of numeric
# codes on discrete axes and in the dictionary.
# ----------------------------------------------------------------------------
vl_path <- "value_labels.csv"
value_labels_tbl <- if (file.exists(vl_path)) {
  readr::read_csv(
    vl_path,
    show_col_types = FALSE,
    col_types = readr::cols(.default = "c")
  )
} else {
  tibble(question = character(), value = character(), label = character())
}
vl_lookup <- split(
  value_labels_tbl[c("value", "label")],
  value_labels_tbl$question
)

# Value-label table for a column: an exact export-tag match, or the matrix base
# tag after dropping a trailing _<n> sub-item suffix (strongmayor_effect_3 ->
# strongmayor_effect). NULL when the variable has no coding.
vl_for <- function(v) {
  if (v %in% names(vl_lookup)) {
    return(vl_lookup[[v]])
  }
  base <- sub("_[0-9]+$", "", v)
  if (base %in% names(vl_lookup)) {
    return(vl_lookup[[base]])
  }
  NULL
}

# Only relabel when every observed code is covered by the labels, so combined
# multi-selects (race = "2;12") and any diverging code fall back to the raw
# number rather than a wrong label.
has_value_labels <- function(v) {
  t <- vl_for(v)
  if (is.null(t) || v %in% continuous_vars) {
    return(FALSE)
  }
  obs <- unique(as.character(dat[[v]][!is.na(dat[[v]])]))
  length(obs) > 0 && all(obs %in% t$value)
}

decode <- function(v, x) {
  t <- vl_for(v)
  if (is.null(t)) {
    return(as.character(x))
  }
  lab <- t$label[match(as.character(x), t$value)]
  ifelse(is.na(lab), as.character(x), lab)
}

# Factor with numeric-ordered levels, labelled with answer text when available.
labeled_factor <- function(x, v) {
  lv <- sort(unique(x[!is.na(x)]))
  if (has_value_labels(v)) {
    factor(x, levels = lv, labels = decode(v, lv))
  } else if (is.numeric(x)) {
    factor(x, levels = lv)
  } else {
    factor(x)
  }
}

# Ordered "value = label; ..." string for the dictionary (empty for continuous
# vars and vars with no coding).
value_label_string <- function(v) {
  t <- vl_for(v)
  if (is.null(t) || v %in% continuous_vars) {
    return("")
  }
  paste(sprintf("%s = %s", t$value, t$label), collapse = "; ")
}

# Dropdown choices labelled "name -- question wording"
choice_labels <- function(vars) {
  setNames(
    vars,
    vapply(
      vars,
      function(v) {
        lab <- label_of(v)
        if (identical(lab, v)) v else paste0(v, "  —  ", lab)
      },
      character(1)
    )
  )
}

# ----------------------------------------------------------------------------
# STATISTICS HELPERS
# ----------------------------------------------------------------------------
corr_stat <- function(x, y, method = "pearson") {
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 3) {
    return(list(coef = NA, n = sum(ok)))
  }
  list(coef = suppressWarnings(cor(x[ok], y[ok], method = method)), n = sum(ok))
}

eta_squared <- function(num, grp) {
  d <- data.frame(num = num, grp = grp) |> tidyr::drop_na()
  if (nrow(d) < 3 || dplyr::n_distinct(d$grp) < 2) {
    return(list(eta2 = NA, p = NA, n = nrow(d)))
  }
  fit <- tryCatch(aov(num ~ grp, data = d), error = function(e) NULL)
  if (is.null(fit)) {
    return(list(eta2 = NA, p = NA, n = nrow(d)))
  }
  a <- summary(fit)[[1]]
  ss <- a[["Sum Sq"]]
  list(eta2 = ss[1] / sum(ss), p = a[["Pr(>F)"]][1], n = nrow(d))
}

cramers_v <- function(a, b) {
  tab <- table(a, b)
  if (min(dim(tab)) < 2 || sum(tab) < 3) {
    return(list(v = NA, p = NA, n = sum(tab)))
  }
  ch <- suppressWarnings(chisq.test(tab))
  n <- sum(tab)
  list(
    v = sqrt(as.numeric(ch$statistic) / (n * (min(dim(tab)) - 1))),
    p = ch$p.value,
    n = n
  )
}

# ----------------------------------------------------------------------------
# PLOT HELPERS
# ----------------------------------------------------------------------------
theme_eda <- theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(), plot.title = element_blank())

# Scatter cell for the continuous/interval grid: jittered points, lm + loess,
# Pearson r label.
scatter_cell <- function(df, xvar, yvar, jitter = TRUE, colvar = NULL) {
  s <- corr_stat(df[[xvar]], df[[yvar]], "pearson")
  rlab <- if (is.na(s$coef)) {
    "r = NA"
  } else {
    sprintf("r = %.2f (n = %d)", s$coef, s$n)
  }
  aes_base <- if (!is.null(colvar)) {
    aes(.data[[xvar]], .data[[yvar]], colour = .data[[colvar]])
  } else {
    aes(.data[[xvar]], .data[[yvar]])
  }
  pos <- if (jitter) {
    position_jitter(width = 0.15, height = 0.15, seed = 1)
  } else {
    "identity"
  }
  p <- ggplot(df, aes_base) +
    geom_point(alpha = 0.35, size = 1.1, position = pos)
  if (is.null(colvar)) {
    p <- p +
      geom_smooth(
        method = "lm",
        formula = y ~ x,
        se = FALSE,
        colour = "#c0392b",
        linewidth = 0.7
      ) +
      geom_smooth(
        method = "loess",
        formula = y ~ x,
        se = FALSE,
        colour = "#2980b9",
        linewidth = 0.7,
        linetype = "22"
      )
  } else {
    p <- p +
      geom_smooth(method = "lm", formula = y ~ x, se = FALSE, linewidth = 0.6)
  }
  p +
    labs(x = xvar, y = yvar, colour = colvar) +
    annotate(
      "label",
      x = -Inf,
      y = Inf,
      label = rlab,
      hjust = -0.05,
      vjust = 1.1,
      size = 3,
      label.size = 0,
      fill = alpha("white", 0.6)
    ) +
    theme_eda
}

# Box + violin of a numeric var across the levels of a grouping var. A numeric
# grouping var (e.g. an ordinal item) is turned into a factor with its levels in
# numeric order, not the alphabetical order factor() would default to.
box_violin <- function(df, numv, grpv) {
  df[[grpv]] <- labeled_factor(df[[grpv]], grpv)
  ggplot(df, aes(.data[[grpv]], .data[[numv]])) +
    geom_violin(fill = "#ecf0f1", colour = "grey70") +
    geom_boxplot(width = 0.18, outlier.shape = NA, fill = "white") +
    geom_jitter(width = 0.12, height = 0.12, alpha = 0.3, size = 1) +
    labs(
      x = paste0(grpv, "  —  ", label_of(grpv)),
      y = paste0(numv, "  —  ", label_of(numv))
    ) +
    theme_eda +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
}

# Cross-tab heatmap for two ordinal items: cells shaded by row-conditional
# proportion (each X level sums to 100%), labelled with counts.
crosstab_plot <- function(df, xvar, yvar) {
  d <- df[, c(xvar, yvar)] |> tidyr::drop_na()
  tab <- table(x = d[[xvar]], y = d[[yvar]])
  cnt <- as.data.frame(tab)
  cnt$prop <- as.data.frame(prop.table(tab, margin = 1))$Freq
  # relabel the code levels with answer text where available
  xl <- levels(cnt$x)
  yl <- levels(cnt$y)
  if (has_value_labels(xvar)) {
    cnt$x <- factor(cnt$x, xl, decode(xvar, xl))
  }
  if (has_value_labels(yvar)) {
    cnt$y <- factor(cnt$y, yl, decode(yvar, yl))
  }
  ggplot(cnt, aes(x, y, fill = prop)) +
    geom_tile(colour = "white") +
    geom_label(
      aes(label = Freq),
      size = 3,
      fill = "white",
      border.color = "white"
    ) +
    scale_fill_gradient(
      low = "white",
      high = "#2c3e50",
      labels = percent,
      name = "% within X"
    ) +
    labs(
      x = paste0(xvar, "  —  ", label_of(xvar)),
      y = paste0(yvar, "  —  ", label_of(yvar))
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_text(angle = 30, hjust = 1)
    )
}

# ----------------------------------------------------------------------------
# UI
# ----------------------------------------------------------------------------
scatter_group_choices <- c(
  "None" = "",
  setNames(categorical_vars, categorical_vars)
)

plot_type_choices <- c(
  "Auto (by measurement level)" = "auto",
  "Scatter + lm/loess" = "scatter",
  "Box + violin" = "box",
  "Cross-tab heatmap" = "crosstab",
  "Stacked bars (100%)" = "stacked"
)

ui <- page_fluid(
  theme = bs_theme(version = 5, primary = "#2c3e50"),
  tags$style(HTML(
    "
    .app-wrap { max-width: 100vw; margin: 0 auto; }
    .section-card { margin-bottom: 1.5rem; scroll-margin-top: 4.5rem; }
    .grid-frame { border: 1px solid #dee2e6; border-radius: .375rem;
                  padding: .5rem; background: #fff; }
    .pager { display:flex; align-items:center; gap:.75rem; margin:.5rem 0; }
    .topbar { position: sticky; top: 0; z-index: 1030; display: flex;
              align-items: center; gap: .75rem; background: var(--bs-body-bg, #fff);
              padding: .5rem 0; margin-bottom: 1rem;
              border-bottom: 1px solid #dee2e6; }
    .topbar h2 { margin: 0; font-size: 1.5rem; }
    .toc-btn { font-size: 1.25rem; line-height: 1; padding: .1rem .55rem; }
    .modal-left .modal-dialog { position: fixed; left: 0; top: 0; margin: 0;
                                height: 100%; max-width: 340px; width: 85vw; }
    .modal-left .modal-content { height: 100%; border-radius: 0; }
    .toc-nav a { display: block; padding: .55rem .25rem; color: inherit;
                 text-decoration: none; border-bottom: 1px solid #eee; }
    .toc-nav a:hover { background: #f1f3f5; }
    /* Let long question labels wrap in the dropdown instead of being clipped. */
    .selectize-dropdown .option { white-space: normal; word-break: break-word; }
    .login-wrap { max-width: 360px; margin: 15vh auto 0; }
  "
  )),
  uiOutput("page")
)

# ----------------------------------------------------------------------------
# LOGIN GATE
#
# The whole app sits behind a single password taken from the PASSWORD
# environment variable. The server renders main_ui only after a correct
# password, so no app content is sent to the browser until then. If PASSWORD is
# unset the app stays locked (fail closed).
# ----------------------------------------------------------------------------
login_ui <- div(
  class = "login-wrap",
  card(
    card_header("CAO 2026 — Sign in"),
    card_body(
      passwordInput("password", "Password", width = "100%"),
      actionButton(
        "login_btn",
        "Sign in",
        class = "btn-primary",
        width = "100%"
      ),
      div(class = "text-danger small mt-2", textOutput("login_msg")),
      # Submit on Enter while the password field is present.
      tags$script(HTML(
        "document.addEventListener('keydown', function (e) {
           if (e.key === 'Enter' && document.getElementById('password')) {
             var b = document.getElementById('login_btn');
             if (b) b.click();
           }
         });"
      ))
    )
  )
)

main_ui <- div(
  class = "app-wrap",
  div(
    class = "topbar",
    tags$button(
      "Contents",
      class = "btn btn-outline-secondary toc-btn",
      `data-bs-toggle` = "modal",
      `data-bs-target` = "#tocMenu",
      title = "Sections menu",
      `aria-label` = "Open sections menu"
    ),
    h2("CAO 2026 — Exploratory Data Analysis")
  ),

  # Hamburger -> left side-sheet modal: a table of contents linking to each card.
  div(
    class = "modal modal-left fade",
    id = "tocMenu",
    tabindex = "-1",
    `aria-hidden` = "true",
    div(
      class = "modal-dialog",
      div(
        class = "modal-content",
        div(
          class = "modal-header",
          tags$h5(class = "modal-title", "Sections"),
          tags$button(
            class = "btn-close",
            `data-bs-dismiss` = "modal",
            `aria-label` = "Close"
          )
        ),
        div(
          class = "modal-body",
          tags$nav(
            class = "toc-nav",
            tags$a(href = "#sec-1", "1. Spearman correlation heatmap"),
            tags$a(href = "#sec-2", "2. Most strongly correlated pairs"),
            tags$a(href = "#sec-3", "3. Bivariate comparison"),
            tags$a(href = "#sec-4", "4. Single-variable distribution"),
            tags$a(href = "#sec-5", "5. Variable dictionary")
          )
        )
      )
    )
  ),

  # Close the TOC modal and smooth-scroll to the chosen card. (A plain
  # data-bs-dismiss on an <a href="#..."> misfires: Bootstrap reads the href
  # as the dismiss target, so it never hides #tocMenu.)
  tags$script(HTML(
    "document.addEventListener('click', function (e) {
         var a = e.target.closest('.toc-nav a');
         if (!a) return;
         e.preventDefault();
         var modalEl = document.getElementById('tocMenu');
         if (window.bootstrap && modalEl) {
           bootstrap.Modal.getOrCreateInstance(modalEl).hide();
         }
         var target = document.querySelector(a.getAttribute('href'));
         if (target) {
           setTimeout(function () {
             target.scrollIntoView({ behavior: 'smooth', block: 'start' });
           }, 250);
         }
       });"
  )),

  # 1. SPEARMAN HEATMAP --------------------------------------------------------
  card(
    id = "sec-1",
    class = "section-card",
    card_header("1. Spearman correlation heatmap (numeric items)"),
    layout_sidebar(
      sidebar = sidebar(
        width = 320,
        p(
          class = "text-muted small",
          "Spearman's ρ (rank correlation) across all continuous and ordinal items — appropriate for ordinal data, unlike Pearson. Use it to spot pairs worth a closer look."
        ),
        checkboxInput(
          "hm_cluster",
          "Order by similarity (hierarchical clustering)",
          TRUE
        )
      ),
      plotOutput("heatmap", height = "760px")
    )
  ),

  # 2. MOST CORRELATED PAIRS ---------------------------------------------------
  card(
    id = "sec-2",
    class = "section-card",
    height = "820px",
    card_header("2. Most strongly correlated pairs"),
    layout_sidebar(
      sidebar = sidebar(
        width = 320,
        p(
          class = "text-muted small",
          "The heatmap's strongest cells, ranked live by absolute correlation. Each pair uses every response that answered both items (n), so pairs from questions shown to a subset (e.g. strong-mayor items) have a smaller n."
        ),
        numericInput(
          "tc_n",
          "Number of pairs",
          value = 20,
          min = 5,
          max = 200,
          step = 5
        ),
        radioButtons(
          "tc_method",
          "Correlation",
          choices = c("Spearman ρ" = "spearman", "Pearson r" = "pearson"),
          selected = "spearman"
        ),
        checkboxInput(
          "tc_cross",
          "Only cross-battery pairs (hide items sharing a name prefix, e.g. incivility_*)",
          TRUE
        )
      ),
      DT::DTOutput("topcorr", fill = TRUE)
    )
  ),

  # 3. BIVARIATE COMPARISON ----------------------------------------------------
  card(
    id = "sec-3",
    class = "section-card",
    card_header("3. Bivariate comparison (any two variables)"),
    layout_sidebar(
      sidebar = sidebar(
        width = 320,
        p(
          class = "text-muted small",
          "View scatter, box/violin, cross-tab heatmap, or stacked bars for any two variables."
        ),
        p(
          class = "text-muted small",
          "Pick an X variable, a Y variable, and a plot type. “Auto” chooses the plot from the two variables' measurement levels; you can override it. The association statistic below the plot always follows the measurement levels (Pearson r / Spearman ρ / η² / Cramér's V)."
        ),
        selectInput(
          "bv_x",
          "X variable",
          choices = choice_labels(all_pickable),
          selected = continuous_vars[1]
        ),
        selectInput(
          "bv_y",
          "Y variable",
          choices = choice_labels(all_pickable),
          selected = continuous_vars[2]
        ),
        selectInput("bv_type", "Plot type", choices = plot_type_choices),
        selectInput(
          "scatter_group",
          "Colour points by (scatter only)",
          choices = scatter_group_choices
        ),
        checkboxInput("jitter", "Jitter overlapping points (scatter)", TRUE),
        p(
          class = "text-muted small",
          "On a scatter: solid red = linear fit, dashed blue = loess."
        )
      ),
      strong(textOutput("bv_stat")),
      div(class = "grid-frame", plotOutput("bv_plot", height = "560px"))
    )
  ),

  # 4. SINGLE-VARIABLE DISTRIBUTION -------------------------------------------
  card(
    id = "sec-4",
    class = "section-card",
    card_header("4. Single-variable distribution"),
    layout_sidebar(
      sidebar = sidebar(
        width = 320,
        selectInput(
          "uni_var",
          "Variable",
          choices = choice_labels(all_pickable),
          selected = continuous_vars[1]
        ),
        sliderInput(
          "uni_bins",
          "Number of bins (continuous only)",
          min = 2,
          max = 60,
          value = 20
        )
      ),
      textOutput("uni_summary"),
      plotOutput("uni_plot", height = "420px")
    )
  ),

  # 5. VARIABLE DICTIONARY -----------------------------------------------------
  card(
    id = "sec-5",
    class = "section-card",
    card_header("5. Variable dictionary"),
    layout_sidebar(
      sidebar = sidebar(
        width = 320,
        p(
          class = "text-muted small",
          "Measurement level and question wording for every variable. Search to look up a name."
        )
      ),
      DT::DTOutput("dict")
    )
  )
) # /app-wrap

# ----------------------------------------------------------------------------
# SERVER
# ----------------------------------------------------------------------------
server <- function(input, output, session) {
  # --- 0. Login gate --------------------------------------------------------
  # Render the app only after the password (PASSWORD env var) is entered.
  if (file.exists(".env.local")) {
    dotenv::load_dot_env(".env.local")
  }
  app_password <- Sys.getenv("PASSWORD")
  if (app_password == "") {
    stop("PASSWORD is not set.")
  }
  authed <- reactiveVal(FALSE)
  login_error <- reactiveVal("")
  output$page <- renderUI(if (authed()) main_ui else login_ui)
  output$login_msg <- renderText(login_error())
  observeEvent(input$login_btn, {
    if (nzchar(app_password) && identical(input$password, app_password)) {
      authed(TRUE)
    } else {
      login_error("Incorrect password.")
    }
  })

  # --- 1. Spearman heatmap --------------------------------------------------
  output$heatmap <- renderPlot({
    m <- suppressWarnings(cor(
      dat[numeric_like],
      use = "pairwise.complete.obs",
      method = "spearman"
    ))
    m[!is.finite(m)] <- NA
    ord <- numeric_like
    if (isTRUE(input$hm_cluster)) {
      d <- as.dist(1 - abs(replace(m, is.na(m), 0)))
      ord <- numeric_like[hclust(d)$order]
    }
    long <- as.data.frame(as.table(m)) |>
      setNames(c("Var1", "Var2", "r")) |>
      mutate(Var1 = factor(Var1, ord), Var2 = factor(Var2, ord))
    ggplot(long, aes(Var1, Var2, fill = r)) +
      geom_tile() +
      scale_fill_gradient2(
        low = "#2980b9",
        mid = "white",
        high = "#c0392b",
        midpoint = 0,
        limits = c(-1, 1),
        na.value = "grey90",
        name = "Spearman ρ"
      ) +
      coord_fixed() +
      theme_minimal(base_size = 9) +
      theme(
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.title = element_blank(),
        panel.grid = element_blank()
      )
  })

  # --- 1b. Strongest correlated pairs (ranked companion to the heatmap) ------
  output$topcorr <- DT::renderDT({
    method <- input$tc_method
    if (is.null(method)) {
      method <- "spearman"
    }
    n_show <- input$tc_n
    if (is.null(n_show) || is.na(n_show)) {
      n_show <- 20
    }
    m <- suppressWarnings(cor(
      dat[numeric_like],
      use = "pairwise.complete.obs",
      method = method
    ))
    npair <- crossprod(!is.na(dat[numeric_like])) # pairwise-complete n for every pair
    keep <- upper.tri(m) # each unordered pair once
    idx <- which(keep, arr.ind = TRUE)
    pairs <- data.frame(
      a = numeric_like[idx[, "row"]],
      b = numeric_like[idx[, "col"]],
      r = m[keep],
      n = npair[keep],
      stringsAsFactors = FALSE
    )
    pairs <- pairs[is.finite(pairs$r), , drop = FALSE]
    if (isTRUE(input$tc_cross)) {
      # drop within-battery pairs
      prefix <- function(x) sub("_.*$", "", x) # name up to the first underscore
      pairs <- pairs[prefix(pairs$a) != prefix(pairs$b), , drop = FALSE]
    }
    pairs <- head(pairs[order(-abs(pairs$r)), , drop = FALSE], n_show)
    out <- data.frame(
      round(pairs$r, 2),
      pairs$n,
      pairs$a,
      vapply(pairs$a, label_of, character(1)),
      pairs$b,
      vapply(pairs$b, label_of, character(1)),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    names(out) <- c(
      if (identical(method, "pearson")) "Pearson r" else "Spearman ρ",
      "n",
      "Variable A",
      "Question A",
      "Variable B",
      "Question B"
    )
    DT::datatable(
      out,
      rownames = FALSE,
      fillContainer = TRUE,
      options = list(pageLength = 20, autoWidth = FALSE)
    )
  })

  # --- 2. Bivariate comparison ----------------------------------------------
  nlev <- function(v) dplyr::n_distinct(v, na.rm = TRUE)

  # Resolve "Auto" into a concrete plot type from the two measurement levels.
  resolve_type <- function(x, y, ptype) {
    if (ptype != "auto") {
      return(ptype)
    }
    tx <- var_type(x)
    ty <- var_type(y)
    if (tx == "continuous" && ty == "continuous") {
      "scatter"
    } else if (is_num(x) && is_num(y)) {
      if (tx == "ordinal" && ty == "ordinal") "crosstab" else "box"
    } else if (is_cat(x) && is_cat(y)) {
      "stacked"
    } else {
      "box"
    } # one numeric_like, one categorical
  }

  output$bv_plot <- renderPlot({
    x <- input$bv_x
    y <- input$bv_y
    req(x, y)
    validate(need(x != y, "Pick two different variables."))
    ptype <- resolve_type(x, y, input$bv_type)
    colvar <- if (nzchar(input$scatter_group)) input$scatter_group else NULL
    # Include the colour-by column so a coloured scatter can find it; only x and y
    # need be complete (colour NAs just render as a grey "NA" group).
    d <- dat[, unique(c(x, y, colvar))] |> tidyr::drop_na(all_of(c(x, y)))
    validate(need(
      nrow(d) >= 3,
      "Not enough complete observations for this pair."
    ))

    if (ptype == "scatter") {
      validate(need(
        is_num(x) && is_num(y),
        "Scatter needs two numeric / ordinal variables. For a categorical variable, use Cross-tab heatmap, Stacked bars, or Box + violin."
      ))
      scatter_cell(d, x, y, jitter = input$jitter, colvar = colvar) +
        labs(
          x = paste0(x, "  —  ", label_of(x)),
          y = paste0(y, "  —  ", label_of(y))
        )
    } else if (ptype == "box") {
      validate(need(
        is_num(x) || is_num(y),
        "Box + violin needs at least one numeric / ordinal variable. Use Stacked bars for two categoricals."
      ))
      # numeric measure on y, grouping on x; prefer a continuous var as the measure
      numv <- if (is_num(x) && is_num(y)) {
        if (var_type(x) == "continuous" && var_type(y) != "continuous") {
          x
        } else if (var_type(y) == "continuous" && var_type(x) != "continuous") {
          y
        } else {
          y
        }
      } else if (is_num(x)) {
        x
      } else {
        y
      }
      grpv <- if (identical(numv, x)) y else x
      validate(need(
        nlev(d[[grpv]]) <= 30,
        "The grouping variable has too many levels for a box/violin — it's (near-)continuous. Try Scatter."
      ))
      box_violin(d, numv, grpv)
    } else if (ptype == "crosstab") {
      validate(need(
        nlev(d[[x]]) <= 30 && nlev(d[[y]]) <= 30,
        "Cross-tab works for discrete variables (≤ 30 levels each); one of these has too many values."
      ))
      crosstab_plot(d, x, y)
    } else {
      # stacked
      validate(need(
        nlev(d[[x]]) <= 40 && nlev(d[[y]]) <= 8,
        "Stacked bars are limited to 8 or fewer categories in the Y variable."
      ))
      d[[x]] <- labeled_factor(d[[x]], x)
      d[[y]] <- labeled_factor(d[[y]], y)
      ggplot(d, aes(.data[[x]], fill = .data[[y]])) +
        geom_bar(position = "fill") +
        scale_fill_brewer(palette = "Set2") +
        scale_y_continuous(labels = percent) +
        labs(x = paste0(x, "  —  ", label_of(x)), y = "Share", fill = y) +
        theme_eda +
        theme(axis.text.x = element_text(angle = 30, hjust = 1))
    }
  })

  output$bv_stat <- renderText({
    x <- input$bv_x
    y <- input$bv_y
    req(x, y)
    if (x == y) {
      return("")
    }
    tx <- var_type(x)
    ty <- var_type(y)
    if (tx == "continuous" && ty == "continuous") {
      s <- corr_stat(dat[[x]], dat[[y]], "pearson")
      sprintf("Pearson r = %.3f  (n = %d)", s$coef, s$n)
    } else if (is_num(x) && is_num(y)) {
      s <- corr_stat(dat[[x]], dat[[y]], "spearman")
      sprintf("Spearman ρ = %.3f  (n = %d)", s$coef, s$n)
    } else if (is_cat(x) && is_cat(y)) {
      s <- cramers_v(dat[[x]], dat[[y]])
      sprintf(
        "Cramér's V = %.3f  ·  chi-square p = %.3g  (n = %d)",
        s$v,
        s$p,
        s$n
      )
    } else {
      numv <- if (is_num(x)) x else y
      catv <- if (is_num(x)) y else x
      s <- eta_squared(as.numeric(dat[[numv]]), dat[[catv]])
      sprintf("η² = %.3f  ·  ANOVA p = %.3g  (n = %d)", s$eta2, s$p, s$n)
    }
  })

  # --- 3. Single-variable distribution --------------------------------------
  output$uni_plot <- renderPlot({
    v <- input$uni_var
    req(v)
    if (var_type(v) == "continuous") {
      ggplot(dat, aes(.data[[v]])) +
        geom_histogram(
          bins = input$uni_bins,
          fill = "#2c3e50",
          colour = "white"
        ) +
        labs(x = paste0(v, "  —  ", label_of(v)), y = "Count") +
        theme_eda
    } else {
      dd <- tibble(.lab = fct_infreq(labeled_factor(dat[[v]], v)))
      ggplot(dd, aes(.lab)) +
        geom_bar(fill = "#2c3e50") +
        labs(x = paste0(v, "  —  ", label_of(v)), y = "Count") +
        theme_eda +
        theme(axis.text.x = element_text(angle = 30, hjust = 1))
    }
  })

  output$uni_summary <- renderText({
    v <- input$uni_var
    req(v)
    if (is_num(v)) {
      x <- dat[[v]]
      sprintf(
        "%s · n = %d · missing = %d · mean = %.2f · median = %.2f · sd = %.2f · range = %g–%g",
        var_type(v),
        sum(!is.na(x)),
        sum(is.na(x)),
        mean(x, na.rm = TRUE),
        median(x, na.rm = TRUE),
        sd(x, na.rm = TRUE),
        min(x, na.rm = TRUE),
        max(x, na.rm = TRUE)
      )
    } else {
      x <- dat[[v]]
      sprintf(
        "categorical · n = %d · missing = %d · %d distinct levels",
        sum(!is.na(x)),
        sum(is.na(x)),
        dplyr::n_distinct(x, na.rm = TRUE)
      )
    }
  })

  # --- 4. Dictionary --------------------------------------------------------
  output$dict <- DT::renderDT(
    {
      tibble(
        variable = all_pickable,
        type = vapply(all_pickable, var_type, character(1)),
        question = vapply(all_pickable, label_of, character(1)),
        `value labels` = vapply(all_pickable, value_label_string, character(1))
      )
    },
    options = list(pageLength = 15, autoWidth = TRUE),
    rownames = FALSE
  )
}

shinyApp(ui, server)
