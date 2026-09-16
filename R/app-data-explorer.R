.ec_register_data_explorer_server <- function(input, output, data) {
  get_data <- function() if (is.function(data)) data() else data
  output$explore_profile <- shiny::renderUI({
    shiny::req(input$explore_var)
    dat <- get_data(); prof <- .ec_variable_profile(dat, input$explore_var)
    if (!nrow(prof)) return(shiny::p(class = "ec-note", "No summary is available for this variable."))
    shiny::div(class = "ec-profile-list", lapply(seq_len(nrow(prof)), function(i) {
      shiny::div(class = "ec-profile-row",
        shiny::tags$span(prof$measure[i]),
        shiny::tags$b(prof$value[i])
      )
    }))
  })
    
  output$explore_category_balance <- shiny::renderUI({
    shiny::req(input$explore_var)
    dat <- get_data(); x <- dat[[input$explore_var]]
    if (!(is.factor(x) || is.character(x) || is.logical(x) || .ec_is_binary_indicator(x))) return(NULL)
    z <- .ec_category_profile(x)
    if (!nrow(z)) return(NULL)
    show <- z
    show$share <- paste0(format(round(100 * show$share, 1), trim = TRUE), "%")
    names(show) <- c("Category", "N", "Share")
    shiny::tagList(
      shiny::h5("Category balance"),
      shiny::p(class = "ec-note", "Counts and shares are descriptive. A rare category can make categorical models harder to estimate, but econcompare does not decide whether a category should be changed or removed."),
      shiny::div(class = "ec-table-wrap ec-table-compact", shiny::HTML(.ec_html_table(show)))
    )
  })
    
  output$explore_distribution <- shiny::renderPlot({
    shiny::req(input$explore_var)
    dat <- get_data(); x <- dat[[input$explore_var]]
    old_par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old_par), add = TRUE)

    if (.ec_is_binary_indicator(x) || is.factor(x) || is.character(x) || is.logical(x)) {
      all_tab <- .ec_category_profile(x)
      tab <- .ec_category_counts(x)
      if (!nrow(tab)) {
        graphics::plot.new(); graphics::text(.5, .5, "No observed values")
      } else {
        labels <- as.character(tab$category)
        use_horizontal <- nrow(tab) > 6L || max(nchar(labels), na.rm = TRUE) > 12L
        plot_labels <- ifelse(
          nchar(labels) > 28L,
          paste0(substr(labels, 1L, 25L), "…"),
          labels
        )
        main <- if (nrow(all_tab) > nrow(tab)) paste0(input$explore_var, " · top ", nrow(tab), " categories") else input$explore_var
        if (use_horizontal) {
          graphics::par(mar = c(4.2, 9.2, 3.2, 1.2))
          graphics::barplot(
            rev(tab$count), names.arg = rev(plot_labels), horiz = TRUE, las = 1,
            main = main, xlab = "Count", cex.names = 0.8
          )
        } else {
          graphics::par(mar = c(6.8, 4.5, 3.2, 1.2))
          graphics::barplot(
            tab$count, names.arg = plot_labels, las = 2, main = main,
            ylab = "Count", cex.names = 0.85
          )
        }
      }
    } else if (is.numeric(x) || is.integer(x)) {
      vals <- suppressWarnings(as.numeric(x))
      vals <- vals[is.finite(vals)]
      if (!length(vals)) {
        graphics::plot.new(); graphics::text(.5, .5, "No finite numeric values")
      } else if (length(unique(vals)) < 2L) {
        graphics::plot.new(); graphics::text(.5, .5, "Constant numeric variable: no distributional spread")
      } else {
        graphics::par(mar = c(4.8, 4.5, 3.2, 1.2))
        graphics::hist(vals, main = input$explore_var, xlab = input$explore_var, border = NA)
      }
    } else {
      graphics::plot.new(); graphics::text(.5, .5, "No simple distribution view is available")
    }
  }, res = 96)
    
  output$explore_correlation <- shiny::renderUI({
    shiny::req(input$explore_x, input$explore_y)
    dat <- get_data(); status <- .ec_relationship_status(dat[[input$explore_x]], dat[[input$explore_y]])
    if (!isTRUE(status$ok)) return(shiny::p(class = "ec-note", status$message))
    r <- stats::cor(status$x, status$y, method = "pearson")
    shiny::div(class = "ec-corr-note",
      shiny::tags$b(paste0("Pearson correlation: ", .ec_fmt(r))),
      shiny::tags$span(paste0(" · ", status$n, " complete pair(s)"))
    )
  })
    
  output$explore_scatter <- shiny::renderPlot({
    shiny::req(input$explore_x, input$explore_y)
    dat <- get_data(); x <- suppressWarnings(as.numeric(dat[[input$explore_x]]))
    y <- suppressWarnings(as.numeric(dat[[input$explore_y]]))
    ok <- is.finite(x) & is.finite(y)
    old_par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old_par), add = TRUE)
    graphics::par(mar = c(4.8, 4.8, 3.2, 1.2))
    if (sum(ok) < 2L) {
      graphics::plot.new(); graphics::text(.5, .5, "Not enough complete numeric pairs")
    } else {
      graphics::plot(
        x[ok], y[ok], xlab = input$explore_x, ylab = input$explore_y,
        main = paste(input$explore_y, "vs", input$explore_x), pch = 19
      )
      if (length(unique(x[ok])) > 1L) {
        fit0 <- stats::lm(y[ok] ~ x[ok])
        coef0 <- stats::coef(fit0)
        if (length(coef0) >= 2L && all(is.finite(coef0[1:2]))) graphics::abline(fit0, lwd = 2)
      }
    }
  }, res = 96)
    
  output$explore_notices <- shiny::renderUI({
    dat <- get_data(); msgs <- .ec_data_notices(dat)
    shiny::tags$ul(class = "ec-check-list", lapply(msgs, shiny::tags$li))
  })
    
  invisible(NULL)
}
