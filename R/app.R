#' Launch the interactive econcompare model laboratory
#'
#' @param data A data.frame to analyse. For example `mtcars`.
#' @param launch.browser Passed to [shiny::runApp()]. In RStudio, the default
#'   opens the application in the Viewer pane when available.
#' @return A Shiny application object, invisibly after the app exits.
#' @export
#' @examples
#' \dontrun{
#' eco_app(mtcars)
#' }

eco_app <- function(data, launch.browser = getOption("shiny.launch.browser", interactive())) {
  .ec_require("shiny", "interactive app")
  if (missing(data) || !is.data.frame(data)) .ec_stop("`data` must be supplied as a data.frame, e.g. eco_app(mtcars).")

  data <- as.data.frame(data)
  .ec_validate_data_columns(data)
  if (nrow(data) < 1L) .ec_stop("`data` must contain at least one row.")
  all_vars <- names(data)
  numeric_vars <- all_vars[vapply(data, is.numeric, logical(1))]
  binary_candidates <- all_vars[vapply(data, .ec_is_binary_indicator, logical(1))]
  if (length(all_vars) < 2L) .ec_stop("`data` needs at least two columns.")
  initial_y <- if (length(numeric_vars)) numeric_vars[1L] else all_vars[1L]
  initial_type <- .ec_outcome_type(data[[initial_y]])
  if (!initial_type %in% c("continuous", "binary", "nominal", "ordinal")) initial_type <- "continuous"
  registry <- eco_models()
  structure_info <- eco_data_structure(data)
  initial_mode <- if (identical(structure_info$structure, "time_series")) "time_series" else "cross_section"
  initial_time <- if (!is.null(structure_info$time_variable)) structure_info$time_variable else ""

  ui <- .ec_app_ui(data, all_vars, numeric_vars, initial_y, initial_type, structure_info, initial_mode, initial_time)

  server <- .ec_app_server(data, all_vars, numeric_vars, binary_candidates, registry, initial_y, initial_type, structure_info, initial_mode, initial_time)

  app <- shiny::shinyApp(ui = ui, server = server)
  shiny::runApp(app, launch.browser = launch.browser)
  invisible(app)
}
