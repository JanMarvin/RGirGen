#' Generate Bindings from GIR files
#'
#' @param gir_paths Character vector of paths to GIR files
#' @param out_dir   Directory containing R/ and src/ subdirectories
#' @export
generate_bindings <- function(gir_paths, out_dir) {
  if (!dir.exists(file.path(out_dir, "R")))
    dir.create(file.path(out_dir, "R"), recursive = TRUE)
  if (!dir.exists(file.path(out_dir, "src")))
    dir.create(file.path(out_dir, "src"), recursive = TRUE)

  seen_global <- new.env(parent = emptyenv())
  seen_global$symbols <- character()
  int_types_global <- list()

  message("Pass 1: collecting type registries...")
  all_parsed <- lapply(gir_paths, function(p) parse_gir(p))
  for (parsed in all_parsed)
    int_types_global <- c(int_types_global, parsed$int_types)
  message(sprintf("  %d int-types in merged registry", length(int_types_global)))

  callbacks_by_name <- list()
  for (parsed in all_parsed) {
    for (cb in parsed$callbacks) {
      cb$return_type <- enrich_type(cb$return_type, int_types_global)
      cb$params <- lapply(cb$params, function(p) {
        p$type <- enrich_type(p$type, int_types_global)
        p
      })
      callbacks_by_name[[cb$name]] <- cb
      callbacks_by_name[[paste0(parsed$namespace, ".", cb$name)]] <- cb
    }
  }
  message(sprintf("  %d callback definitions in registry",
                  sum(!grepl("\\.", names(callbacks_by_name)))))

  message("Pass 2: generating code...")
  for (parsed in all_parsed) {
    parsed$functions <- lapply(parsed$functions, function(fn) {
      fn$params <- lapply(fn$params, function(p) {
        p$type <- enrich_type(p$type, int_types_global)
        p
      })
      fn$return_type <- enrich_type(fn$return_type, int_types_global)
      fn
    })

    ns <- parsed$namespace
    writeLines(generate_r_file(parsed, ns, seen_global, callbacks_by_name),
               file.path(out_dir, "R",   paste0(ns, "_autogen.R")))
    writeLines(generate_c_file(parsed, ns, seen_global, callbacks_by_name),
               file.path(out_dir, "src", paste0(ns, "_autogen.c")))
    message(sprintf("  %-12s %d functions, %d enums, %d callbacks",
                    paste0(ns, ":"),
                    length(parsed$functions),
                    length(parsed$enums),
                    length(parsed$callbacks)))
  }

  unique_cbs <- callbacks_by_name[!grepl("\\.", names(callbacks_by_name))]
  writeLines(generate_callbacks_header(unique_cbs),
             file.path(out_dir, "src", "rgtk4_autogen_callbacks.h"))
  writeLines(generate_callbacks_file(unique_cbs),
             file.path(out_dir, "src", "Callbacks_autogen.c"))
  message(sprintf("  Callbacks_autogen.c: %d trampolines", length(unique_cbs)))

  message("Done. Generated autogen bindings only.")
  message("NOTE: Copy rgtk4_eventloop.c, rgtk4_signals.c, rgtk4_helpers.c,")
  message("      rgtk4_callbacks.c, and rgtk4_callbacks.h to your package manually.")
}
