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

  # Shared state across all GIR files in this run:
  #   seen_global  — prevents duplicate symbols (e.g. g_variant_get_gtype in
  #                  both GLib-2.0.gir and GObject-2.0.gir)
  #   int_types_global — merged integer-type registry so cross-namespace types
  #                  like GLib.Quark (defined in GLib, used in GObject) resolve
  #                  correctly in every generated file.
  seen_global <- new.env(parent = emptyenv())
  seen_global$symbols <- character()
  int_types_global <- list()

  # First pass: parse all GIRs and merge their integer-type registries.
  # No code generation yet — we just need the complete merged registry.
  message("Pass 1: collecting type registries...")
  all_parsed <- lapply(gir_paths, function(p) parse_gir(p))
  for (parsed in all_parsed)
    int_types_global <- c(int_types_global, parsed$int_types)
  message(sprintf("  %d int-types in merged registry", length(int_types_global)))

  # Second pass: enrich types with the merged registry, then generate.
  message("Pass 2: generating code...")
  for (parsed in all_parsed) {
    # Re-annotate int_entry on every param/return using the full merged registry
    parsed$functions <- lapply(parsed$functions, function(fn) {
      fn$params <- lapply(fn$params, function(p) {
        p$type <- enrich_type(p$type, int_types_global)
        p
      })
      fn$return_type <- enrich_type(fn$return_type, int_types_global)
      fn
    })

    ns <- parsed$namespace
    writeLines(generate_r_file(parsed, ns, seen_global),
               file.path(out_dir, "R",   paste0(ns, "_autogen.R")))
    writeLines(generate_c_file(parsed, ns, seen_global),
               file.path(out_dir, "src", paste0(ns, "_autogen.c")))
    message(sprintf("  %-12s %d functions, %d enums",
                    paste0(ns, ":"), length(parsed$functions), length(parsed$enums)))
  }
  message("Done. Generated autogen bindings only.")
  message("NOTE: Copy rgtk4_eventloop.c, rgtk4_signals.c, and rgtk4_helpers.c to your package manually.")
}
