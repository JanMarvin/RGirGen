#' @keywords internal
#'
#' Callback (function-pointer API) trampoline generation.
#'
#' GTK exposes two distinct callback mechanisms:
#'   1. GObject signals — handled by g_signal_connect_closure with a single
#'      generic marshaller (see rgtk4_signals.c). No code generation needed.
#'   2. Plain C function-pointer parameters such as GtkDrawingAreaDrawFunc,
#'      GCompareDataFunc, GAsyncReadyCallback. These have fixed C signatures
#'      and require one trampoline per signature.
#'
#' This file handles category 2. For each <callback> definition in the GIR,
#' we emit one C trampoline that:
#'   - matches the callback's exact C signature
#'   - converts each argument from C to SEXP using gi_type_to_map / int_entry
#'   - invokes rgtk4_eval_callback with the R closure stored in user_data
#'   - converts the R result back to the callback's return type, if any
#'
#' Generated bindings detect callback params via classify_callback_param() and
#' emit the right registration sequence (allocate RCallbackClosure, pass
#' trampoline + closure + rgtk4_closure_free as the destroy notify).

# ---------------------------------------------------------------------------
# Parser additions: read <callback> nodes.
# ---------------------------------------------------------------------------

parse_callbacks <- function(ns_node, int_types) {
  nodes <- xml2::xml_find_all(ns_node, ".//callback")
  parsed <- lapply(nodes, function(cb) {
    if (identical(xml2::xml_attr(cb, "introspectable"), "0")) return(NULL)
    name <- xml2::xml_attr(cb, "name")
    ctype <- xml2::xml_attr(cb, "type")
    if (is.na(name)) return(NULL)

    params <- xml2::xml_find_all(cb, "./parameters/parameter")
    parsed_params <- lapply(params, function(p) {
      direction <- xml2::xml_attr(p, "direction")
      if (is.na(direction)) direction <- "in"
      list(
        name      = xml2::xml_attr(p, "name"),
        type      = parse_type(p, int_types),
        direction = direction,
        nullable  = identical(xml2::xml_attr(p, "nullable"), "1") ||
          identical(xml2::xml_attr(p, "allow-none"), "1"),
        closure   = xml2::xml_attr(p, "closure"),
        destroy   = xml2::xml_attr(p, "destroy"),
        is_user_data = identical(xml2::xml_attr(p, "name"), "user_data") ||
          identical(xml2::xml_attr(p, "name"), "data")
      )
    })

    list(
      name = name,
      c_type = if (is.na(ctype)) name else ctype,
      params = parsed_params,
      return_type = parse_type(xml2::xml_find_first(cb, "./return-value"), int_types),
      throws = identical(xml2::xml_attr(cb, "throws"), "1")
    )
  })
  parsed[lengths(parsed) > 0]
}

# ---------------------------------------------------------------------------
# Trampoline name mangling.
# ---------------------------------------------------------------------------

trampoline_name <- function(callback_name) {
  paste0("_rgtk4_cb_", callback_name)
}

# ---------------------------------------------------------------------------
# C declaration for one callback parameter inside a trampoline signature.
# Returns a string like "GtkDrawingArea *area".
# ---------------------------------------------------------------------------

callback_param_decl <- function(p) {
  ct <- p$type$c
  if (is.null(ct) || is.na(ct) || !nzchar(ct)) {
    if (isTRUE(p$type$is_array)) ct <- "gpointer" else ct <- "gpointer"
  }
  pname <- p$name
  if (is.null(pname) || is.na(pname) || !nzchar(pname)) pname <- "arg"
  pname <- gsub("[^a-zA-Z0-9_]", "_", pname)
  sprintf("%s %s", ct, pname)
}

# ---------------------------------------------------------------------------
# Convert one trampoline argument (already typed in C) to an SEXP. Returns
# a snippet like "Rf_ScalarInteger((int)foo)" — paired with the C variable
# name from callback_param_decl. Mirrors the boxing logic in generate_c.R
# but operates on a single value rather than a return slot.
# ---------------------------------------------------------------------------

callback_box_expr <- function(p, c_var) {
  gi <- p$type$gi
  ct <- p$type$c
  int_entry <- p$type$int_entry

  # Arrays and pointer-typed C types must be wrapped as external pointers
  # regardless of GI element type. The GI `gi` field for an array reflects
  # the element type, not the parameter type.
  is_array <- isTRUE(p$type$is_array)
  is_pointer_ct <- !is.null(ct) && !is.na(ct) && grepl("\\*", ct)

  if (is_array || (is_pointer_ct && !(!is.null(gi) && gi %in% c("utf8","filename")))) {
    return(sprintf("R_MakeExternalPtr((void*)%s, R_NilValue, R_NilValue)", c_var))
  }

  if (!is.null(int_entry)) {
    return(box_inttype(int_entry, c_var))
  }

  if (!is.null(gi) && gi %in% c("utf8", "filename")) {
    return(sprintf("Rf_mkString(%s ? (const char*)%s : \"\")", c_var, c_var))
  }

  tm <- gi_type_to_map(gi)
  if (isTRUE(tm$known) && !isTRUE(tm$is_ptr)) {
    return(gsub("{{V}}", c_var, tm$box, fixed = TRUE))
  }

  sprintf("R_MakeExternalPtr((void*)%s, R_NilValue, R_NilValue)", c_var)
}

# ---------------------------------------------------------------------------
# Convert the R result back to the callback's return type. Returns a C
# snippet that, given an SEXP variable named `_r`, evaluates to the desired
# C value. Used inside the trampoline's `return ...;` statement.
# ---------------------------------------------------------------------------

callback_unbox_return <- function(rt) {
  gi <- rt$gi
  if (is.null(gi) || is.na(gi) || gi == "none") return(NULL)

  int_entry <- rt$int_entry

  # Pointer returns: regardless of GI element type, if c:type is a pointer,
  # treat as opaque external pointer (or NULL).
  ct <- if (nonempty(rt$c)) rt$c else NULL
  is_ptr <- !is.null(ct) && grepl("\\*", ct)

  if (is_ptr) {
    # String-like pointer returns: g_strdup the R string.
    if (gi %in% c("utf8","filename") ||
        identical(ct, "char*") || identical(ct, "gchar*") ||
        identical(ct, "const char*") || identical(ct, "const gchar*")) {
      decl <- ct
      expr <- paste0(
        "(_r == R_NilValue || TYPEOF(_r) != STRSXP || Rf_length(_r) == 0 || ",
        "STRING_ELT(_r, 0) == NA_STRING) ? NULL : g_strdup(CHAR(STRING_ELT(_r, 0)))"
      )
      return(list(decl = decl, expr = sprintf("(%s)(%s)", decl, expr)))
    }
    return(list(decl = ct,
                expr = sprintf("(%s)(_r == R_NilValue ? NULL : R_ExternalPtrAddr(_r))", ct)))
  }

  if (!is.null(int_entry)) {
    decl <- if (nonempty(int_entry$c_type)) int_entry$c_type else "gint"
    return(list(decl = decl, expr = unbox_inttype(int_entry, decl, "_r")))
  }

  if (gi == "gboolean") {
    return(list(decl = "gboolean", expr = "(gboolean)(Rf_asLogical(_r) == TRUE)"))
  }
  if (gi %in% c("gint","guint","gint8","guint8","gint16","guint16",
                "gint32","guint32","glong","gulong","gsize","gssize",
                "goffset","gint64","guint64","GQuark","GType")) {
    decl <- if (nonempty(rt$c)) rt$c else gi
    return(list(decl = decl, expr = sprintf("(%s)Rf_asInteger(_r)", decl)))
  }
  if (gi %in% c("gdouble","gfloat")) {
    decl <- if (nonempty(rt$c)) rt$c else gi
    return(list(decl = decl, expr = sprintf("(%s)Rf_asReal(_r)", decl)))
  }
  if (gi %in% c("utf8","filename")) {
    decl <- if (nonempty(rt$c)) rt$c else "char*"
    expr <- paste0(
      "(_r == R_NilValue || TYPEOF(_r) != STRSXP || Rf_length(_r) == 0 || ",
      "STRING_ELT(_r, 0) == NA_STRING) ? NULL : g_strdup(CHAR(STRING_ELT(_r, 0)))"
    )
    return(list(decl = decl, expr = sprintf("(%s)(%s)", decl, expr)))
  }
  ct2 <- if (nonempty(rt$c)) rt$c else "gpointer"
  list(decl = ct2, expr = sprintf("(%s)(_r == R_NilValue ? NULL : R_ExternalPtrAddr(_r))", ct2))
}

# Identify the user_data parameter index (1-based) for a callback. Returns
# NA if the callback has no slot for arbitrary user data — such callbacks
# cannot carry an R closure and must be rejected.
find_user_data_index <- function(params) {
  # Strict: only accept params explicitly named user_data or data with type
  # gpointer / void*. The "last gpointer" heuristic is unsafe — it has
  # produced wrong picks for callbacks like GCompareFunc where every param
  # is gconstpointer.
  for (i in rev(seq_along(params))) {
    p <- params[[i]]
    nm <- p$name
    if (is.null(nm) || is.na(nm)) next
    if (!nm %in% c("user_data", "data")) next
    ct <- p$type$c
    if (is.null(ct) || is.na(ct)) next
    if (ct %in% c("gpointer", "void*", "gconstpointer", "const void*")) return(i)
  }
  NA_integer_
}

# ---------------------------------------------------------------------------
# Emit one C trampoline. Returns either a string (the C function definition)
# or "" if the callback can't be supported (e.g. variadic, or a return type
# we can't safely synthesize).
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# Build the C signature string for one callback (without body). Returns a
# list(ok = TRUE/FALSE, prototype = "...", definition_head = "...", fname).
# Used by both the .c emitter (full definition) and the .h emitter (extern).
# ---------------------------------------------------------------------------

callback_signature <- function(cb) {
  if (!nonempty(cb$name)) return(list(ok = FALSE))
  if (any(sapply(cb$params, function(p) identical(p$name, "...")))) return(list(ok = FALSE))

  blocked_types <- c("GtkPrintJob", "GtkPrinter", "GtkPrintOperation",
                     "GtkPageSetup", "GtkPrintSettings",
                     "GDBusConnection", "GDBusMessage", "GDBusInterfaceInfo",
                     "GDBusMethodInvocation", "GDBusObjectProxy",
                     "GSubprocess", "GSubprocessLauncher",
                     "GTlsConnection", "GDtlsConnection",
                     "GdkPixbufModule",
                     "GtkAccessibleHypertext", "GtkAccessibleHyperlink",
                     "GtkTextBufferNotifyFlags",
                     "GskRenderReplay",
                     "GskPathIntersection",
                     "GskComponentTransfer",
                     "GskIsolation",
                     "GskPorterDuff",
                     "GdkColorChannel")
  all_types <- paste(c(cb$return_type$c, cb$return_type$gi,
                       sapply(cb$params, function(p) c(p$type$c, p$type$gi))),
                     collapse = " ")
  for (bt in blocked_types) {
    if (grepl(bt, all_types, fixed = TRUE)) return(list(ok = FALSE))
  }

  if (isTRUE(cb$throws)) return(list(ok = FALSE))

  ud_idx <- find_user_data_index(cb$params)
  has_user_data <- !is.na(ud_idx)
  # No-user_data callbacks are dispatched via TLS — only safe for callbacks
  # invoked synchronously from the registering call. We assume that's the
  # case for any no-ud callback (sort/compare/foreach pattern) and mark it.

  ret <- cb$return_type
  is_void <- is.null(ret$gi) || is.na(ret$gi) || ret$gi == "none"

  unbox_ret <- if (!is_void) callback_unbox_return(ret) else NULL
  if (!is_void && is.null(unbox_ret)) return(list(ok = FALSE))

  ret_decl <- if (is_void) "void" else unbox_ret$decl

  decls <- vapply(cb$params, callback_param_decl, character(1))
  sig <- paste(decls, collapse = ", ")
  fname <- trampoline_name(cb$name)

  list(
    ok            = TRUE,
    fname         = fname,
    ret_decl      = ret_decl,
    sig           = sig,
    ud_idx        = if (has_user_data) ud_idx else NA_integer_,
    has_user_data = has_user_data,
    unbox_ret     = if (is_void) NULL else unbox_ret$expr,
    prototype     = sprintf("extern %s %s(%s);", ret_decl, fname, sig)
  )
}

# ---------------------------------------------------------------------------
# Emit one C trampoline. Returns either a string (the C function definition)
# or "" if the callback can't be supported (e.g. variadic, or a return type
# we can't safely synthesize).
# ---------------------------------------------------------------------------

generate_callback_trampoline <- function(cb) {
  s <- callback_signature(cb)
  if (!isTRUE(s$ok)) return("")

  body <- c(sprintf("%s %s(%s) {", s$ret_decl, s$fname, s$sig))

  if (isTRUE(s$has_user_data)) {
    ud_name <- gsub("[^a-zA-Z0-9_]", "_", cb$params[[s$ud_idx]]$name)
    if (!nzchar(ud_name)) ud_name <- "arg"
    body <- c(body, sprintf("  RCallbackClosure *rc = (RCallbackClosure *)%s;", ud_name))
  } else {
    # No user_data slot — fetch from thread-local register set by binding.
    body <- c(body, "  RCallbackClosure *rc = rgtk4_current_closure();")
  }

  arg_lines <- character()
  arg_vars <- character()
  for (i in seq_along(cb$params)) {
    if (isTRUE(s$has_user_data) && i == s$ud_idx) next
    p <- cb$params[[i]]
    pname <- gsub("[^a-zA-Z0-9_]", "_", p$name)
    if (!nzchar(pname)) pname <- sprintf("arg%d", i)
    box <- callback_box_expr(p, pname)
    var <- sprintf("_a%d", i)
    arg_lines <- c(arg_lines,
                   sprintf("  SEXP %s = PROTECT(%s);", var, box))
    arg_vars <- c(arg_vars, var)
  }

  if (length(arg_lines) > 0) {
    body <- c(body, arg_lines)
    body <- c(body, sprintf("  SEXP _argv[%d] = { %s };",
                            length(arg_vars),
                            paste(arg_vars, collapse = ", ")))
    eval_call <- sprintf("rgtk4_eval_callback(rc, %d, _argv)", length(arg_vars))
  } else {
    eval_call <- "rgtk4_eval_callback(rc, 0, NULL)"
  }

  if (s$ret_decl == "void") {
    body <- c(body, sprintf("  (void)%s;", eval_call))
    if (length(arg_lines) > 0) {
      body <- c(body, sprintf("  UNPROTECT(%d);", length(arg_lines)))
    }
    body <- c(body, "}")
  } else {
    body <- c(body, sprintf("  SEXP _r = PROTECT(%s);", eval_call))
    body <- c(body, sprintf("  %s _result = %s;", s$ret_decl, s$unbox_ret))
    body <- c(body, sprintf("  UNPROTECT(%d);", length(arg_lines) + 1))
    body <- c(body, "  return _result;")
    body <- c(body, "}")
  }

  paste(body, collapse = "\n")
}

# ---------------------------------------------------------------------------
# Top-level: emit the entire callbacks autogen file.
# ---------------------------------------------------------------------------

generate_callbacks_file <- function(all_callbacks) {
  trampolines <- vapply(all_callbacks, generate_callback_trampoline, character(1))
  trampolines <- trampolines[nzchar(trampolines)]

  header <- c(
    "#define R_NO_REMAP",
    "#include <R.h>",
    "#include <Rinternals.h>",
    "#include <gtk/gtk.h>",
    "#include <glib.h>",
    "#include <glib-object.h>",
    "#include <gio/gio.h>",
    "#include <stdint.h>",
    "#include \"rgtk4_callbacks.h\"",
    "#include \"rgtk4_autogen_callbacks.h\"",
    "",
    "#pragma GCC diagnostic ignored \"-Wpedantic\"",
    "#pragma GCC diagnostic ignored \"-Wunused-parameter\"",
    "#pragma GCC diagnostic ignored \"-Wdeprecated-declarations\"",
    ""
  )

  paste(c(header, trampolines), collapse = "\n")
}

# Emit a header with extern declarations for every successfully generated
# trampoline. Included by every per-namespace autogen .c so the references
# from generated bindings resolve at compile time.
generate_callbacks_header <- function(all_callbacks) {
  protos <- character()
  for (cb in all_callbacks) {
    s <- callback_signature(cb)
    if (isTRUE(s$ok)) protos <- c(protos, s$prototype)
  }

  body <- c(
    "#ifndef RGTK4_AUTOGEN_CALLBACKS_H",
    "#define RGTK4_AUTOGEN_CALLBACKS_H",
    "",
    "#include <gtk/gtk.h>",
    "#include <glib.h>",
    "#include <glib-object.h>",
    "#include <gio/gio.h>",
    "",
    "#pragma GCC diagnostic push",
    "#pragma GCC diagnostic ignored \"-Wdeprecated-declarations\"",
    "",
    protos,
    "",
    "#pragma GCC diagnostic pop",
    "",
    "#endif"
  )
  paste(body, collapse = "\n")
}

# ---------------------------------------------------------------------------
# Used by generate_c.R when emitting bindings for functions that take a
# callback parameter. Returns NULL if the param isn't a callback.
# ---------------------------------------------------------------------------

callback_lookup <- function(callbacks_by_name, gi_name) {
  if (is.null(gi_name) || is.na(gi_name)) return(NULL)
  if (!is.null(callbacks_by_name[[gi_name]])) return(callbacks_by_name[[gi_name]])
  if (grepl("\\.", gi_name)) {
    short <- sub("^.*\\.", "", gi_name)
    return(callbacks_by_name[[short]])
  }
  NULL
}

# Heuristic for params that *look* like a callback but whose <callback>
# definition we don't have — falls back to checking the GI name suffix.
looks_like_callback <- function(gi_name) {
  if (is.null(gi_name) || is.na(gi_name)) return(FALSE)
  grepl("(Func|Notify|Callback)$", gi_name)
}

# ---------------------------------------------------------------------------
# Identify callback / closure / destroy parameter roles in a function's
# parameter list. Returns a character vector with one entry per param of role:
#   "callback"  - the R function ends up here
#   "user_data" - hidden, filled in with our RCallbackClosure*
#   "destroy"   - hidden, filled in with rgtk4_closure_free
#   "normal"    - regular param exposed to R
# Indices in `closure` and `destroy` GIR attributes are 0-based and refer to
# parameter positions excluding `instance-parameter` — we offset accordingly.
# Used by both generate_c.R and generate_r.R.
# ---------------------------------------------------------------------------

classify_param_roles <- function(params, callbacks_by_name) {
  n <- length(params)
  roles <- rep("normal", n)
  if (n == 0) return(roles)

  instance_offset <- 0L
  for (i in seq_len(n)) {
    if (isTRUE(params[[i]]$is_instance)) {
      instance_offset <- instance_offset + 1L
    } else {
      break
    }
  }

  is_user_data_slot <- function(p) {
    if (!identical(p$direction, "in")) return(FALSE)
    ct <- p$type$c
    gi <- p$type$gi
    if (is.null(ct) || is.na(ct)) ct <- ""
    ct_ok <- ct %in% c("gpointer", "void*", "gconstpointer", "const void*")
    gi_ok <- !is.null(gi) && !is.na(gi) && gi %in% c("gpointer", "none")
    ct_ok || gi_ok
  }

  is_destroy_slot <- function(p) {
    if (!identical(p$direction, "in")) return(FALSE)
    gi <- p$type$gi
    if (is.null(gi) || is.na(gi)) return(FALSE)
    gi %in% c("GLib.DestroyNotify", "DestroyNotify", "GDestroyNotify") ||
      grepl("DestroyNotify$", gi)
  }

  cb_idx <- integer()
  for (i in seq_len(n)) {
    p <- params[[i]]
    if (!identical(p$direction, "in")) next
    # Destroy-notify params are technically GIR callbacks too, but in our
    # model they're hidden destroy slots paired with a real callback.
    if (is_destroy_slot(p)) next
    cb_def <- callback_lookup(callbacks_by_name, p$type$gi)
    if (is.null(cb_def)) next
    cb_idx <- c(cb_idx, i)
  }

  for (i in cb_idx) {
    p <- params[[i]]

    closure_attr <- p$closure
    closure_idx <- NA_integer_
    if (!is.null(closure_attr) && !is.na(closure_attr) && nzchar(closure_attr)) {
      closure_idx <- suppressWarnings(as.integer(closure_attr)) + 1L + instance_offset
    }
    if (!is.na(closure_idx) && closure_idx >= 1L && closure_idx <= n &&
        is_user_data_slot(params[[closure_idx]])) {
      roles[closure_idx] <- "user_data"
    } else {
      for (j in seq.int(i + 1L, n)) {
        if (j > n) break
        if (is_user_data_slot(params[[j]]) && roles[j] == "normal") {
          roles[j] <- "user_data"
          break
        }
      }
    }

    destroy_attr <- p$destroy
    destroy_idx <- NA_integer_
    if (!is.null(destroy_attr) && !is.na(destroy_attr) && nzchar(destroy_attr)) {
      destroy_idx <- suppressWarnings(as.integer(destroy_attr)) + 1L + instance_offset
    }
    if (!is.na(destroy_idx) && destroy_idx >= 1L && destroy_idx <= n &&
        is_destroy_slot(params[[destroy_idx]])) {
      roles[destroy_idx] <- "destroy"
    } else {
      for (j in seq.int(i + 1L, n)) {
        if (j > n) break
        if (is_destroy_slot(params[[j]]) && roles[j] == "normal") {
          roles[j] <- "destroy"
          break
        }
      }
    }
  }

  for (i in cb_idx) roles[i] <- "callback"
  roles
}
