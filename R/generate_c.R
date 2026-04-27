#' @keywords internal

nonempty <- function(x) !is.null(x) && length(x) == 1L && !is.na(x) && nchar(x) > 0

# ---------------------------------------------------------------------------
# Skip tracing.
#
# To debug why a function is being dropped from the generated bindings, set
#   options(rgirgen.trace = c("gdk_pixbuf_new_from_data", "gtk_..."))
# (or rgirgen.trace = TRUE for all skips, rgirgen.trace = "*" same).
# Each rejection inside generate_c_function / generate_r_function is routed
# through skip() which prints the reason for matching symbols.
# ---------------------------------------------------------------------------

skip_trace_match <- function(c_symbol) {
  pat <- getOption("rgirgen.trace", FALSE)
  if (isFALSE(pat) || is.null(pat)) return(FALSE)
  if (isTRUE(pat) || identical(pat, "*")) return(TRUE)
  if (is.character(pat)) return(c_symbol %in% pat)
  FALSE
}

# Used inside generators: replace `return("")` with `return(skip(fn, "reason"))`.
# When tracing is off this is a zero-cost passthrough.
skip <- function(fn, reason) {
  if (!is.null(fn) && !is.null(fn$c_symbol) && skip_trace_match(fn$c_symbol)) {
    where <- sys.call(-1L)
    fname <- if (is.null(where)) "?" else as.character(where[[1L]])
    message(sprintf("[skip] %s in %s: %s", fn$c_symbol, fname, reason))
  }
  ""
}

strip_one_pointer <- function(ct) sub("\\*\\s*$", "", ct)
c_type_is_pointer <- function(ct) grepl("\\*", ct)

SAFE_BASE_TYPES <- c(
  "gpointer","gconstpointer","gboolean","gint","guint","gint8","guint8",
  "gint16","guint16","gint32","guint32","gint64","guint64","gdouble","gfloat",
  "gsize","gssize","goffset","glong","gulong","gchar","gunichar","GQuark","GType",
  "char","guchar","int","unsigned","long","short","float","double","void"
)

KNOWN_STRUCT_TYPES <- c(
  "GtkTextIter", "GdkRectangle", "GdkRGBA", "PangoRectangle", "graphene_rect_t",
  "graphene_point_t", "graphene_size_t", "graphene_vec2_t", "graphene_vec3_t",
  "graphene_vec4_t", "graphene_matrix_t", "graphene_quaternion_t",
  "graphene_box_t", "graphene_sphere_t", "graphene_triangle_t",
  "graphene_plane_t", "graphene_ray_t", "graphene_frustum_t",
  "graphene_euler_t", "graphene_point3d_t",
  "GTimeVal", "cairo_rectangle_int_t",
  "GdkEvent", "GStatBuf", "GPollFD"
)

NUMERIC_GI_TYPES <- c(
  "gboolean","gint","guint","gint8","guint8","gint16","guint16",
  "gint32","guint32","gint64","guint64","gdouble","gfloat",
  "gsize","gssize","goffset","glong","gulong","gchar","gunichar",
  "GQuark","GType"
)

EXTRA_HEADER_MAP <- list(
  "gstdio.h" = paste(c(
    "#include <glib/gstdio.h>",
    "#include <fcntl.h>",
    "#include <unistd.h>",
    "#include <sys/stat.h>",
    "#include <stdio.h>",
    "#include <utime.h>",
    "#include <time.h>"
  ), collapse = "\n"),
  "glib-unix.h" = "#include <glib-unix.h>",
  "gobject/"     = "#include <glib-object.h>",
  "glib-types.h" = "#include <glib-object.h>",
  "gio/"         = "#include <gio/gio.h>",
  "gio.h"        = "#include <gio/gio.h>",
  "gunixmounts.h"              = "unix:#include <gio/gunixmounts.h>",
  "gunixinputstream.h"         = "unix:#include <gio/gunixinputstream.h>",
  "gunixoutputstream.h"        = "unix:#include <gio/gunixoutputstream.h>",
  "gunixconnection.h"          = "unix:#include <gio/gunixconnection.h>",
  "gunixcredentialsmessage.h"  = "unix:#include <gio/gunixcredentialsmessage.h>",
  "gunixfdlist.h"              = "unix:#include <gio/gunixfdlist.h>",
  "gunixsocketaddress.h"       = "unix:#include <gio/gunixsocketaddress.h>",
  "gdesktopappinfo.h"          = "unix:#include <gio/gdesktopappinfo.h>",
  "gunixfdmessage.h"           = "unix:#include <gio/gunixfdmessage.h>",
  "gsettingsbackend.h" = paste(c(
    "#define G_SETTINGS_ENABLE_BACKEND",
    "#include <gio/gsettingsbackend.h>"
  ), collapse = "\n"),
  "gnetworking.h"              = "#include <gio/gnetworking.h>"
)

# ---------------------------------------------------------------------------
# Note: classify_param_roles is defined in generate_callbacks.R.

# ---------------------------------------------------------------------------
generate_c_file <- function(parsed, namespace, seen_global = NULL, callbacks_by_name = list()) {
  local_seen <- character()
  unique_fns <- Filter(function(f) {
    sym <- f$c_symbol
    if (sym %in% local_seen) return(FALSE)
    if (!is.null(seen_global) && sym %in% seen_global$symbols) return(FALSE)
    local_seen <<- c(local_seen, sym)
    TRUE
  }, parsed$functions)

  if (!is.null(seen_global))
    seen_global$symbols <- c(seen_global$symbols, local_seen)

  src_headers <- sapply(unique_fns, function(f) {
    if (!is.null(f$source_header) && !is.na(f$source_header)) f$source_header else ""
  })
  plain_includes <- character()
  unix_includes  <- character()
  for (pattern in names(EXTRA_HEADER_MAP)) {
    val <- EXTRA_HEADER_MAP[[pattern]]
    if (any(grepl(pattern, src_headers, fixed = TRUE))) {
      if (startsWith(val, "unix:"))
        unix_includes  <- c(unix_includes,  substring(val, 6))
      else
        plain_includes <- c(plain_includes, val)
    }
  }
  plain_includes <- unique(plain_includes)
  unix_includes  <- unique(unix_includes)
  guarded_unix <- if (length(unix_includes) > 0)
    c("#ifndef G_OS_WIN32", unix_includes, "#endif /* G_OS_WIN32 */")
  else character()

  blocks <- sapply(unique_fns, generate_c_function, callbacks_by_name = callbacks_by_name)
  blocks <- blocks[nchar(blocks) > 0]

  header <- paste(c(
    "#define R_NO_REMAP",
    "#include <R.h>",
    "#include <Rinternals.h>",
    "#include <gtk/gtk.h>",
    "#include <glib.h>",
    "#include <stdint.h>",
    "#include <string.h>",
    plain_includes,
    guarded_unix,
    "#include \"rgtk4_callbacks.h\"",
    "#include \"rgtk4_autogen_callbacks.h\"",
    "",
    "/* Suppress pedantic warnings in auto-generated GTK glue code */",
    "#pragma GCC diagnostic ignored \"-Wpedantic\"",
    "",
    "/* Agnostic numeric extraction helper */",
    "static inline double _unbox_numeric(SEXP s) {",
    "  if (TYPEOF(s) == REALSXP) return REAL(s)[0];",
    "  if (TYPEOF(s) == INTSXP)  return (double)INTEGER(s)[0];",
    "  if (TYPEOF(s) == LGLSXP)  return (double)LOGICAL(s)[0];",
    "  return 0.0;",
    "}",
    "",
    "/* Safe pointer extraction with validation */",
    "static inline void* get_ptr_internal(SEXP s, const char* func) __attribute__((unused));",
    "static inline void* get_ptr_internal(SEXP s, const char* func) {",
    "  if (s == R_NilValue) return NULL;",
    "  if (TYPEOF(s) != EXTPTRSXP) {",
    "    Rf_error(\"%s: expected external pointer, got %s\", func, Rf_type2char(TYPEOF(s)));",
    "  }",
    "  return R_ExternalPtrAddr(s);",
    "}",
    "#define get_ptr(s) get_ptr_internal(s, __func__)",
    "",
    "static void _finalizer_g_free(SEXP s) __attribute__((unused));",
    "static void _finalizer_g_free(SEXP s) {",
    "  void *p = R_ExternalPtrAddr(s);",
    "  if (p) g_free(p);",
    "}",
    "",
    "extern SEXP make_gobject_ptr(gpointer obj);",
    "extern SEXP make_boxed_struct(const void *src, size_t size);",
    "",
    "static SEXP _box_GStrv(char **strv) __attribute__((unused));",
    "static SEXP _box_GStrv(char **strv) {",
    "  if (!strv) return R_NilValue;",
    "  int n = g_strv_length(strv);",
    "  SEXP res = PROTECT(Rf_allocVector(STRSXP, n));",
    "  for (int i = 0; i < n; i++) SET_STRING_ELT(res, i, Rf_mkChar(strv[i]));",
    "  UNPROTECT(1);",
    "  return res;",
    "}",
    "",
    "static SEXP tag_pointer(SEXP ptr, const char* fallback_name) {",
    "  if (ptr == R_NilValue || TYPEOF(ptr) != EXTPTRSXP) return ptr;",
    "  void *obj = R_ExternalPtrAddr(ptr);",
    "  if ((uintptr_t)obj < 0x1000) {",
    "    SEXP classes = PROTECT(Rf_allocVector(STRSXP, 3));",
    "    SET_STRING_ELT(classes, 0, Rf_mkChar(fallback_name));",
    "    SET_STRING_ELT(classes, 1, Rf_mkChar(\"GObject\"));",
    "    SET_STRING_ELT(classes, 2, Rf_mkChar(\"RGtkObject\"));",
    "    Rf_setAttrib(ptr, R_ClassSymbol, classes);",
    "    UNPROTECT(1);",
    "    return ptr;",
    "  }",
    "  if (G_IS_OBJECT(obj)) {",
    "    SEXP classes = PROTECT(Rf_allocVector(STRSXP, 3));",
    "    SET_STRING_ELT(classes, 0, Rf_mkChar(G_OBJECT_TYPE_NAME(obj)));",
    "    SET_STRING_ELT(classes, 1, Rf_mkChar(\"GObject\"));",
    "    SET_STRING_ELT(classes, 2, Rf_mkChar(\"RGtkObject\"));",
    "    Rf_setAttrib(ptr, R_ClassSymbol, classes);",
    "    UNPROTECT(1);",
    "  } else {",
    "    SEXP classes = PROTECT(Rf_allocVector(STRSXP, 3));",
    "    SET_STRING_ELT(classes, 0, Rf_mkChar(fallback_name));",
    "    SET_STRING_ELT(classes, 1, Rf_mkChar(\"GObject\"));",
    "    SET_STRING_ELT(classes, 2, Rf_mkChar(\"RGtkObject\"));",
    "    Rf_setAttrib(ptr, R_ClassSymbol, classes);",
    "    UNPROTECT(1);",
    "  }",
    "  return ptr;",
    "}",
    "",
    sprintf("/* Autogenerated for %s */", namespace),
    "#pragma GCC diagnostic ignored \"-Wdeprecated-declarations\"",
    "#pragma GCC diagnostic ignored \"-Wint-to-pointer-cast\"",
    "#pragma GCC diagnostic ignored \"-Wpointer-to-int-cast\"",
    "#ifdef __clang__",
    "#pragma GCC diagnostic ignored \"-Wimplicit-enum-enum-cast\"",
    "#endif",
    ""
  ), collapse = "\n")

  paste(c(header, blocks), collapse = "\n")
}

classify_type <- function(type_info) {
  if (is.null(type_info)) return("none")
  gi <- type_info$gi
  if (is.null(gi) || is.na(gi) || gi == "none") return("none")
  if (isTRUE(type_info$is_array))      return("array")
  if (!is.null(type_info$int_entry))   return("inttype")
  tm <- gi_type_to_map(gi)
  if (isTRUE(tm$known))                return("scalar")
  return("pointer")
}

box_inttype <- function(int_entry, call_str) {
  inner <- if (nonempty(int_entry$inner_gi)) int_entry$inner_gi else "gint"
  if (inner == "gboolean")
    return(sprintf("Rf_ScalarLogical((int)(size_t)(%s))", call_str))
  if (inner %in% c("gint64","guint64","gdouble","gfloat",
                   "gsize","gssize","goffset","glong","gulong"))
    return(sprintf("Rf_ScalarReal((double)(size_t)(%s))", call_str))
  sprintf("Rf_ScalarInteger((int)(size_t)(%s))", call_str)
}

unbox_inttype <- function(int_entry, decl_type, s_nm) {
  inner <- if (nonempty(int_entry$inner_gi)) int_entry$inner_gi else "gint"
  if (inner == "gboolean")
    return(sprintf("(%s)(TYPEOF(%s)==EXTPTRSXP ? (size_t)R_ExternalPtrAddr(%s) : LOGICAL(%s)[0])",
                   decl_type, s_nm, s_nm, s_nm))
  if (inner %in% c("gint64","guint64","gdouble","gfloat",
                   "gsize","gssize","goffset","glong","gulong"))
    return(sprintf("(%s)(TYPEOF(%s)==EXTPTRSXP ? (size_t)R_ExternalPtrAddr(%s) : REAL(%s)[0])",
                   decl_type, s_nm, s_nm, s_nm))
  sprintf("(%s)(TYPEOF(%s)==EXTPTRSXP ? (size_t)R_ExternalPtrAddr(%s) : INTEGER(%s)[0])",
          decl_type, s_nm, s_nm, s_nm)
}

resolve_in_param <- function(p, s_nm) {
  gi  <- p$type$gi;  if (is.null(gi)  || is.na(gi))  gi  <- "none"
  ct  <- p$type$c;   if (is.null(ct)  || is.na(ct))  ct  <- ""
  cls <- classify_type(p$type)
  posix_structs <- c("tm", "utimbuf", "stat")

  if (cls == "none") return(list(decl_type = "gpointer", unbox_expr = "NULL"))

  if (cls == "array") {
    decl <- if (nonempty(ct)) ct else "gpointer"
    return(list(decl_type = decl, unbox_expr = sprintf("get_ptr(%s)", s_nm)))
  }

  if (cls == "inttype") {
    entry <- p$type$int_entry
    decl  <- if (nonempty(entry$c_type)) entry$c_type else "gint"
    return(list(decl_type = decl, unbox_expr = unbox_inttype(entry, decl, s_nm)))
  }

  if (gi %in% STRING_GI_TYPES) {
    tm <- gi_type_to_map(gi)
    decl <- if (nonempty(ct) && grepl("\\*.*\\*", ct)) ct
    else if (nonempty(ct) && grepl("\\*", ct) && !grepl("^const", trimws(ct))) ct
    else tm$decl_type
    return(list(decl_type  = decl, unbox_expr = gsub("{{X}}", s_nm, tm$unbox, fixed = TRUE)))
  }

  if (cls == "scalar" && nonempty(ct) && c_type_is_pointer(ct)) {
    if (gi %in% c("guint8","gint8"))
      return(list(decl_type  = ct, unbox_expr = sprintf("(%s)RAW(%s)", ct, s_nm)))
    if (gi %in% c("gdouble", "gfloat"))
      return(list(decl_type  = ct, unbox_expr = sprintf("(TYPEOF(%s)==INTSXP ? (double)INTEGER(%s)[0] : REAL(%s)[0])", s_nm, s_nm, s_nm)))
    if (gi == "guint32")
      return(list(decl_type  = ct, unbox_expr = sprintf("(guint32)REAL(%s)[0]", s_nm)))
    if (gi %in% c("gint32","guint","gint","guint16","gint16"))
      return(list(decl_type  = ct, unbox_expr = sprintf("(%s)INTEGER(%s)", ct, s_nm)))
    return(list(decl_type  = ct, unbox_expr = sprintf("get_ptr(%s)", s_nm)))
  }

  if (cls == "scalar") {
    tm <- gi_type_to_map(gi)
    return(list(decl_type  = tm$decl_type, unbox_expr = gsub("{{X}}", s_nm, tm$unbox, fixed = TRUE)))
  }

  decl <- if (nonempty(ct)) ct else "gpointer"
  if (trimws(decl) %in% posix_structs) decl <- paste("struct", decl)
  list(decl_type  = decl, unbox_expr = sprintf("get_ptr(%s)", s_nm))
}

# ---------------------------------------------------------------------------
generate_c_function <- function(fn, callbacks_by_name = list()) {
  if (isTRUE(fn$moved_to) || !nonempty(fn$c_symbol)) return(skip(fn, "moved_to or no c_symbol"))

  MIN_VERSIONS <- list(
    "GLib"      = "2.56",
    "GObject"   = "2.56",
    "Gio"       = "2.56",
    "Gdk"       = "4.12",
    "Gtk"       = "4.12",
    "Pango"     = "1.50",
    "GdkPixbuf" = "2.42"
  )

  namespace_detect <- NULL
  if (!is.null(fn$c_symbol)) {
    if (grepl("^g_(?!tk_|dk_)", fn$c_symbol, perl=TRUE)) {
      if (grepl("^g_object_", fn$c_symbol)) namespace_detect <- "GObject"
      else if (grepl("^g_file_|^g_app_|^g_settings_|^g_dbus_", fn$c_symbol)) namespace_detect <- "Gio"
      else namespace_detect <- "GLib"
    } else if (grepl("^gdk_pixbuf_", fn$c_symbol)) {
      namespace_detect <- "GdkPixbuf"
    } else if (grepl("^gdk_", fn$c_symbol)) {
      namespace_detect <- "Gdk"
    } else if (grepl("^gtk_", fn$c_symbol)) {
      namespace_detect <- "Gtk"
    } else if (grepl("^pango_", fn$c_symbol)) {
      namespace_detect <- "Pango"
    }
  }

  if (!is.null(namespace_detect) && !is.null(fn$version) && !is.na(fn$version)) {
    min_version <- MIN_VERSIONS[[namespace_detect]]
    if (!is.null(min_version) && compareVersion(fn$version, min_version) > 0) {
      return(skip(fn, sprintf("version filter: %s > %s for %s", fn$version, min_version, namespace_detect)))
    }
  }

  newer_types <- c(
    "GdkDmabufTextureBuilder", "GdkMemoryTextureBuilder",
    "GdkCicpParams", "GdkColorState",
    "GtkSymbolicPaintable",
    "GdkToplevelCapabilities",
    "GtkPopoverBin"
  )

  all_types <- paste(c(fn$return_type, sapply(fn$params, function(p) p$type)), collapse = " ")
  for (newer_type in newer_types) {
    if (grepl(newer_type, all_types, fixed = TRUE)) {
      return(skip(fn, sprintf("references newer type %s", newer_type)))
    }
  }

  if (grepl("^gdk_rgba_print|^gtk_svg_error_quark", fn$c_symbol)) {
    return(skip(fn, "explicit name skip (rgba_print / svg_error_quark)"))
  }

  if (grepl("^_|^g_osx_|^g_win32_|^g_msys_|^gtk_osx_|^g_unix_|^g_atomic_|^g_io_module_|^g_once_init_|^gtk_print_|^gtk_printer_|^gtk_enumerate_printers|^g_pointer_bit_|^g_dtls_|^g_tls_|^gtk_page_|^g_dbus_|^g_subprocess_|_unix_fd|_unix_user|_unix_pid|gdk_pixbuf_non_anim", fn$c_symbol)) {
    return(skip(fn, "regex blacklist (subsystems disabled)"))
  }

  posix_structs <- c("tm", "utimbuf", "stat")

  # Identify callback / user_data / destroy roles in the param list.
  roles <- classify_param_roles(fn$params, callbacks_by_name)

  # If we see a callback-typed param but it has no role assigned, that means
  # role classification failed to recognize it. Skip the binding — but only
  # if the unrecognized callback is a *real* user callback. Standalone
  # destroy-notify slots have no R-callable behavior; they pass through as
  # raw function pointers and the binding is emitted normally.
  is_destroy_typed <- function(gi) {
    if (is.null(gi) || is.na(gi)) return(FALSE)
    gi %in% c("GLib.DestroyNotify", "DestroyNotify", "GDestroyNotify") ||
      grepl("DestroyNotify$", gi)
  }
  for (i in seq_along(fn$params)) {
    p <- fn$params[[i]]
    if (looks_like_callback(p$type$gi) && roles[i] == "normal" &&
        !is_destroy_typed(p$type$gi)) {
      return(skip(fn, sprintf("param %d (%s) looks like callback but no role assigned",
                              i, p$type$gi)))
    }
  }

  for (i in seq_along(fn$params)) {
    if (roles[i] != "callback") next
    cb_def <- callback_lookup(callbacks_by_name, fn$params[[i]]$type$gi)
    if (is.null(cb_def)) return(skip(fn, sprintf("no <callback> def for param %d (%s)",
                                                 i, fn$params[[i]]$type$gi)))
    sig <- callback_signature(cb_def)
    if (!isTRUE(sig$ok)) {
      return(skip(fn, sprintf("callback %s rejected by signature check (returns string? throws? blocked type?)",
                              cb_def$name)))
    }
    # No-user_data callbacks dispatch via TLS; user_data slot is not required.
  }

  # Map each callback index to the trampoline name it needs.
  cb_partner_name <- function(i) {  # name of variable holding the closure
    sprintf("_cb_closure_%d", i)
  }

  in_params <- Filter(function(p) identical(p$direction, "in"), fn$params)
  out_params <- Filter(function(p) p$direction %in% c("out", "inout"), fn$params)
  has_error <- isTRUE(fn$throws)

  # Decide which "in" params are visible to R: drop user_data / destroy.
  # Callback params remain visible — they take an R function.
  visible_in_idx <- integer()
  for (i in seq_along(fn$params)) {
    p <- fn$params[[i]]
    if (!identical(p$direction, "in")) next
    if (roles[i] %in% c("user_data", "destroy")) next
    visible_in_idx <- c(visible_in_idx, i)
  }

  sexp_args <- if (length(visible_in_idx) == 0) "void" else
    paste0("SEXP s", seq_along(visible_in_idx), collapse = ", ")

  decls <- character(); call_args <- character()
  s_idx <- 1L  # index into the visible SEXP slots

  # Map fn$params index -> callback variable name (for matching user_data/destroy).
  cb_closure_var <- character(length(fn$params))
  # Names of closure vars that need TLS dispatch (callback has no user_data slot).
  no_ud_closures <- character()

  for (i in seq_along(fn$params)) {
    p <- fn$params[[i]]

    if (p$direction %in% c("out", "inout")) {
      local_nm <- paste0("_out_", p$name)
      base_ct <- strip_one_pointer(p$type$c)
      if (trimws(base_ct) == "void") base_ct <- "gpointer"

      decl_type <- base_ct
      bare_base <- sub("^const\\s+", "", trimws(decl_type))
      if (bare_base %in% posix_structs && !grepl("^struct\\s+", trimws(decl_type))) {
        decl_type <- paste("struct", decl_type)
      }

      is_ptr <- c_type_is_pointer(base_ct)
      init_val <- if (!is_ptr && !(base_ct %in% SAFE_BASE_TYPES)) "{0}" else "0"

      decls <- c(decls, sprintf("  %s %s = %s; (void)%s;", decl_type, local_nm, init_val, local_nm))
      call_args <- c(call_args, paste0("&", local_nm))
      next
    }

    role <- roles[i]

    if (role == "callback") {
      cb_def <- callback_lookup(callbacks_by_name, p$type$gi)
      if (is.null(cb_def)) return(skip(fn, sprintf("safety net: cb_def lost during emit for param %d", i)))
      sig <- callback_signature(cb_def)
      s_nm <- paste0("s", s_idx); s_idx <- s_idx + 1L
      var <- cb_partner_name(i)
      cb_closure_var[i] <- var
      tramp <- trampoline_name(cb_def$name)
      decls <- c(decls, sprintf("  RCallbackClosure *%s = (%s == R_NilValue) ? NULL : rgtk4_closure_new(%s); (void)%s;",
                                var, s_nm, s_nm, var))
      cb_decl <- if (nonempty(p$type$c)) p$type$c else "gpointer"
      call_args <- c(call_args, sprintf("(%s)(%s ? %s : NULL)", cb_decl, var, tramp))
      if (!isTRUE(sig$has_user_data)) {
        no_ud_closures <- c(no_ud_closures, var)
      }
      next
    }

    if (role == "user_data") {
      # Partner = nearest preceding callback. classify_param_roles already
      # ensured at most one user_data per callback in source order.
      partner_idx <- NA_integer_
      for (j in seq.int(i - 1L, 1L)) {
        if (j < 1L) break
        if (roles[j] == "callback") { partner_idx <- j; break }
      }
      if (!is.na(partner_idx)) {
        call_args <- c(call_args, cb_closure_var[partner_idx])
      } else {
        call_args <- c(call_args, "NULL")
      }
      next
    }

    if (role == "destroy") {
      call_args <- c(call_args, "rgtk4_closure_free")
      next
    }

    # Normal in param.
    s_nm <- paste0("s", s_idx); v_nm <- paste0("v", s_idx); s_idx <- s_idx + 1L
    r <- resolve_in_param(p, s_nm)
    v_type <- r$decl_type

    bare_type <- sub("^const\\s+", "", trimws(v_type))
    bare_type <- sub("\\s*\\*+$", "", bare_type)

    if (bare_type %in% posix_structs && !grepl("^struct\\s+", trimws(v_type))) {
      v_type <- paste("struct", v_type)
    }
    if (isTRUE(p$nullable) || isTRUE(p$allow_none)) {
      line <- sprintf("  %s %s = (%s != R_NilValue) ? (%s)(%s) : NULL; (void)%s;",
                      v_type, v_nm, s_nm, v_type, r$unbox_expr, v_nm)
    } else {
      line <- sprintf("  %s %s = (%s)(%s); (void)%s;",
                      v_type, v_nm, v_type, r$unbox_expr, v_nm)
    }

    decls <- c(decls, line)
    call_args <- c(call_args, v_nm)
  }

  if (has_error) { decls <- c(decls, "  GError *_err = NULL;"); call_args <- c(call_args, "&_err") }

  if (length(no_ud_closures) > 1) {
    return(skip(fn, sprintf("multiple no-user_data callbacks (TLS dispatch supports only one): %s",
                            paste(no_ud_closures, collapse = ", "))))
  }

  ret_gi <- fn$return_type$gi; if (is.null(ret_gi) || is.na(ret_gi)) ret_gi <- "none"
  items_to_return <- list()
  call_str <- sprintf("%s(%s)", fn$c_symbol, paste(call_args, collapse = ", "))
  body <- ""

  # If the call uses TLS dispatch for a no-user_data callback, save/restore
  # the current closure register around the call.
  use_tls <- length(no_ud_closures) == 1L
  if (use_tls) {
    body <- paste0(body, "  RCallbackClosure *_prev_closure = rgtk4_set_current_closure(",
                   no_ud_closures[1], ");\n")
  }

  if (ret_gi != "none") {
    is_ret_ptr <- c_type_is_pointer(fn$return_type$c)
    ret_decl <- if (is_ret_ptr) "gconstpointer" else fn$return_type$c
    body <- paste0(body, sprintf("  %s _ret = (%s)%s;\n", ret_decl, ret_decl, call_str))
    items_to_return[[1]] <- list(type=fn$return_type, var="_ret", name="result", is_ptr=is_ret_ptr, is_ret=TRUE)
  } else {
    body <- paste0(body, sprintf("  %s;\n", call_str))
  }

  if (use_tls) {
    body <- paste0(body, "  rgtk4_set_current_closure(_prev_closure);\n")
    body <- paste0(body, sprintf("  if (%s) rgtk4_closure_free(%s);\n",
                                 no_ud_closures[1], no_ud_closures[1]))
  }

  for (p in out_params) {
    items_to_return[[length(items_to_return)+1]] <- list(
      type=p$type, var=paste0("_out_", p$name), name=p$name,
      is_ptr=c_type_is_pointer(strip_one_pointer(p$type$c)), is_ret=FALSE
    )
  }

  n_items <- length(items_to_return)
  if (n_items > 0) {
    body <- paste0(body, sprintf("  SEXP _ans = PROTECT(Rf_allocVector(VECSXP, %d));\n", n_items))
    body <- paste0(body, sprintf("  SEXP _ans_names = PROTECT(Rf_allocVector(STRSXP, %d));\n", n_items))

    for (i in seq_along(items_to_return)) {
      it <- items_to_return[[i]]
      map <- gi_type_to_map(it$type$gi)

      is_numeric <- !is.null(it$type$gi) && it$type$gi %in% NUMERIC_GI_TYPES
      is_enum <- !is.null(it$type$int_entry)
      bare_ct <- sub("^struct\\s+", "", sub("^const\\s+", "", trimws(strip_one_pointer(it$type$c))))

      is_item_scalar <- (is_numeric || is_enum) && !it$is_ptr && (bare_ct %in% SAFE_BASE_TYPES)

      val_expr <- it$var
      if (!it$is_ret && !it$is_ptr && !is_item_scalar && !map$known) {
        val_expr <- paste0("&", val_expr)
      }

      type_label <- if (nonempty(it$type$gi)) it$type$gi else "gpointer"

      box_expr <- if (bare_ct %in% KNOWN_STRUCT_TYPES && !it$is_ptr) {
        sprintf("make_boxed_struct(&%s, sizeof(%s))", it$var, bare_ct)
      } else if (bare_ct %in% KNOWN_STRUCT_TYPES && it$is_ptr) {
        # Pointer-returning boxed-struct constructors (graphene_rect_alloc,
        # graphene_rect_init, etc.). Don't run through make_gobject_ptr or
        # tag_pointer — both call G_IS_OBJECT, which dereferences the
        # pointer as if it were a GTypeInstance. For a non-GObject struct
        # those bytes are arbitrary and the read may crash. We tag the
        # extptr's class statically below instead.
        sprintf("R_MakeExternalPtr((void*)%s, R_NilValue, R_NilValue)", val_expr)
      } else if (it$type$gi %in% c("utf8", "filename") && !grepl("\\*\\s*\\*", it$type$c)) {
        sprintf("Rf_mkString(%s ? (const char*)%s : \"\")", val_expr, val_expr)
      } else if (is_item_scalar) {
        sprintf("Rf_ScalarInteger((int)(%s))", val_expr)
      } else {
        if (it$is_ptr && !map$known) {
          sprintf("make_gobject_ptr((gpointer)%s)", val_expr)
        } else {
          standard_box <- gsub("\\{\\{V\\}\\}", val_expr, map$box)
          sprintf("tag_pointer(%s, \"%s\")", standard_box, type_label)
        }
      }

      # For boxed-struct pointer returns we bypassed tag_pointer; set the
      # class explicitly to (type, "GObject") for nicer printing without
      # dereferencing the pointer.
      static_class_set <- bare_ct %in% KNOWN_STRUCT_TYPES && it$is_ptr

      final_val <- if (it$is_ptr) sprintf("(%s == NULL) ? R_NilValue : %s", it$var, box_expr) else box_expr
      body <- paste0(body, sprintf("  SET_VECTOR_ELT(_ans, %d, %s);\n", i-1, final_val))

      if (static_class_set) {
        body <- paste0(body, sprintf("  if (VECTOR_ELT(_ans, %d) != R_NilValue) {\n", i-1))
        body <- paste0(body, sprintf("    SEXP _cls%d = PROTECT(Rf_allocVector(STRSXP, 2));\n", i-1))
        body <- paste0(body, sprintf("    SET_STRING_ELT(_cls%d, 0, Rf_mkChar(\"%s\"));\n", i-1, type_label))
        body <- paste0(body, sprintf("    SET_STRING_ELT(_cls%d, 1, Rf_mkChar(\"RGtkObject\"));\n", i-1))
        body <- paste0(body, sprintf("    Rf_setAttrib(VECTOR_ELT(_ans, %d), R_ClassSymbol, _cls%d);\n", i-1, i-1))
        body <- paste0(body, "    UNPROTECT(1);\n")
        body <- paste0(body, "  }\n")
      }

      body <- paste0(body, sprintf("  if (VECTOR_ELT(_ans, %d) != R_NilValue) {\n", i-1))
      body <- paste0(body, sprintf("    Rf_setAttrib(VECTOR_ELT(_ans, %d), Rf_install(\"glib_type\"), Rf_mkString(\"%s\"));\n", i-1, it$type$gi))
      body <- paste0(body, "  }\n")

      body <- paste0(body, sprintf("  SET_STRING_ELT(_ans_names, %d, Rf_mkChar(\"%s\"));\n", i-1, it$name))
    }
    body <- paste0(body, "  Rf_setAttrib(_ans, R_NamesSymbol, _ans_names);\n  UNPROTECT(2);\n  return _ans;")
  } else {
    body <- paste0(body, "  return R_NilValue;")
  }

  sprintf("\nSEXP R_%s(%s) {\n%s\n%s\n}\n", fn$c_symbol, sexp_args, paste(decls, collapse="\n"), body)
}
