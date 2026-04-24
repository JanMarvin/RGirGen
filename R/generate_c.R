#' @keywords internal

nonempty <- function(x) !is.null(x) && length(x) == 1L && !is.na(x) && nchar(x) > 0

# Strip one pointer level from a C type
strip_one_pointer <- function(ct) {
  sub("\\*\\s*$", "", ct)
}

# Check if C type is a pointer
c_type_is_pointer <- function(ct) {
  grepl("\\*", ct)
}

# Known safe C base types for out-param local variable declarations.
SAFE_BASE_TYPES <- c(
  "gpointer","gconstpointer","gboolean","gint","guint","gint8","guint8",
  "gint16","guint16","gint32","guint32","gint64","guint64","gdouble","gfloat",
  "gsize","gssize","goffset","glong","gulong","gchar","gunichar","GQuark","GType",
  "char","guchar","int","unsigned","long","short","float","double","void"
)

# Structs that are typedefs but require {0} initialization
KNOWN_STRUCT_TYPES <- c(
  "GtkTextIter", "GdkRectangle", "GdkRGBA", "PangoRectangle", "graphene_rect_t",
  "graphene_point_t", "graphene_size_t", "GTimeVal", "cairo_rectangle_int_t",
  "GdkEvent", "GStatBuf", "GPollFD"
)

# GI types that are plain numeric scalars (never pointers)
NUMERIC_GI_TYPES <- c(
  "gboolean","gint","guint","gint8","guint8","gint16","guint16",
  "gint32","guint32","gint64","guint64","gdouble","gfloat",
  "gsize","gssize","goffset","glong","gulong","gchar","gunichar",
  "GQuark","GType"
)

# Map source-header path fragments to the #include lines they require.
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
generate_c_file <- function(parsed, namespace, seen_global = NULL) {
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

  blocks <- sapply(unique_fns, generate_c_function)
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
    "",
    "/* Suppress pedantic warnings in auto-generated GTK glue code */",
    "#pragma GCC diagnostic ignored \"-Wpedantic\"",
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
    "/* Finalizer for heap-allocated value structs */",
    "static void _finalizer_g_free(SEXP s) __attribute__((unused));",
    "static void _finalizer_g_free(SEXP s) {",
    "  void *p = R_ExternalPtrAddr(s);",
    "  if (p) g_free(p);",
    "}",
    "",
    "/* Helpers from rgtk4_helpers.c */",
    "extern SEXP make_gobject_ptr(gpointer obj);",
    "extern SEXP make_boxed_struct(const void *src, size_t size);",
    "",
    "/* Helper to box GStrv (char**) into R character vector */",
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
    "/* Helper to add an S3 class to an external pointer for easier debugging */",
    "static SEXP tag_pointer(SEXP ptr, const char* fallback_name) {",
    "  if (ptr == R_NilValue || TYPEOF(ptr) != EXTPTRSXP) return ptr;",
    "",
    "  void *obj = R_ExternalPtrAddr(ptr);",
    "",
    "  // Safety check: skip G_IS_OBJECT if the address is clearly invalid (< 4096)",
    "  if ((uintptr_t)obj < 0x1000) {",
    "    SEXP classes = PROTECT(Rf_allocVector(STRSXP, 2));",
    "    SET_STRING_ELT(classes, 0, Rf_mkChar(fallback_name));",
    "    SET_STRING_ELT(classes, 1, Rf_mkChar(\"GObject\"));",
    "    Rf_setAttrib(ptr, R_ClassSymbol, classes);",
    "    UNPROTECT(1);",
    "    return ptr;",
    "  }",
    "",
    "  if (G_IS_OBJECT(obj)) {",
    "    SEXP classes = PROTECT(Rf_allocVector(STRSXP, 2));",
    "    SET_STRING_ELT(classes, 0, Rf_mkChar(G_OBJECT_TYPE_NAME(obj)));",
    "    SET_STRING_ELT(classes, 1, Rf_mkChar(\"GObject\"));",
    "    Rf_setAttrib(ptr, R_ClassSymbol, classes);",
    "    UNPROTECT(1);",
    "  } else {",
    "    SEXP classes = PROTECT(Rf_allocVector(STRSXP, 2));",
    "    SET_STRING_ELT(classes, 0, Rf_mkChar(fallback_name));",
    "    SET_STRING_ELT(classes, 1, Rf_mkChar(\"GObject\"));",
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
    # Honor C-type if it has more than one level of pointer (e.g. const gchar**)
    decl <- if (nonempty(ct) && grepl("\\*.*\\*", ct)) ct
    else if (nonempty(ct) && grepl("\\*", ct) && !grepl("^const", trimws(ct))) ct
    else tm$decl_type
    return(list(decl_type  = decl, unbox_expr = gsub("{{X}}", s_nm, tm$unbox, fixed = TRUE)))
  }

  if (cls == "scalar" && nonempty(ct) && c_type_is_pointer(ct)) {
    if (gi %in% c("guint8","gint8"))
      return(list(decl_type  = ct, unbox_expr = sprintf("(%s)RAW(%s)", ct, s_nm)))
    if (gi %in% c("guint32","gint32","guint","gint","guint16","gint16"))
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
generate_c_function <- function(fn) {
  if (isTRUE(fn$moved_to) || !nonempty(fn$c_symbol)) return("")

  # ============================================================================
  # GTK Version Compatibility Filter
  # ============================================================================
  # Define minimum required versions per namespace for maximum compatibility
  # These target Ubuntu 24.04 LTS which has: GLib 2.80, GTK 4.14
  MIN_VERSIONS <- list(
    "GLib"   = "2.56",   # Very stable, Ubuntu 24.04 has 2.80
    "GObject"= "2.56",   # Part of GLib
    "Gio"    = "2.56",   # Part of GLib
    "Gdk"    = "4.12",   # Ubuntu 24.04 has GTK 4.14
    "Gtk"    = "4.12"    # Ubuntu 24.04 has GTK 4.14
  )

  # Determine which namespace this function belongs to
  namespace <- NULL
  if (!is.null(fn$c_symbol)) {
    # Extract namespace from C symbol prefix
    if (grepl("^g_(?!tk_|dk_)", fn$c_symbol, perl=TRUE)) {
      if (grepl("^g_object_", fn$c_symbol)) namespace <- "GObject"
      else if (grepl("^g_file_|^g_app_|^g_settings_|^g_dbus_", fn$c_symbol)) namespace <- "Gio"
      else namespace <- "GLib"
    } else if (grepl("^gdk_", fn$c_symbol)) {
      namespace <- "Gdk"
    } else if (grepl("^gtk_", fn$c_symbol)) {
      namespace <- "Gtk"
    }
  }

  # Check GIR version attribute (now extracted by parser)
  if (!is.null(namespace) && !is.null(fn$version) && !is.na(fn$version)) {
    min_version <- MIN_VERSIONS[[namespace]]
    if (!is.null(min_version) && compareVersion(fn$version, min_version) > 0) {
      return("")  # Skip functions newer than minimum version
    }
  }

  # - Skip symbols starting with underscores (private/internal)

  # Fallback: type blacklist for types introduced after 4.12
  # This catches cases where the type is new but functions using it lack version tags
  newer_types <- c(
    "GdkDmabufTextureBuilder", "GdkMemoryTextureBuilder",  # 4.14
    "GdkCicpParams", "GdkColorState",                      # 4.16
    "GtkSymbolicPaintable",                                # 4.18
    "GdkToplevelCapabilities",                             # 4.20
    "GtkPopoverBin"                                        # 4.22
  )

  all_types <- paste(c(fn$return_type, sapply(fn$params, function(p) p$type)), collapse = " ")
  for (newer_type in newer_types) {
    if (grepl(newer_type, all_types, fixed = TRUE)) {
      return("")
    }
  }

  # somehow missing as well
  if (grepl("^gdk_rgba_print|^gtk_svg_error_quark", fn$c_symbol)) {
    return("")
  }

  # - Skip OS-specific backends (OSX, Win32, Unix-specific)
  # - Skip internal module/atomic symbols
  # - Skip g_io_module symbols (internal to GIO's plugin system)
  # - Skip gtk_printer_ printer things (could be enabled later if needed)
  # - Skip g_pointer_bit_
  # - Skip g_dtls_
  # - Skip DBus (Unix-specific in practice)
  # - Skip subprocess (Unix-specific)
  # - Skip Unix FD, Unix user, Unix PID functions
  if (grepl("^_|^g_osx_|^g_win32_|^g_msys_|^gtk_osx_|^g_unix_|^g_atomic_|^g_io_module_|^g_once_init_|^gtk_print_|^gtk_printer_|^gtk_enumerate_printers|^g_pointer_bit_|^g_dtls_|^g_tls_|^gtk_page_|^g_dbus_|^g_subprocess_|_unix_fd|_unix_user|_unix_pid", fn$c_symbol)) {
    return("")
  }

  posix_structs <- c("tm", "utimbuf", "stat")

  # --- Argument Setup ---
  in_params <- Filter(function(p) identical(p$direction, "in"), fn$params)
  out_params <- Filter(function(p) p$direction %in% c("out", "inout"), fn$params)
  has_error <- isTRUE(fn$throws)

  sexp_args <- if (length(in_params) == 0) "void" else
    paste0("SEXP s", seq_along(in_params), collapse = ", ")

  decls <- character(); call_args <- character(); s_idx <- 1L

  for (p in fn$params) {
    if (p$direction %in% c("out", "inout")) {
      local_nm <- paste0("_out_", p$name)
      base_ct <- strip_one_pointer(p$type$c)

      # FIX: C cannot declare a 'void' variable. If we stripped void*, use gpointer.
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

    s_nm <- paste0("s", s_idx); v_nm <- paste0("v", s_idx); s_idx <- s_idx + 1L
    r <- resolve_in_param(p, s_nm)
    v_type <- r$decl_type

    bare_type <- sub("^const\\s+", "", trimws(v_type))
    bare_type <- sub("\\s*\\*+$", "", bare_type)

    if (bare_type %in% posix_structs && !grepl("^struct\\s+", trimws(v_type))) {
      v_type <- paste("struct", v_type)
    }
    # Check if the parameter is allowed to be NULL/None
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

  # --- Execution ---
  ret_gi <- fn$return_type$gi; if (is.null(ret_gi) || is.na(ret_gi)) ret_gi <- "none"
  items_to_return <- list()
  call_str <- sprintf("%s(%s)", fn$c_symbol, paste(call_args, collapse = ", "))
  body <- ""

  if (ret_gi != "none") {
    is_ret_ptr <- c_type_is_pointer(fn$return_type$c)
    ret_decl <- if (is_ret_ptr) "gconstpointer" else fn$return_type$c
    body <- paste0(body, sprintf("  %s _ret = (%s)%s;\n", ret_decl, ret_decl, call_str))
    items_to_return[[1]] <- list(type=fn$return_type, var="_ret", name="result", is_ptr=is_ret_ptr, is_ret=TRUE)
  } else {
    body <- paste0(body, sprintf("  %s;\n", call_str))
  }

  for (p in out_params) {
    items_to_return[[length(items_to_return)+1]] <- list(
      type=p$type, var=paste0("_out_", p$name), name=p$name,
      is_ptr=c_type_is_pointer(strip_one_pointer(p$type$c)), is_ret=FALSE
    )
  }

  # --- Return Packaging ---
  n_items <- length(items_to_return)
  if (n_items > 0) {
    body <- paste0(body, sprintf("  SEXP _ans = PROTECT(Rf_allocVector(VECSXP, %d));\n", n_items))
    body <- paste0(body, sprintf("  SEXP _ans_names = PROTECT(Rf_allocVector(STRSXP, %d));\n", n_items))

    for (i in seq_along(items_to_return)) {
      it <- items_to_return[[i]]
      map <- gi_type_to_map(it$type$gi)

      # FIX: Strict scalar check. A type is only a scalar if it's a numeric/enum
      # AND it's not a pointer. Also check SAFE_BASE_TYPES to ensure it's not a struct.
      is_numeric <- !is.null(it$type$gi) && it$type$gi %in% NUMERIC_GI_TYPES
      is_enum <- !is.null(it$type$int_entry)
      bare_ct <- sub("^struct\\s+", "", sub("^const\\s+", "", trimws(strip_one_pointer(it$type$c))))

      is_item_scalar <- (is_numeric || is_enum) && !it$is_ptr && (bare_ct %in% SAFE_BASE_TYPES)

      val_expr <- it$var
      # If it's a stack-allocated struct (like GByteArray), we need its address for the external pointer.
      if (!it$is_ret && !it$is_ptr && !is_item_scalar && !map$known) {
        val_expr <- paste0("&", val_expr)
      }

      type_label <- if (nonempty(it$type$gi)) it$type$gi else "gpointer"

      box_expr <- if (bare_ct %in% KNOWN_STRUCT_TYPES && !it$is_ptr) {
        # Copy the stack-allocated struct into a new heap-allocated ExternalPtr
        sprintf("make_boxed_struct(&%s, sizeof(%s))", it$var, bare_ct)
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

      final_val <- if (it$is_ptr) sprintf("(%s == NULL) ? R_NilValue : %s", it$var, box_expr) else box_expr
      body <- paste0(body, sprintf("  SET_VECTOR_ELT(_ans, %d, %s);\n", i-1, final_val))

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
