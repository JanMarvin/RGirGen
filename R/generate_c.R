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
# Anything else (bare struct names: tm, GPollFD, GStatBuf, utimbuf…) triggers a skip.
SAFE_BASE_TYPES <- c(
  # Primitive C types
  "gpointer","gconstpointer","gboolean","gint","guint","gint8","guint8",
  "gint16","guint16","gint32","guint32","gint64","guint64","gdouble","gfloat",
  "gsize","gssize","goffset","glong","gulong","gchar","gunichar","GQuark","GType",
  "char","guchar","int","unsigned","long","short","float","double","void",

  # GTK/GDK structs - small, stack-safe, commonly used as OUT params
  "GtkTextIter",           # Text iterator - CRITICAL for text editing
  "GdkRectangle",          # Rectangle {x, y, width, height}
  "GdkRGBA",               # Color {red, green, blue, alpha}
  "PangoRectangle",        # Text layout rectangle
  "graphene_rect_t",       # GTK4 rectangle
  "graphene_point_t",      # GTK4 point
  "graphene_size_t",       # GTK4 size
  "GTimeVal",              # Time value
  "cairo_rectangle_int_t"  # Cairo rectangle
)

# Map source-header path fragments to the #include lines they require.
# Each entry is checked with grepl(pattern, source_header, fixed=TRUE).
#
# Rules:
#  - glib.h does NOT include gstdio.h, glib-object.h, or gio/gio.h.
#  - gstdio.h macros expand to bare POSIX names requiring system headers.
#  - gio/gio.h is the Gio umbrella but does NOT include unix-specific headers.
#  - gobject/*.h all need glib-object.h.
EXTRA_HEADER_MAP <- list(
  # GLib stdio wrappers + the POSIX headers their macros expand to
  "gstdio.h" = paste(c(
    "#include <glib/gstdio.h>",
    "#include <fcntl.h>",     # open, creat
    "#include <unistd.h>",    # access, chdir, close, fsync, rmdir, unlink
    "#include <sys/stat.h>",  # chmod, mkdir, stat, lstat
    "#include <stdio.h>",     # fopen, freopen, remove, rename
    "#include <utime.h>"      # utime
  ), collapse = "\n"),

  # GLib Unix-specific (not in glib.h)
  "glib-unix.h" = "#include <glib-unix.h>",

  # GObject type system (not in glib.h)
  "gobject/"     = "#include <glib-object.h>",
  "glib-types.h" = "#include <glib-object.h>",

  # Gio umbrella
  "gio/"         = "#include <gio/gio.h>",
  "gio.h"        = "#include <gio/gio.h>",

  # Gio Unix/desktop-specific headers NOT in gio/gio.h on all platforms.
  # Marked with "unix:" prefix so the emitter wraps them in #ifndef G_OS_WIN32.
  "gunixmounts.h"              = "unix:#include <gio/gunixmounts.h>",
  "gunixinputstream.h"         = "unix:#include <gio/gunixinputstream.h>",
  "gunixoutputstream.h"        = "unix:#include <gio/gunixoutputstream.h>",
  "gunixconnection.h"          = "unix:#include <gio/gunixconnection.h>",
  "gunixcredentialsmessage.h"  = "unix:#include <gio/gunixcredentialsmessage.h>",
  "gunixfdlist.h"              = "unix:#include <gio/gunixfdlist.h>",
  "gunixsocketaddress.h"       = "unix:#include <gio/gunixsocketaddress.h>",
  "gdesktopappinfo.h"          = "unix:#include <gio/gdesktopappinfo.h>",
  "gunixfdmessage.h"           = "unix:#include <gio/gunixfdmessage.h>",
  # Opt-in gio headers that are cross-platform but not in gio/gio.h
  "gsettingsbackend.h" = paste(c(
    "#define G_SETTINGS_ENABLE_BACKEND",
    "#include <gio/gsettingsbackend.h>"
  ), collapse = "\n"),
  "gnetworking.h"              = "#include <gio/gnetworking.h>"
)

# ---------------------------------------------------------------------------
generate_c_file <- function(parsed, namespace, seen_global = NULL) {
  # Deduplicate within this file AND across files (via seen_global environment).
  # seen_global is an environment with a character vector $symbols, shared
  # across all generate_c_file calls in one generate_bindings() run.
  local_seen <- character()
  unique_fns <- Filter(function(f) {
    sym <- f$c_symbol
    if (sym %in% local_seen) return(FALSE)
    if (!is.null(seen_global) && sym %in% seen_global$symbols) return(FALSE)
    local_seen <<- c(local_seen, sym)
    TRUE
  }, parsed$functions)
  # Register all symbols emitted in this file into the global set
  if (!is.null(seen_global))
    seen_global$symbols <- c(seen_global$symbols, local_seen)

  # Collect extra #includes needed by functions in this file
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
  # Deduplicate (multiple patterns can map to the same include)
  plain_includes <- unique(plain_includes)
  unix_includes  <- unique(unix_includes)
  # Wrap unix-only headers in a platform guard
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
    plain_includes,
    guarded_unix,
    "",
    sprintf("/* Autogenerated for %s */", namespace),
    "/* Suppress unavoidable casts in generated wrapper code */",
    "#pragma GCC diagnostic ignored \"-Wdeprecated-declarations\"",
    "#pragma GCC diagnostic ignored \"-Wint-to-pointer-cast\"",
    "#pragma GCC diagnostic ignored \"-Wpointer-to-int-cast\"",
    "",
    "/* GObject refcounting helper from rgtk4_helpers.c */",
    "extern SEXP make_gobject_ptr(gpointer obj);",
    ""
  ), collapse = "\n")

  paste(c(header, blocks), collapse = "\n")
}

# ---------------------------------------------------------------------------
# classify_type(): "none" | "scalar" | "inttype" | "array" | "pointer"
# ---------------------------------------------------------------------------
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
    return(sprintf("Rf_ScalarLogical((int)(%s))", call_str))
  if (inner %in% c("gint64","guint64","gdouble","gfloat",
                   "gsize","gssize","goffset","glong","gulong"))
    return(sprintf("Rf_ScalarReal((double)(%s))", call_str))
  sprintf("Rf_ScalarInteger((int)(%s))", call_str)
}

unbox_inttype <- function(int_entry, decl_type, s_nm) {
  inner <- if (nonempty(int_entry$inner_gi)) int_entry$inner_gi else "gint"
  # For integers/enums, accept either INTSXP (normal) or EXTPTRSXP (cast from pointer)
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

# ---------------------------------------------------------------------------
# is_safe_out_base: can we safely declare a local of this stripped c:type?
# ---------------------------------------------------------------------------
is_safe_out_base <- function(base) {
  if (!nonempty(base)) return(FALSE)
  bare <- trimws(sub("^const\\s+", "", base))
  if (bare == "void") return(FALSE)
  if (grepl("\\*", bare)) return(TRUE)          # pointer type — safe
  if (bare %in% SAFE_BASE_TYPES) return(TRUE)   # known primitive — safe
  # All other bare names (GPollFD, GStatBuf, tm, utimbuf, GDate…) — NOT safe
  return(FALSE)
}

# ---------------------------------------------------------------------------
# resolve_in_param: (decl_type, unbox_expr) for a direction="in" parameter.
# ---------------------------------------------------------------------------
resolve_in_param <- function(p, s_nm) {
  gi  <- p$type$gi;  if (is.null(gi)  || is.na(gi))  gi  <- "none"
  ct  <- p$type$c;   if (is.null(ct)  || is.na(ct))  ct  <- ""
  cls <- classify_type(p$type)

  if (cls == "none") return(list(decl_type = "gpointer", unbox_expr = "NULL"))

  # Array: use outer c:type as opaque pointer
  if (cls == "array") {
    decl <- if (nonempty(ct)) ct else "gpointer"
    return(list(decl_type = decl,
                unbox_expr = sprintf("R_ExternalPtrAddr(%s)", s_nm)))
  }

  # Enum/alias integer type
  if (cls == "inttype") {
    entry <- p$type$int_entry
    decl  <- if (nonempty(entry$c_type)) entry$c_type else "gint"
    return(list(decl_type = decl,
                unbox_expr = unbox_inttype(entry, decl, s_nm)))
  }

  # String types (utf8/filename): always CHAR(STRING_ELT) for unboxing.
  # For the declaration type, honour the GIR c:type when it is explicitly
  # non-const (gchar*, char*) — these are mutable string buffer parameters.
  # When c:type is const or absent, fall back to "const char*".
  if (gi %in% STRING_GI_TYPES) {
    tm <- gi_type_to_map(gi)
    # Use the GIR c:type as decl_type only when it is a plain (non-const) pointer
    decl <- if (nonempty(ct) && grepl("\\*", ct) && !grepl("^const", trimws(ct))) ct
    else tm$decl_type
    return(list(decl_type  = decl,
                unbox_expr = gsub("{{X}}", s_nm, tm$unbox, fixed = TRUE)))
  }

  # Known scalar but c:type is a pointer — typed array/buffer passed from R
  if (cls == "scalar" && nonempty(ct) && c_type_is_pointer(ct)) {
    if (gi %in% c("guint8","gint8"))
      return(list(decl_type  = ct,
                  unbox_expr = sprintf("(%s)RAW(%s)", ct, s_nm)))
    if (gi %in% c("guint32","gint32","guint","gint","guint16","gint16"))
      return(list(decl_type  = ct,
                  unbox_expr = sprintf("(%s)INTEGER(%s)", ct, s_nm)))
    # Other scalar-with-pointer (e.g. gulong*) — pass as external pointer
    return(list(decl_type  = ct,
                unbox_expr = sprintf("R_ExternalPtrAddr(%s)", s_nm)))
  }

  # Plain known scalar
  if (cls == "scalar") {
    tm <- gi_type_to_map(gi)
    return(list(decl_type  = tm$decl_type,
                unbox_expr = gsub("{{X}}", s_nm, tm$unbox, fixed = TRUE)))
  }

  # Pointer / unknown
  decl <- if (nonempty(ct)) ct else "gpointer"
  list(decl_type  = decl,
       unbox_expr = sprintf("R_ExternalPtrAddr(%s)", s_nm))
}

# ---------------------------------------------------------------------------
# resolve_out_param: local variable declaration for direction="out"/"inout"
# ---------------------------------------------------------------------------
resolve_out_param <- function(p, local_nm) {
  cls <- classify_type(p$type)
  ct  <- p$type$c;  if (is.null(ct)  || is.na(ct))  ct  <- ""
  gi  <- p$type$gi; if (is.null(gi)  || is.na(gi))  gi  <- ""

  if (cls == "inttype") {
    entry <- p$type$int_entry
    decl  <- if (nonempty(entry$c_type)) entry$c_type else "gint"
    return(sprintf("  %s %s = 0;", decl, local_nm))
  }

  if (cls == "scalar") {
    if (gi %in% STRING_GI_TYPES) {
      # String out-param: strip one * to get the local type.
      # "gchar**"        -> "gchar*"        (pointer local, init NULL)
      # "const gchar**"  -> "const gchar*"  (pointer local, init NULL)
      # "gchar*"         -> "gchar"         (scalar buffer byte, init 0)
      base <- strip_one_pointer(ct)
      init <- if (c_type_is_pointer(base)) "NULL" else "0"
      return(sprintf("  %s %s = %s;", base, local_nm, init))
    }
    # Plain numeric scalar
    tm <- gi_type_to_map(gi)
    return(sprintf("  %s %s = 0;", tm$decl_type, local_nm))
  }

  # Pointer / array / unknown: strip one * from c:type
  base <- strip_one_pointer(ct)
  bare <- trimws(sub("^const\\s+", "", base))
  if (!nonempty(bare) || bare == "void")
    return(sprintf("  gpointer %s = NULL;", local_nm))

  # If it's a pointer, use NULL.
  # If it's a known struct (like GTimeVal, GdkRectangle), use {0}.
  # Otherwise, use 0.
  if (c_type_is_pointer(base)) {
    init <- "NULL"
  } else if (bare %in% SAFE_BASE_TYPES && grepl("^[G|P|c]|graphene", bare)) {
    # Most structs in SAFE_BASE_TYPES start with G, P (Pango), c (cairo), or graphene
    # Primitive types (gint, etc.) should stay as 0.
    struct_types <- c("GTimeVal", "GtkTextIter", "GdkRectangle", "GdkRGBA",
                      "PangoRectangle", "graphene_rect_t", "graphene_point_t",
                      "graphene_size_t", "cairo_rectangle_int_t")

    if (bare %in% struct_types) {
      init <- "{0}"
    } else {
      init <- "0"
    }
  } else {
    init <- "0"
  }

  sprintf("  %s %s = %s;", base, local_nm, init)
}

# ---------------------------------------------------------------------------
# resolve_box: SEXP boxing for a return value
# ---------------------------------------------------------------------------
resolve_box <- function(ret_type, var_nm) {
  cls <- classify_type(ret_type)
  gi  <- ret_type$gi; if (is.null(gi) || is.na(gi)) gi <- "none"
  ct  <- ret_type$c;  if (is.null(ct) || is.na(ct)) ct <- ""

  # Add any others that cause the "arithmetic or pointer type required" error here
  # Strip pointer to find the base type name
  bare <- trimws(sub("^const\\s+", "", strip_one_pointer(ct)))

  # List of structs that the generator creates as local stack variables
  struct_types <- c("GtkTextIter", "GdkRectangle", "GTimeVal", "GdkRGBA",
                    "PangoRectangle", "graphene_rect_t", "cairo_rectangle_int_t")

  # Handle struct types BEFORE falling through to generic pointer handling
  if (bare %in% struct_types) {
    # OUT parameters are always struct by value (local variable)
    if (grepl("^_out_", var_nm)) {
      return(sprintf("R_MakeExternalPtr((void*)&%s, R_NilValue, R_NilValue)", var_nm))
    }

    if (c_type_is_pointer(ct)) {
      # Function return pointer - check for NULL
      if (grepl("\\(", var_nm)) {
        return(sprintf("(%s) == NULL ? R_NilValue : R_MakeExternalPtr((void*)%s, R_NilValue, R_NilValue)", var_nm, var_nm))
      }
    }
    # Struct by value
    return(sprintf("R_MakeExternalPtr((void*)&%s, R_NilValue, R_NilValue)", var_nm))
  }

  if (cls == "none")    return(NULL)
  if (cls == "inttype") return(box_inttype(ret_type$int_entry, var_nm))
  # ---------------------------

  # Scalar gi but pointer c:type (e.g. gint64* return)
  if (cls == "scalar" && nonempty(ct) && c_type_is_pointer(ct) &&
      !gi %in% STRING_GI_TYPES)
    return(sprintf("R_MakeExternalPtr((void*)(%s), R_NilValue, R_NilValue)", var_nm))

  if (cls == "scalar")
    return(gsub("{{V}}", var_nm, gi_type_to_map(gi)$box, fixed = TRUE))

  # Check if this is a GObject type that needs refcounting
  is_gobject <- !is.null(gi) && !is.na(gi) && (
    grepl("^(Gtk|GObject|Gio|Gdk|Pango|GdkPixbuf)\\.", gi) ||
      gi %in% c("GLib.List", "GLib.SList") ||
      gi %in% c("GObject.Object", "GObject.InitiallyUnowned")
  )

  # For GObject types, use make_gobject_ptr which adds finalizer
  if (is_gobject && c_type_is_pointer(ct)) {
    return(sprintf("make_gobject_ptr((gpointer)(%s))", var_nm))
  }

  # Default for all other pointers
  sprintf("R_MakeExternalPtr((void*)(%s), R_NilValue, R_NilValue)", var_nm)

}

# ---------------------------------------------------------------------------
generate_c_function <- function(fn) {
  # Skip only if moved_to (superseded) or no c_symbol
  # KEEP deprecated functions - user wants them available
  if (isTRUE(fn$moved_to) || !nonempty(fn$c_symbol)) return("")

  # Platform-specific types that need to be skipped on all platforms
  platform_types <- c("utimbuf","tm","GPollFD","GFileDescriptorBased",
                      "GtkPageSetupUnixDialog","GtkPrintUnixDialog",
                      "GtkPrintJob","GtkPrinter","GOsxAppInfo",
                      "uid_t","gid_t","pid_t")  # POSIX types

  # --- Skip rules ---

  # Variadic
  if (any(sapply(fn$params, function(p)
    identical(p$name, "...") || identical(p$type$gi, "GLib.VarArgs")))) return("")

  # Functions with problematic fixed-size array out params
  # These cause -Wstringop-overflow warnings
  if (fn$c_symbol == "gdk_texture_downloader_download_bytes_with_planes") return("")

  # GLib atomic/once/pointer-bit macros — require lvalue pointers
  if (grepl("^g_atomic_|^g_once_init_|^g_pointer_bit_", fn$c_symbol)) return("")

  # GIO module loader functions - not part of public API
  if (grepl("^g_io_module_", fn$c_symbol)) return("")

  # Platform-specific functions that only exist on Unix/macOS
  # These cause compilation errors on other platforms - SKIP entirely
  platform_function_patterns <- c(
    "^gtk_page_setup_unix_dialog_",
    "^gtk_print_unix_dialog_",
    "^gtk_print_job_",
    "^gtk_printer_",
    "^g_osx_app_info_"
    # NOTE: g_unix_* functions are NOT in this list
    # They should be generated WITH guards, not skipped
  )

  # Check if this is a Linux-only GTK function that can't compile
  is_linux_only <- any(sapply(platform_function_patterns, function(pattern) {
    grepl(pattern, fn$c_symbol)
  }))

  # Detect Unix functions for platform guards
  # These WILL be generated but with #if defined(G_OS_UNIX) guards
  is_unix_function <- grepl("_unix_", fn$c_symbol, ignore.case = TRUE) ||
    grepl("^g_subprocess_", fn$c_symbol) ||
    grepl("^g_credentials_", fn$c_symbol) ||
    grepl("^g_date_time_new_from_unix", fn$c_symbol)

  # Detect Windows-only functions (for future - none currently but keep the pattern)
  is_windows_function <- grepl("_win32_", fn$c_symbol, ignore.case = TRUE)

  # Only skip Linux-only GTK patterns (print dialogs, etc)
  # Do NOT skip unix functions - they will be generated with guards
  if (is_linux_only) return("")

  # Callback / function-pointer IN parameters.
  # Match on GI name (Func, Notify, Callback, Marshaller suffixes) OR on C
  # type ending in Func or Marshaller (e.g. GSignalCVaMarshaller).
  if (any(sapply(fn$params, function(p) {
    if (!identical(p$direction, "in")) return(FALSE)
    gi <- p$type$gi
    ct <- p$type$c
    gi_hit <- nonempty(gi) && grepl("Func$|Notify$|Callback$|Marshaller$|^GLib\\.VarArgs", gi)
    ct_hit <- nonempty(ct) && grepl("Func\\*?$|Marshaller\\*?$", ct)
    gi_hit || ct_hit
  }))) return("")

  # Volatile parameters
  if (any(sapply(fn$params, function(p) {
    nonempty(p$type$c) && grepl("volatile", p$type$c)
  }))) return("")

  # IN params that cannot be correctly wrapped:
  #   a) double-pointer (** or *const*) IN param not marked as array
  #   b) POSIX/system struct types that need extra headers: tm, utimbuf, ...
  #   c) GIO interfaces without header includes: GFileDescriptorBased, ...
  if (any(sapply(fn$params, function(p) {
    if (!identical(p$direction, "in")) return(FALSE)
    ct <- p$type$c; if (!nonempty(ct)) return(FALSE)
    # Double-pointer IN (e.g. gchar**) not marked as array — likely mislabeled OUT
    if (grepl("\\*[[:space:]]*(?:const[[:space:]]*)?\\*", ct) &&
        !isTRUE(p$type$is_array)) return(TRUE)
    # Platform-specific struct pointers that need extra headers or are unavailable
    if (grepl("\\*", ct)) {
      base <- trimws(sub("^const\\s+", "", strip_one_pointer(ct)))
      if (base %in% platform_types) return(TRUE)
    }
    FALSE
  }))) return("")

  # OUT params with unsafe base types OR platform-specific types anywhere
  if (any(sapply(fn$params, function(p) {
    ct <- p$type$c; if (!nonempty(ct)) return(FALSE)
    # OUT param with unsafe base?
    if (p$direction %in% c("out","inout") && !is_safe_out_base(strip_one_pointer(ct)))
      return(TRUE)
    # ANY param with Unix-only print type?
    if (grepl("\\*", ct)) {
      base <- trimws(sub("^const\\s+", "", strip_one_pointer(ct)))
      if (base %in% platform_types) return(TRUE)
    }
    FALSE
  }))) return("")

  # Return type with Unix-only print type?
  ret_ct <- fn$return_type$c
  if (nonempty(ret_ct) && grepl("\\*", ret_ct)) {
    ret_base <- trimws(sub("^const\\s+", "", strip_one_pointer(ret_ct)))
    if (ret_base %in% platform_types) return("")
  }

  # --- Generate ---
  in_params <- Filter(function(p) identical(p$direction, "in"), fn$params)
  has_error <- isTRUE(fn$throws)

  sexp_args <- if (length(in_params) == 0) "void" else
    paste0("SEXP s", seq_along(in_params), collapse = ", ")

  decls     <- character()
  call_args <- character()
  s_idx     <- 1L

  for (p in fn$params) {
    if (p$direction %in% c("out","inout")) {
      local_nm  <- paste0("_out_", p$name)
      decls     <- c(decls, resolve_out_param(p, local_nm))
      call_args <- c(call_args, paste0("&", local_nm))
      next
    }

    s_nm <- paste0("s", s_idx); v_nm <- paste0("v", s_idx); s_idx <- s_idx + 1L
    r    <- resolve_in_param(p, s_nm)

    # Nullable pointer: inline NULL check
    if (isTRUE(p$nullable) && classify_type(p$type) == "pointer") {
      decls <- c(decls, sprintf(
        "  %s %s = (%s != R_NilValue) ? (%s)R_ExternalPtrAddr(%s) : NULL;",
        r$decl_type, v_nm, s_nm, r$decl_type, s_nm))
    } else {
      decls <- c(decls, sprintf("  %s %s = (%s)(%s);",
                                r$decl_type, v_nm, r$decl_type, r$unbox_expr))
    }
    call_args <- c(call_args, v_nm)
  }

  if (has_error) {
    decls     <- c(decls, "  GError *_err = NULL;")
    call_args <- c(call_args, "&_err")
  }

  ret_gi   <- fn$return_type$gi; if (is.null(ret_gi) || is.na(ret_gi)) ret_gi <- "none"
  call_str <- sprintf("%s(%s)", fn$c_symbol, paste(call_args, collapse = ", "))
  err_cl   <- if (has_error) "  if (_err) g_error_free(_err);" else ""

  # --- Logic to handle 'void' functions that have 'out' params ---
  out_params <- Filter(function(p) p$direction %in% c("out", "inout"), fn$params)

  body <- if (ret_gi == "none") {
    if (length(out_params) == 1) {
      # If void return but has one OUT param, return the OUT param instead of Nil
      p_out <- out_params[[1]]
      local_nm <- paste0("_out_", p_out$name)
      box <- resolve_box(p_out$type, local_nm)
      sprintf("  %s;\n%s\n  return %s;", call_str, err_cl, box)
    } else {
      # Truly void or multiple out params (not yet supported)
      sprintf("  %s;\n%s\n  return R_NilValue;", call_str, err_cl)
    }
  } else {
    # Existing logic for non-void returns
    box <- resolve_box(fn$return_type, call_str)
    if (is.null(box)) sprintf("  %s;\n%s\n  return R_NilValue;", call_str, err_cl)
    else if (has_error) sprintf("  SEXP _ret = %s;\n%s\n  return _ret;", box, err_cl)
    else sprintf("  return %s;", box)
  }

  function_code <- paste0(
    sprintf("\nSEXP R_%s(%s) {", fn$c_symbol, sexp_args),
    if (length(decls) > 0) paste0("\n", paste(decls, collapse = "\n")) else "",
    "\n", body, "\n}\n"
  )

  # Add platform guards for Unix-only or Windows-only functions
  if (is_unix_function || is_windows_function) {
    # Determine stub return value based on return type
    stub_return <- if (ret_gi == "none") {
      "R_NilValue"
    } else {
      ret_map <- gi_type_to_map(ret_gi)
      if (!is.null(ret_map) && ret_map$known) {
        # Scalar types - return appropriate zero/false value
        if (ret_gi == "gboolean") "Rf_ScalarLogical(FALSE)"
        else if (ret_gi %in% c("gdouble", "gfloat")) "Rf_ScalarReal(0.0)"
        else if (ret_gi %in% c("gint", "guint", "gint64", "guint64")) "Rf_ScalarInteger(0)"
        else "R_NilValue"
      } else {
        "R_NilValue"
      }
    }

    # Choose appropriate platform guard
    if (is_unix_function) {
      # Unix/Linux only (not Windows, not macOS)
      guard_start <- "#if defined(G_OS_UNIX) && !defined(__APPLE__)\n"
    } else {
      # Windows only
      guard_start <- "#ifdef G_OS_WIN32\n"
    }

    # Build guarded version
    return(paste0(
      "\nSEXP R_", fn$c_symbol, "(", sexp_args, ") {\n",
      guard_start,
      if (length(decls) > 0) paste0(paste(decls, collapse = "\n"), "\n") else "",
      body, "\n",
      "#else\n",
      "  Rf_warning(\"%s is not available on this platform\", \"", fn$c_symbol, "\");\n",
      "  return ", stub_return, ";\n",
      "#endif\n",
      "}\n"
    ))
  }

  function_code
}
