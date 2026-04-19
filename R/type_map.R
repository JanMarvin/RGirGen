#' @keywords internal

# GI types that are string-like: always use CHAR(STRING_ELT) regardless of c:type
STRING_GI_TYPES <- c("utf8", "filename")

# GI types that are plain numeric scalars (never pointers)
NUMERIC_GI_TYPES <- c(
  "gboolean","gint","guint","gint8","guint8","gint16","guint16",
  "gint32","guint32","gint64","guint64","gdouble","gfloat",
  "gsize","gssize","goffset","glong","gulong","gchar","gunichar",
  "GQuark","GType"
)

gi_type_to_map <- function(gi_name) {
  m <- list(
    "none"     = list(d="void",        u="NULL",                          b="R_NilValue",                    p=FALSE, k=TRUE),
    "gboolean" = list(d="gboolean",    u="(gboolean)LOGICAL({{X}})[0]",   b="Rf_ScalarLogical({{V}})",          p=FALSE, k=TRUE),
    "gchar"    = list(d="gchar",       u="(gchar)INTEGER({{X}})[0]",      b="Rf_ScalarInteger((int)({{V}}))",   p=FALSE, k=TRUE),
    "gunichar" = list(d="gunichar",    u="(gunichar)INTEGER({{X}})[0]",   b="Rf_ScalarInteger((int)({{V}}))",   p=FALSE, k=TRUE),
    "gint"     = list(d="gint",        u="(gint)INTEGER({{X}})[0]",       b="Rf_ScalarInteger({{V}})",          p=FALSE, k=TRUE),
    "guint"    = list(d="guint",       u="(guint)INTEGER({{X}})[0]",      b="Rf_ScalarInteger((int)({{V}}))",   p=FALSE, k=TRUE),
    "gint8"    = list(d="gint8",       u="(gint8)INTEGER({{X}})[0]",      b="Rf_ScalarInteger((int)({{V}}))",   p=FALSE, k=TRUE),
    "guint8"   = list(d="guint8",      u="(guint8)INTEGER({{X}})[0]",     b="Rf_ScalarInteger((int)({{V}}))",   p=FALSE, k=TRUE),
    "gint16"   = list(d="gint16",      u="(gint16)INTEGER({{X}})[0]",     b="Rf_ScalarInteger((int)({{V}}))",   p=FALSE, k=TRUE),
    "guint16"  = list(d="guint16",     u="(guint16)INTEGER({{X}})[0]",    b="Rf_ScalarInteger((int)({{V}}))",   p=FALSE, k=TRUE),
    "gint32"   = list(d="gint32",      u="(gint32)INTEGER({{X}})[0]",     b="Rf_ScalarInteger((int)({{V}}))",   p=FALSE, k=TRUE),
    "guint32"  = list(d="guint32",     u="(guint32)INTEGER({{X}})[0]",    b="Rf_ScalarInteger((int)({{V}}))",   p=FALSE, k=TRUE),
    "gint64"   = list(d="gint64",      u="(gint64)REAL({{X}})[0]",        b="Rf_ScalarReal((double)({{V}}))",   p=FALSE, k=TRUE),
    "guint64"  = list(d="guint64",     u="(guint64)REAL({{X}})[0]",       b="Rf_ScalarReal((double)({{V}}))",   p=FALSE, k=TRUE),
    "gdouble"  = list(d="gdouble",     u="(gdouble)REAL({{X}})[0]",       b="Rf_ScalarReal({{V}})",             p=FALSE, k=TRUE),
    "gfloat"   = list(d="gfloat",      u="(gfloat)REAL({{X}})[0]",        b="Rf_ScalarReal((double)({{V}}))",   p=FALSE, k=TRUE),
    "gsize"    = list(d="gsize",       u="(gsize)REAL({{X}})[0]",         b="Rf_ScalarReal((double)({{V}}))",   p=FALSE, k=TRUE),
    "gssize"   = list(d="gssize",      u="(gssize)REAL({{X}})[0]",        b="Rf_ScalarReal((double)({{V}}))",   p=FALSE, k=TRUE),
    "goffset"  = list(d="goffset",     u="(goffset)REAL({{X}})[0]",       b="Rf_ScalarReal((double)({{V}}))",   p=FALSE, k=TRUE),
    "glong"    = list(d="glong",       u="(glong)REAL({{X}})[0]",         b="Rf_ScalarReal((double)({{V}}))",   p=FALSE, k=TRUE),
    "gulong"   = list(d="gulong",      u="(gulong)REAL({{X}})[0]",        b="Rf_ScalarReal((double)({{V}}))",   p=FALSE, k=TRUE),
    "GQuark"   = list(d="GQuark",      u="(GQuark)INTEGER({{X}})[0]",     b="Rf_ScalarInteger((int)({{V}}))",   p=FALSE, k=TRUE),
    "GType"    = list(d="GType",       u="(GType)REAL({{X}})[0]",         b="Rf_ScalarReal((double)({{V}}))",   p=FALSE, k=TRUE),
    "utf8"     = list(d="const char*", u="CHAR(STRING_ELT({{X}},0))",     b="Rf_mkString({{V}} ? {{V}} : \"\")",p=FALSE, k=TRUE),
    "filename" = list(d="const char*", u="CHAR(STRING_ELT({{X}},0))",     b="Rf_mkString({{V}} ? {{V}} : \"\")",p=FALSE, k=TRUE)
  )
  if (!is.null(gi_name) && !is.na(gi_name) && gi_name %in% names(m)) {
    e <- m[[gi_name]]
    return(list(decl_type=e$d, unbox=e$u, box=e$b, is_ptr=e$p, known=e$k))
  }
  list(decl_type=NULL, unbox="R_ExternalPtrAddr({{X}})",
       box="R_MakeExternalPtr((void*)({{V}}), R_NilValue, R_NilValue)",
       is_ptr=TRUE, known=FALSE)
}

c_type_is_pointer <- function(c_type) {
  if (is.null(c_type) || is.na(c_type) || nchar(c_type) == 0) return(TRUE)
  grepl("\\*", c_type) || c_type %in% c("gpointer","gconstpointer")
}

# Strip exactly one trailing * from a C type string.
# Does NOT strip const — preserves all qualifiers.
# "gchar**"          -> "gchar*"
# "const gchar**"    -> "const gchar*"
# "const char**"     -> "const char*"
# "gsize*"           -> "gsize"
# "const char*"      -> "const char"   (further stripping done by caller if needed)
strip_one_pointer <- function(c_type) {
  if (is.null(c_type) || is.na(c_type) || nchar(c_type) == 0) return("gpointer")
  s <- trimws(c_type)
  sub("\\s*\\*\\s*$", "", s)
}
