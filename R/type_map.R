#' @keywords internal

STRING_GI_TYPES <- c("utf8", "filename")

gi_type_to_map <- function(gi_name) {
  m <- list(
    "none"     = list(d="void",        u="NULL",               b="R_NilValue",                 p=FALSE, k=TRUE),
    "gboolean" = list(d="gboolean",    u="(gboolean)LOGICAL({{X}})[0]", b="Rf_ScalarLogical((int)(size_t)({{V}}))", p=FALSE, k=TRUE),

    # Small Integers (Agnostic)
    "gchar"    = list(d="gchar",       u="(gchar)_unbox_numeric({{X}})",   b="Rf_ScalarInteger((int)(size_t)({{V}}))", p=FALSE, k=TRUE),
    "gunichar" = list(d="gunichar",    u="(gunichar)_unbox_numeric({{X}})", b="Rf_ScalarInteger((int)(size_t)({{V}}))", p=FALSE, k=TRUE),
    "gint"     = list(d="gint",        u="(gint)_unbox_numeric({{X}})",    b="Rf_ScalarInteger((int)(size_t)({{V}}))", p=FALSE, k=TRUE),
    "guint"    = list(d="guint",       u="(guint)_unbox_numeric({{X}})",   b="Rf_ScalarInteger((int)(size_t)({{V}}))", p=FALSE, k=TRUE),
    "gint8"    = list(d="gint8",       u="(gint8)_unbox_numeric({{X}})",   b="Rf_ScalarInteger((int)({{V}}))", p=FALSE, k=TRUE),
    "guint8"   = list(d="guint8",      u="(guint8)_unbox_numeric({{X}})",  b="Rf_ScalarInteger((int)({{V}}))", p=FALSE, k=TRUE),
    "gint16"   = list(d="gint16",      u="(gint16)_unbox_numeric({{X}})",  b="Rf_ScalarInteger((int)({{V}}))", p=FALSE, k=TRUE),
    "guint16"  = list(d="guint16",     u="(guint16)_unbox_numeric({{X}})", b="Rf_ScalarInteger((int)({{V}}))", p=FALSE, k=TRUE),
    "gint32"   = list(d="gint32",      u="(gint32)_unbox_numeric({{X}})",  b="Rf_ScalarInteger((int)({{V}}))", p=FALSE, k=TRUE),
    "guint32"  = list(d="guint32",     u="(guint32)_unbox_numeric({{X}})", b="Rf_ScalarReal((double)({{V}}))",   p=FALSE, k=TRUE),

    # Large Integers & Floats (Agnostic)
    "gint64"   = list(d="gint64",      u="(gint64)_unbox_numeric({{X}})",  b="Rf_ScalarReal((double)(size_t)({{V}}))", p=FALSE, k=TRUE),
    "guint64"  = list(d="guint64",     u="(guint64)_unbox_numeric({{X}})", b="Rf_ScalarReal((double)(size_t)({{V}}))", p=FALSE, k=TRUE),
    "gdouble"  = list(d="gdouble",     u="(gdouble)_unbox_numeric({{X}})", b="Rf_ScalarReal((double)(size_t)({{V}}))", p=FALSE, k=TRUE),
    "gfloat"   = list(d="gfloat",      u="(gfloat)_unbox_numeric({{X}})",  b="Rf_ScalarReal((double)(size_t)({{V}}))", p=FALSE, k=TRUE),
    "gsize"    = list(d="gsize",       u="(gsize)_unbox_numeric({{X}})",   b="Rf_ScalarReal((double)(size_t)({{V}}))", p=FALSE, k=TRUE),
    "gssize"   = list(d="gssize",      u="(gssize)_unbox_numeric({{X}})",  b="Rf_ScalarReal((double)(ssize_t)({{V}}))",p=FALSE, k=TRUE),
    "goffset"  = list(d="goffset",     u="(goffset)_unbox_numeric({{X}})", b="Rf_ScalarReal((double)(size_t)({{V}}))", p=FALSE, k=TRUE),
    "glong"    = list(d="glong",       u="(glong)_unbox_numeric({{X}})",   b="Rf_ScalarReal((double)(size_t)({{V}}))", p=FALSE, k=TRUE),
    "gulong"   = list(d="gulong",      u="(gulong)_unbox_numeric({{X}})",  b="Rf_ScalarReal((double)(size_t)({{V}}))", p=FALSE, k=TRUE),
    "long"     = list(d="long",        u="(long)_unbox_numeric({{X}})",    b="Rf_ScalarReal((double)(size_t)({{V}}))", p=FALSE, k=TRUE),
    "time_t"   = list(d="time_t",      u="(time_t)_unbox_numeric({{X}})",  b="Rf_ScalarReal((double)(size_t)({{V}}))", p=FALSE, k=TRUE),
    "GType"    = list(d="GType",       u="(GType)_unbox_numeric({{X}})",   b="Rf_ScalarReal((double)(size_t)({{V}}))", p=FALSE, k=TRUE),

    "GQuark"   = list(d="GQuark",      u="(GQuark)INTEGER({{X}})[0]",  b="Rf_ScalarInteger((int)(size_t)({{V}}))", p=FALSE, k=TRUE),
    "utf8"     = list(d="const char*", u="CHAR(STRING_ELT({{X}},0))",  b="Rf_mkString({{V}} ? (const char*){{V}} : \"\")", p=FALSE, k=TRUE),
    "filename" = list(d="const char*", u="CHAR(STRING_ELT({{X}},0))",  b="Rf_mkString({{V}} ? (const char*){{V}} : \"\")", p=FALSE, k=TRUE),
    "GStrv"    = list(d="GStrv",       u="NULL", b="_box_GStrv((char**){{V}})", p=TRUE, k=TRUE),
    "char**"   = list(d="char**",      u="NULL", b="_box_GStrv((char**){{V}})", p=TRUE, k=TRUE),
    "const gchar**"      = list(d="const gchar**",      u="NULL", b="_box_GStrv((char**){{V}})", p=TRUE, k=TRUE),
    "const gchar*const*" = list(d="const gchar*const*", u="NULL", b="_box_GStrv((char**){{V}})", p=TRUE, k=TRUE)
  )

  if (!is.null(gi_name) && !is.na(gi_name) && gi_name %in% names(m)) {
    e <- m[[gi_name]]
    return(list(decl_type=e$d, unbox=e$u, box=e$b, is_ptr=e$p, known=e$k))
  }

  list(decl_type=NULL, unbox="R_ExternalPtrAddr({{X}})",
       box="R_MakeExternalPtr((void*)({{V}}), R_NilValue, R_NilValue)",
       is_ptr=TRUE, known=FALSE)
}
