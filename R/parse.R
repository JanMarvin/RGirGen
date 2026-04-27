#' @import xml2
#' @export
parse_gir <- function(path) {
  doc <- xml2::read_xml(path)
  xml2::xml_ns_strip(doc)
  ns_node <- xml2::xml_find_first(doc, "//namespace")
  int_types <- build_int_type_registry(ns_node)
  list(
    namespace = xml2::xml_attr(ns_node, "name"),
    functions = parse_functions(ns_node, int_types),
    enums     = parse_enums(ns_node),
    callbacks = parse_callbacks(ns_node, int_types),
    int_types = int_types
  )
}

xattr_c <- function(node) xml2::xml_attr(node, "type")

build_int_type_registry <- function(ns_node) {
  reg <- list()
  SCALAR_GI <- c("gboolean","gint","guint","gint8","guint8","gint16","guint16",
                 "gint32","guint32","gint64","guint64","gdouble","gfloat",
                 "gsize","gssize","goffset","glong","gulong","gchar","gunichar")
  add <- function(gi_name, c_type, inner_gi) {
    entry <- list(c_type=c_type, inner_gi=inner_gi)
    if (!is.na(gi_name) && nchar(gi_name)>0) reg[[gi_name]] <<- entry
    if (!is.na(c_type)  && nchar(c_type)>0)  reg[[c_type]]  <<- entry
  }
  nodes <- xml2::xml_find_all(ns_node, ".//enumeration | .//bitfield")
  for (n in nodes) {
    gi <- xml2::xml_attr(n, "name"); ct <- xattr_c(n)
    if (!is.na(gi) && !is.na(ct)) add(gi, ct, "gint")
  }
  aliases <- xml2::xml_find_all(ns_node, ".//alias")
  for (a in aliases) {
    gi <- xml2::xml_attr(a, "name"); ct <- xattr_c(a)
    inner <- xml2::xml_find_first(a, "./type")
    if (inherits(inner, "xml_missing")) next
    inner_gi <- xml2::xml_attr(inner, "name")
    inner_ct <- xattr_c(inner)
    resolved <- if (!is.na(inner_gi) && inner_gi %in% SCALAR_GI) inner_gi
    else if (!is.na(inner_ct) && inner_ct %in% SCALAR_GI) inner_ct
    else NULL
    if (!is.null(resolved) && !is.na(gi) && !is.na(ct)) add(gi, ct, resolved)
  }
  reg
}

parse_functions <- function(ns, int_types) {
  fns <- xml2::xml_find_all(ns, ".//function | .//method | .//constructor")
  parsed <- lapply(fns, function(f) {
    if (identical(xml2::xml_attr(f, "introspectable"), "0")) return(NULL)
    c_sym <- xml2::xml_attr(f, "identifier")
    if (is.na(c_sym)) return(NULL)

    src_node   <- xml2::xml_find_first(f, "./source-position")
    src_header <- if (!inherits(src_node, "xml_missing"))
      xml2::xml_attr(src_node, "filename") else NA_character_

    list(
      name        = xml2::xml_attr(f, "name"),
      c_symbol    = c_sym,
      deprecated  = !is.na(xml2::xml_attr(f, "deprecated")),
      moved_to    = !is.na(xml2::xml_attr(f, "moved-to")),
      throws      = identical(xml2::xml_attr(f, "throws"), "1"),
      version     = xml2::xml_attr(f, "version"),
      source_header = src_header,
      params      = parse_params(f, int_types),
      return_type = parse_type(xml2::xml_find_first(f, "./return-value"), int_types)
    )
  })
  parsed[lengths(parsed) > 0]
}

parse_params <- function(node, int_types) {
  params <- xml2::xml_find_all(
    node, "./parameters/parameter | ./parameters/instance-parameter"
  )
  lapply(params, function(p) {
    direction <- xml2::xml_attr(p, "direction")
    direction <- if (is.na(direction)) "in" else direction
    nullable  <- identical(xml2::xml_attr(p, "nullable"), "1") ||
      identical(xml2::xml_attr(p, "allow-none"), "1")
    is_instance <- identical(xml2::xml_name(p), "instance-parameter")
    list(
      name        = xml2::xml_attr(p, "name"),
      type        = parse_type(p, int_types),
      direction   = direction,
      nullable    = nullable,
      closure     = xml2::xml_attr(p, "closure"),
      destroy     = xml2::xml_attr(p, "destroy"),
      scope       = xml2::xml_attr(p, "scope"),
      is_instance = is_instance
    )
  })
}

parse_type <- function(node, int_types=list()) {
  empty <- list(gi="none", c="void", is_array=FALSE, int_entry=NULL)
  if (is.null(node) || inherits(node, "xml_missing")) return(empty)
  arr   <- xml2::xml_find_first(node, "./array")
  tnode <- xml2::xml_find_first(node, "./type")
  if (!inherits(arr, "xml_missing")) {
    inner <- xml2::xml_find_first(arr, "./type")
    ct    <- xattr_c(arr)
    gi    <- if (!inherits(inner, "xml_missing")) xml2::xml_attr(inner, "name") else NA_character_
    return(list(gi=gi, c=ct, is_array=TRUE, int_entry=NULL))
  }
  if (!inherits(tnode, "xml_missing")) {
    gi <- xml2::xml_attr(tnode, "name"); ct <- xattr_c(tnode)
    int_entry <- resolve_int_entry(gi, ct, int_types)
    return(list(gi=gi, c=ct, is_array=FALSE, int_entry=int_entry))
  }
  empty
}

# Resolve an int-type registry entry, preferring matches whose c_type agrees
# with the parsed c:type. Avoids cross-namespace collisions (e.g. the bare
# GI name "Variant" exists in both Pango — c:type=PangoVariant — and GLib —
# c:type=GVariant*, which is NOT an enum and must not match Pango's entry).
resolve_int_entry <- function(gi, ct, int_types) {
  if ((is.null(gi) || is.na(gi)) && (is.null(ct) || is.na(ct))) return(NULL)

  norm_ct <- if (!is.null(ct) && !is.na(ct)) {
    x <- gsub("\\*", "", ct)
    x <- sub("^const\\s+", "", trimws(x))
    trimws(x)
  } else {
    NA_character_
  }

  POINTER_TYPEDEFS <- c("gpointer", "gconstpointer")
  ct_is_pointer <- (!is.na(ct) && grepl("\\*", ct)) ||
    (!is.na(norm_ct) && norm_ct %in% POINTER_TYPEDEFS) ||
    (!is.null(gi) && !is.na(gi) && gi %in% POINTER_TYPEDEFS)
  if (ct_is_pointer) return(NULL)

  candidates <- list()
  if (!is.null(gi) && !is.na(gi) && !is.null(int_types[[gi]])) {
    candidates[[length(candidates)+1L]] <- int_types[[gi]]
  }
  if (!is.null(gi) && !is.na(gi) && grepl("\\.", gi)) {
    short <- sub("^.*\\.", "", gi)
    if (!is.null(int_types[[short]])) candidates[[length(candidates)+1L]] <- int_types[[short]]
  }
  if (!is.null(ct) && !is.na(ct) && !is.null(int_types[[ct]])) {
    candidates[[length(candidates)+1L]] <- int_types[[ct]]
  }
  if (!is.na(norm_ct) && !is.null(int_types[[norm_ct]])) {
    candidates[[length(candidates)+1L]] <- int_types[[norm_ct]]
  }

  if (length(candidates) == 0) return(NULL)

  if (!is.na(norm_ct)) {
    for (cand in candidates) {
      if (!is.null(cand$c_type) && identical(cand$c_type, norm_ct)) return(cand)
    }
  }
  if (!is.na(norm_ct)) return(NULL)
  candidates[[1L]]
}

parse_enums <- function(ns_node) {
  nodes <- xml2::xml_find_all(ns_node, ".//enumeration | .//bitfield")
  lapply(nodes, function(e) {
    members <- xml2::xml_find_all(e, "./member")
    list(
      name   = xml2::xml_attr(e, "name"),
      values = setNames(xml2::xml_attr(members, "value"),
                        make.names(toupper(xml2::xml_attr(members, "name"))))
    )
  })
}

enrich_type <- function(type_info, int_types) {
  if (is.null(type_info) || isTRUE(type_info$is_array)) return(type_info)
  type_info$int_entry <- resolve_int_entry(type_info$gi, type_info$c, int_types)
  type_info
}
