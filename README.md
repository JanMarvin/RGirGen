# RGirGen

GObject Introspection-based binding generator for R.

## Overview

RGirGen is a code generator that creates R bindings for GTK4 and related libraries from GObject Introspection (GIR) files. It generates complete, type-safe C wrappers and R functions with meaningful parameter names.

## What It Does

RGirGen parses GIR XML files and generates:

- **C wrapper functions** with proper type conversions and memory management
- **R functions** with descriptive parameter names from GIR metadata
- **Platform-specific filtering** (automatically excludes Unix-only functions on Windows, etc.)
- **Type-safe bindings** with automatic integer conversions where appropriate

## Architecture

RGirGen is a **pure generator** - it only creates code. All runtime functionality (event loops, helpers, signal handling) lives in the Rgtk4 package that consumes the generated code.

## Usage

```r
library(RGirGen)

# Generate bindings for GTK4 and dependencies
generate_bindings(
  gir_paths = c(
    "/usr/share/gir-1.0/GLib-2.0.gir",
    "/usr/share/gir-1.0/GObject-2.0.gir",
    "/usr/share/gir-1.0/Gio-2.0.gir",
    "/usr/share/gir-1.0/Gdk-4.0.gir",
    "/usr/share/gir-1.0/Gtk-4.0.gir"
  ),
  out_dir = "path/to/Rgtk4"
)

# On macOS with Homebrew:
# gir_paths = c(
#   "/opt/homebrew/share/gir-1.0/GLib-2.0.gir",
#   "/opt/homebrew/share/gir-1.0/GObject-2.0.gir",
#   ...
# )
```

This creates:
- `R/GLib_autogen.R`, `R/GObject_autogen.R`, `R/Gtk_autogen.R`, etc. - R wrapper functions
- `src/GLib_autogen.c`, `src/GObject_autogen.c`, `src/Gtk_autogen.c`, etc. - C glue code
- `src/init.c` - Registration of all C functions

## Requirements

- R >= 4.0.0
- GObject Introspection installed
- GIR files for the libraries you want to bind (e.g., Gtk-4.0.gir)

## Key Features

### Meaningful Names
```r
# Not this:
gtk_window_set_title(arg1, arg2)

# But this:
gtkWindowSetTitle(window, title)
```

### Automatic Type Conversion
```r
# Users can pass numeric or integer
gtkWindowSetDefaultSize(window, 400, 300)  # Works
gtkWindowSetDefaultSize(window, 400L, 300L)  # Also works
```

### Clean Returns
```r
# Void functions return invisible(NULL)
gtkInit()  # No output clutter

# Functions with outputs unwrap automatically
text <- gtkLabelGetText(label)  # Just the string, not list(result = "...")
```

### Platform Filtering
Unix-specific functions (like `g_unix_*`) are automatically excluded from Windows builds.

## Design Principles

1. **Generator generates, package provides** - No runtime code in the generator
2. **Type safety** - Proper C type conversions, not just void pointer casts
3. **R-like API** - Automatic conversions, clean returns, camelCase naming
4. **Maintainable** - Re-run generator to update bindings when GTK updates

## Generated Code Structure

### C Wrapper
```c
SEXP R_gtk_window_set_title(SEXP s1, SEXP s2) {
  GtkWindow* v1 = (GtkWindow*)(get_ptr(s1));
  const char* v2 = (const char*)(CHAR(STRING_ELT(s2, 0)));
  gtk_window_set_title(v1, v2);
  return R_NilValue;
}
```

### R Wrapper
```r
gtkWindowSetTitle <- function(window, title) {
  invisible(.Call("R_gtk_window_set_title", window, as.character(title)))
}
```

## Contributing

Improvements to the generator benefit all generated bindings. Key areas:

- Better type inference from GIR metadata
- More sophisticated memory management patterns
- Enhanced documentation generation
- Additional platform-specific filtering

## License

GPL-2 | GPL-3

## See Also

- **Rgtk4** - R package that uses RGirGen-generated bindings
- **GObject Introspection** - https://gi.readthedocs.io/
