nc_test_url <- "https://nc.test/s/TOKEN"
nc_test_dav <- "https://nc.test/public.php/dav/files/TOKEN"

# In-memory Nextcloud server answering the WebDAV requests of the package
# through httr2::local_mocked_responses(). `files` is a named list: remote
# path -> content (character). The state (files, etags, requests) is in the
# returned environment and can be changed by the tests.
local_fake_nextcloud <- function(files = list(), env = parent.frame()) {
  server <- new.env()
  server$files <- lapply(files, charToRaw)
  server$etags <- stats::setNames(
    paste0("e", seq_along(files)), names(files)
  )
  server$dirs <- character()
  server$requests <- character()
  server$offline <- FALSE
  httr2::local_mocked_responses(
    function(req) fake_nextcloud_answer(server, req),
    env = env
  )
  server
}

fake_set_file <- function(server, path, content) {
  server$files[[path]] <- charToRaw(content)
  server$etags[[path]] <- paste0("e", as.integer(Sys.time()), sample(1e6, 1))
}

fake_dirs <- function(server) {
  paths <- c(names(server$files), server$dirs)
  parents <- unique(unlist(lapply(paths, function(p) {
    parts <- strsplit(sub("/$", "", p), "/")[[1]]
    if (length(parts) < 2) return(character())
    vapply(seq_len(length(parts) - 1), function(i) {
      paste(parts[seq_len(i)], collapse = "/")
    }, character(1))
  })))
  unique(c(parents, sub("/$", "", server$dirs)))
}

fake_nextcloud_answer <- function(server, req) {
  if (server$offline) {
    stop("Could not resolve host: nc.test")
  }
  method <- req$method %||% "GET"
  url <- sub("[?].*$", "", req$url)
  query <- if (grepl("[?]", req$url)) sub("^[^?]*[?]", "", req$url) else ""
  path <- curl::curl_unescape(sub(paste0("^", nc_test_dav, "/?"), "", url))
  path <- sub("/$", "", path)
  server$requests <- c(server$requests, paste(method, path))
  dirs <- fake_dirs(server)
  is_file <- path %in% names(server$files)
  is_dir <- path == "" || path %in% dirs
  switch(
    method,
    GET = {
      if (is_dir && query == "accept=zip") {
        return(fake_zip(server, path))
      }
      if (!is_file) return(httr2::response(404))
      httr2::response(
        200,
        headers = list(ETag = paste0('"', server$etags[[path]], '"')),
        body = server$files[[path]]
      )
    },
    PROPFIND = {
      if (!is_file && !is_dir) return(httr2::response(404))
      depth <- req$headers$Depth
      entries <- path
      if (is_dir && identical(depth, "1")) {
        prefix <- if (path == "") "" else paste0(path, "/")
        children <- c(names(server$files), dirs)
        children <- children[startsWith(children, prefix) &
                               !grepl("/", substring(children,
                                                     nchar(prefix) + 1))]
        entries <- c(path, children)
      }
      httr2::response(
        207,
        headers = list(`Content-Type` = "application/xml"),
        body = charToRaw(fake_multistatus(server, entries, dirs))
      )
    },
    PUT = {
      fake_set_file(server, path, readChar(
        req$body$data, file.size(req$body$data), useBytes = TRUE
      ))
      httr2::response(201)
    },
    MKCOL = {
      if (is_dir) return(httr2::response(405))
      server$dirs <- c(server$dirs, path)
      httr2::response(201)
    },
    DELETE = {
      if (!is_file && !is_dir) return(httr2::response(404))
      inside <- names(server$files) == path |
        startsWith(names(server$files), paste0(path, "/"))
      server$files <- server$files[!inside]
      server$etags <- server$etags[!inside]
      server$dirs <- server$dirs[server$dirs != path &
                                   !startsWith(server$dirs, paste0(path, "/"))]
      httr2::response(204)
    },
    httr2::response(405)
  )
}

fake_multistatus <- function(server, entries, dirs) {
  responses <- vapply(entries, function(p) {
    is_dir <- p == "" || p %in% dirs
    href <- paste0(
      "/public.php/dav/files/TOKEN/", escape_path(p),
      if (is_dir && p != "") "/"
    )
    href <- sub("TOKEN//$", "TOKEN/", href)
    props <- if (is_dir) {
      inside <- startsWith(names(server$files), paste0(p, "/")) | p == ""
      etag <- paste(server$etags[inside], collapse = "")
      paste0(
        "<d:resourcetype><d:collection/></d:resourcetype>",
        "<d:quota-used-bytes>", sum(lengths(server$files[inside])),
        "</d:quota-used-bytes>",
        "<d:getetag>&quot;", etag, "&quot;</d:getetag>"
      )
    } else {
      paste0(
        "<d:resourcetype/>",
        "<d:getcontentlength>", length(server$files[[p]]),
        "</d:getcontentlength>",
        "<d:getetag>&quot;", server$etags[[p]], "&quot;</d:getetag>"
      )
    }
    paste0(
      "<d:response><d:href>", href, "</d:href><d:propstat><d:prop>",
      "<d:getlastmodified>Wed, 21 Oct 2026 08:36:27 GMT</d:getlastmodified>",
      props, "</d:prop><d:status>HTTP/1.1 200 OK</d:status></d:propstat>",
      "</d:response>"
    )
  }, character(1))
  paste0(
    '<?xml version="1.0"?><d:multistatus xmlns:d="DAV:">',
    paste(responses, collapse = ""), "</d:multistatus>"
  )
}

# ZIP of a folder, holding the folder itself, as Nextcloud does.
fake_zip <- function(server, path) {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))
  top <- if (path == "") "TOKEN" else basename(path)
  inside <- names(server$files)[startsWith(names(server$files),
                                           paste0(path, "/"))]
  dir.create(file.path(tmp, top))
  for (f in inside) {
    dest <- file.path(tmp, top, substring(f, nchar(path) + 2))
    dir.create(dirname(dest), showWarnings = FALSE, recursive = TRUE)
    writeBin(server$files[[f]], dest)
  }
  zip <- tempfile(fileext = ".zip")
  withr::with_dir(tmp, utils::zip(zip, top, flags = "-r9Xq"))
  body <- readBin(zip, "raw", file.size(zip))
  unlink(zip)
  httr2::response(200, headers = list(`Content-Type` = "application/zip"),
                  body = body)
}

skip_if_no_zip <- function() {
  testthat::skip_if(!nzchar(Sys.which("zip")), "no zip program")
}

local_cache <- function(env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  withr::local_envvar(KCLOUD_CACHE = dir, .local_envir = env)
  dir
}

fake_storage <- function(...) {
  cloud_nextcloud(nc_test_url, name = "fake", ...)
}
