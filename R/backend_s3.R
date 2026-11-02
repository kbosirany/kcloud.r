#' @export
cloud_url.cloud_s3 <- function(storage, ...) {
  s3_url(storage, s3_key(storage, remote_path(...)))
}

# Path-style URL (endpoint/bucket/key), which every S3 server understands.
s3_url <- function(storage, key = NULL) {
  url <- paste0(storage$endpoint, "/", storage$bucket)
  if (is.null(key)) url else paste0(url, "/", escape_path(key))
}

# Key of a remote path: the storage prefix is the root of the storage.
s3_key <- function(storage, path) {
  # paste0() would return the prefix alone for no path.
  if (length(path) == 0) character() else paste0(storage$prefix, path)
}

# `transfer`: the request moves file data. It may last long: it only fails
# when the transfer stalls (less than 1 kB/s for a minute).
s3_request <- function(storage, key = NULL, method = "GET",
                       transfer = FALSE) {
  req <- httr2::request(s3_url(storage, key))
  req <- httr2::req_method(req, method)
  req <- httr2::req_user_agent(
    req, "kcloud.r (https://github.com/kbosirany/kcloud.r)"
  )
  # A cold listing of a big bucket can take more than 20 seconds.
  req <- if (transfer) {
    req <- httr2::req_timeout(req, 6 * 3600)
    httr2::req_options(req, low_speed_limit = 1000L, low_speed_time = 60L)
  } else {
    httr2::req_timeout(req, 120)
  }
  req <- httr2::req_retry(
    req, max_tries = 5, retry_on_failure = TRUE,
    is_transient = function(resp) {
      httr2::resp_status(resp) %in% c(429L, 500L, 502L, 503L, 504L)
    }
  )
  req <- httr2::req_error(req, is_error = function(resp) FALSE)
  # Signed last: the signature covers the final URL and headers.
  if (!is.null(storage$auth)) req <- storage$auth(req)
  req
}

# The secret stays in the closure: it is neither printed nor shown by str().
make_auth_s3 <- function(access_key, secret_key, token, region) {
  force(access_key)
  force(secret_key)
  force(token)
  force(region)
  function(req) {
    httr2::req_auth_aws_v4(
      req, aws_access_key_id = access_key,
      aws_secret_access_key = secret_key,
      aws_session_token = token, aws_service = "s3", aws_region = region
    )
  }
}

# One page of ListObjectsV2.
s3_list_request <- function(storage, prefix, token = NA_character_,
                            delimiter = "/", max_keys = NULL) {
  max_keys <- max_keys %||% getOption("kcloud.r.s3_page_size", 1000L)
  req <- s3_request(storage)
  query <- list(`list-type` = 2, prefix = prefix, delimiter = delimiter,
                `max-keys` = max_keys)
  if (!is.na(token)) query$`continuation-token` <- token
  httr2::req_url_query(req, !!!query)
}

s3_check <- function(storage, resp, path, what, ok = 200L) {
  status <- httr2::resp_status(resp)
  if (status %in% ok) {
    return(invisible(resp))
  }
  reason <- if (status == 404L) {
    "not found"
  } else if (status %in% c(401L, 403L)) {
    paste("access denied (check the credentials, or use",
          "`anonymous = TRUE` for a public bucket)")
  } else {
    httr2::resp_status_desc(resp) %||% "error"
  }
  stop("kcloud.r - ", what, " - ", storage$name, " - HTTP ", status, " ",
       reason, ": ", path, call. = FALSE)
}

s3_time <- function(x) {
  as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
}

# Parses a ListObjectsV2 page. The entries are vectors built by one XPath
# query per column: this stays fast with a million keys.
# Returns the listing (paths relative to the storage root), and the
# continuation token (NA when it is the last page) as an attribute.
s3_parse_list <- function(storage, resp, listed) {
  doc <- xml2::read_xml(httr2::resp_body_raw(resp))
  xml2::xml_ns_strip(doc)
  contents <- xml2::xml_find_all(doc, "//Contents")
  field <- function(name) {
    if (length(contents) == 0) {
      return(character())
    }
    xml2::xml_text(xml2::xml_find_first(contents, name))
  }
  key <- field("Key")
  # The marker object of the folder itself ("folder/") is not an entry.
  files <- key != listed
  files <- data.frame(
    path = substring(key[files], nchar(storage$prefix) + 1),
    is_dir = rep(FALSE, sum(files)),
    size = as.numeric(field("Size")[files]),
    modified = s3_time(field("LastModified")[files]),
    etag = gsub("^W/|\"", "", field("ETag")[files]),
    stringsAsFactors = FALSE
  )
  dirs <- xml2::xml_text(xml2::xml_find_all(doc, "//CommonPrefixes/Prefix"))
  dirs <- data.frame(
    path = substring(dirs, nchar(storage$prefix) + 1),
    is_dir = rep(TRUE, length(dirs)),
    size = rep(NA_real_, length(dirs)),
    modified = s3_time(rep(NA_character_, length(dirs))),
    etag = rep(NA_character_, length(dirs)),
    stringsAsFactors = FALSE
  )
  out <- rbind(files, dirs)
  truncated <- identical(
    xml2::xml_text(xml2::xml_find_first(doc, "//IsTruncated")), "true"
  )
  attr(out, "token") <- if (truncated) {
    xml2::xml_text(xml2::xml_find_first(doc, "//NextContinuationToken"))
  } else {
    NA_character_
  }
  out
}

#' @export
backend_ls.cloud_s3 <- function(storage, path, recursive, workers,
                                missing_ok = FALSE) {
  start <- dir_slash(path)
  prefixes <- s3_key(storage, start)
  tokens <- rep(NA_character_, length(prefixes))
  flat <- rep(FALSE, length(prefixes))
  out <- list()
  out_flat <- list()
  # Round after round, one page of every pending prefix is requested in
  # parallel. The pages of one prefix follow each other (continuation
  # token), but the prefixes advance together.
  # A recursive listing starts level by level (delimiter "/"), which
  # costs one request per folder. As soon as the folders can keep all the
  # workers busy, each of them is listed flat instead (no delimiter): one
  # request per 1000 keys, whatever the depth.
  while (length(prefixes)) {
    reqs <- Map(function(p, t, f) {
      s3_list_request(storage, p, t, delimiter = if (!f) "/")
    }, prefixes, tokens, flat)
    resps <- nc_perform(unname(reqs), workers = workers)
    next_prefixes <- character()
    next_tokens <- character()
    next_flat <- logical()
    dirs <- character()
    for (i in seq_along(resps)) {
      s3_check(storage, resps[[i]], prefixes[i], "list")
      page <- s3_parse_list(storage, resps[[i]], prefixes[i])
      if (flat[i]) {
        out_flat <- c(out_flat, list(page))
      } else {
        out <- c(out, list(page))
        if (recursive) dirs <- c(dirs, s3_key(storage, page$path[page$is_dir]))
      }
      token <- attr(page, "token")
      if (!is.na(token)) {
        next_prefixes <- c(next_prefixes, prefixes[i])
        next_tokens <- c(next_tokens, token)
        next_flat <- c(next_flat, flat[i])
      }
    }
    go_flat <- length(dirs) + length(next_prefixes) >= 4 * max(1L, workers)
    prefixes <- c(next_prefixes, dirs)
    tokens <- c(next_tokens, rep(NA_character_, length(dirs)))
    flat <- c(next_flat, rep(go_flat, length(dirs)))
  }
  if (length(out_flat)) {
    out <- c(out, list(s3_flat_with_folders(out_flat, start)))
  }
  out <- bind_listings(out)
  out <- out[!duplicated(out$path), , drop = FALSE]
  rownames(out) <- NULL
  if (!missing_ok) {
    found <- vapply(start[start != ""], function(p) {
      any(startsWith(out$path, p))
    }, logical(1))
    if (!all(found)) {
      stop("kcloud.r - list - ", storage$name, " - not found: ",
           start[start != ""][!found][1], call. = FALSE)
    }
  }
  out
}

# Flat pages only hold keys: the folders above them are added, as a
# delimiter listing does.
s3_flat_with_folders <- function(pages, start) {
  files <- do.call(rbind, pages)
  files$is_dir <- endsWith(files$path, "/")
  ancestors <- remote_ancestors(files$path[!files$is_dir])
  inside <- Reduce(`|`, lapply(start, function(s) {
    startsWith(ancestors, s) & ancestors != s
  }))
  ancestors <- ancestors[inside & !ancestors %in% files$path]
  folders <- empty_listing()[rep(NA_integer_, length(ancestors)), ]
  folders$path <- ancestors
  folders$is_dir <- rep(TRUE, length(ancestors))
  rbind(files, folders)
}

#' @export
backend_stat.cloud_s3 <- function(storage, path, workers) {
  out <- data.frame(
    path = path, is_dir = NA, size = NA_real_,
    modified = as.POSIXct(rep(NA, length(path)), tz = "UTC"),
    etag = NA_character_, exists = FALSE, stringsAsFactors = FALSE
  )
  # A file is found by a HEAD request (it also gives its ETag).
  files <- which(!is_dir_path(path))
  if (length(files)) {
    reqs <- lapply(path[files], function(p) {
      s3_request(storage, s3_key(storage, p), "HEAD")
    })
    resps <- nc_perform(reqs, workers = workers)
    for (j in seq_along(files)) {
      i <- files[j]
      resp <- resps[[j]]
      if (httr2::resp_status(resp) == 404L) next
      s3_check(storage, resp, path[i], "stat")
      out$exists[i] <- TRUE
      out$is_dir[i] <- FALSE
      out$size[i] <- as.numeric(httr2::resp_header(resp, "Content-Length"))
      out$modified[i] <- curl::parse_date(
        httr2::resp_header(resp, "Last-Modified")
      )
      out$etag[i] <- nc_etag(httr2::resp_header(resp, "ETag"))
    }
  }
  # What is not a file can be a folder: S3 has no folder, only keys that
  # start with its name.
  folders <- which(!out$exists & path != "")
  if (length(folders)) {
    reqs <- lapply(path[folders], function(p) {
      s3_list_request(storage, s3_key(storage, dir_slash(p)),
                      delimiter = NULL, max_keys = 1)
    })
    resps <- nc_perform(reqs, workers = workers)
    for (j in seq_along(folders)) {
      s3_check(storage, resps[[j]], path[folders[j]], "stat")
      found <- xml2::xml_text(xml2::xml_find_first(
        xml2::read_xml(httr2::resp_body_raw(resps[[j]])),
        "//*[local-name()='KeyCount']"
      ))
      if (!identical(found, "0")) {
        out$exists[folders[j]] <- TRUE
        out$is_dir[folders[j]] <- TRUE
      }
    }
  }
  root <- path == ""
  out$exists[root] <- TRUE
  out$is_dir[root] <- TRUE
  out
}

# Writes the body of a response to `path` when httr2 did not stream it
# there (mocked responses). The body of an empty file is empty.
s3_save_body <- function(resp, path) {
  if (!file.exists(path)) {
    writeBin(if (httr2::resp_has_body(resp)) httr2::resp_body_raw(resp)
             else raw(), path)
  }
  invisible(path)
}

# Size of the chunks a file is downloaded in.
s3_chunk_size <- function() {
  getOption("kcloud.r.s3_chunk_size", 64 * 1024^2)
}

# The first chunk of each file is requested with a Range header: its
# response gives the size of the file (Content-Range), and holds all of a
# small file. The other chunks of a big file are downloaded in parallel.
#' @export
backend_get.cloud_s3 <- function(storage, path, path_local, workers) {
  for (d in unique(dirname(path_local))) {
    dir.create(d, showWarnings = FALSE, recursive = TRUE)
  }
  chunk <- s3_chunk_size()
  reqs <- lapply(path, function(p) {
    req <- s3_request(storage, s3_key(storage, p), transfer = TRUE)
    httr2::req_headers(req, Range = sprintf("bytes=0-%.0f", chunk - 1))
  })
  resps <- nc_perform(reqs, paths = path_local, workers = workers)
  vapply(seq_along(resps), function(i) {
    resp <- resps[[i]]
    if (httr2::resp_status(resp) == 416L) {
      # The range of an empty file cannot be satisfied.
      resp <- httr2::req_perform(
        s3_request(storage, s3_key(storage, path[i]), transfer = TRUE),
        path = path_local[i]
      )
    }
    if (!httr2::resp_status(resp) %in% c(200L, 206L)) {
      unlink(path_local[i])
      s3_check(storage, resp, path[i], "download")
    }
    s3_save_body(resp, path_local[i])
    etag <- nc_etag(httr2::resp_header(resp, "ETag"))
    range <- httr2::resp_header(resp, "Content-Range")
    total <- as.numeric(sub("^.*/", "", range %||% "bytes 0-0/0"))
    if (httr2::resp_status(resp) == 206L && total > chunk) {
      s3_get_rest(storage, path[i], path_local[i], total, etag, chunk)
    }
    etag
  }, character(1))
}

# Downloads the chunks after the first one, in parallel, then appends them
# to `dest` (which holds the first). The chunks are kept in a folder named
# after the ETag and the size: an interrupted download resumes there, and
# a file that changed on the server is never mixed with its old version
# (If-Match).
s3_get_rest <- function(storage, path, dest, total, etag, chunk) {
  max_bytes <- storage$max_size * 1024^3
  if (total > max_bytes) {
    unlink(dest)
    stop("kcloud.r - download - ", storage$name, " - ", path, " is ",
         round(total / 1024^3, 1), " GB, more than the cache limit ",
         "(max_size = ", storage$max_size, " GB).", call. = FALSE)
  }
  starts <- seq(chunk, total - 1, by = chunk)
  ends <- pmin(starts + chunk - 1, total - 1)
  size <- ends - starts + 1
  dir <- file.path(
    cloud_cache_dir(storage), ".parts",
    if (is.na(etag)) basename(tempfile("noetag")) else {
      paste0(sanitize_name(etag), "-", format(total, scientific = FALSE))
    }
  )
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  parts <- file.path(dir, seq_along(starts))
  done <- file.exists(parts) & (file.size(parts) == size) %in% TRUE
  todo <- which(!done)
  reqs <- lapply(todo, function(j) {
    req <- s3_request(storage, s3_key(storage, path), transfer = TRUE)
    req <- httr2::req_headers(
      req, Range = sprintf("bytes=%.0f-%.0f", starts[j], ends[j])
    )
    if (!is.na(etag)) {
      req <- httr2::req_headers(req, `If-Match` = paste0('"', etag, '"'))
    }
    req
  })
  if (length(reqs)) {
    resps <- nc_perform(reqs, paths = parts[todo],
                        workers = workers_default(storage))
    for (k in seq_along(todo)) {
      j <- todo[k]
      status <- httr2::resp_status(resps[[k]])
      if (status == 412L) {
        unlink(c(dest, dir), recursive = TRUE)
        stop("kcloud.r - download - ", storage$name, " - ", path,
             " changed on the server during the download.", call. = FALSE)
      }
      if (status != 206L) {
        unlink(parts[j])
        s3_check(storage, resps[[k]], path, "download", ok = 206L)
      }
      s3_save_body(resps[[k]], parts[j])
      if (file.size(parts[j]) != size[j]) {
        unlink(parts[j])
        stop("kcloud.r - download - ", storage$name, " - incomplete chunk ",
             "of ", path, ", run it again to resume.", call. = FALSE)
      }
    }
  }
  # Appending in order, and removing each chunk once appended, keeps the
  # disk use close to one copy of the file.
  for (part in parts) {
    if (!file.append(dest, part)) {
      stop("Cannot write: ", dest, call. = FALSE)
    }
    unlink(part)
  }
  unlink(dir, recursive = TRUE)
  invisible()
}

#' @export
backend_put.cloud_s3 <- function(storage, path_local, path, workers) {
  reqs <- Map(function(f, p) {
    httr2::req_body_file(
      s3_request(storage, s3_key(storage, p), "PUT", transfer = TRUE), f
    )
  }, path_local, path)
  resps <- nc_perform(unname(reqs), workers = workers)
  for (i in seq_along(resps)) {
    s3_check(storage, resps[[i]], path[i], "upload")
  }
  invisible()
}

# S3 has no folder to create: they exist as long as they hold a key.
#' @export
backend_mkdir.cloud_s3 <- function(storage, path, workers) {
  invisible()
}

# A folder is deleted key by key; S3 answers 204 for a key that is absent.
#' @export
backend_delete.cloud_s3 <- function(storage, path) {
  keys <- character()
  if (!is_dir_path(path)) keys <- path
  listing <- backend_ls(storage, path, recursive = TRUE,
                        workers = workers_default(storage), missing_ok = TRUE)
  keys <- c(keys, listing$path[!listing$is_dir])
  if (is_dir_path(path) || nrow(listing)) keys <- c(keys, dir_slash(path))
  reqs <- lapply(keys, function(k) {
    s3_request(storage, s3_key(storage, k), "DELETE")
  })
  resps <- nc_perform(reqs, workers = workers_default(storage))
  for (i in seq_along(resps)) {
    s3_check(storage, resps[[i]], keys[i], "delete", ok = c(200L, 204L))
  }
  invisible()
}
