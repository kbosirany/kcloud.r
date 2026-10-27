#' @export
cloud_url.cloud_nextcloud <- function(storage, ...) {
  path <- remote_path(...)
  url <- nc_url(storage, path)
  if (is_dir_path(path)) paste0(url, "?accept=zip") else url
}

nc_url <- function(storage, path) {
  paste0(storage$url_dav, "/", escape_path(path))
}

nc_request <- function(storage, path, method = "GET") {
  req <- httr2::request(nc_url(storage, path))
  req <- httr2::req_method(req, method)
  if (!is.null(storage$auth)) req <- storage$auth(req)
  req <- httr2::req_user_agent(
    req, "kcloud.r (https://github.com/kbosirany/kcloud.r)"
  )
  # Nextcloud limits the request rate (HTTP 429): the retries wait for
  # Retry-After, or back off exponentially.
  req <- httr2::req_retry(req, max_tries = 5, retry_on_failure = TRUE)
  httr2::req_error(req, is_error = function(resp) FALSE)
}

nc_propfind <- function(storage, path, depth) {
  httr2::req_headers(nc_request(storage, path, "PROPFIND"),
                     Depth = as.character(depth))
}

# Performs one or several requests, in parallel when there are several.
# The responses are returned whatever their status.
nc_perform <- function(reqs, paths = NULL, workers = 1L) {
  if (length(reqs) == 1) {
    return(list(httr2::req_perform(reqs[[1]], path = paths[[1]])))
  }
  resps <- httr2::req_perform_parallel(
    reqs, paths = paths, on_error = "continue", progress = FALSE,
    max_active = max(1L, as.integer(workers))
  )
  failed <- vapply(resps, inherits, logical(1), what = "error")
  if (any(failed)) {
    stop(resps[[which(failed)[1]]])
  }
  resps
}

nc_check <- function(storage, resp, path, what, ok = c(200L, 207L)) {
  status <- httr2::resp_status(resp)
  if (status %in% ok) {
    return(invisible(resp))
  }
  reason <- if (status == 404L) {
    "not found"
  } else if (status %in% c(401L, 403L)) {
    "access denied (check the user and the password)"
  } else {
    httr2::resp_status_desc(resp) %||% "error"
  }
  stop("kcloud.r - ", what, " - ", storage$name, " - HTTP ", status, " ",
       reason, ": ", path, call. = FALSE)
}

# Writes the body of a response to `path`, when httr2 did not stream it
# there (mocked responses).
nc_save_body <- function(resp, path) {
  if (!file.exists(path)) {
    writeBin(httr2::resp_body_raw(resp), path)
  }
  invisible(path)
}

nc_etag <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(NA_character_)
  }
  gsub('^W/|"', "", x)
}

# Parses a WebDAV multistatus response into a listing (see cloud_list()).
nc_parse_propfind <- function(storage, resp) {
  doc <- xml2::read_xml(httr2::resp_body_raw(resp))
  responses <- xml2::xml_find_all(doc, "//*[local-name()='response']")
  if (length(responses) == 0) {
    return(empty_listing())
  }
  prop <- function(name) {
    xml2::xml_text(xml2::xml_find_first(
      responses, paste0(".//*[local-name()='", name, "']")
    ))
  }
  href <- prop("href")
  is_dir <- xml2::xml_find_lgl(
    responses, "boolean(.//*[local-name()='collection'])"
  )
  size <- suppressWarnings(as.numeric(prop("getcontentlength")))
  size_dir <- suppressWarnings(as.numeric(prop("quota-used-bytes")))
  size[is_dir] <- size_dir[is_dir]
  modified <- curl::parse_date(prop("getlastmodified"))
  attr(modified, "tzone") <- "UTC"
  data.frame(
    path = nc_href_to_path(storage, href, is_dir),
    is_dir = is_dir,
    size = size,
    modified = modified,
    etag = nc_etag(prop("getetag")),
    stringsAsFactors = FALSE
  )
}

nc_href_to_path <- function(storage, href, is_dir) {
  base <- curl::curl_unescape(sub("^https?://[^/]+", "", storage$url_dav))
  href <- curl::curl_unescape(sub("^https?://[^/]+", "", href))
  path <- substring(href, nchar(base) + 2)
  path <- strip_slash(path)
  ifelse(is_dir & path != "", paste0(path, "/"), path)
}

#' @export
backend_get.cloud_nextcloud <- function(storage, path, path_local, workers) {
  for (d in unique(dirname(path_local))) {
    dir.create(d, showWarnings = FALSE, recursive = TRUE)
  }
  reqs <- lapply(path, function(p) nc_request(storage, p))
  resps <- nc_perform(reqs, paths = path_local, workers = workers)
  vapply(seq_along(resps), function(i) {
    if (httr2::resp_status(resps[[i]]) != 200L) {
      unlink(path_local[i])
      nc_check(storage, resps[[i]], path[i], "download")
    }
    nc_save_body(resps[[i]], path_local[i])
    nc_etag(httr2::resp_header(resps[[i]], "ETag"))
  }, character(1))
}

#' @export
backend_get_dir.cloud_nextcloud <- function(storage, path, path_local) {
  zip <- paste0(path_local, ".zip")
  unzipped <- paste0(path_local, ".unzip")
  on.exit(unlink(c(zip, unzipped), recursive = TRUE, force = TRUE),
          add = TRUE)
  req <- httr2::req_url_query(nc_request(storage, dir_slash(path)),
                              accept = "zip")
  resp <- httr2::req_perform(req, path = zip)
  if (httr2::resp_status(resp) != 200L) {
    nc_check(storage, resp, path, "download")
  }
  nc_save_body(resp, zip)
  utils::unzip(zip, exdir = unzipped)
  # The archive holds the folder itself: its content is moved.
  top <- list.files(unzipped, all.files = TRUE, no.. = TRUE)
  src <- if (length(top) == 1 && dir.exists(file.path(unzipped, top))) {
    file.path(unzipped, top)
  } else {
    unzipped
  }
  replace_path(src, path_local)
  invisible(path_local)
}

#' @export
backend_put.cloud_nextcloud <- function(storage, path_local, path, workers) {
  reqs <- Map(function(f, p) {
    httr2::req_body_file(nc_request(storage, p, "PUT"), f)
  }, path_local, path)
  resps <- nc_perform(unname(reqs), workers = workers)
  for (i in seq_along(resps)) {
    nc_check(storage, resps[[i]], path[i], "upload", ok = c(200L, 201L, 204L))
  }
  invisible()
}

#' @export
backend_mkdir.cloud_nextcloud <- function(storage, path, workers) {
  depth <- lengths(gregexpr("/", dir_slash(path)))
  # A folder can only be created once its parent exists: one level at once.
  for (level in sort(unique(depth))) {
    dirs <- path[depth == level]
    reqs <- lapply(dirs, function(p) nc_request(storage, dir_slash(p), "MKCOL"))
    resps <- nc_perform(reqs, workers = workers)
    for (i in seq_along(resps)) {
      # 405: the folder already exists
      nc_check(storage, resps[[i]], dirs[i], "create folder",
               ok = c(201L, 405L))
    }
  }
  invisible()
}

#' @export
backend_delete.cloud_nextcloud <- function(storage, path) {
  resp <- httr2::req_perform(nc_request(storage, path, "DELETE"))
  nc_check(storage, resp, path, "delete", ok = c(200L, 204L))
  invisible()
}

#' @export
backend_stat.cloud_nextcloud <- function(storage, path, workers) {
  reqs <- lapply(path, function(p) nc_propfind(storage, p, 0))
  resps <- nc_perform(reqs, workers = workers)
  rows <- lapply(seq_along(resps), function(i) {
    if (httr2::resp_status(resps[[i]]) == 404L) {
      row <- data.frame(
        path = path[i], is_dir = NA, size = NA_real_,
        modified = as.POSIXct(NA, tz = "UTC"), etag = NA_character_,
        exists = FALSE, stringsAsFactors = FALSE
      )
      return(row)
    }
    nc_check(storage, resps[[i]], path[i], "stat")
    row <- nc_parse_propfind(storage, resps[[i]])[1, , drop = FALSE]
    row$path <- path[i]
    row$exists <- TRUE
    row
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' @export
backend_ls.cloud_nextcloud <- function(storage, path, recursive, workers,
                                       missing_ok = FALSE) {
  level <- dir_slash(path)
  out <- list()
  # Breadth first: the folders of one level are listed in parallel.
  while (length(level)) {
    reqs <- lapply(level, function(p) nc_propfind(storage, p, 1))
    resps <- nc_perform(reqs, workers = workers)
    dfs <- lapply(seq_along(resps), function(i) {
      if (missing_ok && httr2::resp_status(resps[[i]]) == 404L) {
        return(NULL)
      }
      nc_check(storage, resps[[i]], level[i], "list")
      df <- nc_parse_propfind(storage, resps[[i]])
      df[df$path != level[i], , drop = FALSE]
    })
    df <- bind_listings(dfs)
    out <- c(out, list(df))
    level <- if (recursive) df$path[df$is_dir] else character()
  }
  bind_listings(out)
}
