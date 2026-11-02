s3_test_endpoint <- "https://s3.test"

# In-memory S3 server (path-style, bucket "bkt") answering the requests of
# the package through httr2::local_mocked_responses(). `files` is a named
# list: key -> content (character). Honours prefix, delimiter, max-keys and
# continuation tokens, like ListObjectsV2.
local_fake_s3 <- function(files = list(), env = parent.frame()) {
  server <- new.env()
  server$files <- lapply(files, charToRaw)
  server$etags <- stats::setNames(paste0("e", seq_along(files)), names(files))
  server$requests <- character()
  server$headers <- list()
  server$ranges <- character()
  server$refuse <- character()
  httr2::local_mocked_responses(
    function(req) fake_s3_answer(server, req),
    env = env
  )
  server
}

fake_s3_answer <- function(server, req) {
  method <- req$method %||% "GET"
  parsed <- httr2::url_parse(req$url)
  key <- curl::curl_unescape(sub("^/?bkt/?", "", parsed$path))
  query <- parsed$query
  server$requests <- c(server$requests, paste(method, key))
  server$headers <- c(server$headers, list(req$headers))
  has <- key %in% names(server$files)
  switch(
    method,
    GET = if (key == "") {
      fake_s3_list(server, query)
    } else if (!has) {
      httr2::response(404)
    } else {
      fake_s3_get(server, key, req$headers)
    },
    HEAD = if (!has) {
      httr2::response(404)
    } else {
      httr2::response(200, headers = list(
        ETag = paste0('"', server$etags[[key]], '"'),
        `Content-Length` = as.character(length(server$files[[key]])),
        `Last-Modified` = "Wed, 21 Oct 2026 08:36:27 GMT"
      ))
    },
    PUT = {
      server$files[[key]] <- readBin(req$body$data, "raw",
                                     file.size(req$body$data))
      server$etags[[key]] <- paste0("p", length(server$requests))
      httr2::response(200)
    },
    DELETE = {
      if (has) {
        server$files[[key]] <- NULL
        server$etags <- server$etags[names(server$etags) != key]
      }
      httr2::response(204)
    },
    httr2::response(405)
  )
}

fake_s3_list <- function(server, query) {
  prefix <- query$prefix %||% ""
  keys <- sort(names(server$files))
  keys <- keys[startsWith(keys, prefix)]
  rest <- substring(keys, nchar(prefix) + 1)
  dirs <- character()
  if (!is.null(query$delimiter)) {
    nested <- grepl(query$delimiter, rest, fixed = TRUE)
    dirs <- unique(sub(paste0("(", query$delimiter, ").*$"), "\\1",
                       rest[nested]))
    dirs <- paste0(prefix, dirs[nzchar(dirs)])
    if (!any(nested)) dirs <- character()
    keys <- keys[!nested]
  }
  # Entries, sorted as S3 does; the token is the last entry returned.
  entries <- sort(c(keys, dirs))
  after <- query$`continuation-token`
  if (!is.null(after)) entries <- entries[entries > after]
  page <- as.integer(query$`max-keys` %||% 1000)
  truncated <- length(entries) > page
  entries <- utils::head(entries, page)
  is_key <- entries %in% keys
  contents <- vapply(entries[is_key], function(k) {
    paste0("<Contents><Key>", k, "</Key>",
           "<LastModified>2026-10-21T08:36:27.000Z</LastModified>",
           "<ETag>&quot;", server$etags[[k]], "&quot;</ETag>",
           "<Size>", length(server$files[[k]]), "</Size></Contents>")
  }, character(1))
  prefixes <- character()
  if (any(!is_key)) {
    prefixes <- paste0("<CommonPrefixes><Prefix>", entries[!is_key],
                       "</Prefix></CommonPrefixes>")
  }
  body <- paste0(
    '<?xml version="1.0"?><ListBucketResult xmlns="http://s3.amazonaws.com/',
    'doc/2006-03-01/"><Name>bkt</Name><KeyCount>', length(entries),
    "</KeyCount><IsTruncated>", tolower(truncated), "</IsTruncated>",
    if (truncated) {
      paste0("<NextContinuationToken>", utils::tail(entries, 1),
             "</NextContinuationToken>")
    },
    paste(contents, collapse = ""), paste(prefixes, collapse = ""),
    "</ListBucketResult>"
  )
  httr2::response(200, headers = list(`Content-Type` = "application/xml"),
                  body = charToRaw(body))
}

fake_s3_storage <- function(...) {
  cloud_s3("bkt", endpoint = s3_test_endpoint, anonymous = TRUE,
           name = "fake", ...)
}

# GET of an object, with the Range and If-Match headers. `server$refuse`
# lists the ranges answered with an error (403) to interrupt a download.
fake_s3_get <- function(server, key, headers) {
  body <- server$files[[key]]
  etag <- paste0('"', server$etags[[key]], '"')
  range <- headers$Range
  if (is.null(range)) {
    return(httr2::response(200, headers = list(ETag = etag), body = body))
  }
  server$ranges <- c(server$ranges, range)
  if (range %in% server$refuse) {
    return(httr2::response(403))
  }
  if (!is.null(headers$`If-Match`) && headers$`If-Match` != etag) {
    return(httr2::response(412))
  }
  if (length(body) == 0) {
    return(httr2::response(416))
  }
  bounds <- as.numeric(strsplit(sub("bytes=", "", range), "-")[[1]])
  last <- min(bounds[2], length(body) - 1)
  httr2::response(
    206,
    headers = list(
      ETag = etag,
      `Content-Range` = sprintf("bytes %d-%d/%d", bounds[1], last,
                                length(body))
    ),
    body = body[(bounds[1] + 1):(last + 1)]
  )
}
