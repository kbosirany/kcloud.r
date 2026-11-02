#' Copy a folder from a storage to another
#'
#' Copies the files of a folder of the storage `from` that are missing from
#' (or have another size in) the storage `to`, whatever the backends: S3 to
#' a local disk, S3 to Nextcloud, local to S3...
#'
#' The files go through this machine, a few at a time (`workers`), without
#' using the cache: the disk holds at most `workers` files of the transfer
#' in a temporary folder. The big files are downloaded in parallel chunks.
#'
#' The copy can be interrupted and run again: the files already copied are
#' skipped (same path and size), and a file being downloaded resumes with
#' its missing chunks. Nothing is ever deleted from `to`. A file that fails
#' does not stop the others: it is reported in the result and a warning.
#'
#' For a lot of data, run it from a machine with a fast connection.
#'
#' @param from,to Storages (see [cloud_local()], [cloud_s3()]...).
#' @param path Folder of `from` to copy. `""` is the whole storage.
#' @param to_path Folder of `to` that receives it. By default `path`.
#' @param pattern Regular expression the paths (relative to the root of
#'   `from`) must match, or `NULL` to copy everything.
#' @param dry_run If `TRUE`, nothing is copied: the result tells what would
#'   be.
#' @param workers Number of files transferred at the same time.
#' @param tmp_dir Folder of the temporary files, when `to` is not a local
#'   storage. With a local `to`, they are in a hidden folder of its root,
#'   to be moved in place without a second copy.
#'
#' @return A data.frame, invisibly, with one row per file of `from`: `path`
#'   (relative to `path`), `size`, `status` (`"skipped"`, `"copied"`,
#'   `"failed"`, or `"todo"` with `dry_run`) and `error`.
#' @family operations
#' @export
#'
#' @examples
#' from <- cloud_local(tempfile())
#' f <- tempfile(fileext = ".csv")
#' write.csv(head(iris), f)
#' cloud_upload(from, f, "iris/")
#' to <- cloud_local(tempfile())
#' cloud_sync(from, to, "iris/")
#' cloud_list(to, recursive = TRUE)
cloud_sync <- function(from, to, path = "", to_path = path, pattern = NULL,
                       dry_run = FALSE, workers = 4L, tmp_dir = tempdir()) {
  check_storage(from)
  check_storage(to)
  check_writable(to, "cloud_sync")
  check_flag(dry_run, "dry_run")
  check_number(workers, "workers", min = 1)
  check_string(tmp_dir, "tmp_dir")
  path <- dir_slash(remote_path(path))
  to_path <- dir_slash(remote_path(to_path))
  log_base <- paste0("kcloud.r - cloud_sync - ", from$name, " -> ", to$name)

  src <- cloud_list(from, path, recursive = TRUE, pattern = pattern)
  src <- src[!src$is_dir, , drop = FALSE]
  rel <- substring(src$path, nchar(path) + 1)
  dest <- paste0(to_path, rel)
  have <- tryCatch(
    cloud_list(to, to_path, recursive = TRUE),
    error = function(e) {
      if (!grepl("not found", conditionMessage(e))) stop(e)
      empty_listing()
    }
  )
  same <- have$size[match(dest, have$path)] %in% src$size &
    dest %in% have$path
  same <- same & have$size[match(dest, have$path)] == src$size
  same[is.na(same)] <- FALSE
  out <- data.frame(
    path = rel, size = src$size,
    status = ifelse(same, "skipped", "todo"), error = NA_character_,
    stringsAsFactors = FALSE
  )
  todo <- which(!same)
  message(log_base, " - ", length(todo), " of ", nrow(src), " files to ",
          "copy, ", format_gb(sum(src$size[todo])))
  if (dry_run || length(todo) == 0) {
    return(invisible(out))
  }

  stage_root <- if (inherits(to, "cloud_local")) {
    file.path(to$root, ".kcloud-sync")
  } else {
    file.path(tmp_dir, paste0("kcloud-sync-", Sys.getpid()))
  }
  on.exit(unlink(stage_root, recursive = TRUE, force = TRUE), add = TRUE)
  copy <- function(idx) {
    stage <- file.path(stage_root, "batch")
    unlink(stage, recursive = TRUE, force = TRUE)
    on.exit(unlink(stage, recursive = TRUE, force = TRUE), add = TRUE)
    staged <- file.path(stage, rel[idx])
    backend_get(from, src$path[idx], staged, workers = workers)
    bad <- file.size(staged) != src$size[idx]
    if (any(bad %in% TRUE | is.na(bad))) {
      stop("incomplete file: ", rel[idx][which(bad | is.na(bad))[1]],
           call. = FALSE)
    }
    if (inherits(to, "cloud_local")) {
      for (i in seq_along(idx)) {
        full <- local_full_path(to, dest[idx[i]])
        dir.create(dirname(full), showWarnings = FALSE, recursive = TRUE)
        replace_path(staged[i], full)
      }
    } else {
      cloud_upload(to, stage, to_path, overwrite = TRUE, workers = workers)
    }
  }
  total <- sum(src$size[todo])
  done <- 0
  batches <- split(todo, ceiling(seq_along(todo) / workers))
  for (idx in batches) {
    err <- tryCatch({
      copy(idx)
      NULL
    }, error = function(e) e)
    if (is.null(err)) {
      out$status[idx] <- "copied"
    } else {
      # One failing file must not hide the others: they go one by one.
      for (i in idx) {
        err <- tryCatch({
          copy(i)
          NULL
        }, error = function(e) e)
        out$status[i] <- if (is.null(err)) "copied" else "failed"
        if (!is.null(err)) out$error[i] <- conditionMessage(err)
      }
    }
    done <- done + sum(src$size[idx])
    message(log_base, " - ", sum(out$status == "copied"), "/",
            length(todo), " files, ", format_gb(done), " of ",
            format_gb(total))
  }
  if (any(out$status == "failed")) {
    warning(log_base, " - ", sum(out$status == "failed"), " files failed ",
            "(see the `error` column), run it again to retry them.",
            call. = FALSE)
  }
  invisible(out)
}

format_gb <- function(bytes) {
  if (bytes < 1e9) sprintf("%.1f MB", bytes / 1e6) else {
    sprintf("%.1f GB", bytes / 1e9)
  }
}
