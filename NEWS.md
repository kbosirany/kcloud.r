# kcloud.r 0.0.0.9000

* `cloud_s3()`: S3 buckets (AWS, MinIO, Ceph, public buckets), with signed
  requests (or `anonymous = TRUE`), a fast parallel recursive listing that
  switches from folder by folder to flat listings, and the usual
  operations, downloads of big files in parallel chunks that resume after
  an interruption, and `cloud_vsi()` to read a part of a remote file with
  GDAL (`terra`, `stars`, `sf`) without downloading it, and `cloud_sync()`
  to copy a folder from a storage to another, with resume (#5).

* First version: storages `cloud_local()` and `cloud_nextcloud()` (public
  link or account, WebDAV), operations `cloud_path()`, `cloud_url()`,
  `cloud_download()`, `cloud_upload()`, `cloud_delete()`, `cloud_exists()`
  and `cloud_list()`, persistent cache with `max_age` and `max_size`,
  configuration with `load_config()` and `cloud_from_config()` (#3).
