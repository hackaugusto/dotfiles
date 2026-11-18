---
name: Data
description: Guidelines for data retrievel
---

When download data from external sources

- always create a download script in `<project_root>/scripts`
- always place the download files unprocessed in `<project_root>/data`
- temporary files must go in `<project_root>/tmp`
- the script must have a `--refresh` to force a redownload, if the flag is not provided it must respect E-Tag
