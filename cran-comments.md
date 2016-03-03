## Test environments
* local OS X install, R 3.2.3
* ubuntu 12.04 (on travis-ci), R 3.2.3
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 notes

* Found the following (possibly) invalid URLs:
  URL: https://console.cloud.google.com/
  
  This appears to because the server is incorrectly configured and returns
  a 404 for HEAD requests. I've forwarded the problem on to my contact at
  google and they're going to look into it.

## Reverse dependencies

* There are currently no reverse dependencies
