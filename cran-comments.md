## After checks on 10 Feb 2022 (v1.1.1 resubmitted as v1.1.2)
- Removed all progress bars

## After re-submission on 09 Feb 2022
- Version was not bumped, now updated from version 1.1.0 to version 1.1.1
- Updated DESCRIPTION Depends: R (>= 4.1.0) due to the use of the new native pipe in some functions

## After CRAN Package Check Results on 2022-02-09 15:00:53 CET
- Modified test-CoA.R to comply with `M1mac` and `noLD`
Results:
- rhub::check(platform = "macos-m1-bigsur-release") OK

## After second checks on 08 Feb 2022
- Modified description in DESCRIPTION
- Placed immediate call of `on.exit()` when `par()` is called within a function

## After first checks on 07 Feb 2022
- Modified LICENSE file according to CRAN standards
- Modified wrong link in README
- Modified title in DESCRIPTION

## R CMD check results
Local using `devtools::check()`: 0 errors | 0 warnings | 0 notes\cr
Remote using github workflows (workflow derived from https://github.com/r-lib/actions/tree/master/examples):
- macOS-latest (release) 0 errors | 0 warnings | 0 notes\cr
- windows-latest (release) 0 errors | 0 warnings | 0 notes\cr
- ubuntu-latest (devel) 0 errors | 0 warnings | 0 notes\cr
- ubuntu-latest (release) 0 errors | 0 warnings | 0 notes\cr
