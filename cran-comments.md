## R CMD check results
Local using `devtools::check()`: 0 errors | 0 warnings | 0 notes\cr
Remote using github workflows (workflow derived from https://github.com/r-lib/actions/tree/master/examples):
- macOS-latest (release) 0 errors | 0 warnings | 0 notes\cr
- windows-latest (release) 0 errors | 0 warnings | 0 notes\cr
- ubuntu-latest (devel) 0 errors | 0 warnings | 0 notes\cr
- ubuntu-latest (release) 0 errors | 0 warnings | 0 notes\cr

## After first checks on 07 Feb 2022
- Modified LICENSE file according to CRAN standards
- Modified wrong link in README
- Modified title in DESCRIPTION

## After second checks on 08 Feb 2022
- Modified description in DESCRIPTION
- Placed immediate call of `on.exit()` when `par()` is called within a function
