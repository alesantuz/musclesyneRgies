# How to contribute to musclesyneRgies
Thank you for taking the time to read this. We are always happy of receiving contributions. Our code might contain inaccuracies or it might not generalise enough for your needs. For issues like these, but also for other kinds of contributions, please feel free to suggest one or more edits using the instructions below.

## Reporting issues
Bugs are tracked as [GitHub issues](https://guides.github.com/features/issues/). Whenever you report an issue, please make sure to:
- Use a concise and descriptive title
- Report the OS you tested the code on and its version, obtainable through the function Sys.info()["sysname"], Sys.info()["release"] and Sys.info()["version"]
- Report the R version obtainable through the function R.version()
- Report whether the code ran successfully on the test data [CYCLE_TIMES.RData](https://github.com/alesantuz/musclesyneRgies/blob/master/CYCLE_TIMES.RData) and [RAW_EMG.RData](https://github.com/alesantuz/musclesyneRgies/blob/master/RAW_EMG.RData), meaning that the "Graphs" folder, subfolders and graphs were created and no errors returned.

## Contributing with code
- Check whether the code ran successfully on the test data [CYCLE_TIMES.RData](https://github.com/alesantuz/musclesyneRgies/blob/master/CYCLE_TIMES.RData) and [RAW_EMG.RData](https://github.com/alesantuz/musclesyneRgies/blob/master/RAW_EMG.RData), meaning that the "Graphs" folder, subfolders and graphs were created and no errors returned.
- Keep coding style consistent with the master
- Send a new [GitHub pull request to musclesyneRgies](https://github.com/alesantuz/musclesyneRgies/compare) and state in a clear and concise way what you are suggesting (read more about pull requests [here](https://docs.github.com/en/free-pro-team@latest/github/collaborating-with-issues-and-pull-requests/about-pull-requests)).
- Consider starting the commit message with a descriptive emoji:
    - :art: `:art:` when improving the format/structure of the code
    - :racehorse: `:racehorse:` when improving performance
    - :memo: `:memo:` when writing docs
    - :penguin: `:penguin:` when fixing something on Linux
    - :apple: `:apple:` when fixing something on macOS
    - :checkered_flag: `:checkered_flag:` when fixing something on Windows
    - :bug: `:bug:` when fixing a bug
    - :white_check_mark: `:white_check_mark:` when adding tests
    - :arrow_up: `:arrow_up:` when upgrading dependencies
    - :arrow_down: `:arrow_down:` when downgrading dependencies.
