% erl-influx changelog

# Next Version
## Misc
- Replace gun by [erl-mhttp](https://github.com/galdor/erl-mhttp).
- Send points every 2.5 seconds by default, instead of every second.

# 1.1.0
## Features
- The default option map is now `#{}`. `influx_client:default_options/0` is
  therefore useless and has been removed.
## Bugs
- Fix type specification in the `influx_memory_probe` module.

# 1.0.1
## Fixes
- Fix various type specifications.

# 1.0.0
First public version.
