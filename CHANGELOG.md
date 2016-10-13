# Change Log
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/)
and this project adheres to [Semantic Versioning](http://semver.org/).

## [0.6.0] - 2016-10-13
### Changed
- Moved train input, evaluation function, selection function, fitness function and bloat control function from `GSGP.World.Seq.SeqWorld` to `GSGP.World.Seq.SeqWorldParams`

## [0.5.1] - 2016-10-13
## Changed
- `GSGP.Data.loadTxt` now filters blank lines from the dataset files


## [0.5.0] - 2016-10-12
## Added
- `GSGP.Data.fromList` & `GSGP.Data.fromList2` for creating datasets from lists (1D & 2D)
- Basic generic world module (`GSGP.World`)
- Basic sequential world module (`GSGP.World.Seq`)
- `GSGP.Data.saveTxt` saves datasets to .txt files
- `GSGP.Metrics.mse` computes the mean squared error for two datasets
- `GSGP.Statistics` with covariance, variance and mean functions
- `GSGP.BloatControl` module with `parsimonyPressure` function. This module should contain functions useful for controlling program size
- `GSGP.Language.Program` data type which holds program code and size for doing some useful optimizations and bloat control
- `GSGP.Selection` module with `tournamentSelection` function which performs tournament selection on a population


## Changed
- Renamed `GSGP.Data.reshape` to `GSGP.Data.slice`
- `GSGP.Metrics.rmse` now uses the `GSGP.Metrics.mse` internally
- Most functions that returned `Language l` now return `Language l => Program l`


## [0.4.0] - 2016-10-10
### Added
- Dataset per line map `GSGP.Data.mapR` function


## [0.3.0] - 2016-10-10
### Added
- Ability to generalize language constants instantiation with the `LanguageConstant` type class

## [0.2.0] - 2016-10-07
### Added
- Basic metrics module (`GSGP.Metrics`)


## 0.1.0 - 2016-10-06
### Added
- Basic dataset module (`GSGP.Data`)
- Basic language module (`GSGP.Language`)


[Unreleased]: https://github.com/FelipeRosa/haskell-gsgp/compare/master...develop
[0.6.0]: https://github.com/FelipeRosa/haskell-gsgp/compare/v0.5.1...v0.6.0
[0.5.1]: https://github.com/FelipeRosa/haskell-gsgp/compare/v0.5.0...v0.5.1
[0.5.0]: https://github.com/FelipeRosa/haskell-gsgp/compare/v0.4.0...v0.5.0
[0.4.0]: https://github.com/FelipeRosa/haskell-gsgp/compare/v0.3.0...v0.4.0
[0.3.0]: https://github.com/FelipeRosa/haskell-gsgp/compare/v0.2.0...v0.3.0
[0.2.0]: https://github.com/FelipeRosa/haskell-gsgp/compare/v0.1.0...v0.2.0
