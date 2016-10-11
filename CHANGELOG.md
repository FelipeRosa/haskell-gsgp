# Change Log
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/)
and this project adheres to [Semantic Versioning](http://semver.org/).

## Unreleased
## Added
- `GSGP.Data.fromList` & `GSGP.Data.fromList2` for creating datasets from lists (1D & 2D) : @FelipeRosa
- Basic generic world module (GSGP.World) : @FelipeRosa
- Basic sequential world module (GSGP.World.Seq) : @FelipeRosa
- `GSGP.Data.saveTxt` saves datasets to .txt files : @FileRosa
- `GSGP.Metrics.mse` computes the mean squared error for two datasets : @FelipeRosa

## Changed
- Renamed `GSGP.Data.reshape` to `GSGP.Data.slice` : @FelipeRosa
- `GSGP.Metrics.rmse` now uses the `GSGP.Metrics.mse` internally : @FelipeRosa


## [0.4.0] - 2016-10-10
### Added
- Dataset per line map `GSGP.Data.mapR` function : @FelipeRosa

## [0.3.0] - 2016-10-10
### Added
- Ability to generalize language constants instantiation with the `LanguageConstant` type class : @FelipeRosa

## [0.2.0] - 2016-10-07
### Added
- Basic metrics module (GSGP.Metrics) : @FelipeRosa

## 0.1.0 - 2016-10-06
### Added
- Basic dataset module (GSGP.Data) : @FelipeRosa
- Basic language module (GSGP.Language) : @FelipeRosa

[Unreleased]: https://github.com/FelipeRosa/haskell-gsgp/compare/master...develop
[0.4.0]: https://github.com/FelipeRosa/haskell-gsgp/compare/v0.3.0...v0.4.0
[0.3.0]: https://github.com/FelipeRosa/haskell-gsgp/compare/v0.2.0...v0.3.0
[0.2.0]: https://github.com/FelipeRosa/haskell-gsgp/compare/v0.1.0...v0.2.0
