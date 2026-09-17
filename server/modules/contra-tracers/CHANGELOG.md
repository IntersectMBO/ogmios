# Changelog

## [1.0.1] -- 2026-09-17

### Added

N/A

### Changed

- Require `Monad m` instead of `Applicative m` in the `GConfigureTracers` instance for `Tracer m msg`, since `contra-tracer >= 0.2` requires `Monad m` for `Contravariant (Tracer m)`.

### Removed

N/A

## [1.0.0] -- 2021-19-12

### Added

- Initial release. Key features:
  - Contravariant tracers, easily composable with parent context
  - Concurrent-safe, simple, structured (JSON), stdout writer
  - Generic definition of record of tracers for multi-component logging

### Changed 

N/A

### Removed

N/A
