# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/), and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [v1.1.0] - 2026-11-14

### Added
- Support for Debian and RPM package generation, passing a list of package kinds to `--out-package`.
- A `--single-exe` option to keep only the selected executable in generated packages.
- A `--debug` option to keep the generation directory after a correct run.
- Improved desktop metadata handling and terminal-related packaging options.
- Tests using [bbt](https://github.com/LionelDraghi/bbt) including packaging checks
  and validation for AppImage, Debian and RPM artifacts.
- RPM and Debian packages uploaded to the Releases section in GitHub.

### Changed
- Internally changed `spoon` crate by `aShell` for spawning processes. No user-visible changes.

### Fixed
- Corrected packaging problems related to file permissions.

## [v1.0.0] - 2025-03-21

### Added
- New `--use-version` switch to use the crate version from `alire.toml` for the AppImage version.

### Changed

### Fixed
- Avoided searching too deeply for the executable and improved executable discovery logic.

## [v0.9.3] - 2024-03-19

### Added
- New `--executable` argument to specify the executable to use when the crate name and the primary executable differ.

### Changed
- Improved the continuous release workflow and artifact naming.
- Updated the project documentation to reflect the Alire index release and usage details.

### Fixed
- Fixed the case where the crate name and the first executable were not the same.
- Improved README guidance and updated release automation.

## [v0.9.2] - 2024-03-14

### Changed
- The project was renamed to `alr2appimage` to avoid confusion and conflicts with a possible official Alire extension.

### Fixed
- Updated the GitHub Action after the rename so the workflow continued to build and upload artifacts correctly.

## [v0.9.0] - 2024-03-10

### Added
- First public release of the project.
- Initial workflow to build and upload self-generated AppImages.
- Initial release setup for inclusion in the Alire index.

### Changed
- Established the first public packaging and release pipeline.

### Fixed
- Initial cleanup and metadata updates for the initial public release.
