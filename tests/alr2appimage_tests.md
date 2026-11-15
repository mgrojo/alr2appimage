# Black box tests for alr2appimage

These scenarios exercise the real command line and the generated packages produced
by `alr2appimage` itself.

You can run them with:
`bbt --yes --cleanup --exec_dir .. alr2appimage_tests.md`

Prerequirements: `curl`, `lintian` and `rpmlint` must be installed.

## Feature: command-line interface

### Scenario: version output
- When I run `./bin/alr2appimage --version`
- Then output contains `alr2appimage version: `
- and there is no error

### Scenario: help output
- When I run `./bin/alr2appimage --help`
- Then output contains `Usage: `
- and there is no error

## Feature: package generation

### Background :
- Given there is no `alr2appimage-x86_64.AppImage` file
- And there is no `alr2appimage-1.1.1-amd64.deb` file
- And there is no `alr2appimage-1.1.1-1.x86_64.rpm` file

### Scenario: create the AppImage, Debian and RPM bundles
- When I run `./bin/alr2appimage --out-package AppImage,Deb,RPM`
- Then output contains `Success! Package is ready`
- And there is a file `alr2appimage-x86_64.AppImage`
- And there is a file `alr2appimage-1.1.1-amd64.deb`
- And there is a file `alr2appimage-1.1.1-1.x86_64.rpm`
- and there is no error

## Feature: generated package quality checks

### Scenario: AppImage linting
- Given the new executable file `lint_appimage.sh` containing
```sh
#!/bin/sh
curl --silent https://github.com/TheAssassin/appimagelint/releases/download/continuous/appimagelint-x86_64.AppImage -o appimagelint 
chmod +x appimagelint
exec ./appimagelint ./alr2appimage-x86_64.AppImage
```
- When I run `./lint_appimage.sh`
- Then there is no error output
- And I get no error

### Scenario: Debian package linting
- When I run `lintian --suppress-tags=no-changelog,unstripped-binary-or-object alr2appimage-1.1.1-amd64.deb`
- Then there is no error output
- And I get no error

### Scenario: RPM package linting
- When I run `rpmlint -r tests/rpmlintrc alr2appimage-1.1.1-1.x86_64.rpm`
- Then there is no error output
- And the output contains `0 errors`

## Feature: testing options

### Background :
- Given there is no `alr2appimage-1.1.1-x86_64.AppImage` file
- And there is no `alr2appimage-1.1.1-amd64.deb` file
- And there is no `alr2appimage-1.1.1-amd64.deb` file
- And there is no `alr2appimage-1.1.1-1.x86_64.rpm` file

### Scenario: single exe
- When I run `./bin/alr2appimage --out-package rpm --single-exe`
- Then I get no error
  - And output contains `Removed other executables from`
  - And there is a file `alr2appimage-1.1.1-1.x86_64.rpm`

### Scenario: icon
- When I run `./bin/alr2appimage --out-package Deb --icon ./tests/tests.png`
- Then I get no error
  - And output contains `Copying ./tests/tests.png to `
  - And there is a file `alr2appimage-1.1.1-amd64.deb`

### Scenario: terminal
- When I run `./bin/alr2appimage --out-package deb --terminal`
- Then I get no error
  - And the file `alr2appimage.desktop` contains `Terminal=true`

### Scenario: use version
- When I run `./bin/alr2appimage --out-package appimage --use-version`
- Then I get no error
  - And there is a file `alr2appimage-1.1.1-x86_64.AppImage`

### Scenario: no options
- When I run `./bin/alr2appimage --out-package appimage`
- Then I get no error
  - And the file `alr2appimage.desktop` contains `Terminal=false`
  - And there is a file `alr2appimage-x86_64.AppImage`
