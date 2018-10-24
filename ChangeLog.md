# Changelog for build-output-checker

## Unreleased changes

## Version 0.1.2

Added checks:
- added duration per repository

- added generatedOn timestamp to the report

## Version 0.1.1

Added checks:
- Report when multiple versions of maven plugin are used

- update to lts-12.14
- refactored maven transfer parsing

## Version 0.1.0

Added checks:
- Total duration of the build
- How long / what proportion of total build time do we spend maven-downloading stuff?
- How long do individual test classes / methods take?
- Which maven plugins / executions are slow?
