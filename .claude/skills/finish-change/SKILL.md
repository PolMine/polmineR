---
name: finish-change
description: Wrap up a logical change to polmineR by regenerating docs, running tests, bumping the dev version and Date, and adding a NEWS.md bullet. Does not commit.
disable-model-invocation: true
---

Wrap up the current logical change. `$ARGUMENTS` may describe the change or name the issue number.

1. Review the change with `git status` and `git diff`. If it spans unrelated changes, stop and ask how to split it.
2. Run `Rscript -e 'devtools::document()'`. Check that the `man/`, `NAMESPACE` and `Collate` changes follow from the change. If it produces unrelated churn, report it rather than keeping it.
3. If the change adds a help topic, add it to the reference index in `_pkgdown.yml`.
4. Run `Rscript -e 'devtools::test()'`. If tests fail, stop and report the failures. Do not bump the version or change expected values to get a pass.
5. In `DESCRIPTION`, increment the last component of `Version` by one (e.g. `0.8.9.9011` to `0.8.9.9012`) and set `Date` to today. If the version has already been bumped for this change (compare with `git diff HEAD -- DESCRIPTION`), leave it.
6. If the change is user-visible, add a bullet to the top section of `NEWS.md`, ending with the issue number if there is one. Do not rewrite the section heading unless asked.
7. Report what changed, the test result and a suggested commit message (a short lowercase phrase, with the issue number if there is one). Do not commit unless asked.
