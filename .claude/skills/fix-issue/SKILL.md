---
name: fix-issue
description: Work through a GitHub issue in this R package, from reproducing it in a test to a commit that closes it
disable-model-invocation: true
---

# Fix GitHub issue #$ARGUMENTS

## 1. Understand
- Run `gh issue view $ARGUMENTS --comments` and read the issue and all comments.
- Find the relevant code in `R/` and the existing tests in `tests/testthat/`.
- If the issue is unclear, contradicts itself, or needs a design decision
  (new exported function, changed API, new dependency), stop and ask me first.

## 2. Branch
- Make sure the working tree is clean (`git status`); if not, stop and tell me.
- Create a branch from the default branch: `git checkout -b issue-$ARGUMENTS`.

## 3. Reproduce
- Write a testthat test that reproduces the problem and fails.
- Run `devtools::test(filter = "<file>")` and confirm it fails for the right reason.
- For feature requests, write tests describing the new behaviour instead.

## 4. Fix
- Make the smallest change that fixes the issue; don't refactor unrelated code.
- Update roxygen comments if behaviour or arguments change, then run `devtools::document()`.
- Don't add package dependencies without asking me.

## 5. Verify
- Run `devtools::test()`; all tests must pass.
- Run `devtools::check()`; there must be no errors or warnings, and no new notes.
- If something fails, fix it and run the checks again.

## 6. Record
- Add a bullet to the top section of `NEWS.md` describing the change, ending with `(#$ARGUMENTS)`.
- Commit with a short summary line and a body ending in `Closes #$ARGUMENTS`.
- Don't push and don't open a PR unless I ask.

## 7. Report
Summarize briefly:
- What caused the problem
- What you changed (files)
- Test and check results
- Anything you were unsure about or that I should review
