# How to contribute

Thank you for using and contributing to StGit! This brief guide
describes how to contribute bug reports, feature ideas, and code changes
to the Stacked Git project.

## Bugs

StGit is not bug-free software. If you think you may have found a bug,
please visit the [issue tracker](https://github.com/stacked-git/stgit/issues)
and search to see if an issue for the bug has already been created.

If a relevant issue already exists, you may want to subscribe to the
issue so that you can be notified when the issue status changes. Also,
you may want to add notes about the bug if your observations differ
from the original issue.

If an existing issue does not match, then you are encouraged to create a
new issue.

### Creating an Issue

When creating an issue, please refer to this checklist. The goal is to
capture the relevant information about your environment and the actions
that triggered the bug needed to debug the problem.

Please include the following in StGit issue reports:

1. The output from `stg --version`, which includes StGit and Git
   versions.

2. What StGit command(s) triggered the problem.

3. Details about the StGit stack and the Git repository.


## Features

Ideas for StGit features may be submitted on the [issue
tracker][issue-tracker] for consideration and discussion. A proposed
feature's value will be weighed against its reach to StGit users and its
ongoing cost to StGit maintenance. High-reach, low-maintenance features
are most likely to be accepted by StGit's maintainers.

And for a StGit feature to be realized, it needs to be implemented.
While it is *possible* that another member of the StGit community will
champion your idea, the more you contribute, the more likely the feature
will get done. So writing test cases, specifying details about the
feature, and writing working code are all helpful toward getting a
feature into StGit.

[issue-tracker]: https://github.com/stacked-git/stgit/issues


## Code Changes

Code changes for bug fixes and features should be submitted as [pull
requests][PR] to the [stgit repository][stgit-repo] on GitHub, with the
following guidelines:

- Test cases! Please add test cases to the test suite in the `t`
  directory to verify any new or changed StGit behaviors. And run `make
  test` to ensure the test suite passes.
  
- Use [Conventional Commit][conventional-commit] style commit messages.

- Add a `Signed-off-by:` trailer to each commit message to indicate that
  certifies that you wrote or otherwise have the right to contribute the
  patch as open-source, according to the [Developers Certificate of
  Origin](#developers-certificate-of-origin-11). A `Signed-off-by:` line
  can be added to a patch by running `stg edit --sign`, and you can add one to
  new patches by default with the setting by setting the config
  `stgit.autosign` to `Signed-off-by`.
  
- Lint. Run `make lint` to ensure that the code meets the project's
  syntactic standards and passes static checks.
  
[PR]: https://help.github.com/en/github/collaborating-with-issues-and-pull-requests
[stgit-repo]: https://github.com/stacked-git/stgit
[conventional-commit]: https://www.conventionalcommits.org/

### Developer's Certificate of Origin 1.1

By making a contribution to this project, I certify that:

(a) The contribution was created in whole or in part by me and
    I have the right to submit it under the open source
    license indicated in the file; or

(b) The contribution is based upon previous work that, to the
    best of my knowledge, is covered under an appropriate open
    source license and I have the right under that license to
    submit that work with modifications, whether created in
    whole or in part by me, under the same open source license
    (unless I am permitted to submit under a different
    license), as indicated in the file; or

(c) The contribution was provided directly to me by some other
    person who certified (a), (b) or (c) and I have not
    modified it.

(d) I understand and agree that this project and the
    contribution are public and that a record of the
    contribution (including all personal information I submit
    with it, including my sign-off) is maintained indefinitely
    and may be redistributed consistent with this project or
    the open source license(s) involved.

### Checklist

- Each commit (patch) addresses a coherent topic.
- `make lint` passes.
- No commented-out code or unneeded files in commits.
- Each commit has a meaningful commit message using the [Conventional
  Commit][conventional-commit] style.
- Each commit has a `Signed-off-by: Your Name <you@example.com` trailer.
- Tests are added/modified that cover the bug fix or feature being
  added.
- `make test` passes.
