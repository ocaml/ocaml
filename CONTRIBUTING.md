# How to contribute changes

:+1::tada: First off, thank you for taking time to contribute! :tada::+1:

The following is a set of guidelines for proposing changes to the
OCaml distribution. These are just guidelines, not rules, use your
best judgment and feel free to propose changes to this document itself
in a pull request.

This document assumes that you have a patch against the sources of the
compiler distribution, that you wish to submit to the OCaml
maintainers upstream. See [INSTALL.adoc](INSTALL.adoc) for details on
how to build the compiler distribution from sources. See
[HACKING.adoc](HACKING.adoc) for details on how to modify the sources.

## Contribution

Modifying the sources is far from the only way to contribute to the
OCaml distribution. Bug reports (in particular when they come with
a reproducible example), simple typos or clarifications in the
documentation also help, and help evaluating and integrating existing
change proposals also help. Providing good answers on the discussion
forums, or asking the good questions that highlight deficiencies in
existing documentations, also help.

There are also many valuable ways to contribute to the wider OCaml
ecosystem that do not involve changes to the OCaml distribution.

The rest of the document is concerned with the form of change
proposals against the OCaml distribution. (Code changes, but also
improvement to documentation or implementation comments, which are
valuable changes on their own.)

## Workflow

All changes to the OCaml distribution need to be processed through the
GitHub Pull Request (PR) system.  In order to propose a change, a
contributor thus needs to have a GitHub account, fork the ocaml/ocaml
repository, create a branch for the proposal on their fork and submit
it as a Pull Request on the upstream repository.  (If you are not yet
familiar with GitHub, don't worry, all these steps are actually quite
easy!)

The current rule is that a PR needs to get an explicit approval from
one of the core maintainer in order to be merged.  Reviews by
external contributors are very much appreciated.

Since core maintainers cannot push directly without going through an
approved PR, they need to be able to apply small changes to the
contributed branches themselves.  Such changes include fixing
conflicts, adjusting a Changelog entry, or applying some code changes
required by the reviewers.  Contributors are thus strongly advised to
check the [**Allow edits from maintainer**](
https://help.github.com/articles/allowing-changes-to-a-pull-request-branch-created-from-a-fork/
) flag on their PRs in the GitHub interface.  Failing to do so might
significantly delay the inclusion of an otherwise perfectly ok
contribution.

### Maintainers

The current list of maintainers is as follows:

- @alainfrisch Alain Frisch
- @Armael Armaël Guéneau
- @avsm Anil Madhavapeddy
- @chambart Pierre Chambart
- @damiendoligez Damien Doligez
- @dra27 David Allsopp
- @Engil Enguerrand
- @garrigue Jacques Garrigue
- @gasche Gabriel Scherer
- @jhjourdan Jacques-Henri Jourdan
- @kayceesrk KC Sivaramakrishnan
- @let-def Frédéric Bour
- @lpw25 Leo White
- @lthls Vincent Laviron
- @maranget Luc Maranget
- @mshinwell Mark Shinwell
- @nojb Nicolás Ojeda Bär
- @Octachron Florian Angeletti
- @sadiqj Sadiq Jaffer
- @shindere Sébastien Hinderer
- @stedolan Stephen Dolan
- @trefis Thomas Refis
- @xavierleroy Xavier Leroy
- @yallop Jeremy Yallop

<!-- Note: the list comes from [this Github
page](https://github.com/orgs/ocaml/teams/ocaml-dev/members), plus
Anil as co-owner of the github/ocaml/ organization. Oddly enough,
Github does not make the page publicly accessible. -->


## Coding guidelines

You should not leave trailing whitespace; not have line longer than 80
columns, not use tab characters (spaces only), and not use non-ASCII
characters. These typographical rules can be checked with the script
`tools/check-typo`, see [HACKING.adoc: check-typo](HACKING.adoc#check-typo).

Otherwise, there are no strongly enforced guidelines specific to the
compiler -- and, as a result, the style may differ in the different
parts of the compiler. The general [OCaml Programming
Guidelines](https://ocaml.org/learn/tutorials/guidelines.html) are
good to keep in mind, and otherwise we strive for good taste and local
consistency (following the code located around your change).

If you strongly feel that a style-related change would improve quality
of the existing code (for example, giving more descriptive names to
some variables throughout a module, factoring repeated code patterns
as auxiliary functions, or adding comments to document a part of the
code that you had trouble understanding), you can have code cleanup
commits at the beginning of your patch series, or submit code cleanups
as your change proposal. Those cleanups should remain separate commits
from the functional changes in the rest of the patch series; it is
easier to review commits that are specifically marked as exactly
preserving the code semantics.


## Test you must.

Whenever applicable, merge requests must come with tests exercising
the affected features: regression tests for bug fixes, and correctness
tests for new features (including corner cases and
failure cases). Warnings and errors should also be tested.

See [testsuite/HACKING.adoc](testsuite/HACKING.adoc) for details on
how to write tests and run the testsuite.

Adding tests is also a way to make sure reviewers see working
(and failing) examples of the feature you fix, extend or
introduce, rather than just an abstract description of it.


## Description of the proposed change

### In the merge request interface

The description of the merge request must contain a precise
explanation of the proposed change.

Before going into the implementation details, you should include
a summary of the change, a justification of why it is beneficial, and
a high-level description of the design of the proposed change with
example use cases.

Changes have a cost, they require review work and may accidentally
introduce new bugs. Communicating as clearly as you can the benefits
of your PR will reassure and motivate potential reviewers.

### In the patches

If some of the explanations you provide for the merge request would
make sense as comments in the code, or documentation in the manual,
you should include them there as well.

In-code comments help make the codebase more accessible to newcomers
(many places in the compiler could benefit from a few
extra explanations), and they are also useful to code reviewers. In
particular, any subtlety in code that cannot be made
self-explanatory should come with an explanation in comment. If you
add some non-obvious code specifically to fix a bug, include the
issue number in comments.

Do not assume that code reviewers are all experts in the existing
codebase. If you use subtle code, add a comment, even if the same
kind of code is used somewhere else in the same module. (If this is
a common and useful domain-specific idiom that is already explained
somewhere, pointing to this explanation in your commit message is
better than adding redundant explanations.)

### User documentation

Changes affecting the compiler libraries should be reflected in the
documentation comments of the relevant `.mli` files. After running
`make html_doc`, you can find the HTML Standard Library documentation
at `./api_docgen/html/libref/index.html`.

It is recommended to include changes to the OCaml Reference Manual
(in particular for any change in the surface language), which is now
part of the main repository (under `manual/`). To build the full manual,
see the instructions in `manual/README.md`.

Finally, changes in command-line options should be integrated in the
manual, but also in the man pages present in the `man/` sub-directory
of the OCaml distribution.


### Changelog

Any user-visible change should have a `Changes` entry:

- in the right section (named sections if major feature, generic
  "Bug fixes" and "Feature requests" otherwise)

- using the label "`*`" if it breaks existing programs, "`-`" otherwise

- with all relevant issue and PR numbers `#{N}`, in ascending numerical order
  (separated by commas if necessary)

- maintaining the order: the entries in each section should be sorted by
  issue/PR number (the first of each entry, if more than one is available)

- with a concise readable description of the change (possibly taken
  from a commit message, but it should make sense to end-users
  reading release notes)

- crediting the people that worked on the feature. The people that
  wrote the code should be credited of course, but also substantial
  code reviews or design advice, and the reporter of the bug
  (if applicable) or designer of the feature request (if novel).

- following the format

        {label} {issue number(s)}: {readable description}
                ({credits})

      note that the `{credits}` should be on their own line, aligned with the
      issue number for readability
      (`{readable description}` can be multiline to not overflow 80
      columns, and should be aligned with the issue number as well.)

This changelog can be included in the main commit, if the merge
request is just one patch, or as a separate commit, if it's
a patch series and no particular commit feels best suited to
receive the Changelog entry.

(Do not under-estimate the importance of a good changelog. Users do
 read the release notes, and things forgotten from the changelog
 will cause pain or regrets down the line.)


## Clean patch series

Clean patch series are useful, both during the review process and
for code maintenance after it has been merged. Before submitting
your request, you should rebase your patch series:

- on top of the OCaml branch in which you want to merge
  (usually `trunk`), solving any conflicts.

- into a few well-separated, self-contained patches (github PRs
  can generate gazillions of micro-changes)

- erasing history that does not make sense after the issue is merged
  (back-and-forth between different designs, etc. The PR number
  allows interested people to go back to the original discussion if
  needed.)

- bisectable: the distribution should be in a good state after
  the application of each patch (in particular, later commits that
  fix bugs in previous commits should always be squashed into the commit
  they fix)

- with readable commit messages (this is for future developers
  needing to understand a change that happened in the past). Commit
  messages should not overflow 80 columns, with the following format:

        {one-liner header description (with issue number if applicable)}
        {blank line}
        {one or several paragraphs of explanation if needed}

During review, you may make many other changes to the patch
series. You can rebase it on the fly (if you `git push -f` on the
branch of the pull request in your personal clone, Github will
update the pull request automatically; remember to always create
a new branch for any) or wait until the discussion has converged,
once we agree the request is ready for merging. Doing a good
rebase is grunt work that takes some time and care (use `git
log -u` to make sure the rebase patches make sense), but:

- It is easier and faster to do for the author of the patch than
  for others (if rebasing against the current trunk creates
  a conflict with another change you don't understand well, feel
  free to ask).

- Maintainers are usually short on time, and asking them to do
  a rebase means they have less time to review and merge other
  contributions.

- The long-term benefits of keeping a clean, bisectable history
  cannot be overstated. Imagine that in three years, under the
  pressure of a coming release, a contributor ends up somewhere in
  the middle of your patch series, wondering if or why it is the
  cause of a specific issue. Wasting his or her time then
  (with a "yolo" commit message, a big ugly commit of unrelated
  changes, or an un-testable intermediary state) is a sure way to
  generate ill will.

## Contributing to the standard library

Contributions to the standard library are very welcome.
See the dedicated [stdlib/CONTRIBUTING.md](stdlib/CONTRIBUTING.md)
for more information.

## Contributing optimizations

Contributions to improve the compiler's optimization capabilities are
welcome. However, due to the potential risks involved with such
changes, we ask the following of contributors when submitting pull
requests:

 - Explain the benefits of the optimization (faster code, smaller
   code, improved cache behaviour, lower power consumption, increased
   compilation speed).

 - Explain when the optimization does and does not apply.

 - Explain when, if ever, the optimization may be detrimental.

 - Provide benchmark measurements to justify the expected
   benefits. Measurements should ideally include experiments with
   full-scale applications as well as with microbenchmarks.  Which
   kinds of measurements are appropriate will vary depending on the
   optimization; some optimizations may have to be measured indirectly
   (for example, by measuring cache misses for a code size
   optimization). Measurements showing clear benefits when combined
   with some other optimization/change are acceptable.

 - At least some of the measurements provided should be from
   experiments on open source code.

 - If assistance is sought with benchmarking then this should be made
   clear on the initial pull request submission.

 - Justify the correctness of the optimization, and discuss a testing
   strategy to ensure that it does not introduce bugs. The use of
   formal methods to increase confidence is encouraged.

A major criterion in assessing whether to include an optimisation in
the compiler is the balance between the increased complexity of the
compiler code and the expected benefits of the benchmark. Contributors
are asked to bear this in mind when making submissions.

## Collective maintenance

Proposing changes to the OCaml compiler contribution generates
"maintenance work" for other people. Maintenance work includes, for
example:

- reviewing Pull Requests or language change proposals,

- considering change suggestions and giving feedback to turn them into
  actionable issues,

- implementing bug fixes or feature requests of general interest,

- improving the documentation of the tools or other usability aspects,

- or documenting or clarifying the codebase to preserve and improve
  our ability to change it in the future.

Doing this collective maintenance work is a selfless task, and we
typically have much fewer people willing to to do it than people
willing to submit new language features or generally evolve the
codebase for their own specific needs. Without a collective effort to
participate, we end up with a handful of people doing the vast
majority of this collective maintenance work. This is exhausting, does
not scale, and slows down the pace of improvement of the compiler
distribution.

To keep a healthy open source project, we need the total maintenance
work performed by all contributors to scale proportionally with the
total demand for maintenance work they generate. This can only work if
as many contributors as possible perform some (possibly small) amount of
maintenance work: collective maintenance. One could use the metaphor
of a shared house: things work well when most people, not just a few
people, participate to the house chores.

If your contributions generate maintenance work for others -- in
particular, if you spend a substantial effort working on a change to
the language or compiler codebase meant to be eventually proposed
upstream -- we expect that you will spend a fraction of your
contribution time on maintenance tasks, typically on the parts of the
compiler codebase that you are already working on. This approach is
good for the project, and also for you: helping maintain the codebase
will improve the quality of your own contributions, and the social
ties created by infrequent collaboration with other contributors will
be useful when submitting your own work.

Note: we have been asked whether groups of contributors could balance
maintenance work at the level of the whole group, rather than
individual contributors -- for example a company where some frequent
OCaml contributors would do less maintenance and others would do more
to compensate. Yes, that sounds reasonable, but also harder to balance
than encouraging everyone to play nice individually.

## Contributor License Agreement

We distinguish two kind of contributions:

- Small changes that do not bear a specific mark of their authors
  (another developer recreating the change without access to the
  original patch would write an indistinguishable patch), and are thus
  not protected by copyright, do not require any particular
  paperwork. This is convenient for everyone, and of course does not
  mean that those contributions are of lesser importance. (For example
  a bugfix can be obvious once a bug is understood, reported and
  reproduced, and yet invaluable for users.)

- Larger changes that are covered by copyright. For them, we require
  contributors to sign a Contributor License Agreement (CLA), which
  gives [INRIA](http://www.inria.fr/en/) (Institut National de
  Recherche en Informatique et en Automatique) the rights to integrate
  the contribution, maintain it, evolve it, and redistribute it under
  the license of its choice. This is not a copyright *assignment*
  (as requested by the Free Software Foundation for example),
  contributors retain the copyright on their contribution, and can use
  it as they see fit. The OCaml CLA is lightly adapted from [the
  CLA](https://www.apache.org/licenses/icla.txt) of the Apache
  Foundation, and is available in two versions: [for individual
  contributors](http://caml.inria.fr/pub/docs/CLA-individual.doc) and
  [for corporations](http://caml.inria.fr/pub/docs/CLA-corporate.doc).

You must understand that, by proposing a contribution for integration
in the OCaml distribution, you accept that it be considered under one
of those regimes. In particular, in all cases you give INRIA the
permission to freely re-license the OCaml distribution including the
contribution.

This ability to re-license allows INRIA to provide members of the
[Caml Consortium](http://caml.inria.fr/consortium/) with a license on
the Caml code base that is more permissive than the public license.

### How to sign the CLA

If your contribution is large enough, you should sign the CLA. If you
are contributing on your own behalf, you should sign [the individual
CLA](http://caml.inria.fr/pub/docs/CLA-individual.doc). For corporate
contributions, if your employer has not already done so, they should
sign [the corporate
CLA](http://caml.inria.fr/pub/docs/CLA-corporate.doc). Review the CLA,
sign it, and send it -- scanned PDF by email, or postail mail -- to
Xavier Leroy ([contact
info](http://gallium.inria.fr/%7Exleroy/contact.html)).
