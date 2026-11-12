# Contributing

## What & How Can You Contribute?

#### 1. Feedback

Contributions in the form of feedback and issue is very much welcome. Might it be a suggestion, a bug report or maybe some questions that you have. It helps improving Ogmios in the long run and these are probably the best kind of contributions to start with. See [About Issues/Discussions](#about-issues--discussion) below for guidelines.

Do not hesitate to upvote discussions or comments to show your interest!

#### 2. Code

 Refer to the [building](https://ogmios.dev/getting-started/building/) and [testing](https://ogmios.dev/getting-started/testing/) sections of the [user-manual](https://ogmios.dev) if you're willing to hack a bit on Ogmios. The project follows a quite strict architecture as detailed in the [server's README](https://github.com/CardanoSolutions/ogmios/tree/master/server#architecture).

 Past (and future) design decisions have been (will be) recorded as [Architectural Decision Records](https://github.com/CardanoSolutions/ogmios/tree/master/architectural-decisions/accepted). Have a look at them before you start hacking to better understand some of trade-offs made during the development.

 Coding standards are enforced using tools and configuration defined in the repository. Make sure to configure your editor to pick them up.

 Pull requests are welcome, but we do recommend to open an issue to bring any idea to discussion first!

##### GitHub Actions checks

Fork pull requests build and lint the TypeScript client without repository secrets. Same-repository pull requests, pushes to `master`, and manual Continuous Integration runs also run the AWS-backed integration tests. Manual runs retain the branch/ref selector; select a trusted repository revision because those runs have AWS access. Repository writers are trusted with that access.

The fork job conditions route checks; GitHub's restrictions on fork tokens and secrets provide the security boundary. Keep those restrictions enabled in repository settings. Container publication requires a successful Continuous Integration run for the exact tagged commit from a push to `master` or a manual run on `master`; a PR check cannot satisfy it. Actions caching is disabled in these workflows; S3 database snapshots and the Nix binary cache remain available.

Integration tests restore the preview database from S3 but do not upload it. Network Synchronization is the sole cache writer, with scheduled and manual runs serialized across refs. Restore fetches immutable data, the two newest ledger snapshots, and volatile data. After a successful restore and clean node shutdown, the writer uploads retained snapshots before pruning obsolete snapshots and mirrors volatile files so they stop accumulating. Partial synchronization progress can still be saved; an unclean shutdown cannot replace the cache.

The first successful writer run prunes the legacy ledger accumulation. Legacy volatile data is still restored in full until that run mirrors the node's current files. The shared S3 prefix is not an atomic snapshot; readers can overlap an upload, and restores retry transfer failures. Avoid running older workflows that still upload the entire database alongside the updated writer. If S3 versioning is enabled, deleting current objects does not reclaim their noncurrent versions; bucket lifecycle rules control that storage.

Run the local cache regression tests with `python3 .github/workflows/scripts/test-preview-cache.py` (requires Bash and jq). They use fixture data and fake S3 commands, without AWS access or a running container.

#### 3. Donation

Want to give some financial support? Have a look at the [sponsors page](https://github.com/sponsors/KtorZ/) for more details.

## About Issues

### :bug: How To Report A Bug

Open a [Bug Issue](https://github.com/cardanosolutions/ogmios/issues/new?template=bug.md).

### :bulb: How To Propose An Idea

Feel free to bring it as a [discussion [category: idea]](https://github.com/CardanoSolutions/kupo/discussions/new?category=ideas). Make sure to highlight your use case so we can understand the design space and agree on a solution.

> [!INFO]
> Ideally, follow this simple template:
>
> - What is your idea? Describe it in simple words. Provide a use case.
> - Why is it a good idea?
> - What is the current alternative and why is it not good enough?

### :question: How To Ask a Question

Open a [Q&A Discussion](https://github.com/CardanoSolutions/ogmios/discussions/new?category=q-a).

> [!IMPORTANT]
> Make sure to mark your question as _Answered_ once resolved!

## Need Help Getting Started?

Should you be unsure about where to start, feel free to come and chat on <a href="https://discord.gg/zMr6EGvBpr"><img src=".github/discord.svg" alt="Discord">Discord (#ogmios)</a>.
