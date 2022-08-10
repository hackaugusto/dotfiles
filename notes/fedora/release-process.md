# Overview

Overview of the [process][FESCo-update-policy]:

- Developement is done on main/rawhide
    - This is a rolling release packaging
    - New packages are *manually* triggered on [koji][koji], updates are sent to [bodhi][bodhi], then tests are run on the CI
    - If the package is not [gated][rawhide-gating], it will be automatically pushed to fedora if CI result is green
- Branched is a fork of rawhide which starts the stabilization process for a new release
    - After the branch the repos are frozen until the 


# Links

[releases]: https://docs.fedoraproject.org/en-US/releases/
[rawhide-gating]: https://docs.fedoraproject.org/en-US/rawhide-gating/
[FESCo]: https://docs.fedoraproject.org/en-US/fesco/
[FESCo-update-policy]: https://docs.fedoraproject.org/en-US/fesco/Updates_Policy/
[package-maintainers]: https://docs.fedoraproject.org/en-US/package-maintainers

[freeze-exception-bug]: https://fedoraproject.org/wiki/QA:SOP_freeze_exception_bug_process
[blocker-bug]: https://fedoraproject.org/wiki/QA:SOP_blocker_bug_process
[rationale]: https://listman.redhat.com/archives/fedora-advisory-board/2006-December/002039.html

[koji]: https://koji.fedoraproject.org/koji/
[bodhi]: https://bodhi.fedoraproject.org/
[datagrepper]: https://apps.fedoraproject.org/datagrepper/
[copr]: https://copr.fedorainfracloud.org/
[pungi]: https://pagure.io/pungi

[labs]: https://labs.fedoraproject.org/
[spins]: https://spins.fedoraproject.org/
