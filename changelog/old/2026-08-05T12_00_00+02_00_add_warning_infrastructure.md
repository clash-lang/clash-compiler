---
issues: [3306]
prs: []
---

# ADDED
Warnings emitted by Clash now have names and can be controlled individually with GHC-style flags:
`-W<name>`, `-Wno-<name>`, `-Werror=<name>`, and `-Wwarn=<name>` / `-Wno-error=<name>`.
The initial set of named warnings is `clash-dubious-primitive`, `clash-non-synthesizable`,
`clash-primitive-definition`, `clash-cast-specialization`, and `clash-integer-narrowing`;
all of them are enabled by default. GHC's global `-Werror` continues to promote all Clash warnings to errors,
with `-Wwarn=<name>` exempting individual warnings from it.
