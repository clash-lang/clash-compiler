---
issues: []
prs: [3411]
---

# CHANGED
The `Unmatchable constant as case subject` report (emitted when invariants are checked, e.g. under `-fclash-debug DebugSilent`) is now a proper warning instead of a trace, so `-Werror` / `-Werror=clash-unmatchable-constant` turns it into an error. It also names the binder being normalized. The warning can be suppressed with `-Wno-clash-unmatchable-constant`.
