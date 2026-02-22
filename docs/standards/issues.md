# Issues

## Labels

Two dimensions. Every issue gets exactly one of each.

### Category (what kind of work)

- `bug` -- something isn't working as expected
- `enhancement` -- new feature or improvement
- `ux` -- user experience improvement (same priority model as enhancement)
- `research` -- investigation spike (output is a decision, not code)

### Priority (how urgent)

- `P1` (red) -- critical. Broken functionality, data loss, blocking a release, or actively hurting users. Drop everything.
- `P2` (yellow) -- medium. Important, planned for the next milestone. This is the active work queue.
- `P3` (green) -- low. Nice to have, backlog. Gets done when relevant or someone's bored.

## Issue Types

- `Bug` -- fix it, patch release (0.x.1). Category label: `bug`.
- `Enhancement` -- plan it, milestone it, minor release (0.x.0). Category label: `enhancement`.
- `Research spike` -- timebox the investigation. Output is a decision, not code. Close with a conclusion in the issue comments, open follow-up issues if warranted. Category label: `research`.
- `UX improvement` -- same lifecycle as enhancement. Category label: `ux`.

## Triage

Every new issue gets triaged before work starts:

1. Read the issue, understand the scope
2. Assign category label (`bug`, `enhancement`, `ux`, or `research`)
3. Assign priority label (`P1`, `P2`, or `P3`)
4. Optionally assign to a milestone (if it belongs to a planned release)

Triage format (when processing multiple issues):

```
| # | Title | Category | Priority | Rationale |
```

## Issue Templates

- Bug report (`bug_report.yml`): auto-labels `bug`. Priority added manually during triage.
- Feature request (`feature_request.yml`): auto-labels `enhancement`. Priority added manually during triage.

## Milestones

Milestones = releases. Each planned minor version gets a GitHub milestone (e.g., "v0.4.0"). Issues assigned to milestones indicate planned scope for that release.

- P2 issues are typically assigned to the next milestone
- P3 issues go to Backlog milestone or no milestone
- Close milestone after the release is published
- Check milestones: `gh api repos/rocketman-code/chainsaw/milestones`
