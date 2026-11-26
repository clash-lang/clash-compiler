---
issues: []
prs: []

---

# ADDED
Clock domains support fractional picoseconds through `ClockPeriodFraction`.
`DomainPeriodFraction` projects the fractional part for a logical domain, and
`knownFraction` supplies its `SRatioNat` singleton. `sClockDomainPeriod` returns
the complete period from an `SClockDomainConfiguration`.

# CHANGED
`VClockDomainConfiguration.vPeriod` now holds a `Period` in picoseconds,
including any fractional part. Use `hzToPeriod @Period freq` for an exact
period or `floor (hzToPeriod @Period freq)` for the former whole-picosecond
result. `clockPeriod @dom` gives the whole picoseconds; use
`vPeriod (knownVClockDomain @dom)` for the complete period.

Clock frequencies, cycle counts, DDR period constraints, RAM simulation,
clock generation, timing constraints, and VCD traces account for the complete
period. `DomainPeriodMultiple` expresses exact ratios between clock periods.

In `clash-manifest.json`, whole periods remain JSON numbers. For a fractional
period, divide the object's `numerator` by its `denominator` to obtain
picoseconds.
