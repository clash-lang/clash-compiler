---
issues: []
prs: [3163]

---

# Major Domain overhaul
Clash's `Domain`-system has had a major overhaul. The most important changes are:
- Synthesis domains are empty data types. Their type parameters can distinguish logical domains sharing a clock.
- Domains are now split between a regular domain and a clock domain.
  The clock domain represents everything connected to the same hardware clock.
  Domains are built on top of these:
  a `KnownDomain` instance links the domain to a clock domain, which might be shared with other domains.
- With these changes, new domain switching machinery is introduced.
  Functions like `switchDomain` can be used to switch between domains with the same clock domain,
  provided that the switch is marked as allowed by an instance of `SupportsSwitchingTo`.

These changes allow for the creation of domain constructions like
`Delayed 3 System` or `SecureDom System Public`. Keeping the clock domains separate means
that these types can still be on the same clock (i.e. `Delayed 3 System` and `System`,
or `SecureDom System Public` and `SecureDom System Private`).
In the latter case, the switching machinery makes it possible to define your domains such that
signals can go from the public into the private domain, but not the other way around.


# ADDED
Clock domains are now implemented through `KnownClockDomain`, which has types
`ClockPeriod`, `ClockActiveEdge`, `ClockResetKind`, `ClockInitBehavior`, `ClockResetPolarity`, and `ClockPeriodFraction`.

# ADDED
Similar to `knownDomain`, the functions `knownActiveEdge`, `knownResetKind`, `knownInitBehavior`, `knownResetPolarity` and `knownFraction`
can now be used to obtain singleton versions of domain parameters.

# ADDED
Several functions were added to switch between domains using the same clock domain.
Instances of `SupportsSwitchingTo` give rise to `EligibleDomainSwitch`,
allowing them to be switched between using `switchDomain`,
`switchClockDomain`, `switchResetDomain`, `switchEnableDomain`,
`switchHiddenClockDomain`, `switchHiddenResetDomain`, `switchHiddenEnableDomain`
and `switchHiddenClockResetEnableDomain`.

# CHANGED
`Domain` is now of kind `Type` instead of `Symbol`.

# CHANGED
`knownDomain` is no longer a class method of `KnownDomain`.

# CHANGED
Domains no longer specify the hardware parameters directly.
The open type family `ClockDomainOf` selects the physical clock for each logical domain.
Its result must have a `KnownClockDomain` instance. A physical clock maps to itself.

Clock domains are now Haskell types. Replace `Signal "System" a` with
`Signal System a`. For a custom domain, `createDomain vSystem{vName="Fast"}`
declares the type and its instances. To migrate a handwritten `KnownDomain`
instance, declare `data Fast`, set `type instance ClockDomainOf Fast = Fast`,
move the clock properties from `KnownConf` to the associated types of
`KnownClockDomain Fast`, and add `instance KnownDomain Fast`.

# CHANGED
`vPeriod` now holds a `Period` in picoseconds, including any fractional part.
Use `hzToPeriod @Period freq` for an exact period or
`floor (hzToPeriod @Period freq)` for the former whole-picosecond result.
`clockPeriod @dom` gives the whole picoseconds. Use
`vPeriod (knownVDomain @dom)` for the complete period.

# CHANGED
In `clash-manifest.json`, use a clock port's `domain` field to look up its
physical clock in `domains`. Keys include the qualified type identity.
Whole periods are JSON numbers. For a fractional period, divide the object's
`numerator` by its `denominator` to obtain picoseconds.

In blackbox templates, `~TAG[n]` expands to the qualified manifest key for
the physical clock domain of argument `n`. The key can contain punctuation
and spaces. For a local HDL identifier, use `~GENSYM[monitor][0]` at its
declaration and `~SYM[0]` at each reference.

# CHANGED
Signals and bidirectional pins can carry data without a clock configuration. When
Clash can resolve their physical clock, it records that clock in the manifest.
Clock ports require a resolvable configuration.
