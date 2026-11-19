---
issues: []
prs: [3163]

---

# Major Domain overhaul
Clash's `Domain`-system has had a major overhaul. The most important changes are:
- Domains are not longer `Symbol`s. Instead, they can be any type.
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
Instead, a `KnownDomain` instance has a type `ClockDomainOf`,
which must have a `KnownClockDomain` instance.
By default, this is the same as the domain.

# CHANGED
Clock periods are now of the fractional type `Period`, rather than an integer number of picoseconds.

# CHANGED
`VDomainConfiguration` now uses stable names for the `vName` field. These are also used
in the manifest.

# CHANGED
It was previously possible to create signals with "fake" domains,
as long as the domain details were never needed (i.e. there were no clocks etc. for the domain).
This is no longer the case.
