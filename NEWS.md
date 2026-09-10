# stratifyR 2.0-1  (2026-06-14)

## New features

* `strata.data()` and `strata.distr()` gain a `method` argument supporting
  three optimisation strategies:
  - `"dp"` (default) — the original exhaustive Dynamic Programming grid search,
    guaranteed globally optimal on the discrete grid.
  - `"cobyla"` — multi-start gradient-free local optimiser (COBYLA) via
    **nloptr**; faster than DP for large `h`, suitable for exploratory work.
  - `"global"` — two-phase global optimiser combining DIRECT-L (deterministic
    space-filling search) followed by COBYLA local refinement; closest to DP
    accuracy among the two new methods.
* New arguments `n_starts`, `max_iter`, `tol`, and `verbose` in both functions
  to control COBYLA/GLOBAL behaviour.
* `DESCRIPTION` updated: **nloptr** added to Imports.

## Bug fixes

* **Weibull and Gamma recurrences (critical):** `distr.root()` and `data.root()`
  were using the *unregularized* upper incomplete gamma function `UGamma(s, z)`
  (via **zipfR**) where the *regularized* form `Q(s, z)` was required by the
  mathematical formulation. Fixed by replacing all calls with
  `pgamma(z, shape = s, lower.tail = FALSE)`, which correctly implements
  `Q(s, z) = Γ(s, z) / Γ(s)` as defined in Abramowitz & Stegun (1972),
  Chapter 6, equation 6.5.2. Results for Weibull and Gamma populations are now
  consistent with the published theory.
* **Triangular fitting in `.refit_real_scale()`:** simultaneous MLE of all three
  triangular parameters (min, max, mode) produced degenerate estimates
  (min ≈ max ≈ mode). Fixed by fixing min and max to sample bounds and
  estimating only the mode via MLE, matching the approach already used in
  `get.dist()`.
* **`get.dist()`:** triangular distribution now only selected over a tail model
  (Gamma, Weibull, Log-normal, Exponential) when its AIC advantage exceeds 10
  units, preventing spurious triangular wins on mildly right-skewed data.

## Vignette

* New Section 6: *Alternative Optimisation Methods: COBYLA and GLOBAL* —
  describes the motivation, algorithm, advantages, and limitations of each
  method, with a worked comparison example using the Anaemia/Iron data.
* New Section 8: *Summary of Method Comparison Across All Supported
  Distributions* — presents a table of DP vs COBYLA vs GLOBAL results across
  all ten distributions with explanation of observed patterns.
* Corrected description of `Q(r, y)` (regularized) vs `Γ(r, y)` (unregularized)
  in the Weibull and Gamma sections, with explicit R implementation note.
* Fixed incorrect Normal distribution example (wrong `mean`/`sd` parameters).
* Updated triangular parameter estimation example to use the fixed `fix.arg`
  approach.
* Minor fixes: removed duplicate `NeedsCompilation` field, removed stray
  `\url{}` in Introduction.

## Backward compatibility

* All `strata.data()` / `strata.distr()` call signatures from v1.0-5 work
  without modification (`method` defaults to `"dp"`).
* The `"strata"` S3 class and all slot names are unchanged.

---

# stratifyR 1.0-5  (2026-06-04)

Initial CRAN-submission-ready version. Implemented DP solver, 10-family
distribution fitting via AIC, Neyman allocation with cost support, and
coloured `summary.strata()` output.
