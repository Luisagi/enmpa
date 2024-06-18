# enmpa 0.1.8
================================================================================

- **New Classes**:
  - Added two new classes: `enmpa_calibration` and `enmpa_fitted_models`.
    - These classes help manage the list outputs from the functions `calibration_glm` and `fit_selected`.
    - Each class has two associated methods: `summary()` and `print()`, which provide summaries and print representations of the objects, respectively.

- **Updates to `predict_glm`**:
  - Added a new flag `extrapolation_type` to indicate the type of extrapolation:
    - `"E"`: Free extrapolation
    - `"NE"`: No extrapolation
    - `"EC"`: Extrapolation with clamping
  - The flag `var_to_clamp` was replaced by `restricted_vars`.
  - The flag `clamping` was removed.

- **Updates to `model_validation`**:
  - Now includes 'residual deviance' as a validation metric.

# enmpa 0.1.5
================================================================================

- Initial CRAN submission.
