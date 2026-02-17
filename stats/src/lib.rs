pub fn median(data: &[f64]) -> f64 {
    let mut sorted = data.to_vec();
    sorted.sort_by(|a, b| a.partial_cmp(b).unwrap());
    let n = sorted.len();
    if n % 2 == 1 {
        sorted[n / 2]
    } else {
        (sorted[n / 2 - 1] + sorted[n / 2]) / 2.0
    }
}

/// Median absolute deviation: median of |x_i - median(x)|.
/// Robust measure of spread with 50% breakdown point.
pub fn mad(data: &[f64]) -> f64 {
    let med = median(data);
    let deviations: Vec<f64> = data.iter().map(|x| (x - med).abs()).collect();
    median(&deviations)
}

/// Minimum number of benchmarks needed for reliable session bias estimation.
/// Below this, the median is too volatile to use as a bias estimator.
const MIN_BENCHMARKS_FOR_BIAS: usize = 3;

/// Estimate environmental noise floor from suite-level adjusted changes.
/// Uses MAD * 1.4826 (consistency factor for normal distributions) as the
/// sigma estimate. Returns max(estimate, min_floor). With fewer than 3
/// benchmarks, returns min_floor directly.
pub fn noise_floor(adjusted_changes: &[f64], min_floor: f64) -> f64 {
    if adjusted_changes.len() < MIN_BENCHMARKS_FOR_BIAS {
        return min_floor;
    }
    let sigma_env = 1.4826 * mad(adjusted_changes);
    sigma_env.max(min_floor)
}

/// Welch's t-test with environmental noise floor (GUM quadrature).
/// noise_floor_frac is the environmental sigma as a fraction of the baseline
/// mean. The environmental SE is added in quadrature with the sampling SE,
/// inflating the denominator to absorb between-run variance.
pub fn noise_aware_welch_t_test(
    baseline: &[f64],
    candidate: &[f64],
    noise_floor_frac: f64,
) -> f64 {
    let n1 = baseline.len() as f64;
    let n2 = candidate.len() as f64;
    let v1 = variance(baseline);
    let v2 = variance(candidate);
    let m1 = mean(baseline);
    let m2 = mean(candidate);

    if v1 == 0.0 && v2 == 0.0 && noise_floor_frac == 0.0 {
        return 1.0;
    }

    let se_sampling_sq = v1 / n1 + v2 / n2;
    let se_env = noise_floor_frac * m1;
    let se_total = (se_sampling_sq + se_env * se_env).sqrt();

    let t = (m2 - m1) / se_total;

    // Welch-Satterthwaite df (from sampling variance only)
    let num = (v1 / n1 + v2 / n2).powi(2);
    let den = (v1 / n1).powi(2) / (n1 - 1.0) + (v2 / n2).powi(2) / (n2 - 1.0);
    let df = if den == 0.0 { 2.0 } else { (num / den).max(2.0) };

    2.0 * student_t_cdf(-t.abs(), df)
}

/// Subtract estimated session-level bias from per-benchmark change percentages.
/// Returns (adjusted_changes, estimated_drift). If fewer than 3 benchmarks,
/// returns the original changes unchanged with drift = 0.
pub fn session_bias_adjust(change_pcts: &[f64]) -> (Vec<f64>, f64) {
    if change_pcts.len() < MIN_BENCHMARKS_FOR_BIAS {
        return (change_pcts.to_vec(), 0.0);
    }
    let drift = median(change_pcts);
    let adjusted = change_pcts.iter().map(|c| c - drift).collect();
    (adjusted, drift)
}

pub fn mean(data: &[f64]) -> f64 {
    data.iter().sum::<f64>() / data.len() as f64
}

pub fn variance(data: &[f64]) -> f64 {
    let m = mean(data);
    data.iter().map(|x| (x - m).powi(2)).sum::<f64>() / (data.len() - 1) as f64
}

/// Coefficient of variation: std_dev / mean.
pub fn cv(data: &[f64]) -> f64 {
    variance(data).sqrt() / mean(data)
}

/// Trim the bottom and top `fraction` of values from a sorted copy of data.
/// Returns the middle portion. Used for robust statistics (Yuen's test).
pub fn trim(data: &[f64], fraction: f64) -> Vec<f64> {
    let mut sorted = data.to_vec();
    sorted.sort_by(|a, b| a.partial_cmp(b).unwrap());
    let k = (sorted.len() as f64 * fraction).floor() as usize;
    sorted[k..sorted.len() - k].to_vec()
}

/// Trimmed mean: sort data, discard the bottom and top `fraction` of values,
/// return the mean of the remaining middle portion. Robust to outliers.
pub fn trimmed_mean(data: &[f64], fraction: f64) -> f64 {
    mean(&trim(data, fraction))
}

/// Welch's t-test for two independent samples with unequal variance.
/// Returns two-tailed p-value.
pub fn welch_t_test(baseline: &[f64], candidate: &[f64]) -> f64 {
    let n1 = baseline.len() as f64;
    let n2 = candidate.len() as f64;
    let v1 = variance(baseline);
    let v2 = variance(candidate);

    // Identical samples (zero variance)
    if v1 == 0.0 && v2 == 0.0 {
        return 1.0;
    }

    let se = (v1 / n1 + v2 / n2).sqrt();
    let t = (mean(candidate) - mean(baseline)) / se;

    // Welch-Satterthwaite degrees of freedom
    let num = (v1 / n1 + v2 / n2).powi(2);
    let den = (v1 / n1).powi(2) / (n1 - 1.0) + (v2 / n2).powi(2) / (n2 - 1.0);
    let df = num / den;

    // Two-tailed p-value
    2.0 * student_t_cdf(-t.abs(), df)
}

/// CDF of Student's t-distribution.
/// P(T <= t) for t < 0 using the regularized incomplete beta function.
fn student_t_cdf(t: f64, df: f64) -> f64 {
    let x = df / (df + t * t);
    0.5 * regularized_beta(x, df / 2.0, 0.5)
}

/// Regularized incomplete beta function I_x(a, b).
fn regularized_beta(x: f64, a: f64, b: f64) -> f64 {
    if x <= 0.0 {
        return 0.0;
    }
    if x >= 1.0 {
        return 1.0;
    }

    let ln_beta = ln_gamma(a) + ln_gamma(b) - ln_gamma(a + b);
    let ln_prefix = a * x.ln() + b * (1.0 - x).ln() - ln_beta;
    let prefix = ln_prefix.exp();

    // Use symmetry relation for numerical stability:
    // I_x(a,b) = 1 - I_{1-x}(b,a)
    if x < (a + 1.0) / (a + b + 2.0) {
        prefix / a * beta_cf(x, a, b)
    } else {
        1.0 - {
            let ln_prefix2 = b * (1.0 - x).ln() + a * x.ln() - ln_beta;
            ln_prefix2.exp() / b * beta_cf(1.0 - x, b, a)
        }
    }
}

/// Continued fraction for the incomplete beta function (Lentz's algorithm).
fn beta_cf(x: f64, a: f64, b: f64) -> f64 {
    const MAX_ITER: usize = 200;
    const EPS: f64 = 1e-15;
    const TINY: f64 = 1e-30;

    let mut c = 1.0_f64;

    // First term (m=0): coefficient is 1
    let mut d = 1.0 - (a + b) * x / (a + 1.0);
    if d.abs() < TINY {
        d = TINY;
    }
    d = 1.0 / d;
    let mut f = d;

    for m in 1..=MAX_ITER {
        let m = m as f64;

        // Even step: d_{2m}
        let num_even = m * (b - m) * x / ((a + 2.0 * m - 1.0) * (a + 2.0 * m));
        d = 1.0 + num_even * d;
        if d.abs() < TINY {
            d = TINY;
        }
        c = 1.0 + num_even / c;
        if c.abs() < TINY {
            c = TINY;
        }
        d = 1.0 / d;
        f *= c * d;

        // Odd step: d_{2m+1}
        let num_odd =
            -(a + m) * (a + b + m) * x / ((a + 2.0 * m) * (a + 2.0 * m + 1.0));
        d = 1.0 + num_odd * d;
        if d.abs() < TINY {
            d = TINY;
        }
        c = 1.0 + num_odd / c;
        if c.abs() < TINY {
            c = TINY;
        }
        d = 1.0 / d;
        let delta = c * d;
        f *= delta;

        if (delta - 1.0).abs() < EPS {
            break;
        }
    }

    f
}

/// Lanczos approximation for ln(Gamma(x)).
fn ln_gamma(x: f64) -> f64 {
    const COEFFS: [f64; 9] = [
        0.99999999999980993,
        676.5203681218851,
        -1259.1392167224028,
        771.32342877765313,
        -176.61502916214059,
        12.507343278686905,
        -0.13857109526572012,
        9.9843695780195716e-6,
        1.5056327351493116e-7,
    ];

    if x < 0.5 {
        // Reflection formula
        let s = std::f64::consts::PI / (std::f64::consts::PI * x).sin();
        return s.ln() - ln_gamma(1.0 - x);
    }

    let x = x - 1.0;
    let mut a = COEFFS[0];
    for (i, &c) in COEFFS[1..].iter().enumerate() {
        a += c / (x + 1.0 + i as f64);
    }

    let t = x + 7.5; // x + g + 0.5 where g = 7
    0.5 * (2.0 * std::f64::consts::PI).ln() + (x + 0.5) * t.ln() - t + a.ln()
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Create synthetic samples with deterministic pseudo-noise.
    fn synthetic_samples(mean_ns: f64, noise: f64, n: usize) -> Vec<f64> {
        let mut rng = 12345_u64;
        (0..n)
            .map(|_| {
                rng = rng.wrapping_mul(6364136223846793005).wrapping_add(1);
                let uniform = (rng >> 33) as f64 / (1u64 << 31) as f64;
                mean_ns + (uniform - 0.5) * 2.0 * noise
            })
            .collect()
    }

    #[test]
    fn identical_samples_p_is_one() {
        let data = synthetic_samples(100.0, 0.0, 30);
        let p = welch_t_test(&data, &data);
        assert_eq!(p, 1.0, "identical samples should have p=1.0");
    }

    #[test]
    fn clear_regression_detected() {
        let baseline = synthetic_samples(100.0, 1.0, 30);
        let candidate = synthetic_samples(110.0, 1.0, 30);
        let p = welch_t_test(&baseline, &candidate);
        assert!(p < 0.001, "10% regression should have p << 0.01, got {p}");
    }

    #[test]
    fn improvement_has_low_p() {
        let baseline = synthetic_samples(110.0, 1.0, 30);
        let candidate = synthetic_samples(100.0, 1.0, 30);
        let p = welch_t_test(&baseline, &candidate);
        assert!(p < 0.001, "10% improvement should be significant, got {p}");
    }

    #[test]
    fn within_noise_has_high_p() {
        let baseline = synthetic_samples(100.0, 50.0, 30);
        let candidate = synthetic_samples(101.0, 50.0, 30);
        let p = welch_t_test(&baseline, &candidate);
        assert!(
            p > 0.01,
            "1% change with high noise should not be significant, got {p}"
        );
    }

    #[test]
    fn moderate_regression_near_threshold() {
        let baseline = synthetic_samples(100.0, 2.0, 50);
        let candidate = synthetic_samples(103.0, 2.0, 50);
        let change_pct = (mean(&candidate) - mean(&baseline)) / mean(&baseline);
        let p = welch_t_test(&baseline, &candidate);
        assert!(change_pct > 0.02, "change should exceed threshold: {change_pct}");
        assert!(
            p < 0.01,
            "3% regression with tight variance should be significant, got {p}"
        );
    }

    #[test]
    fn moderate_regression_high_noise_passes() {
        let baseline = synthetic_samples(100.0, 50.0, 30);
        let candidate = synthetic_samples(103.0, 50.0, 30);
        let p = welch_t_test(&baseline, &candidate);
        assert!(
            p > 0.01,
            "3% regression with wide variance should be within noise, got {p}"
        );
    }

    #[test]
    fn p_value_is_always_valid() {
        for &(mean_a, mean_b, noise) in &[
            (100.0, 100.0, 1.0),
            (100.0, 200.0, 5.0),
            (100.0, 101.0, 100.0),
            (100.0, 115.0, 30.0),
            (1000.0, 1050.0, 10.0),
        ] {
            let a = synthetic_samples(mean_a, noise, 30);
            let b = synthetic_samples(mean_b, noise, 30);
            let p = welch_t_test(&a, &b);
            assert!(
                (0.0..=1.0).contains(&p),
                "p-value out of range: {p} for means ({mean_a}, {mean_b}) noise {noise}"
            );
        }
    }

    #[test]
    fn ln_gamma_matches_known_values() {
        assert!((ln_gamma(1.0)).abs() < 1e-10);
        assert!((ln_gamma(2.0)).abs() < 1e-10);
        assert!((ln_gamma(5.0) - 24.0_f64.ln()).abs() < 1e-10);
        assert!((ln_gamma(0.5) - 0.5 * std::f64::consts::PI.ln()).abs() < 1e-10);
    }

    #[test]
    fn outliers_do_not_skew_comparison() {
        // Baseline: 50 samples, mostly ~134ns but with fast outliers pulling mean to ~132.
        // Comparison: 5 samples at ~136ns (normal for this benchmark).
        // Without trimming, change_pct = (136-132)/132 = 3.0% → false positive.
        // With trimming, both means are ~134ns → change_pct ~1.5% → clean.
        let mut baseline = vec![134.0; 45];
        baseline.extend_from_slice(&[123.0, 124.0, 125.0, 126.0, 127.0]); // fast outliers
        let comparison = vec![136.0; 5];

        let base_mean = trimmed_mean(&baseline, 0.10);
        let comp_mean = trimmed_mean(&comparison, 0.10);
        let change_pct = (comp_mean - base_mean) / base_mean;

        assert!(
            change_pct < 0.02,
            "trimmed comparison should be below 2% threshold, got {:.1}%",
            change_pct * 100.0,
        );
    }

    #[test]
    fn trimmed_mean_removes_extremes() {
        let data = vec![1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 100.0];
        // 10% trim: remove 1 from each end → [2,3,4,5,6,7,8,9] → mean = 5.5
        let tm = trimmed_mean(&data, 0.10);
        assert!((tm - 5.5).abs() < 1e-10, "expected 5.5, got {tm}");
    }

    #[test]
    fn trimmed_mean_no_trim_on_small_samples() {
        // 5 samples with 10% trim: floor(5*0.10) = 0, no trimming
        let data = vec![1.0, 2.0, 3.0, 4.0, 5.0];
        let tm = trimmed_mean(&data, 0.10);
        assert!((tm - 3.0).abs() < 1e-10, "expected 3.0, got {tm}");
    }

    #[test]
    fn cv_of_constant_data_is_zero() {
        let data = vec![100.0; 10];
        assert_eq!(cv(&data), 0.0);
    }

    #[test]
    fn session_bias_removes_uniform_drift() {
        // All benchmarks shifted +3% due to thermal throttling.
        // After adjustment, all should be ~0%.
        let changes = vec![0.03, 0.031, 0.029, 0.032, 0.028, 0.03, 0.031, 0.029, 0.03];
        let (adjusted, drift) = session_bias_adjust(&changes);
        assert!(
            (drift - 0.03).abs() < 0.002,
            "drift should be ~3%, got {:.1}%",
            drift * 100.0
        );
        for (i, &a) in adjusted.iter().enumerate() {
            assert!(
                a.abs() < 0.005,
                "bench {i} adjusted change should be ~0%, got {:.1}%",
                a * 100.0
            );
        }
    }

    #[test]
    fn session_bias_preserves_real_regression() {
        // 8 benchmarks unaffected (+1% session noise), 1 genuinely regressed +5%.
        // After adjustment, the regressed one should still show ~+4%.
        let changes = vec![0.01, 0.012, 0.008, 0.011, 0.009, 0.01, 0.011, 0.009, 0.05];
        let (adjusted, drift) = session_bias_adjust(&changes);
        assert!(
            (drift - 0.01).abs() < 0.002,
            "drift should be ~1%, got {:.1}%",
            drift * 100.0
        );
        // The regressed benchmark (last) should show ~4% after adjustment
        let regressed = *adjusted.last().unwrap();
        assert!(
            (regressed - 0.04).abs() < 0.005,
            "regressed bench should be ~4%, got {:.1}%",
            regressed * 100.0
        );
    }

    #[test]
    fn session_bias_too_few_benchmarks() {
        // With only 2 benchmarks, can't reliably estimate session bias.
        let changes = vec![0.03, 0.05];
        let (adjusted, drift) = session_bias_adjust(&changes);
        assert_eq!(drift, 0.0, "drift should be 0 with too few benchmarks");
        assert_eq!(adjusted, changes, "should return unadjusted with too few");
    }

    #[test]
    fn median_odd_count() {
        assert_eq!(median(&[3.0, 1.0, 2.0]), 2.0);
    }

    #[test]
    fn median_even_count() {
        assert_eq!(median(&[4.0, 1.0, 3.0, 2.0]), 2.5);
    }

    #[test]
    fn median_single_element() {
        assert_eq!(median(&[42.0]), 42.0);
    }

    #[test]
    fn mad_symmetric_data() {
        // [1, 2, 3, 4, 5] → median=3, deviations=[2,1,0,1,2] → MAD=1
        assert!((mad(&[1.0, 2.0, 3.0, 4.0, 5.0]) - 1.0).abs() < 1e-10);
    }

    #[test]
    fn mad_with_outlier() {
        // [1, 2, 3, 4, 100] → median=3, deviations=[2,1,0,1,97] → MAD=1
        // MAD is robust to the outlier
        assert!((mad(&[1.0, 2.0, 3.0, 4.0, 100.0]) - 1.0).abs() < 1e-10);
    }

    #[test]
    fn mad_constant_data() {
        assert_eq!(mad(&[5.0, 5.0, 5.0, 5.0, 5.0]), 0.0);
    }

    #[test]
    fn noise_floor_from_adjusted_changes() {
        // 8 benches with ~1% noise, 1 outlier at 10%
        // adjusted = [-0.005, 0.003, -0.008, 0.005, -0.002, 0.006, -0.004, 0.003, 0.10]
        // |adjusted| = [0.005, 0.003, 0.008, 0.005, 0.002, 0.006, 0.004, 0.003, 0.10]
        // median(|adj|) = 0.005 → sigma_env = 1.4826 * 0.005 = 0.007413
        let adj = vec![-0.005, 0.003, -0.008, 0.005, -0.002, 0.006, -0.004, 0.003, 0.10];
        let floor = noise_floor(&adj, 0.01);
        // MAD = 0.005, 1.4826 * 0.005 = 0.007413, but min is 0.01
        assert!(
            (floor - 0.01).abs() < 1e-10,
            "should be clamped to min 0.01, got {floor}"
        );
    }

    #[test]
    fn noise_floor_high_spread() {
        // All benchmarks scatter widely: adjusted = [-0.04, 0.05, -0.03, 0.02, -0.05, 0.04, -0.02, 0.03, 0.06]
        // |adj| = [0.04, 0.05, 0.03, 0.02, 0.05, 0.04, 0.02, 0.03, 0.06]
        // sorted |adj| = [0.02, 0.02, 0.03, 0.03, 0.04, 0.04, 0.05, 0.05, 0.06]
        // median(|adj|) = 0.04 → sigma_env = 1.4826 * 0.04 = 0.059304
        let adj = vec![-0.04, 0.05, -0.03, 0.02, -0.05, 0.04, -0.02, 0.03, 0.06];
        let floor = noise_floor(&adj, 0.01);
        assert!(
            (floor - 0.059304).abs() < 0.001,
            "expected ~5.9%, got {:.1}%",
            floor * 100.0
        );
    }

    #[test]
    fn noise_floor_too_few() {
        let adj = vec![0.03, 0.05];
        let floor = noise_floor(&adj, 0.01);
        assert_eq!(floor, 0.01, "should return min_floor with too few benchmarks");
    }

    #[test]
    fn noise_aware_t_test_absorbs_environmental_shift() {
        // Baseline: 50 samples at mean=100, sigma=0.5 (CV=0.5%)
        // Candidate: 5 samples at mean=105 (5% environmental shift, not real)
        // Standard t-test would give p ≈ 0 (false positive)
        // With noise_floor=0.03 (3%), SE_env = 3.0, SE_total ≈ 3.0
        // t ≈ 5.0/3.0 ≈ 1.67, p ≈ 0.12 → correctly NOT significant at 0.05
        let baseline = synthetic_samples(100.0, 0.5, 50);
        let candidate = synthetic_samples(105.0, 0.5, 5);

        let p_standard = welch_t_test(&baseline, &candidate);
        let p_noise_aware = noise_aware_welch_t_test(&baseline, &candidate, 0.03);

        assert!(
            p_standard < 0.001,
            "standard t-test should flag this, got p={p_standard}"
        );
        assert!(
            p_noise_aware > 0.05,
            "noise-aware test should absorb 5% shift with 3% floor, got p={p_noise_aware}"
        );
    }

    #[test]
    fn noise_aware_t_test_detects_large_regression() {
        // 10% regression with 3% noise floor → should still detect
        let baseline = synthetic_samples(100.0, 0.5, 50);
        let candidate = synthetic_samples(110.0, 0.5, 5);

        let p = noise_aware_welch_t_test(&baseline, &candidate, 0.03);
        assert!(
            p < 0.05,
            "10% regression should be detected even with 3% floor, got p={p}"
        );
    }

    #[test]
    fn noise_aware_t_test_zero_floor_equals_standard() {
        let baseline = synthetic_samples(100.0, 1.0, 30);
        let candidate = synthetic_samples(103.0, 1.0, 30);

        let p_standard = welch_t_test(&baseline, &candidate);
        let p_noise_aware = noise_aware_welch_t_test(&baseline, &candidate, 0.0);

        assert!(
            (p_standard - p_noise_aware).abs() < 1e-10,
            "zero floor should equal standard: {p_standard} vs {p_noise_aware}"
        );
    }

    #[test]
    fn cv_of_known_distribution() {
        // data: [90, 100, 110] -> mean=100, std_dev=10, cv=0.1
        let data = vec![90.0, 100.0, 110.0];
        let result = cv(&data);
        assert!((result - 0.1).abs() < 1e-10, "expected cv ~0.1, got {result}");
    }
}
