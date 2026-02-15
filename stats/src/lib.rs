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
        // variance is 0, mean is 100, cv = 0/100 = NaN... but variance(constant) = 0
        // Actually with Bessel's correction, sum of squared diffs is 0, so variance = 0
        // cv = sqrt(0) / 100 = 0
        assert_eq!(cv(&data), 0.0);
    }

    #[test]
    fn cv_of_known_distribution() {
        // data: [90, 100, 110] -> mean=100, std_dev=10, cv=0.1
        let data = vec![90.0, 100.0, 110.0];
        let result = cv(&data);
        assert!((result - 0.1).abs() < 1e-10, "expected cv ~0.1, got {result}");
    }
}
