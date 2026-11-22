#include <Rcpp.h>
#include "lta/lta.hpp"

#include <algorithm>
#include <chrono>
#include <cmath>
#include <cstdint>
#include <string>

using namespace Rcpp;

namespace {

int measurement_from(const std::string& name) {
    if (name == "categorical" || name == "lca") return lta::MEAS_CATEGORICAL;
    if (name == "gaussian" || name == "continuous" || name == "lpa") return lta::MEAS_GAUSSIAN;
    if (name == "mixed") return lta::MEAS_MIXED;
    Rcpp::stop("measurement must be 'categorical', 'gaussian', or 'mixed'");
}

lta::Panel panel_from(const List& d) {
    lta::Panel p;
    p.N = as<int>(d["N"]);
    p.T = as<int>(d["T"]);
    p.J = as<int>(d["J"]);
    p.measurement = measurement_from(as<std::string>(d["measurement"]));
    if (d.containsElementNamed("family") && !Rf_isNull(d["family"])) {
        CharacterVector fam = d["family"];
        for (R_xlen_t i = 0; i < fam.size(); ++i)
            p.family.push_back(lta::parse_family_name(as<std::string>(fam[i])));
    }
    if (d.containsElementNamed("ncat") && !Rf_isNull(d["ncat"])) p.ncat = as<std::vector<int>>(d["ncat"]);
    p.Rmax = d.containsElementNamed("Rmax") ? as<int>(d["Rmax"]) : 1;
    if (d.containsElementNamed("cat") && !Rf_isNull(d["cat"])) p.cat = as<std::vector<int>>(d["cat"]);
    if (d.containsElementNamed("gauss") && !Rf_isNull(d["gauss"])) p.gauss = as<std::vector<double>>(d["gauss"]);
    return p;
}

lta::Spec spec_from(const List& d) {
    lta::Spec s;
    s.K = as<int>(d["K"]);
    if (d.containsElementNamed("covariance") && !Rf_isNull(d["covariance"]))
        s.cov_model = lta::parse_covariance_name(as<std::string>(d["covariance"]));
    if (d.containsElementNamed("measurement_invariant")) s.measurement_invariant = as<bool>(d["measurement_invariant"]);
    if (d.containsElementNamed("means_invariant")) s.means_invariant = as<bool>(d["means_invariant"]);
    if (d.containsElementNamed("covariance_invariant")) s.covariance_invariant = as<bool>(d["covariance_invariant"]);
    if (d.containsElementNamed("transition_equal")) s.transition_equal = as<bool>(d["transition_equal"]);
    if (d.containsElementNamed("n_starts")) s.n_starts = as<int>(d["n_starts"]);
    if (d.containsElementNamed("screen_iter")) s.screen_iter = as<int>(d["screen_iter"]);
    if (d.containsElementNamed("n_full")) s.n_full = as<int>(d["n_full"]);
    if (d.containsElementNamed("max_iter")) s.max_iter = as<int>(d["max_iter"]);
    if (d.containsElementNamed("tol")) s.tol = as<double>(d["tol"]);
    if (d.containsElementNamed("replicate_tol")) s.replicate_tol = as<double>(d["replicate_tol"]);
    if (d.containsElementNamed("n_threads")) s.n_threads = as<int>(d["n_threads"]);
    if (d.containsElementNamed("seed")) s.seed = static_cast<std::uint64_t>(as<double>(d["seed"]));
    if (d.containsElementNamed("align_labels")) s.align_labels = as<bool>(d["align_labels"]);
    const bool has_start = d.containsElementNamed("has_start") && as<bool>(d["has_start"]);
    const bool append_start = d.containsElementNamed("append_supplied_start") && as<bool>(d["append_supplied_start"]);
    if ((has_start || append_start) && d.containsElementNamed("start") && !Rf_isNull(d["start"])) {
        s.has_start = has_start;
        s.append_supplied_start = append_start && !has_start;
        List st = d["start"];
        if (st.containsElementNamed("delta") && !Rf_isNull(st["delta"]))
            s.start.delta = as<std::vector<double>>(st["delta"]);
        if (st.containsElementNamed("tau") && !Rf_isNull(st["tau"]))
            s.start.tau = as<std::vector<double>>(st["tau"]);
        if (st.containsElementNamed("rho") && !Rf_isNull(st["rho"]))
            s.start.rho = as<std::vector<double>>(st["rho"]);
        if (st.containsElementNamed("means") && !Rf_isNull(st["means"]))
            s.start.means = as<std::vector<double>>(st["means"]);
        if (st.containsElementNamed("cov") && !Rf_isNull(st["cov"]))
            s.start.cov = as<std::vector<double>>(st["cov"]);
    }
    if (d.containsElementNamed("estep") && !Rf_isNull(d["estep"])) {
        const std::string estep = as<std::string>(d["estep"]);
        if (estep == "stored") s.estep = lta::ESTEP_STORED;
        else if (estep == "streaming") s.estep = lta::ESTEP_STREAMING;
        else throw std::invalid_argument("estep must be \"stored\" or \"streaming\"");
    }
    if (d.containsElementNamed("posteriors") && !Rf_isNull(d["posteriors"]))
        s.posteriors = as<bool>(d["posteriors"]);
    if (d.containsElementNamed("estep_threads") && !Rf_isNull(d["estep_threads"])) {
        const char* message = "estep_threads must be a finite whole number from 1 to 1024";
        SEXP raw = d["estep_threads"];
        if (Rf_isLogical(raw) || Rf_length(raw) != 1) throw std::invalid_argument(message);
        double threads = 0;
        if (Rf_isInteger(raw)) {
            const int value = INTEGER(raw)[0];
            if (value == NA_INTEGER) throw std::invalid_argument(message);
            threads = static_cast<double>(value);
        } else if (Rf_isReal(raw)) {
            threads = REAL(raw)[0];
            if (!std::isfinite(threads) || std::trunc(threads) != threads)
                throw std::invalid_argument(message);
        } else {
            throw std::invalid_argument(message);
        }
        if (threads < 1.0 || threads > static_cast<double>(lta::kMaxEstepThreads))
            throw std::invalid_argument(message);
        s.estep_threads = static_cast<int>(threads);
    }
    if (d.containsElementNamed("patterns") && !Rf_isNull(d["patterns"])) {
        SEXP raw = d["patterns"];
        if (!Rf_isString(raw) || Rf_length(raw) != 1)
            throw std::invalid_argument("patterns must be \"none\" or \"compress\"");
        const std::string patterns = as<std::string>(raw);
        if (patterns == "none") s.patterns = lta::PATTERNS_NONE;
        else if (patterns == "compress") s.patterns = lta::PATTERNS_COMPRESS;
        else throw std::invalid_argument("patterns must be \"none\" or \"compress\"");
    }
    if (d.containsElementNamed("covariates") && !Rf_isNull(d["covariates"])) {
        lta::CovariateSpec c;
        c.names = as<std::vector<std::string>>(d["covariates"]);
        c.on_initial = !c.names.empty();
        lta::check_covariate_spec(c);
    }
    return s;
}

List params_to(const lta::Params& p) {
    return List::create(_["delta"] = p.delta, _["tau"] = p.tau, _["rho"] = p.rho,
                        _["means"] = p.means, _["cov"] = p.cov);
}

List fit_to(const lta::FitResult& r) {
    std::string status = r.status == lta::STATUS_OK ? "ok" : r.status == lta::STATUS_WARNED ? "warned" : "unusable";
    CharacterVector family(r.family.size());
    for (size_t i = 0; i < r.family.size(); ++i) family[i] = lta::family_name(r.family[i]);
    List out = List::create(
        _["status"] = status, _["status_reason"] = r.status_reason, _["engine"] = r.engine,
        _["schema_version"] = r.schema_version, _["loglik"] = r.loglik, _["converged"] = r.converged,
        _["regularized"] = r.regularized, _["ic_supported"] = r.ic_supported, _["n_iter"] = r.n_iter,
        _["n_starts"] = r.n_starts, _["n_full"] = r.n_full, _["n_converged_starts"] = r.n_converged_starts,
        _["n_best"] = r.n_best, _["n_parameters"] = r.n_parameters, _["n_subjects"] = r.n_subjects,
        _["n_analyzed"] = r.n_analyzed, _["n_excluded"] = r.n_excluded, _["n_waves"] = r.n_waves,
        _["n_indicators"] = r.n_indicators, _["K"] = r.K, _["aic"] = r.aic, _["bic"] = r.bic,
        _["sabic"] = r.sabic, _["entropy_wave"] = r.entropy_wave, _["entropy_sequence"] = r.entropy_sequence,
        _["entropy_wave_raw"] = r.entropy_wave_raw, _["entropy_sequence_raw"] = r.entropy_sequence_raw,
        _["min_occupancy"] = r.min_occupancy, _["min_eigenvalue"] = r.min_eigenvalue,
        _["cov_positive_definite"] = r.cov_positive_definite,
        _["cov_ill_conditioned"] = r.cov_ill_conditioned,
        _["cov_collapsed_component"] = r.cov_collapsed_component,
        _["condition_number"] = r.condition_number,
        _["min_relative_variance"] = r.min_relative_variance,
        _["n_numerically_empty"] = r.n_numerically_empty,
        _["n_empty_state_waves"] = r.n_empty_state_waves,
        _["n_negligible_transition_rows"] = r.n_negligible_transition_rows,
        _["eligible_model_comparison"] = r.eligible_model_comparison,
        _["eligible_assignment"] = r.eligible_assignment,
        _["eligible_measurement"] = r.eligible_measurement,
        _["eligible_transitions"] = r.eligible_transitions,
        _["eligible_further_starts"] = r.eligible_further_starts,
        _["eligibility_detail"] = r.eligibility_detail,
        _["viterbi_logprob"] = r.viterbi_logprob,
        _["n_boundary"] = r.n_boundary, _["seed"] = static_cast<double>(r.seed),
        _["n_threads"] = r.n_threads, _["tol"] = r.tol, _["replicate_tol"] = r.replicate_tol,
        _["data_hash"] = r.data_hash, _["excluded_subjects"] = r.excluded_subjects, _["params"] = params_to(r.params),
        _["n_tau_blocks"] = r.n_tau_blocks, _["n_meas_blocks"] = r.n_meas_blocks,
        _["n_mean_blocks"] = r.n_mean_blocks, _["n_cov_blocks"] = r.n_cov_blocks, _["Rmax"] = r.Rmax,
        _["measurement"] = r.measurement, _["family"] = family, _["gaussian_index"] = r.gaussian_index,
        _["categorical_index"] = r.categorical_index, _["n_gaussian"] = r.n_gaussian,
        _["n_categorical"] = r.n_categorical, _["covariance"] = r.covariance_name,
        _["measurement_invariant"] = r.measurement_invariant, _["means_invariant"] = r.means_invariant,
        _["covariance_invariant"] = r.covariance_invariant, _["transition_equal"] = r.transition_equal,
        _["gamma"] = r.gamma, _["filtered"] = r.filtered, _["xi"] = r.xi, _["viterbi"] = r.viterbi,
        _["modal"] = r.modal, _["label_order"] = r.label_order,         _["prevalence_model"] = r.prevalence_model,
        _["prevalence_posterior"] = r.prevalence_posterior, _["prevalence_hard"] = r.prevalence_hard,
        _["expected_membership"] = r.expected_membership,
        _["expected_transitions"] = r.expected_transitions, _["start_id"] = r.start_id,
        _["start_phase"] = r.start_phase,
        _["start_loglik"] = r.start_loglik, _["start_iters"] = r.start_iters,
        _["start_converged"] = r.start_converged, _["start_selected"] = r.start_selected,
        _["start_admissible"] = r.start_admissible, _["admissible"] = r.admissible,
        _["rejected_start"] = r.rejected_start, _["rejected_loglik"] = r.rejected_loglik,
        _["rejected_n"] = r.rejected_n, _["rejected_level"] = r.rejected_level,
        _["rejected_reason"] = r.rejected_reason, _["rejected_delta"] = r.rejected_delta,
        _["rejected_means"] = r.rejected_means, _["rejected_cov"] = r.rejected_cov,
        _["rejected_class_eigen"] = r.rejected_class_eigen,
        _["rejected_min_class_n"] = r.rejected_min_class_n,
        _["trace_iter"] = r.trace_iter, _["trace_ll"] = r.trace_ll, _["has_draws"] = r.has_draws,
        _["n_draws"] = r.n_draws, _["draws"] = r.draws, _["parameter_uncertainty"] = r.parameter_uncertainty,
        _["state_uncertainty"] = r.state_uncertainty, _["interval_type"] = r.interval_type,
        _["miss_pattern"] = r.miss_pattern, _["miss_pattern_n"] = r.miss_pattern_n,
        _["miss_indicator_n"] = r.miss_indicator_n, _["miss_indicator_rate"] = r.miss_indicator_rate,
        _["miss_wave_n"] = r.miss_wave_n, _["miss_wave_rate"] = r.miss_wave_rate,
        _["n_monotone"] = r.n_monotone, _["n_intermittent"] = r.n_intermittent,
        _["n_complete_subjects"] = r.n_complete_subjects,
        _["hard_transitions"] = r.hard_transitions);
    if (r.estep == lta::ESTEP_STREAMING)
        out["posteriors_computed"] = r.posteriors_computed;
    if (r.patterns == lta::PATTERNS_COMPRESS) {
        out["patterns"] = "compress";
        out["n_patterns"] = r.n_patterns;
        out["pattern_id"] = r.pattern_id;
    }
    return out;
}

lta::Params params_from(const List& p) {
    lta::Params out;
    if (p.containsElementNamed("delta") && !Rf_isNull(p["delta"]))
        out.delta = as<std::vector<double>>(p["delta"]);
    if (p.containsElementNamed("tau") && !Rf_isNull(p["tau"])) out.tau = as<std::vector<double>>(p["tau"]);
    if (p.containsElementNamed("rho") && !Rf_isNull(p["rho"])) out.rho = as<std::vector<double>>(p["rho"]);
    if (p.containsElementNamed("means") && !Rf_isNull(p["means"]))
        out.means = as<std::vector<double>>(p["means"]);
    if (p.containsElementNamed("cov") && !Rf_isNull(p["cov"])) out.cov = as<std::vector<double>>(p["cov"]);
    return out;
}

template <typename T>
T field_or(const List& d, const char* name, T fallback) {
    if (!d.containsElementNamed(name) || Rf_isNull(d[name])) return fallback;
    return as<T>(d[name]);
}

lta::FitResult fit_from(const List& d) {
    lta::FitResult r;
    std::string st = field_or<std::string>(d, "status", "unusable");
    r.status = st == "ok" ? lta::STATUS_OK : st == "warned" ? lta::STATUS_WARNED : lta::STATUS_UNUSABLE;
    r.status_reason = field_or<std::string>(d, "status_reason", "");
    r.engine = field_or<std::string>(d, "engine", "cpp-em");
    r.loglik = field_or<double>(d, "loglik", 0);
    r.converged = field_or<bool>(d, "converged", false);
    r.regularized = field_or<bool>(d, "regularized", false);
    r.ic_supported = field_or<bool>(d, "ic_supported", false);
    if (d.containsElementNamed("posteriors_computed") && !Rf_isNull(d["posteriors_computed"])) {
        r.posteriors_computed = as<bool>(d["posteriors_computed"]);
        r.estep = lta::ESTEP_STREAMING;
    }
    r.n_iter = field_or<int>(d, "n_iter", 0);
    r.n_starts = field_or<int>(d, "n_starts", 0);
    r.n_full = field_or<int>(d, "n_full", 0);
    r.n_converged_starts = field_or<int>(d, "n_converged_starts", 0);
    r.n_best = field_or<int>(d, "n_best", 0);
    r.n_parameters = field_or<int>(d, "n_parameters", 0);
    r.n_subjects = field_or<int>(d, "n_subjects", 0);
    r.n_analyzed = field_or<int>(d, "n_analyzed", 0);
    r.n_excluded = field_or<int>(d, "n_excluded", 0);
    r.n_waves = field_or<int>(d, "n_waves", 0);
    r.n_indicators = field_or<int>(d, "n_indicators", 0);
    r.K = field_or<int>(d, "K", 0);
    r.aic = field_or<double>(d, "aic", 0);
    r.bic = field_or<double>(d, "bic", 0);
    r.sabic = field_or<double>(d, "sabic", 0);
    r.entropy_wave = field_or<double>(d, "entropy_wave", 0);
    r.entropy_sequence = field_or<double>(d, "entropy_sequence", 0);
    r.entropy_wave_raw = field_or<double>(d, "entropy_wave_raw", 0);
    r.entropy_sequence_raw = field_or<double>(d, "entropy_sequence_raw", 0);
    r.min_occupancy = field_or<double>(d, "min_occupancy", 0);
    r.min_eigenvalue = field_or<double>(d, "min_eigenvalue", 0);
    r.cov_positive_definite = field_or<bool>(d, "cov_positive_definite", true);
    r.cov_ill_conditioned = field_or<bool>(d, "cov_ill_conditioned", false);
    r.cov_collapsed_component = field_or<bool>(d, "cov_collapsed_component", false);
    r.n_boundary = field_or<int>(d, "n_boundary", 0);
    if (d.containsElementNamed("admissible") && !Rf_isNull(d["admissible"]))
        r.admissible = as<bool>(d["admissible"]);
    else
        r.admissible = r.converged && std::isfinite(r.loglik) && !r.regularized &&
                       r.cov_positive_definite && !r.cov_collapsed_component;
    r.data_hash = field_or<std::string>(d, "data_hash", "");
    r.n_tau_blocks = field_or<int>(d, "n_tau_blocks", 0);
    r.n_meas_blocks = field_or<int>(d, "n_meas_blocks", 0);
    r.n_mean_blocks = field_or<int>(d, "n_mean_blocks", 0);
    r.n_cov_blocks = field_or<int>(d, "n_cov_blocks", 0);
    r.Rmax = field_or<int>(d, "Rmax", 1);
    r.measurement = field_or<int>(d, "measurement", 0);
    r.n_gaussian = field_or<int>(d, "n_gaussian", 0);
    r.n_categorical = field_or<int>(d, "n_categorical", 0);
    r.measurement_invariant = field_or<bool>(d, "measurement_invariant", true);
    r.means_invariant = field_or<bool>(d, "means_invariant", true);
    r.covariance_invariant = field_or<bool>(d, "covariance_invariant", true);
    r.transition_equal = field_or<bool>(d, "transition_equal", true);
    r.covariance_name = field_or<std::string>(d, "covariance", "");
    if (!r.covariance_name.empty()) r.cov_model = lta::parse_covariance_name(r.covariance_name);
    if (d.containsElementNamed("family") && !Rf_isNull(d["family"])) {
        CharacterVector fam = d["family"];
        for (R_xlen_t i = 0; i < fam.size(); ++i)
            r.family.push_back(lta::parse_family_name(as<std::string>(fam[i])));
    }
    if (d.containsElementNamed("params") && !Rf_isNull(d["params"])) r.params = params_from(d["params"]);
    auto take_d = [&](const char* name, std::vector<double>& dest) {
        if (d.containsElementNamed(name) && !Rf_isNull(d[name])) dest = as<std::vector<double>>(d[name]);
    };
    auto take_i = [&](const char* name, std::vector<int>& dest) {
        if (d.containsElementNamed(name) && !Rf_isNull(d[name])) dest = as<std::vector<int>>(d[name]);
    };
    take_d("gamma", r.gamma);
    take_d("filtered", r.filtered);
    take_d("xi", r.xi);
    take_i("viterbi", r.viterbi);
    take_i("modal", r.modal);
    take_i("label_order", r.label_order);
    take_d("prevalence_model", r.prevalence_model);
    take_d("prevalence_posterior", r.prevalence_posterior);
    take_d("prevalence_hard", r.prevalence_hard);
    take_d("expected_membership", r.expected_membership);
    take_d("expected_transitions", r.expected_transitions);
    take_d("hard_transitions", r.hard_transitions);
    take_i("start_id", r.start_id);
    take_i("start_phase", r.start_phase);
    take_d("start_loglik", r.start_loglik);
    take_i("start_iters", r.start_iters);
    take_i("start_converged", r.start_converged);
    take_i("start_selected", r.start_selected);
    take_i("start_admissible", r.start_admissible);
    take_i("rejected_start", r.rejected_start);
    take_d("rejected_loglik", r.rejected_loglik);
    take_i("rejected_n", r.rejected_n);
    take_d("rejected_delta", r.rejected_delta);
    take_d("rejected_means", r.rejected_means);
    take_d("rejected_cov", r.rejected_cov);
    take_d("rejected_class_eigen", r.rejected_class_eigen);
    take_d("rejected_min_class_n", r.rejected_min_class_n);
    auto take_s = [&](const char* name, std::vector<std::string>& dest) {
        if (d.containsElementNamed(name) && !Rf_isNull(d[name]))
            dest = as<std::vector<std::string>>(d[name]);
    };
    take_s("rejected_level", r.rejected_level);
    take_s("rejected_reason", r.rejected_reason);
    take_i("trace_iter", r.trace_iter);
    take_d("trace_ll", r.trace_ll);
    take_i("gaussian_index", r.gaussian_index);
    take_i("categorical_index", r.categorical_index);
    take_i("excluded_subjects", r.excluded_subjects);
    if (d.containsElementNamed("schema_version") && !Rf_isNull(d["schema_version"]))
        r.schema_version = as<int>(d["schema_version"]);
    r.eligible_model_comparison = field_or<bool>(d, "eligible_model_comparison", false);
    r.eligible_assignment = field_or<bool>(d, "eligible_assignment", false);
    r.eligible_measurement = field_or<bool>(d, "eligible_measurement", false);
    r.eligible_transitions = field_or<bool>(d, "eligible_transitions", false);
    r.eligible_further_starts = field_or<bool>(d, "eligible_further_starts", true);
    r.eligibility_detail = field_or<std::string>(d, "eligibility_detail", "");
    take_d("viterbi_logprob", r.viterbi_logprob);
    if (d.containsElementNamed("seed") && !Rf_isNull(d["seed"]))
        r.seed = static_cast<std::uint64_t>(as<double>(d["seed"]));
    r.n_threads = field_or<int>(d, "n_threads", 1);
    r.tol = field_or<double>(d, "tol", 0);
    r.replicate_tol = field_or<double>(d, "replicate_tol", 0);
    r.condition_number = field_or<double>(d, "condition_number", 0);
    r.min_relative_variance = field_or<double>(d, "min_relative_variance", 1);
    r.n_numerically_empty = field_or<int>(d, "n_numerically_empty", 0);
    r.n_empty_state_waves = field_or<int>(d, "n_empty_state_waves", 0);
    r.n_negligible_transition_rows = field_or<int>(d, "n_negligible_transition_rows", 0);
    r.min_occupancy = field_or<double>(d, "min_occupancy", r.min_occupancy);
    take_d("miss_indicator_n", r.miss_indicator_n);
    take_d("miss_indicator_rate", r.miss_indicator_rate);
    take_d("miss_wave_n", r.miss_wave_n);
    take_d("miss_wave_rate", r.miss_wave_rate);
    if (d.containsElementNamed("miss_pattern") && !Rf_isNull(d["miss_pattern"]))
        r.miss_pattern = as<std::vector<std::string>>(d["miss_pattern"]);
    take_i("miss_pattern_n", r.miss_pattern_n);
    r.n_monotone = field_or<int>(d, "n_monotone", 0);
    r.n_intermittent = field_or<int>(d, "n_intermittent", 0);
    r.n_complete_subjects = field_or<int>(d, "n_complete_subjects", 0);
    if (d.containsElementNamed("has_draws") && !Rf_isNull(d["has_draws"]))
        r.has_draws = as<bool>(d["has_draws"]);
    r.n_draws = field_or<int>(d, "n_draws", 0);
    take_d("draws", r.draws);
    r.parameter_uncertainty = field_or<std::string>(d, "parameter_uncertainty", "none");
    r.state_uncertainty = field_or<std::string>(d, "state_uncertainty", "plugin_posterior");
    r.interval_type = field_or<std::string>(d, "interval_type", "none");
    return r;
}

List alignment_to(const lta::Alignment& a) {
    return List::create(_["permutation"] = a.permutation, _["cost"] = a.cost,
                        _["runner_up_cost"] = a.runner_up_cost, _["ambiguous"] = a.ambiguous,
                        _["distance"] = a.distance, _["note"] = a.note);
}

}  // namespace

// [[Rcpp::export]]
List cpp_build_info() {
    auto b = lta::build_info();
    return List::create(_["eigen_version"] = b.eigen_version, _["compiler"] = b.compiler,
                        _["flags"] = b.flags, _["cxx_standard"] = b.cxx_standard,
                        _["eigen_dont_parallelize"] = b.eigen_dont_parallelize,
                        _["external_blas"] = b.external_blas);
}

// [[Rcpp::export]]
List cpp_fit(List panel, List spec) {
    return fit_to(lta::fit(panel_from(panel), spec_from(spec), nullptr));
}

// [[Rcpp::export]]
bool cpp_logliks_replicate(double ll, double best, double replicate_tol) {
    return lta::logliks_replicate(ll, best, replicate_tol);
}

// [[Rcpp::export]]
double cpp_loglik(List panel, List spec, List params) {
    lta::Params p;
    p.delta = as<std::vector<double>>(params["delta"]);
    p.tau = as<std::vector<double>>(params["tau"]);
    if (params.containsElementNamed("rho") && !Rf_isNull(params["rho"]))
        p.rho = as<std::vector<double>>(params["rho"]);
    if (params.containsElementNamed("means") && !Rf_isNull(params["means"]))
        p.means = as<std::vector<double>>(params["means"]);
    if (params.containsElementNamed("cov") && !Rf_isNull(params["cov"]))
        p.cov = as<std::vector<double>>(params["cov"]);
    return lta::observed_loglik(panel_from(panel), spec_from(spec), p);
}

// [[Rcpp::export]]
List cpp_evaluate(List panel, List spec, List params) {
    lta::Params p;
    if (params.containsElementNamed("delta") && !Rf_isNull(params["delta"]))
        p.delta = as<std::vector<double>>(params["delta"]);
    if (params.containsElementNamed("tau") && !Rf_isNull(params["tau"]))
        p.tau = as<std::vector<double>>(params["tau"]);
    if (params.containsElementNamed("rho") && !Rf_isNull(params["rho"]))
        p.rho = as<std::vector<double>>(params["rho"]);
    if (params.containsElementNamed("means") && !Rf_isNull(params["means"]))
        p.means = as<std::vector<double>>(params["means"]);
    if (params.containsElementNamed("cov") && !Rf_isNull(params["cov"]))
        p.cov = as<std::vector<double>>(params["cov"]);
    return fit_to(lta::evaluate(panel_from(panel), spec_from(spec), p));
}

// [[Rcpp::export]]
std::string cpp_parse_family(std::string name) {
    return lta::family_name(lta::parse_family_name(name));
}

// [[Rcpp::export]]
std::string cpp_parse_covariance(std::string name) {
    return lta::covariance_name(lta::parse_covariance_name(name));
}

// [[Rcpp::export]]
void cpp_estimate_distal(List spec) {
    lta::DistalSpec s;
    s.name = as<std::string>(spec["name"]);
    std::string family = as<std::string>(spec["family"]);
    if (family == "continuous") s.family = lta::OUT_CONTINUOUS;
    else if (family == "binary") s.family = lta::OUT_BINARY;
    else if (family == "nominal") s.family = lta::OUT_NOMINAL;
    else if (family == "ordinal") s.family = lta::OUT_ORDINAL;
    else if (family == "count" || family == "poisson") s.family = lta::OUT_COUNT;
    else Rcpp::stop("unknown distal outcome family");
    std::string timing = spec.containsElementNamed("timing") && !Rf_isNull(spec["timing"])
                             ? as<std::string>(spec["timing"])
                             : "";
    if (timing == "initial") s.timing = lta::OUTCOME_INITIAL;
    else if (timing == "final") s.timing = lta::OUTCOME_FINAL;
    else if (timing == "wave") s.timing = lta::OUTCOME_WAVE;
    else if (timing == "transition") s.timing = lta::OUTCOME_TRANSITION;
    else s.timing = -1;
    if (spec.containsElementNamed("wave") && !Rf_isNull(spec["wave"])) s.wave = as<int>(spec["wave"]);
    if (spec.containsElementNamed("from_state") && !Rf_isNull(spec["from_state"]))
        s.from_state = as<int>(spec["from_state"]);
    if (spec.containsElementNamed("to_state") && !Rf_isNull(spec["to_state"]))
        s.to_state = as<int>(spec["to_state"]);
    if (spec.containsElementNamed("method") && !Rf_isNull(spec["method"]))
        s.method = as<std::string>(spec["method"]);
    lta::estimate_distal(s);
}

// [[Rcpp::export]]
List cpp_describe_indicators(List panel, bool by_wave) {
    auto rows = lta::describe_indicators(panel_from(panel), by_wave);
    IntegerVector variable, wave, category;
    NumericVector n, n_missing, mean, sd, median, q25, q75, minv, maxv, skew, kurtosis, proportion;
    CharacterVector kind;
    for (const auto& r : rows) {
        variable.push_back(r.variable);
        wave.push_back(r.wave);
        category.push_back(r.category);
        n.push_back(r.n);
        n_missing.push_back(r.n_missing);
        mean.push_back(r.mean);
        sd.push_back(r.sd);
        median.push_back(r.median);
        q25.push_back(r.q25);
        q75.push_back(r.q75);
        minv.push_back(r.min);
        maxv.push_back(r.max);
        skew.push_back(r.skew);
        kurtosis.push_back(r.kurtosis);
        proportion.push_back(r.proportion);
        kind.push_back(r.kind);
    }
    return List::create(_["variable"] = variable, _["wave"] = wave, _["category"] = category, _["n"] = n,
                        _["n_missing"] = n_missing, _["mean"] = mean, _["sd"] = sd, _["median"] = median,
                        _["q25"] = q25, _["q75"] = q75, _["min"] = minv, _["max"] = maxv, _["skew"] = skew,
                        _["kurtosis"] = kurtosis, _["proportion"] = proportion, _["kind"] = kind);
}

// [[Rcpp::export]]
List cpp_correlate_indicators(List panel) {
    auto rows = lta::correlate_indicators(panel_from(panel));
    IntegerVector variable_a, variable_b, wave, n;
    NumericVector correlation, covariance;
    CharacterVector method;
    for (const auto& r : rows) {
        variable_a.push_back(r.variable_a);
        variable_b.push_back(r.variable_b);
        wave.push_back(r.wave);
        n.push_back(r.n);
        correlation.push_back(r.correlation);
        covariance.push_back(r.covariance);
        method.push_back(r.method);
    }
    return List::create(_["variable_a"] = variable_a, _["variable_b"] = variable_b, _["wave"] = wave,
                        _["n"] = n, _["correlation"] = correlation, _["covariance"] = covariance,
                        _["method"] = method);
}

// [[Rcpp::export]]
List cpp_fiml_moments(List panel, std::string covariance) {
    auto m = lta::fiml_moments(panel_from(panel), covariance);
    NumericMatrix cov(m.p, m.p), cor(m.p, m.p);
    for (int a = 0; a < m.p; ++a) {
        for (int b = 0; b < m.p; ++b) {
            cov(a, b) = m.cov[a * m.p + b];
            cor(a, b) = m.cor[a * m.p + b];
        }
    }
    return List::create(_["covariance"] = m.covariance, _["n"] = m.n, _["p"] = m.p, _["loglik"] = m.loglik,
                        _["n_parameters"] = m.n_parameters, _["converged"] = m.converged, _["mean"] = m.mean,
                        _["cov"] = cov, _["cor"] = cor, _["note"] = m.note);
}

// [[Rcpp::export]]
List cpp_add_starts(List panel, List spec, List previous, int n_new, double seed) {
    auto fit = lta::add_starts(panel_from(panel), spec_from(spec), fit_from(previous), n_new,
                               static_cast<std::uint64_t>(seed));
    return fit_to(fit);
}

// [[Rcpp::export]]
List cpp_align_states(List panel, List spec, List fit, List reference_params) {
    lta::Alignment info;
    auto aligned = lta::align_fit(panel_from(panel), spec_from(spec), fit_from(fit), params_from(reference_params),
                                  &info);
    return List::create(_["result"] = fit_to(aligned), _["alignment"] = alignment_to(info));
}

// [[Rcpp::export]]
List cpp_describe_missingness(List panel) {
    auto s = lta::describe_missingness(panel_from(panel));
    return List::create(_["n_subjects"] = s.n_subjects, _["n_excluded"] = s.n_excluded,
                        _["n_complete"] = s.n_complete, _["n_monotone"] = s.n_monotone,
                        _["n_intermittent"] = s.n_intermittent,
                        _["n_complete_indicators"] = s.n_complete_indicators,
                        _["n_monotone_dropout"] = s.n_monotone_dropout,
                        _["excluded_subjects"] = s.excluded_subjects,
                        _["indicator_n_missing"] = s.indicator_n_missing, _["indicator_rate"] = s.indicator_rate,
                        _["wave_n_fully_missing"] = s.wave_n_fully_missing, _["wave_rate"] = s.wave_rate,
                        _["pattern"] = s.pattern, _["pattern_n"] = s.pattern_n, _["note"] = s.note);
}

// [[Rcpp::export]]
List cpp_standardize_indicators(List panel, std::string scope) {
    auto s = lta::standardize_indicators(panel_from(panel), scope);
    return List::create(_["values"] = s.values, _["center"] = s.center, _["scale"] = s.scale,
                        _["scope"] = s.scope, _["warned"] = s.warned, _["warning"] = s.warning);
}

namespace {
List compare_to_r(const lta::CompareResult& c) {
    List models, diffs;
    for (const auto& m : c.models) {
        models.push_back(List::create(_["role"] = m.role, _["loglik"] = m.loglik,
                                      _["n_parameters"] = m.n_parameters, _["aic"] = m.aic, _["bic"] = m.bic,
                                      _["converged"] = m.converged, _["n_best"] = m.n_best,
                                      _["n_starts"] = m.n_starts, _["status"] = m.status,
                                      _["ic_supported"] = m.ic_supported, _["note"] = m.note));
    }
    for (const auto& m : c.differences) {
        diffs.push_back(List::create(_["parameter"] = m.parameter, _["state"] = m.state, _["wave"] = m.wave,
                                     _["variable"] = m.variable, _["category"] = m.category,
                                     _["estimate_a"] = m.estimate_a, _["estimate_b"] = m.estimate_b,
                                     _["difference"] = m.difference));
    }
    return List::create(_["refused"] = c.refused, _["reason"] = c.reason, _["warnings"] = c.warnings,
                        _["lr_statistic"] = c.lr_statistic, _["p_value_available"] = c.p_value_available,
                        _["models"] = models, _["differences"] = diffs);
}
}  // namespace

// [[Rcpp::export]]
List cpp_compare_invariance(List panel, List restricted, List unrestricted) {
    return compare_to_r(lta::compare_invariance(panel_from(panel), fit_from(restricted), fit_from(unrestricted)));
}

// [[Rcpp::export]]
List cpp_compare_transitions(List panel, List equal_fit, List free_fit) {
    return compare_to_r(lta::compare_transitions(panel_from(panel), fit_from(equal_fit), fit_from(free_fit)));
}

// [[Rcpp::export]]
SEXP cpp_split_null_state(List params, int k0, int k1, std::string covariance, int n_tau, int n_meas,
                          int n_mean, int n_cov, int n_gaussian, int J, int Rmax) {
    lta::Params src;
    if (params.containsElementNamed("delta") && !Rf_isNull(params["delta"]))
        src.delta = as<std::vector<double>>(params["delta"]);
    if (params.containsElementNamed("tau") && !Rf_isNull(params["tau"]))
        src.tau = as<std::vector<double>>(params["tau"]);
    if (params.containsElementNamed("rho") && !Rf_isNull(params["rho"]))
        src.rho = as<std::vector<double>>(params["rho"]);
    if (params.containsElementNamed("means") && !Rf_isNull(params["means"]))
        src.means = as<std::vector<double>>(params["means"]);
    if (params.containsElementNamed("cov") && !Rf_isNull(params["cov"]))
        src.cov = as<std::vector<double>>(params["cov"]);
    lta::NullSplitLayout layout;
    layout.cov_model = lta::parse_covariance_name(covariance);
    layout.n_tau = n_tau;
    layout.n_meas = n_meas;
    layout.n_mean = n_mean;
    layout.n_cov = n_cov;
    layout.n_gaussian = n_gaussian;
    layout.J = J;
    layout.Rmax = Rmax;
    lta::Params dest;
    if (!lta::blrt_split_null_state(src, k0, k1, layout, dest)) return R_NilValue;
    return List::create(_["delta"] = dest.delta, _["tau"] = dest.tau, _["rho"] = dest.rho,
                        _["means"] = dest.means, _["cov"] = dest.cov);
}

// [[Rcpp::export]]
int cpp_blrt_hardware_threads() {
    return lta::blrt_hardware_threads();
}

// [[Rcpp::export]]
List cpp_blrt_fit_draws(List panels, LogicalVector present, List h0, List h1, double base,
                        int max_retries, int n_threads, Nullable<IntegerVector> draws = R_NilValue) {
    if (panels.size() != present.size()) stop("panels and present must have the same length");
    std::vector<int> draw_ids;
    if (draws.isNotNull()) {
        IntegerVector ids(draws.get());
        if (ids.size() != panels.size()) stop("draws and panels must have the same length");
        draw_ids.assign(ids.begin(), ids.end());
    }
    const auto convert_t0 = std::chrono::steady_clock::now();
    std::vector<lta::BlrtDrawJob> jobs(panels.size());
    for (R_xlen_t i = 0; i < panels.size(); ++i) {
        if (!present[i]) continue;
        if (Rf_isNull(panels[i])) stop("a present draw needs a panel");
        jobs[i].present = true;
        jobs[i].panel = panel_from(as<List>(panels[i]));
    }
    const auto convert_s = std::chrono::duration<double>(std::chrono::steady_clock::now() - convert_t0).count();
    const auto engine_t0 = std::chrono::steady_clock::now();
    std::vector<lta::WorkerLoad> loads;
    auto rows = lta::blrt_fit_draws(jobs, spec_from(h0), spec_from(h1),
                                    static_cast<std::uint64_t>(base), max_retries, n_threads,
                                    [] { Rcpp::checkUserInterrupt(); }, &loads, draw_ids);
    const auto engine_s = std::chrono::duration<double>(std::chrono::steady_clock::now() - engine_t0).count();
    const auto export_t0 = std::chrono::steady_clock::now();
    LogicalVector ok(rows.size());
    CharacterVector reason(rows.size());
    NumericVector lr(rows.size());
    LogicalVector h0_replicated(rows.size());
    LogicalVector h1_replicated(rows.size());
    LogicalVector h1_best_fit_replicated(rows.size());
    LogicalVector h1_replication_assessed(rows.size());
    IntegerVector attempt_draw, attempt_retry, attempt_starts, attempt_starts_h0, attempt_starts_h1;
    LogicalVector attempt_h1_embedded;
    for (size_t i = 0; i < rows.size(); ++i) {
        ok[i] = rows[i].ok;
        reason[i] = rows[i].reason;
        lr[i] = rows[i].ok ? rows[i].lr : NA_REAL;
        h0_replicated[i] = rows[i].h0_replicated;
        h1_replicated[i] = rows[i].h1_replicated;
        h1_best_fit_replicated[i] = rows[i].h1_best_fit_replicated;
        h1_replication_assessed[i] = rows[i].h1_replication_assessed;
        for (const auto& attempt : rows[i].attempts) {
            attempt_draw.push_back(attempt.draw);
            attempt_retry.push_back(attempt.retry);
            attempt_starts.push_back(attempt.n_starts);
            attempt_starts_h0.push_back(attempt.n_starts_h0);
            attempt_starts_h1.push_back(attempt.n_starts_h1);
            attempt_h1_embedded.push_back(attempt.h1_embedded);
        }
    }
    int used = std::min(n_threads, lta::blrt_hardware_threads());
    if (!rows.empty()) used = std::min(used, static_cast<int>(rows.size()));
    if (used < 1) used = 1;
    const auto export_s = std::chrono::duration<double>(std::chrono::steady_clock::now() - export_t0).count();
    IntegerVector worker_jobs(loads.size());
    NumericVector worker_busy(loads.size());
    for (size_t i = 0; i < loads.size(); ++i) {
        worker_jobs[i] = loads[i].jobs;
        worker_busy[i] = loads[i].busy_seconds;
    }
    return List::create(
        _["ok"] = ok, _["reason"] = reason, _["lr"] = lr,
        _["h0_replicated"] = h0_replicated, _["h1_replicated"] = h1_replicated,
        _["h1_best_fit_replicated"] = h1_best_fit_replicated,
        _["h1_replication_assessed"] = h1_replication_assessed,
        _["n_threads_used"] = used,
        _["attempt_draw"] = attempt_draw, _["attempt_retry"] = attempt_retry,
        _["attempt_starts"] = attempt_starts, _["attempt_starts_h0"] = attempt_starts_h0,
        _["attempt_starts_h1"] = attempt_starts_h1, _["attempt_h1_embedded"] = attempt_h1_embedded,
        _["worker_jobs"] = worker_jobs, _["worker_busy_s"] = worker_busy,
        _["seconds_convert"] = convert_s, _["seconds_engine"] = engine_s,
        _["seconds_export"] = export_s);
}
