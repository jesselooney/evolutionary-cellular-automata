import sys
from pathlib import Path

import numpy as np

sys.path.insert(0, str(Path(__file__).resolve().parent))
sys.path.insert(0, str(Path(__file__).resolve().parent.parent / "shared"))
import analyze_common as ac

from truth_table_loader import (
    METHODS, load_from_experiments, flatten_outputs,
)
from metrics import (
    langton_lambda, input_influence, is_fully_neighbor_symmetric,
    inter_run_agreement, pairwise_hamming, n_unique_tables,
    reflection_symmetry_ew, reflection_symmetry_ns,
    totalistic_deviation, classify_rule_type,
)
from ca_simulator import (
    make_single_seed, make_random_grid, make_sparse_seeds,
    make_target_pattern, run_ca,
)
from plot_symmetry import plot_reflection_symmetry, plot_symmetry_heatmap
from plot_totalistic import (
    plot_totalistic_deviation, plot_rule_classification, plot_sum_class_outputs,
)
from plot_generalization import (
    plot_generalization_by_ic, plot_train_vs_test,
    plot_generalization_gap, plot_grid_size_scaling,
)

RESULTS_DIR = Path(__file__).parent / "results"


def compute_symmetry(data):
    results = {}
    for pattern, methods in data.items():
        results[pattern] = {}
        for method, runs in methods.items():
            ew_scores = []
            ns_scores = []
            for inputs, output in runs:
                ew_scores.append(reflection_symmetry_ew(inputs, output))
                ns_scores.append(reflection_symmetry_ns(inputs, output))
            results[pattern][method] = {"ew": ew_scores, "ns": ns_scores}
    return results


def compute_totalistic(data):
    results = {}
    for pattern, methods in data.items():
        results[pattern] = {}
        for method, runs in methods.items():
            classifications = []
            deviations = []
            for inputs, output in runs:
                cls, dev = classify_rule_type(inputs, output)
                classifications.append(cls)
                deviations.append(dev)
            results[pattern][method] = {
                "classifications": classifications,
                "deviations": deviations,
            }
    return results


def compute_sum_class_probs(data):
    results = {}
    for pattern, methods in data.items():
        results[pattern] = {}
        for method, runs in methods.items():
            per_run_probs = []
            for inputs, output in runs:
                _, per_class = totalistic_deviation(inputs, output)
                per_run_probs.append(per_class)
            all_sums = set()
            for pc in per_run_probs:
                all_sums.update(pc.keys())
            mean_probs = {}
            for s in sorted(all_sums):
                vals = [pc[s] for pc in per_run_probs if s in pc]
                mean_probs[s] = float(np.mean(vals))
            results[pattern][method] = mean_probs
    return results


def compute_generalization(data, n_steps=30, grid_size=(10, 10)):
    h, w = grid_size

    test_ics = {
        "seed_origin": make_single_seed(h, w, (0, 0)),
        "seed_center": make_single_seed(h, w, (5, 5)),
        "seed_corner": make_single_seed(h, w, (9, 9)),
        "seed_offset": make_single_seed(h, w, (3, 7)),
        "random_50pct_s100": make_random_grid(h, w, 0.5, seed=100),
        "random_50pct_s101": make_random_grid(h, w, 0.5, seed=101),
        "random_50pct_s102": make_random_grid(h, w, 0.5, seed=102),
        "sparse_3cell_s200": make_sparse_seeds(h, w, 3, seed=200),
        "sparse_3cell_s201": make_sparse_seeds(h, w, 3, seed=201),
        "sparse_3cell_s202": make_sparse_seeds(h, w, 3, seed=202),
    }

    results = {}
    for pattern, methods in data.items():
        target = make_target_pattern(h, w, pattern)
        results[pattern] = {}

        for method, runs in methods.items():
            results[pattern][method] = {ic: [] for ic in test_ics}
            for inputs, output in runs:
                for ic_name, ic_grid in test_ics.items():
                    errors = run_ca(output, ic_grid, target, n_steps, inputs)
                    results[pattern][method][ic_name].append(min(errors))

    return results


def compute_grid_scaling(data, grid_sizes, n_steps=30):
    results = {}
    for pattern, methods in data.items():
        results[pattern] = {}
        for method, runs in methods.items():
            results[pattern][method] = {}
            for (h, w) in grid_sizes:
                size_key = f"{h}x{w}"
                init_grid = make_single_seed(h, w, (0, 0))
                target = make_target_pattern(h, w, pattern)
                min_errors = []
                for inputs, output in runs:
                    errors = run_ca(output, init_grid, target, n_steps, inputs)
                    min_errors.append(min(errors))
                results[pattern][method][size_key] = min_errors
    return results


def main():
    if len(sys.argv) > 1:
        experiment_names = sys.argv[1:]
    else:
        experiment_names = ["local_patterns"]

    all_data = load_from_experiments(*experiment_names)

    data = {}
    for exp_name, exp_data in all_data.items():
        for pattern, methods in exp_data.items():
            if pattern == "diagonal-bands":
                continue
            if pattern not in data:
                data[pattern] = methods
            else:
                for method, runs in methods.items():
                    if method not in data[pattern]:
                        data[pattern][method] = runs
                    else:
                        data[pattern][method].extend(runs)

    patterns = sorted(data.keys())

    RESULTS_DIR.mkdir(parents=True, exist_ok=True)

    sym_results = compute_symmetry(data)
    plot_reflection_symmetry(sym_results, patterns, RESULTS_DIR)
    plot_symmetry_heatmap(sym_results, patterns, RESULTS_DIR)

    tot_results = compute_totalistic(data)
    sum_class_data = compute_sum_class_probs(data)
    plot_totalistic_deviation(tot_results, patterns, RESULTS_DIR)
    plot_rule_classification(tot_results, patterns, RESULTS_DIR)
    plot_sum_class_outputs(sum_class_data, patterns, RESULTS_DIR)

    gen_results = compute_generalization(data, n_steps=100, grid_size=(10, 10))
    plot_generalization_by_ic(gen_results, patterns, RESULTS_DIR)
    plot_train_vs_test(gen_results, patterns, RESULTS_DIR)
    plot_generalization_gap(gen_results, patterns, RESULTS_DIR)

    grid_sizes = [(10, 10), (15, 15), (20, 20)]
    scale_results = compute_grid_scaling(data, grid_sizes, n_steps=100)
    plot_grid_size_scaling(scale_results, patterns, grid_sizes, RESULTS_DIR)


if __name__ == "__main__":
    main()
