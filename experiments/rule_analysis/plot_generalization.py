import sys
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np

sys.path.insert(0, str(Path(__file__).resolve().parent.parent / "shared"))
import analyze_common as ac

from truth_table_loader import METHODS


def save_fig(fig, out_path):
    for ext in ("png", "pdf"):
        path = out_path.with_suffix(f".{ext}")
        fig.savefig(path, dpi=150, bbox_inches="tight")
        print(f"Saved: {path}")
    plt.close(fig)


IC_CATEGORIES = {
    "training": ["seed_origin"],
    "novel_seeds": ["seed_center", "seed_corner", "seed_offset"],
    "sparse_3cell": ["sparse_3cell_s200", "sparse_3cell_s201", "sparse_3cell_s202"],
    "random_50pct": ["random_50pct_s100", "random_50pct_s101", "random_50pct_s102"],
}

IC_CATEGORY_LABELS = {
    "training": "Training\n(0,0)",
    "novel_seeds": "Novel\nSeeds",
    "sparse_3cell": "Sparse\n3-cell",
    "random_50pct": "Random\n50%",
}


def best_run_index(method_data):
    test_ics = [k for k in method_data.keys() if k != "seed_origin"]
    if not test_ics:
        return 0
    n_runs = len(method_data[test_ics[0]])
    best_idx = 0
    best_err = float("inf")
    for run_idx in range(n_runs):
        mean_test = np.mean([method_data[ic][run_idx] for ic in test_ics])
        if mean_test < best_err:
            best_err = mean_test
            best_idx = run_idx
    return best_idx


def best_run_per_category(method_data):
    cat_errors = {}
    for cat, ic_names in IC_CATEGORIES.items():
        available = [name for name in ic_names if name in method_data]
        if not available:
            continue
        n_runs = len(method_data[available[0]])
        best_idx = 0
        best_err = float("inf")
        for run_idx in range(n_runs):
            mean_err = np.mean([method_data[name][run_idx] for name in available])
            if mean_err < best_err:
                best_err = mean_err
                best_idx = run_idx
        cat_errors[cat] = np.mean([method_data[name][best_idx] for name in available])
    return cat_errors


def plot_generalization_by_ic(gen_results, patterns, out_dir):
    out_dir = Path(out_dir)
    n_patterns = len(patterns)
    ncols = 3
    nrows = (n_patterns + ncols - 1) // ncols

    fig, axes = plt.subplots(nrows, ncols, figsize=(5 * ncols, 4 * nrows),
                             sharey=True)
    axes_flat = axes.flatten() if n_patterns > 1 else [axes]

    categories = list(IC_CATEGORY_LABELS.keys())
    n_cats = len(categories)
    n_methods = len(METHODS)
    width = 0.8 / n_methods

    for idx, pat in enumerate(patterns):
        ax = axes_flat[idx]
        x = np.arange(n_cats)

        for i, mk in enumerate(METHODS):
            if mk not in gen_results.get(pat, {}):
                continue
            cat_data = best_run_per_category(gen_results[pat][mk])
            values = []
            for cat in categories:
                values.append(cat_data.get(cat, np.nan))

            offset = (i - (n_methods - 1) / 2) * width
            color = ac.METHOD_COLORS.get(mk)
            label = ac.METHOD_LABELS.get(mk, mk)
            ax.bar(x + offset, values, width,
                   label=label, color=color, alpha=0.85)

        ax.set_xticks(x)
        ax.set_xticklabels([IC_CATEGORY_LABELS[c] for c in categories],
                           fontsize=8)
        ax.set_title(pat, fontsize=10)
        ax.set_ylim(0, 1.0)
        ax.grid(True, alpha=0.3, axis="y")
        if idx == 0:
            ax.legend(fontsize=7, loc="upper left")

    for idx in range(n_patterns, len(axes_flat)):
        axes_flat[idx].set_visible(False)

    fig.supylabel("Min Error", fontsize=11)
    fig.suptitle("Best Rule per IC Category", fontsize=13)
    fig.tight_layout()
    save_fig(fig, out_dir / "generalization_by_ic")


def plot_train_vs_test(gen_results, patterns, out_dir):
    out_dir = Path(out_dir)
    fig, ax = plt.subplots(figsize=(7, 6))

    for mk in METHODS:
        train_errors = []
        test_errors = []
        for pat in patterns:
            if mk not in gen_results.get(pat, {}):
                continue
            method_data = gen_results[pat][mk]
            if "seed_origin" not in method_data:
                continue
            idx = best_run_index(method_data)
            test_ics = [k for k in method_data.keys() if k != "seed_origin"]
            train_errors.append(method_data["seed_origin"][idx])
            test_errors.append(np.mean([method_data[ic][idx] for ic in test_ics]))

        color = ac.METHOD_COLORS.get(mk)
        label = ac.METHOD_LABELS.get(mk, mk)
        ax.scatter(train_errors, test_errors, color=color, label=label,
                   alpha=0.8, s=60, edgecolors="black", linewidths=0.5)

    lim = max(ax.get_xlim()[1], ax.get_ylim()[1])
    ax.plot([0, lim], [0, lim], "k--", alpha=0.3, label="No gap")
    ax.set_xlim(0, lim)
    ax.set_ylim(0, lim)
    ax.set_xlabel("Training Error (seed at origin)")
    ax.set_ylabel("Mean Generalization Error (novel ICs)")
    ax.set_title("Best-Generalizing Rule: Training vs Test Error")
    ax.legend(fontsize=9)
    ax.grid(True, alpha=0.3)
    ax.set_aspect("equal")
    fig.tight_layout()
    save_fig(fig, out_dir / "train_vs_test")


def plot_generalization_gap(gen_results, patterns, out_dir):
    out_dir = Path(out_dir)
    n_patterns = len(patterns)
    n_methods = len(METHODS)
    x = np.arange(n_patterns)
    width = 0.8 / n_methods

    fig, ax = plt.subplots(figsize=(max(8, n_patterns * 1.5), 5))

    for i, mk in enumerate(METHODS):
        gaps = []
        for pat in patterns:
            if mk not in gen_results.get(pat, {}):
                gaps.append(0)
                continue
            method_data = gen_results[pat][mk]
            if "seed_origin" not in method_data:
                gaps.append(0)
                continue
            idx = best_run_index(method_data)
            test_ics = [k for k in method_data.keys() if k != "seed_origin"]
            train_err = method_data["seed_origin"][idx]
            test_err = np.mean([method_data[ic][idx] for ic in test_ics])
            gaps.append(test_err - train_err)

        offset = (i - (n_methods - 1) / 2) * width
        color = ac.METHOD_COLORS.get(mk)
        label = ac.METHOD_LABELS.get(mk, mk)
        ax.bar(x + offset, gaps, width,
               label=label, color=color, alpha=0.85)

    ax.axhline(0, color="black", linestyle="-", alpha=0.3)
    ax.set_xticks(x)
    ax.set_xticklabels(patterns, rotation=30, ha="right")
    ax.set_ylabel("Generalization Gap (test \u2212 train)")
    ax.set_title("Best-Generalizing Rule: Gap (positive = overfitting)")
    ax.legend(fontsize=8)
    ax.grid(True, alpha=0.3, axis="y")
    fig.tight_layout()
    save_fig(fig, out_dir / "generalization_gap")


def plot_grid_size_scaling(scale_results, patterns, grid_sizes, out_dir):
    out_dir = Path(out_dir)
    n_patterns = len(patterns)
    ncols = 3
    nrows = (n_patterns + ncols - 1) // ncols

    fig, axes = plt.subplots(nrows, ncols, figsize=(5 * ncols, 4 * nrows),
                             sharey=True)
    axes_flat = axes.flatten() if n_patterns > 1 else [axes]

    size_labels = [f"{h}x{w}" for h, w in grid_sizes]
    x = np.arange(len(grid_sizes))

    for idx, pat in enumerate(patterns):
        ax = axes_flat[idx]

        for mk in METHODS:
            if mk not in scale_results.get(pat, {}):
                continue
            errors_by_size = scale_results[pat][mk]
            first_key = f"{grid_sizes[0][0]}x{grid_sizes[0][1]}"
            if first_key not in errors_by_size:
                continue
            n_runs = len(errors_by_size[first_key])
            best_idx = min(range(n_runs),
                           key=lambda r: errors_by_size[first_key][r])

            values = []
            for size in grid_sizes:
                size_key = f"{size[0]}x{size[1]}"
                if size_key in errors_by_size:
                    values.append(errors_by_size[size_key][best_idx])
                else:
                    values.append(np.nan)

            color = ac.METHOD_COLORS.get(mk)
            label = ac.METHOD_LABELS.get(mk, mk)
            ax.plot(x, values, "o-", color=color, label=label, markersize=5)

        ax.set_xticks(x)
        ax.set_xticklabels(size_labels)
        ax.set_title(pat, fontsize=10)
        ax.set_ylim(0, 1.0)
        ax.grid(True, alpha=0.3)
        if idx == 0:
            ax.legend(fontsize=7)

    for idx in range(n_patterns, len(axes_flat)):
        axes_flat[idx].set_visible(False)

    fig.supxlabel("Grid Size", fontsize=11)
    fig.supylabel("Min Error", fontsize=11)
    fig.suptitle("Best Rule: Grid Size Scaling (seed at origin)", fontsize=13)
    fig.tight_layout()
    save_fig(fig, out_dir / "generalization_grid_size")
