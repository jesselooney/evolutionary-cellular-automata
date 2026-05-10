import sys
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np

sys.path.insert(0, str(Path(__file__).resolve().parent.parent / "shared"))
import analyze_common as ac

RESULTS_DIR = Path(__file__).parent / "results"
DEFAULT_PATH = RESULTS_DIR / "generalization_results.edn"

METHODS = ("pca", "nca", "hybrid")


# extract into {pattern: {method: {grid_size: [errors]}}}
def extract_generalization(data):
    result = {}
    for cond in data["conditions"]:
        name = cond["name"]
        gen_data = cond.get("generalization", {})
        result[name] = {}

        for mk in METHODS:
            if mk not in gen_data:
                continue
            runs = gen_data[mk]
            result[name][mk] = {"train": [r["train-error"] for r in runs]}

            if runs and "results" in runs[0]:
                for grid_result in runs[0]["results"]:
                    gs = grid_result["grid-size"]
                    key = f"{gs[0]}x{gs[1]}"
                    result[name][mk][key] = []

                for run in runs:
                    for grid_result in run["results"]:
                        gs = grid_result["grid-size"]
                        key = f"{gs[0]}x{gs[1]}"
                        result[name][mk][key].append(grid_result["min-error"])

    return result


# error vs grid size lines per pattern
def plot_scaling_lines(gen_data, train_grid, test_grids, out_path):
    patterns = list(gen_data.keys())
    n = len(patterns)
    ncols = min(n, 3)
    nrows = (n + ncols - 1) // ncols
    fig, axes = plt.subplots(nrows, ncols, figsize=(5 * ncols, 4 * nrows), sharey=True)
    if n == 1:
        axes = np.array([axes])
    axes = axes.flatten()

    all_sizes = [train_grid] + test_grids
    size_labels = [f"{s[0]}x{s[1]}" for s in all_sizes]
    x = np.arange(len(all_sizes))

    for i, pattern in enumerate(patterns):
        ax = axes[i]
        methods = gen_data[pattern]

        for mk in METHODS:
            if mk not in methods:
                continue
            color = ac.METHOD_COLORS.get(mk)
            label = ac.METHOD_LABELS.get(mk, mk)

            means, stds = [], []
            for sl in size_labels:
                key = "train" if sl == size_labels[0] else sl
                vals = np.array(methods[mk].get(key, []))
                means.append(vals.mean() if len(vals) > 0 else np.nan)
                stds.append(vals.std() if len(vals) > 0 else 0)

            means = np.array(means)
            stds = np.array(stds)
            ax.plot(x, means, "o-", color=color, label=label)
            ax.fill_between(x, means - stds, means + stds, alpha=0.15, color=color)

        ax.set_xticks(x)
        ax.set_xticklabels(size_labels)
        ax.set_xlabel("Grid Size")
        if i % ncols == 0:
            ax.set_ylabel("Best Error")
        ax.set_title(pattern)
        ax.legend(fontsize=9)
        ax.grid(True, alpha=0.3)
        ax.set_ylim(bottom=0)

    for j in range(n, len(axes)):
        axes[j].set_visible(False)

    fig.suptitle("Generalization: Error vs Grid Size", fontsize=14)
    fig.tight_layout()
    fig.savefig(out_path, dpi=150, bbox_inches="tight")
    print(f"Saved: {out_path}")
    plt.close(fig)


# grouped bar chart of error by grid size
def plot_generalization_bars(gen_data, train_grid, test_grids, out_path):
    patterns = list(gen_data.keys())
    all_sizes = [train_grid] + test_grids
    size_labels = [f"{s[0]}x{s[1]}" for s in all_sizes]
    n_sizes = len(size_labels)
    n = len(patterns)
    ncols = min(n, 3)
    nrows = (n + ncols - 1) // ncols

    fig, axes = plt.subplots(nrows, ncols, figsize=(5 * ncols, 4 * nrows), sharey=True)
    if n == 1:
        axes = np.array([axes])
    axes = axes.flatten()

    for pi, pattern in enumerate(patterns):
        ax = axes[pi]
        x = np.arange(n_sizes)
        present_methods = [mk for mk in METHODS if mk in gen_data[pattern]]
        n_methods = len(present_methods)
        width = 0.8 / max(n_methods, 1)

        for mi, mk in enumerate(present_methods):
            color = ac.METHOD_COLORS.get(mk)
            label = ac.METHOD_LABELS.get(mk, mk)
            offset = (mi - (n_methods - 1) / 2) * width

            means, stds = [], []
            for sl in size_labels:
                key = "train" if sl == size_labels[0] else sl
                vals = np.array(gen_data[pattern][mk].get(key, []))
                means.append(vals.mean() if len(vals) > 0 else 0)
                stds.append(vals.std() if len(vals) > 0 else 0)

            ax.bar(x + offset, means, width, yerr=stds, color=color,
                   label=label, alpha=0.8, capsize=3)

        ax.set_xticks(x)
        ax.set_xticklabels(size_labels)
        ax.set_xlabel("Grid Size")
        if pi % ncols == 0:
            ax.set_ylabel("Best Error")
        ax.set_title(pattern)
        ax.legend(fontsize=9)
        ax.grid(True, alpha=0.3, axis="y")

    for j in range(n, len(axes)):
        axes[j].set_visible(False)

    fig.suptitle("Generalization: Error by Grid Size", fontsize=14)
    fig.tight_layout()
    fig.savefig(out_path, dpi=150, bbox_inches="tight")
    print(f"Saved: {out_path}")
    plt.close(fig)


def print_generalization_table(gen_data, train_grid, test_grids):
    all_sizes = [train_grid] + test_grids
    size_labels = [f"{s[0]}x{s[1]}" for s in all_sizes]

    print("\nGENERALIZATION RESULTS (mean +/- std)")
    print("=" * 90)

    for pattern in gen_data:
        print(f"\n  {pattern}")
        print(f"  {'-' * 85}")
        header = f"  {'Method':<15}"
        for sl in size_labels:
            header += f"  {sl:>15}"
        print(header)
        print(f"  {'-' * 85}")

        for mk in METHODS:
            if mk not in gen_data[pattern]:
                continue
            label = ac.METHOD_LABELS.get(mk, mk)
            row = f"  {label:<15}"
            for sl in size_labels:
                key = "train" if sl == size_labels[0] else sl
                vals = np.array(gen_data[pattern][mk].get(key, []))
                if len(vals) > 0:
                    row += f"  {vals.mean():>6.3f}+/-{vals.std():<5.3f}"
                else:
                    row += f"  {'---':>15}"
            print(row)

    print()


def main():
    path = Path(sys.argv[1]) if len(sys.argv) > 1 else DEFAULT_PATH
    data = ac.load_edn(path)
    out_dir = path.parent

    train_grid = data.get("train-grid", [10, 10])
    test_grids = data.get("test-grids", [[15, 15], [20, 20], [25, 25]])

    gen_data = extract_generalization(data)
    plot_scaling_lines(gen_data, train_grid, test_grids, out_dir / "scaling_lines.png")
    plot_scaling_lines(gen_data, train_grid, test_grids, out_dir / "scaling_lines.pdf")
    plot_generalization_bars(gen_data, train_grid, test_grids, out_dir / "generalization_bars.png")
    plot_generalization_bars(gen_data, train_grid, test_grids, out_dir / "generalization_bars.pdf")
    print_generalization_table(gen_data, train_grid, test_grids)


if __name__ == "__main__":
    main()
