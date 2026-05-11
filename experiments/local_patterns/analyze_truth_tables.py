"""Analyze evolved CA truth tables across patterns and approaches."""

import sys
from itertools import combinations
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np

sys.path.insert(0, str(Path(__file__).resolve().parent.parent / "shared"))
import analyze_common as ac

TRUTH_TABLES_DIR = Path(__file__).parent / "results" / "truth_tables"
METHODS = ["pca", "nca", "hybrid"]
INPUT_NAMES = ["N0", "N1", "N2", "N3", "N4"]


def load_truth_table(csv_path):
    data = np.loadtxt(csv_path, delimiter=",", skiprows=1, dtype=int)
    return data[:, :5], data[:, 5]


# load all tables as {pattern: {method: [output_arrays]}}
def load_all(base_dir):
    base = Path(base_dir)
    patterns = sorted(d.name for d in base.iterdir() if d.is_dir())

    outputs = {}
    full = {}

    for pattern in patterns:
        outputs[pattern] = {}
        full[pattern] = {}
        for method in METHODS:
            method_dir = base / pattern / method
            if not method_dir.exists():
                continue
            csvs = sorted(method_dir.glob("run_*.csv"),
                          key=lambda p: int(p.stem.split("_")[1]))
            method_outputs = []
            method_full = []
            for csv in csvs:
                inp, out = load_truth_table(csv)
                method_outputs.append(out)
                method_full.append((inp, out))
            outputs[pattern][method] = method_outputs
            full[pattern][method] = method_full

    return patterns, outputs, full


# fraction of outputs that are 1
def langton_lambda(output):
    return output.sum() / len(output)


# influence of each input on output (sensitivity analysis)
def input_influence(inputs, output):
    n_inputs = inputs.shape[1]
    influences = np.zeros(n_inputs)

    for i in range(n_inputs):
        mask_cols = [c for c in range(n_inputs) if c != i]
        other_inputs = inputs[:, mask_cols]

        flips = 0
        pairs = 0
        for r in range(len(inputs)):
            for s in range(r + 1, len(inputs)):
                if np.array_equal(other_inputs[r], other_inputs[s]):
                    pairs += 1
                    if output[r] != output[s]:
                        flips += 1
        influences[i] = flips / pairs if pairs > 0 else 0.0

    return influences


# fraction of 32 output bits identical across all runs
def inter_run_agreement(output_list):
    if len(output_list) <= 1:
        return 1.0
    stacked = np.stack(output_list)
    all_same = np.all(stacked == stacked[0:1, :], axis=0)
    return all_same.sum() / stacked.shape[1]


# mean pairwise Hamming distance between truth tables
def pairwise_hamming(output_list):
    if len(output_list) <= 1:
        return 0.0
    distances = []
    for i, j in combinations(range(len(output_list)), 2):
        distances.append(np.sum(output_list[i] != output_list[j]))
    return np.mean(distances)


def n_unique_tables(output_list):
    seen = set()
    for out in output_list:
        seen.add(tuple(out))
    return len(seen)


# mean Hamming distance across approaches
def cross_approach_hamming(outputs, pattern):
    result = {}
    available = [m for m in METHODS if m in outputs[pattern]]
    for ma, mb in combinations(available, 2):
        distances = []
        for out_a in outputs[pattern][ma]:
            for out_b in outputs[pattern][mb]:
                distances.append(np.sum(out_a != out_b))
        result[(ma, mb)] = np.mean(distances)
    return result


def save_fig(fig, out_path):
    for ext in ("png", "pdf"):
        path = out_path.with_suffix(f".{ext}")
        fig.savefig(path, dpi=150, bbox_inches="tight")
        print(f"Saved: {path}")
    plt.close(fig)


# Langton's lambda bar chart
def plot_lambda(patterns, outputs, out_path):
    n_patterns = len(patterns)
    n_methods = len(METHODS)
    x = np.arange(n_patterns)
    width = 0.8 / n_methods

    fig, ax = plt.subplots(figsize=(max(8, n_patterns * 1.5), 5))

    for i, mk in enumerate(METHODS):
        means, stds = [], []
        for pat in patterns:
            if mk in outputs[pat]:
                lambdas = [langton_lambda(o) for o in outputs[pat][mk]]
                means.append(np.mean(lambdas))
                stds.append(np.std(lambdas))
            else:
                means.append(0)
                stds.append(0)

        offset = (i - (n_methods - 1) / 2) * width
        color = ac.METHOD_COLORS.get(mk)
        label = ac.METHOD_LABELS.get(mk, mk)
        ax.bar(x + offset, means, width, yerr=stds,
               label=label, color=color, alpha=0.85, capsize=3)

    ax.axhline(0.5, color="gray", linestyle="--", alpha=0.5, label="lambda=0.5")
    ax.set_xticks(x)
    ax.set_xticklabels(patterns, rotation=30, ha="right")
    ax.set_ylabel("Langton's Lambda")
    ax.set_title("Output Density (Langton's Lambda) by Pattern")
    ax.legend(fontsize=8)
    ax.grid(True, alpha=0.3, axis="y")
    fig.tight_layout()
    save_fig(fig, out_path)


# input influence heatmap per approach
def plot_influence(patterns, full, out_path):
    fig, axes = plt.subplots(1, len(METHODS), figsize=(5 * len(METHODS), 5),
                              sharey=True)

    for ax, mk in zip(axes, METHODS):
        matrix = np.zeros((5, len(patterns)))
        for pi, pat in enumerate(patterns):
            if mk not in full[pat]:
                continue
            run_influences = []
            for inp, out in full[pat][mk]:
                run_influences.append(input_influence(inp, out))
            matrix[:, pi] = np.mean(run_influences, axis=0)

        im = ax.imshow(matrix, aspect="auto", cmap="YlOrRd", vmin=0, vmax=1)
        ax.set_xticks(range(len(patterns)))
        ax.set_xticklabels(patterns, rotation=45, ha="right", fontsize=8)
        ax.set_yticks(range(5))
        ax.set_yticklabels(INPUT_NAMES)
        ax.set_title(ac.METHOD_LABELS.get(mk, mk))

        for yi in range(5):
            for xi in range(len(patterns)):
                ax.text(xi, yi, f"{matrix[yi, xi]:.2f}",
                        ha="center", va="center", fontsize=7,
                        color="white" if matrix[yi, xi] > 0.5 else "black")

    fig.suptitle("Input Influence (Sensitivity)", fontsize=14, y=1.02)
    fig.colorbar(im, ax=axes, shrink=0.8, label="Influence")
    save_fig(fig, out_path)


# inter-run agreement heatmap
def plot_agreement(patterns, outputs, out_path):
    matrix = np.zeros((len(patterns), len(METHODS)))
    for pi, pat in enumerate(patterns):
        for mi, mk in enumerate(METHODS):
            if mk in outputs[pat]:
                matrix[pi, mi] = inter_run_agreement(outputs[pat][mk])

    fig, ax = plt.subplots(figsize=(5, max(4, len(patterns) * 0.6)))
    im = ax.imshow(matrix, aspect="auto", cmap="RdYlGn", vmin=0, vmax=1)

    ax.set_xticks(range(len(METHODS)))
    ax.set_xticklabels([ac.METHOD_LABELS.get(m, m) for m in METHODS])
    ax.set_yticks(range(len(patterns)))
    ax.set_yticklabels(patterns)
    ax.set_title("Inter-Run Agreement (fraction of 32 bits identical across all runs)")

    for yi in range(len(patterns)):
        for xi in range(len(METHODS)):
            ax.text(xi, yi, f"{matrix[yi, xi]:.2f}",
                    ha="center", va="center", fontsize=9,
                    color="white" if matrix[yi, xi] < 0.5 else "black")

    fig.colorbar(im, shrink=0.8, label="Agreement")
    fig.tight_layout()
    save_fig(fig, out_path)


# cross-approach Hamming distance bar chart
def plot_cross_approach_hamming(patterns, outputs, out_path):
    pair_labels = []
    for ma, mb in combinations(METHODS, 2):
        la = ac.METHOD_LABELS.get(ma, ma)
        lb = ac.METHOD_LABELS.get(mb, mb)
        pair_labels.append(f"{la} vs {lb}")

    n_patterns = len(patterns)
    n_pairs = len(pair_labels)
    x = np.arange(n_patterns)
    width = 0.8 / n_pairs
    colors = ["tab:purple", "tab:red", "tab:cyan"]

    fig, ax = plt.subplots(figsize=(max(8, n_patterns * 1.5), 5))

    for pi_pair, ((ma, mb), label, color) in enumerate(
            zip(combinations(METHODS, 2), pair_labels, colors)):
        means = []
        for pat in patterns:
            ham = cross_approach_hamming(outputs, pat)
            means.append(ham.get((ma, mb), 0))
        offset = (pi_pair - (n_pairs - 1) / 2) * width
        ax.bar(x + offset, means, width, label=label, color=color, alpha=0.8)

    ax.set_xticks(x)
    ax.set_xticklabels(patterns, rotation=30, ha="right")
    ax.set_ylabel("Mean Hamming Distance (out of 32)")
    ax.set_title("Cross-Approach Truth Table Similarity")
    ax.legend(fontsize=8)
    ax.grid(True, alpha=0.3, axis="y")
    fig.tight_layout()
    save_fig(fig, out_path)


# unique truth table count per pattern/approach
def plot_unique_rules(patterns, outputs, out_path):
    n_patterns = len(patterns)
    n_methods = len(METHODS)
    x = np.arange(n_patterns)
    width = 0.8 / n_methods

    fig, ax = plt.subplots(figsize=(max(8, n_patterns * 1.5), 5))

    for i, mk in enumerate(METHODS):
        counts = []
        for pat in patterns:
            if mk in outputs[pat]:
                counts.append(n_unique_tables(outputs[pat][mk]))
            else:
                counts.append(0)
        offset = (i - (n_methods - 1) / 2) * width
        color = ac.METHOD_COLORS.get(mk)
        label = ac.METHOD_LABELS.get(mk, mk)
        ax.bar(x + offset, counts, width, label=label, color=color, alpha=0.85)

    ax.set_xticks(x)
    ax.set_xticklabels(patterns, rotation=30, ha="right")
    ax.set_ylabel("Unique Truth Tables (out of 10 runs)")
    ax.set_title("Rule Diversity Across Runs")
    ax.legend(fontsize=8)
    ax.grid(True, alpha=0.3, axis="y")
    ax.set_ylim(0, 11)
    fig.tight_layout()
    save_fig(fig, out_path)


def main():
    base_dir = Path(sys.argv[1]) if len(sys.argv) > 1 else TRUTH_TABLES_DIR
    patterns, outputs, full = load_all(base_dir)
    # match the six patterns used in the main local_patterns analysis
    patterns = [p for p in patterns if p != "diagonal-bands"]
    out_dir = Path(base_dir)

    plot_lambda(patterns, outputs, out_dir / "lambda_comparison")
    plot_influence(patterns, full, out_dir / "input_influence")
    plot_agreement(patterns, outputs, out_dir / "inter_run_agreement")
    plot_cross_approach_hamming(patterns, outputs,
                                out_dir / "cross_approach_hamming")
    plot_unique_rules(patterns, outputs, out_dir / "unique_rules")


if __name__ == "__main__":
    main()
