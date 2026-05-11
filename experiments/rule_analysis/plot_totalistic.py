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


CLASSIFICATION_COLORS = {
    "totalistic": "tab:green",
    "approx_totalistic": "gold",
    "parity": "tab:purple",
    "arbitrary": "tab:red",
}

CLASSIFICATION_LABELS = {
    "totalistic": "Totalistic",
    "approx_totalistic": "Approx. Totalistic",
    "parity": "Parity-like",
    "arbitrary": "Arbitrary",
}


def plot_totalistic_deviation(tot_results, patterns, out_dir):
    out_dir = Path(out_dir)
    n_patterns = len(patterns)
    n_methods = len(METHODS)
    x = np.arange(n_patterns)
    width = 0.8 / n_methods

    fig, ax = plt.subplots(figsize=(max(8, n_patterns * 1.5), 5))

    for i, mk in enumerate(METHODS):
        means, stds = [], []
        for pat in patterns:
            if mk in tot_results.get(pat, {}):
                devs = tot_results[pat][mk]["deviations"]
                means.append(np.mean(devs))
                stds.append(np.std(devs))
            else:
                means.append(0)
                stds.append(0)

        offset = (i - (n_methods - 1) / 2) * width
        color = ac.METHOD_COLORS.get(mk)
        label = ac.METHOD_LABELS.get(mk, mk)
        ax.bar(x + offset, means, width, yerr=stds,
               label=label, color=color, alpha=0.85, capsize=3)

    ax.axhline(0, color="green", linestyle="--", alpha=0.4,
               label="Perfectly totalistic")
    ax.set_xticks(x)
    ax.set_xticklabels(patterns, rotation=30, ha="right")
    ax.set_ylabel("Totalistic Deviation (fraction of 32 bits)")
    ax.set_title("Totalistic Deviation (0 = sum-only rule)")
    ax.legend(fontsize=8)
    ax.grid(True, alpha=0.3, axis="y")
    fig.tight_layout()
    save_fig(fig, out_dir / "totalistic_deviation")


def plot_rule_classification(tot_results, patterns, out_dir):
    out_dir = Path(out_dir)
    categories = ["totalistic", "approx_totalistic", "parity", "arbitrary"]

    counts = {mk: {cat: 0 for cat in categories} for mk in METHODS}
    for pat in patterns:
        for mk in METHODS:
            if mk in tot_results.get(pat, {}):
                for cls in tot_results[pat][mk]["classifications"]:
                    counts[mk][cls] += 1

    fig, ax = plt.subplots(figsize=(6, 5))
    x = np.arange(len(METHODS))
    width = 0.5

    bottom = np.zeros(len(METHODS))
    for cat in categories:
        values = [counts[mk][cat] for mk in METHODS]
        ax.bar(x, values, width, bottom=bottom,
               label=CLASSIFICATION_LABELS[cat],
               color=CLASSIFICATION_COLORS[cat], alpha=0.85)
        bottom += values

    ax.set_xticks(x)
    ax.set_xticklabels([ac.METHOD_LABELS.get(m, m) for m in METHODS])
    ax.set_ylabel("Number of Rules")
    ax.set_title("Rule Type Classification (all patterns)")
    ax.legend(fontsize=9, loc="upper right")
    ax.grid(True, alpha=0.3, axis="y")
    fig.tight_layout()
    save_fig(fig, out_dir / "rule_classification")


def plot_sum_class_outputs(sum_class_data, patterns, out_dir):
    out_dir = Path(out_dir)
    fig, axes = plt.subplots(1, len(METHODS), figsize=(5 * len(METHODS), 4),
                             sharey=True)

    for ax, mk in zip(axes, METHODS):
        matrix = np.full((6, len(patterns)), np.nan)
        for pi, pat in enumerate(patterns):
            if mk in sum_class_data.get(pat, {}):
                for s, prob in sum_class_data[pat][mk].items():
                    matrix[s, pi] = prob

        im = ax.imshow(matrix, aspect="auto", cmap="RdBu_r", vmin=0, vmax=1)
        ax.set_xticks(range(len(patterns)))
        ax.set_xticklabels(patterns, rotation=45, ha="right", fontsize=8)
        ax.set_yticks(range(6))
        ax.set_yticklabels([f"sum={s}" for s in range(6)])
        ax.set_title(ac.METHOD_LABELS.get(mk, mk))

        for yi in range(6):
            for xi in range(len(patterns)):
                val = matrix[yi, xi]
                if not np.isnan(val):
                    ax.text(xi, yi, f"{val:.2f}",
                            ha="center", va="center", fontsize=7,
                            color="white" if abs(val - 0.5) > 0.3 else "black")

    fig.suptitle("P(output=1) by Neighborhood Sum", fontsize=13, y=1.02)
    fig.colorbar(im, ax=axes, shrink=0.8, label="P(output=1)")
    save_fig(fig, out_dir / "sum_class_outputs")
