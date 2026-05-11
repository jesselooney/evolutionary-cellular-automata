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


def plot_reflection_symmetry(sym_results, patterns, out_dir):
    out_dir = Path(out_dir)
    n_patterns = len(patterns)
    n_methods = len(METHODS)
    x = np.arange(n_patterns)
    width = 0.8 / n_methods

    fig, axes = plt.subplots(2, 1, figsize=(max(8, n_patterns * 1.5), 8),
                             sharex=True)

    for ax, direction, label in zip(axes, ["ew", "ns"],
                                     ["East-West (N1 \u2194 N3)",
                                      "North-South (N2 \u2194 N4)"]):
        for i, mk in enumerate(METHODS):
            means, stds = [], []
            for pat in patterns:
                if mk in sym_results.get(pat, {}):
                    scores = sym_results[pat][mk][direction]
                    means.append(np.mean(scores))
                    stds.append(np.std(scores))
                else:
                    means.append(0)
                    stds.append(0)

            offset = (i - (n_methods - 1) / 2) * width
            color = ac.METHOD_COLORS.get(mk)
            method_label = ac.METHOD_LABELS.get(mk, mk)
            ax.bar(x + offset, means, width, yerr=stds,
                   label=method_label, color=color, alpha=0.85, capsize=3)

        ax.axhline(1.0, color="green", linestyle="--", alpha=0.4,
                   label="Perfect symmetry")
        ax.axhline(0.5, color="gray", linestyle=":", alpha=0.4)
        ax.set_ylabel("Symmetry Fraction")
        ax.set_title(f"Reflection Symmetry: {label}")
        ax.legend(fontsize=8, loc="lower right")
        ax.set_ylim(0, 1.05)
        ax.grid(True, alpha=0.3, axis="y")

    axes[-1].set_xticks(x)
    axes[-1].set_xticklabels(patterns, rotation=30, ha="right")
    fig.tight_layout()
    save_fig(fig, out_dir / "reflection_symmetry")


def plot_symmetry_heatmap(sym_results, patterns, out_dir):
    out_dir = Path(out_dir)
    col_labels = []
    for mk in METHODS:
        ml = ac.METHOD_LABELS.get(mk, mk)
        col_labels.append(f"{ml}\nE-W")
        col_labels.append(f"{ml}\nN-S")

    matrix = np.zeros((len(patterns), len(METHODS) * 2))

    for pi, pat in enumerate(patterns):
        for mi, mk in enumerate(METHODS):
            if mk in sym_results.get(pat, {}):
                matrix[pi, mi * 2] = np.mean(sym_results[pat][mk]["ew"])
                matrix[pi, mi * 2 + 1] = np.mean(sym_results[pat][mk]["ns"])

    fig, ax = plt.subplots(figsize=(8, max(4, len(patterns) * 0.7)))
    im = ax.imshow(matrix, aspect="auto", cmap="RdYlGn", vmin=0, vmax=1)

    ax.set_xticks(range(len(col_labels)))
    ax.set_xticklabels(col_labels, fontsize=8)
    ax.set_yticks(range(len(patterns)))
    ax.set_yticklabels(patterns)
    ax.set_title("Directional Reflection Symmetry")

    for yi in range(len(patterns)):
        for xi in range(len(col_labels)):
            ax.text(xi, yi, f"{matrix[yi, xi]:.2f}",
                    ha="center", va="center", fontsize=8,
                    color="white" if matrix[yi, xi] < 0.5 else "black")

    fig.colorbar(im, shrink=0.8, label="Symmetry Score")
    fig.tight_layout()
    save_fig(fig, out_dir / "symmetry_heatmap")
