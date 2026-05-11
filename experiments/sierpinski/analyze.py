"""Analyze complex-patterns experiment: PCA vs NCA vs Hybrid on fractal targets."""

import sys
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np

sys.path.insert(0, str(Path(__file__).resolve().parent.parent / "shared"))
import analyze_common as ac

RESULTS_DIR = Path(__file__).parent / "results"
DEFAULT_PATH = RESULTS_DIR / "complex_patterns_results.edn"


# single-run convergence curves per method
def plot_convergence(conditions, out_path):
    for cond in conditions:
        name = cond["name"]
        method_arrays = ac.extract_method_arrays(cond)
        max_len = max(arr[0].shape[0] for arr in method_arrays.values())
        gens = np.arange(max_len)

        fig, ax = plt.subplots(figsize=(10, 6))
        for mk, (g, avg, best, steps) in method_arrays.items():
            best = ac.pad_to(best, max_len)
            color = ac.METHOD_COLORS.get(mk)
            label = ac.METHOD_LABELS.get(mk, mk)
            ax.plot(gens, best[0], label=label, color=color, linewidth=1.5)

        ax.set_title(f"Error Convergence: {name} (30x30 grid)")
        ax.set_xlabel("Generation")
        ax.set_ylabel("Best Error")
        ax.set_ylim(bottom=0)
        ax.legend()
        ax.grid(True, alpha=0.3)
        fig.tight_layout()

        for ext in ("png", "pdf"):
            path = out_path.parent / f"convergence_{name}.{ext}"
            fig.savefig(path, dpi=150, bbox_inches="tight")
            print(f"Saved: {path}")
        plt.close(fig)


def main():
    path = Path(sys.argv[1]) if len(sys.argv) > 1 else DEFAULT_PATH
    data = ac.load_edn(path)
    conditions = data["conditions"]

    plot_convergence(conditions, RESULTS_DIR / "convergence.png")


if __name__ == "__main__":
    main()
