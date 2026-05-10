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


def print_summary_table(conditions):
    print("\nCOMPLEX PATTERNS — SINGLE-RUN RESULTS")
    print("=" * 95)
    print(f"  {'Pattern':<22} {'Method':<12} {'Best Err':>10} {'Best Gen':>10} "
          f"{'Best Step':>10} {'Final Err':>10} {'Complexity':>16}")
    print(f"  {'-' * 90}")

    for cond in conditions:
        name = cond["name"]
        method_keys = ac.get_method_keys(cond)

        for mk in method_keys:
            runs = cond[mk]["runs"]
            run = runs[0]
            label = ac.METHOD_LABELS.get(mk, mk)

            errors = [rec["best-error"] for rec in run]
            best_err = min(errors)
            best_gen = errors.index(best_err)
            best_step = run[best_gen]["best-step"]
            final_err = errors[-1]

            last = run[-1]
            if mk == "pca":
                cplx = f"{last.get('program-length', '?')} instr"
            elif mk == "hybrid":
                cplx = (f"{last.get('program-length', '?')}i+"
                        f"{last.get('num-active-connections', '?')}c")
            else:
                cplx = f"{last.get('num-active-connections', '?')} conns"

            print(f"  {name:<22} {label:<12} {best_err:>10.4f} {best_gen:>10} "
                  f"{best_step:>10} {final_err:>10.4f} {cplx:>16}")

    print()


def main():
    path = Path(sys.argv[1]) if len(sys.argv) > 1 else DEFAULT_PATH
    data = ac.load_edn(path)
    conditions = data["conditions"]

    plot_convergence(conditions, RESULTS_DIR / "convergence.png")
    print_summary_table(conditions)
    ac.print_experiment_summary(data)


if __name__ == "__main__":
    main()
