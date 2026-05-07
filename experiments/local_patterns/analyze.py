"""Analyze local-patterns experiment: PCA vs NCA on locally-solvable targets."""

import sys
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np

sys.path.insert(0, str(Path(__file__).resolve().parent.parent / "shared"))
import analyze_common as ac

RESULTS_DIR = Path(__file__).parent / "results"
DEFAULT_PATH = RESULTS_DIR / "local_patterns_results.edn"


def plot_convergence_grid(conditions, out_path):
    """Subplot grid: error convergence curves per pattern."""
    n = len(conditions)
    ncols = 3
    nrows = (n + ncols - 1) // ncols
    fig, axes = plt.subplots(nrows, ncols, figsize=(5 * ncols, 4 * nrows), sharey=True)
    axes = axes.flatten() if n > 1 else [axes]

    for i, cond in enumerate(conditions):
        ax = axes[i]
        method_arrays = ac.extract_method_arrays(cond)

        max_len = max(arr[0].shape[0] for arr in method_arrays.values())
        gens = np.arange(max_len)

        for mk, (g, avg, best, steps) in method_arrays.items():
            best = ac.pad_to(best, max_len)
            color = ac.METHOD_COLORS.get(mk)
            label = ac.METHOD_LABELS.get(mk, mk)
            mean, std = best.mean(axis=0), best.std(axis=0)
            ax.plot(gens, mean, label=label, color=color)
            ax.fill_between(gens, mean - std, mean + std, alpha=0.2, color=color)

        ax.set_title(cond["name"])
        ax.set_xlabel("Generation")
        if i % ncols == 0:
            ax.set_ylabel("Best Error")
        ax.legend(fontsize=8)
        ax.grid(True, alpha=0.3)

    for j in range(n, len(axes)):
        axes[j].set_visible(False)

    fig.suptitle("Error Convergence: Locally-Solvable Patterns", fontsize=14)
    fig.tight_layout()
    fig.savefig(out_path, dpi=150, bbox_inches="tight")
    print(f"Saved: {out_path}")
    plt.close(fig)


def plot_final_strip(conditions, out_path):
    """Strip plot: per-seed final best-error scattered by pattern and method."""
    names = [c["name"] for c in conditions]
    method_keys = ac.get_method_keys(conditions[0])
    n_methods = len(method_keys)

    fig, ax = plt.subplots(figsize=(max(8, len(names) * 1.5), 5))
    x = np.arange(len(names))
    width = 0.8 / n_methods
    jitter_scale = width * 0.3

    for i, mk in enumerate(method_keys):
        color = ac.METHOD_COLORS.get(mk)
        label = ac.METHOD_LABELS.get(mk, mk)
        offset = (i - (n_methods - 1) / 2) * width

        for ci, cond in enumerate(conditions):
            _, _, best, _ = ac.runs_to_arrays(cond[mk]["runs"])
            finals = best[:, -1]
            xs = x[ci] + offset + np.random.default_rng(42).uniform(
                -jitter_scale, jitter_scale, size=len(finals)
            )
            ax.scatter(xs, finals, color=color, alpha=0.6, s=28, edgecolors="white",
                       linewidths=0.4, label=label if ci == 0 else None, zorder=3)
            ax.scatter(x[ci] + offset, np.median(finals), color=color, marker="_",
                       s=120, linewidths=2, zorder=4)

    ax.set_xticks(x)
    ax.set_xticklabels(names, rotation=30, ha="right")
    ax.set_ylabel("Final Best Error")
    ax.set_title("Local Patterns: Final Error by Pattern (per seed)")
    ax.legend()
    ax.grid(True, alpha=0.3, axis="y")
    fig.tight_layout()
    fig.savefig(out_path, dpi=150, bbox_inches="tight")
    print(f"Saved: {out_path}")
    plt.close(fig)


def plot_individual_patterns(conditions, out_dir):
    """One plot per pattern: best-error convergence with std band."""
    for cond in conditions:
        name = cond["name"]
        method_arrays = ac.extract_method_arrays(cond)
        max_len = max(arr[0].shape[0] for arr in method_arrays.values())
        gens = np.arange(max_len)

        fig, ax = plt.subplots(figsize=(8, 5))
        for mk, (g, avg, best, steps) in method_arrays.items():
            best = ac.pad_to(best, max_len)
            color = ac.METHOD_COLORS.get(mk)
            label = ac.METHOD_LABELS.get(mk, mk)
            mean, std = best.mean(axis=0), best.std(axis=0)
            ax.plot(gens, mean, label=label, color=color)
            ax.fill_between(gens, mean - std, mean + std, alpha=0.2, color=color)

        ax.set_title(f"Error Convergence: {name}")
        ax.set_xlabel("Generation")
        ax.set_ylabel("Best Error")
        ax.set_ylim(bottom=0)
        ax.legend()
        ax.grid(True, alpha=0.3)
        fig.tight_layout()

        for ext in ("png", "pdf"):
            path = out_dir / f"{name}.{ext}"
            fig.savefig(path, dpi=150, bbox_inches="tight")
            print(f"Saved: {path}")
        plt.close(fig)


def extract_final_complexity(runs, method):
    """Extract final-generation complexity from each run.

    PCA records: program-length
    NCA records: num-hidden-nodes, num-connections, num-active-connections

    Returns a dict of {metric_name: np.array of per-run values}.
    """
    result = {}
    if method == "pca":
        vals = [run[-1].get("program-length", 0) for run in runs]
        result["program-length"] = np.array(vals, dtype=float)
    else:
        for key in ("num-hidden-nodes", "num-connections", "num-active-connections"):
            vals = [run[-1].get(key, 0) for run in runs]
            result[key] = np.array(vals, dtype=float)
    return result


def print_complexity_table(conditions):
    """Print a table comparing final solution complexity across patterns."""
    print("\nSOLUTION COMPLEXITY (final generation, mean +/- std)")
    print("=" * 90)
    print(f"{'Pattern':<22} {'PCA prog-len':>14} "
          f"{'NCA hidden':>12} {'NCA conns':>12} {'NCA active':>12}")
    print("-" * 90)

    for cond in conditions:
        name = cond["name"]
        row = f"{name:<22}"

        if "pca" in cond:
            pca_c = extract_final_complexity(cond["pca"]["runs"], "pca")
            pl = pca_c["program-length"]
            row += f"{pl.mean():>7.1f} +/- {pl.std():>4.1f}"
        else:
            row += f"{'--':>14}"

        if "nca" in cond:
            nca_c = extract_final_complexity(cond["nca"]["runs"], "nca")
            for key in ("num-hidden-nodes", "num-connections", "num-active-connections"):
                v = nca_c[key]
                row += f"  {v.mean():>5.1f}+/-{v.std():>4.1f}"
        else:
            row += f"{'--':>12}" * 3

        print(row)
    print()


def plot_complexity_comparison(conditions, out_path):
    """Bar chart comparing final solution complexity: PCA program length vs NCA active connections."""
    names = [c["name"] for c in conditions]
    n = len(names)

    pca_means, pca_stds = [], []
    nca_means, nca_stds = [], []

    for cond in conditions:
        if "pca" in cond:
            pl = extract_final_complexity(cond["pca"]["runs"], "pca")["program-length"]
            pca_means.append(pl.mean())
            pca_stds.append(pl.std())
        else:
            pca_means.append(0)
            pca_stds.append(0)

        if "nca" in cond:
            ac_ = extract_final_complexity(cond["nca"]["runs"], "nca")["num-active-connections"]
            nca_means.append(ac_.mean())
            nca_stds.append(ac_.std())
        else:
            nca_means.append(0)
            nca_stds.append(0)

    x = np.arange(n)
    width = 0.35
    fig, ax = plt.subplots(figsize=(max(8, n * 1.5), 5))

    ax.bar(x - width / 2, pca_means, width, yerr=pca_stds,
           label="PCA program length", color="tab:blue", alpha=0.8, capsize=3)
    ax.bar(x + width / 2, nca_means, width, yerr=nca_stds,
           label="NCA active connections", color="tab:orange", alpha=0.8, capsize=3)

    ax.set_xticks(x)
    ax.set_xticklabels(names, rotation=30, ha="right")
    ax.set_ylabel("Solution Size")
    ax.set_title("Solution Complexity: PCA Program Length vs NCA Active Connections")
    ax.legend()
    ax.grid(True, alpha=0.3, axis="y")
    fig.tight_layout()
    fig.savefig(out_path, dpi=150, bbox_inches="tight")
    print(f"Saved: {out_path}")
    plt.close(fig)


def summarize_runs(runs):
    """Compute per-run summary stats from raw run histories.

    Returns a list of dicts, one per run, with:
      best_error:  overall minimum best-error across all generations
      best_gen:    generation where that minimum was first reached
      final_error: best-error in the last generation
      n_gens:      total generations run (may be < limit if solved early)
    """
    summaries = []
    for run in runs:
        errors = [rec["best-error"] for rec in run]
        best_err = min(errors)
        best_gen = errors.index(best_err)
        summaries.append({
            "best_error": best_err,
            "best_gen": best_gen,
            "final_error": errors[-1],
            "n_gens": len(run),
        })
    return summaries


def print_performance_table(conditions):
    """Print a detailed per-method table: best error, gen to best, solved rate, complexity."""
    method_keys = ac.get_method_keys(conditions[0])

    for mk in method_keys:
        label = ac.METHOD_LABELS.get(mk, mk)
        print(f"\n{'=' * 95}")
        print(f"  {label} — Performance Summary")
        print(f"{'=' * 95}")
        print(f"  {'Pattern':<22} {'Best Err':>10} {'Gen to Best':>13} "
              f"{'Final Err':>11} {'Solved':>8} {'Complexity':>14}")
        print(f"  {'-' * 90}")

        for cond in conditions:
            name = cond["name"]
            runs = cond[mk]["runs"]
            stats = summarize_runs(runs)
            n_runs = len(stats)

            best_errs = np.array([s["best_error"] for s in stats])
            best_gens = np.array([s["best_gen"] for s in stats])
            final_errs = np.array([s["final_error"] for s in stats])
            n_solved = sum(1 for s in stats if s["best_error"] < 0.05)

            # Gen-to-best breakdown
            n_gen0 = sum(1 for g in best_gens if g == 0)
            non_zero_gens = best_gens[best_gens > 0]

            if len(non_zero_gens) > 0:
                gen_str = f"{non_zero_gens.mean():.1f}+/-{non_zero_gens.std():.1f}"
            else:
                gen_str = "---"

            if n_gen0 > 0:
                gen_str += f" ({n_gen0}@g0)"

            # Complexity
            cplx = extract_final_complexity(runs, mk)
            if mk == "pca":
                cplx_str = f"{cplx['program-length'].mean():.1f} instr"
            else:
                cplx_str = f"{cplx['num-active-connections'].mean():.1f} conns"

            solved_str = f"{n_solved}/{n_runs}"

            print(f"  {name:<22} "
                  f"{best_errs.mean():>5.3f}+/-{best_errs.std():<4.3f} "
                  f"{gen_str:>13} "
                  f"{final_errs.mean():>5.3f}+/-{final_errs.std():<4.3f} "
                  f"{solved_str:>8} "
                  f"{cplx_str:>14}")

        print()


def print_success_rates(conditions):
    """Print fraction of runs reaching error < 0.05 per pattern per method."""
    print("\nSUCCESS RATES (error < 0.05)")
    print("-" * 50)
    method_keys = ac.get_method_keys(conditions[0])
    header = f"{'Pattern':<25}" + "".join(f"{ac.METHOD_LABELS.get(mk, mk):>15}" for mk in method_keys)
    print(header)
    print("-" * len(header))

    for cond in conditions:
        row = f"{cond['name']:<25}"
        for mk in method_keys:
            _, _, best, _ = ac.runs_to_arrays(cond[mk]["runs"])
            n_runs = best.shape[0]
            n_success = sum(1 for run in best if run.min() < 0.05)
            row += f"{n_success}/{n_runs:>8}"
        print(row)


def main():
    path = Path(sys.argv[1]) if len(sys.argv) > 1 else DEFAULT_PATH
    data = ac.load_edn(path)
    conditions = [c for c in data["conditions"] if c["name"] != "diagonal-bands"]
    out_dir = path.parent

    plot_convergence_grid(conditions, out_dir / "convergence_grid.png")
    plot_convergence_grid(conditions, out_dir / "convergence_grid.pdf")
    plot_final_strip(conditions, out_dir / "final_strip.png")
    plot_final_strip(conditions, out_dir / "final_strip.pdf")
    plot_individual_patterns(conditions, out_dir)
    plot_complexity_comparison(conditions, out_dir / "complexity_comparison.png")
    plot_complexity_comparison(conditions, out_dir / "complexity_comparison.pdf")
    print_success_rates(conditions)
    print_performance_table(conditions)
    print_complexity_table(conditions)
    ac.print_experiment_summary(data)


if __name__ == "__main__":
    main()
