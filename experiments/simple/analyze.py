"""Analyze stripe_comparison_results.edn — PCA vs NCA comparison."""

import sys
from pathlib import Path

import edn_format
import matplotlib.pyplot as plt
import numpy as np

# ---------------------------------------------------------------------------
# Load EDN
# ---------------------------------------------------------------------------

RESULTS_PATH = Path(__file__).parent / "stripe_comparison_results.edn"


def load_results(path=RESULTS_PATH):
    with open(path) as f:
        raw = edn_format.loads(f.read())
    # edn_format returns Keyword keys — convert to plain strings
    return _convert(raw)


def _convert(obj):
    """Recursively convert edn_format types to plain Python types."""
    if isinstance(obj, edn_format.Keyword):
        return obj.name
    if isinstance(obj, edn_format.Symbol):
        return str(obj)
    if hasattr(obj, 'keys'):
        return {_convert(k): _convert(v) for k, v in obj.items()}
    if isinstance(obj, (list, tuple)) or type(obj).__name__ == 'ImmutableList':
        return [_convert(x) for x in obj]
    return obj


# ---------------------------------------------------------------------------
# Extract per-generation arrays
# ---------------------------------------------------------------------------


def runs_to_arrays(runs):
    """Convert list-of-runs into per-generation numpy arrays.

    Returns:
        gens: 1-d array of generation indices (length = max gen across runs)
        avg_errors:  (n_runs, n_gens) — population avg error per gen
        best_errors: (n_runs, n_gens) — best individual error per gen
        best_steps:  (n_runs, n_gens) — CA step of best individual per gen
    """
    n_runs = len(runs)
    max_len = max(len(r) for r in runs)

    avg_errors = np.full((n_runs, max_len), np.nan)
    best_errors = np.full((n_runs, max_len), np.nan)
    best_steps = np.full((n_runs, max_len), np.nan)

    for i, run in enumerate(runs):
        for j, rec in enumerate(run):
            avg_errors[i, j] = rec["avg-error"]
            best_errors[i, j] = rec["best-error"]
            best_steps[i, j] = rec["best-step"]
        # Pad shorter runs with their final values
        if len(run) < max_len:
            avg_errors[i, len(run):] = avg_errors[i, len(run) - 1]
            best_errors[i, len(run):] = best_errors[i, len(run) - 1]
            best_steps[i, len(run):] = best_steps[i, len(run) - 1]

    gens = np.arange(max_len)
    return gens, avg_errors, best_errors, best_steps


# ---------------------------------------------------------------------------
# Plotting
# ---------------------------------------------------------------------------


def plot_error_curves(gens, pca_best, nca_best, pca_avg, nca_avg, out_path):
    """Best-error and avg-error over generations, mean +/- std across runs."""
    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(14, 5), sharey=True)

    for ax, pca, nca, title in [
        (ax1, pca_best, nca_best, "Best Error per Generation"),
        (ax2, pca_avg, nca_avg, "Avg Population Error per Generation"),
    ]:
        pca_mean, pca_std = pca.mean(axis=0), pca.std(axis=0)
        nca_mean, nca_std = nca.mean(axis=0), nca.std(axis=0)

        ax.plot(gens, pca_mean, label="PCA (Push)", color="tab:blue")
        ax.fill_between(gens, pca_mean - pca_std, pca_mean + pca_std,
                         alpha=0.2, color="tab:blue")
        ax.plot(gens, nca_mean, label="NCA (NEAT)", color="tab:orange")
        ax.fill_between(gens, nca_mean - nca_std, nca_mean + nca_std,
                         alpha=0.2, color="tab:orange")
        ax.set_xlabel("Generation")
        ax.set_ylabel("Error (0 = perfect)")
        ax.set_title(title)
        ax.legend()
        ax.grid(True, alpha=0.3)

    fig.tight_layout()
    fig.savefig(out_path, dpi=150)
    print(f"Saved: {out_path}")
    plt.close(fig)


def plot_best_step(gens, pca_steps, nca_steps, out_path):
    """CA step at which best individual peaks, over generations."""
    fig, ax = plt.subplots(figsize=(8, 5))

    for data, label, color in [
        (pca_steps, "PCA (Push)", "tab:blue"),
        (nca_steps, "NCA (NEAT)", "tab:orange"),
    ]:
        mean = data.mean(axis=0)
        std = data.std(axis=0)
        ax.plot(gens, mean, label=label, color=color)
        ax.fill_between(gens, mean - std, mean + std, alpha=0.2, color=color)

    ax.set_xlabel("Generation")
    ax.set_ylabel("Best CA Step")
    ax.set_title("CA Steps to Best Grid Match")
    ax.legend()
    ax.grid(True, alpha=0.3)
    fig.tight_layout()
    fig.savefig(out_path, dpi=150)
    print(f"Saved: {out_path}")
    plt.close(fig)


def print_summary(data, pca_best, nca_best, pca_steps, nca_steps):
    """Print a compact summary table."""
    cfg = data["config"]
    n_runs = len(data["pca"]["runs"])

    print("\n" + "=" * 55)
    print("STRIPE COMPARISON SUMMARY")
    print("=" * 55)
    print(f"  Target:      {data['target']}")
    print(f"  Grid:        {cfg['grid-limits']}")
    print(f"  Runs:        {n_runs}")
    print(f"  Pop size:    {cfg['population-size']}")
    print(f"  Generations: {cfg['generation-limit']}")
    print(f"  CA steps:    {cfg['ca-steps']}")
    print()

    for label, best, steps, section in [
        ("PCA (Push)", pca_best, pca_steps, data["pca"]),
        ("NCA (NEAT)", nca_best, nca_steps, data["nca"]),
    ]:
        final_best = best[:, -1]
        final_step = steps[:, -1]
        overall = section["best-overall"]
        print(f"  --- {label} ---")
        print(f"    Final best-error:  {final_best.mean():.3f} +/- {final_best.std():.3f}")
        print(f"    Final best-step:   {final_step.mean():.1f} +/- {final_step.std():.1f}")
        print(f"    Overall best:      {overall['error']} (gen {overall['generation']}, step {overall['step']})")

        # Generations to reach threshold
        for thresh in [0.4, 0.3, 0.2, 0.1]:
            reached = []
            for run in best:
                idxs = np.where(run < thresh)[0]
                if len(idxs) > 0:
                    reached.append(idxs[0])
            if reached:
                print(f"    Gen to <{thresh}:      {np.mean(reached):.1f} +/- {np.std(reached):.1f}  ({len(reached)}/{n_runs} runs)")
            else:
                print(f"    Gen to <{thresh}:      never  (0/{n_runs} runs)")
        print()

    print("=" * 55)


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------


def main():
    path = Path(sys.argv[1]) if len(sys.argv) > 1 else RESULTS_PATH
    data = load_results(path)

    pca_gens, pca_avg, pca_best, pca_steps = runs_to_arrays(data["pca"]["runs"])
    nca_gens, nca_avg, nca_best, nca_steps = runs_to_arrays(data["nca"]["runs"])

    # Pad both to the longer generation range (runs may converge at different speeds)
    max_len = max(len(pca_gens), len(nca_gens))
    gens = np.arange(max_len)

    def pad_to(arr, length):
        if arr.shape[1] >= length:
            return arr
        pad_width = length - arr.shape[1]
        return np.pad(arr, ((0, 0), (0, pad_width)), mode="edge")

    pca_avg, pca_best, pca_steps = pad_to(pca_avg, max_len), pad_to(pca_best, max_len), pad_to(pca_steps, max_len)
    nca_avg, nca_best, nca_steps = pad_to(nca_avg, max_len), pad_to(nca_best, max_len), pad_to(nca_steps, max_len)

    out_dir = path.parent
    stem = path.stem.removesuffix("_results")  # e.g. "checkerboard_comparison"
    plot_error_curves(gens, pca_best, nca_best, pca_avg, nca_avg,
                       out_dir / f"{stem}_error_curves.png")
    plot_best_step(gens, pca_steps, nca_steps,
                    out_dir / f"{stem}_best_step.png")
    print_summary(data, pca_best, nca_best, pca_steps, nca_steps)


if __name__ == "__main__":
    main()
