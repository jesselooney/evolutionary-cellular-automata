"""Shared analysis utilities for PCA vs NCA comparison experiments."""

import sys
from pathlib import Path

import edn_format
import matplotlib.pyplot as plt
import numpy as np

# ---------------------------------------------------------------------------
# EDN loading
# ---------------------------------------------------------------------------


def _convert(obj):
    """Recursively convert edn_format types to plain Python types."""
    if isinstance(obj, edn_format.Keyword):
        return obj.name
    if isinstance(obj, edn_format.Symbol):
        return str(obj)
    if hasattr(obj, "keys"):
        return {_convert(k): _convert(v) for k, v in obj.items()}
    if isinstance(obj, (list, tuple)) or type(obj).__name__ == "ImmutableList":
        return [_convert(x) for x in obj]
    return obj


def load_edn(path):
    """Load an EDN file and return plain Python dicts/lists."""
    with open(path) as f:
        raw = edn_format.loads(f.read())
    return _convert(raw)


# ---------------------------------------------------------------------------
# Array conversion
# ---------------------------------------------------------------------------


def runs_to_arrays(runs):
    """Convert list-of-runs into per-generation numpy arrays.

    Returns:
        gens:        1-d array [0, 1, ..., max_gen]
        avg_errors:  (n_runs, n_gens)
        best_errors: (n_runs, n_gens)
        best_steps:  (n_runs, n_gens)
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
        if len(run) < max_len:
            avg_errors[i, len(run) :] = avg_errors[i, len(run) - 1]
            best_errors[i, len(run) :] = best_errors[i, len(run) - 1]
            best_steps[i, len(run) :] = best_steps[i, len(run) - 1]

    return np.arange(max_len), avg_errors, best_errors, best_steps


def pad_to(arr, length):
    """Pad a 2-d array along axis=1 to `length` using edge values."""
    if arr.shape[1] >= length:
        return arr
    pad_width = length - arr.shape[1]
    return np.pad(arr, ((0, 0), (0, pad_width)), mode="edge")


# ---------------------------------------------------------------------------
# Method data extraction
# ---------------------------------------------------------------------------

METHOD_COLORS = {
    "pca": "tab:blue",
    "nca": "tab:orange",
    "nca-local": "tab:orange",
    "nca-position": "tab:green",
}

METHOD_LABELS = {
    "pca": "PCA (Push)",
    "nca": "NCA (NEAT)",
    "nca-local": "NCA-local",
    "nca-position": "NCA-position",
}


def get_method_keys(condition):
    """Return the method keys present in a condition dict."""
    skip = {"name", "config-overrides"}
    return [k for k in condition if k not in skip]


def extract_method_arrays(condition):
    """Extract (gens, best, avg, steps) arrays for each method in a condition.

    Returns dict of {method_key: (gens, avg, best, steps)}.
    """
    result = {}
    for mk in get_method_keys(condition):
        gens, avg, best, steps = runs_to_arrays(condition[mk]["runs"])
        result[mk] = (gens, avg, best, steps)
    return result


# ---------------------------------------------------------------------------
# Plotting: error curves (per-condition)
# ---------------------------------------------------------------------------


def plot_error_curves(gens, method_data, out_path, title=None):
    """Plot best-error and avg-error over generations for multiple methods.

    method_data: dict of {method_key: (best_2d, avg_2d)}
    """
    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(14, 5), sharey=True)

    for ax, metric_idx, subtitle in [
        (ax1, 0, "Best Error per Generation"),
        (ax2, 1, "Avg Population Error per Generation"),
    ]:
        for mk, (best, avg) in method_data.items():
            data = best if metric_idx == 0 else avg
            color = METHOD_COLORS.get(mk, None)
            label = METHOD_LABELS.get(mk, mk)
            mean, std = data.mean(axis=0), data.std(axis=0)
            ax.plot(gens, mean, label=label, color=color)
            ax.fill_between(gens, mean - std, mean + std, alpha=0.2, color=color)
        ax.set_xlabel("Generation")
        ax.set_ylabel("Error (0 = perfect)")
        ax.set_title(subtitle)
        ax.legend()
        ax.grid(True, alpha=0.3)

    if title:
        fig.suptitle(title, fontsize=14, y=1.02)
    fig.tight_layout()
    fig.savefig(out_path, dpi=150, bbox_inches="tight")
    print(f"Saved: {out_path}")
    plt.close(fig)


# ---------------------------------------------------------------------------
# Plotting: best step
# ---------------------------------------------------------------------------


def plot_best_step(gens, method_data, out_path, title=None):
    """Plot CA step of best individual over generations.

    method_data: dict of {method_key: steps_2d}
    """
    fig, ax = plt.subplots(figsize=(8, 5))

    for mk, steps in method_data.items():
        color = METHOD_COLORS.get(mk, None)
        label = METHOD_LABELS.get(mk, mk)
        mean, std = steps.mean(axis=0), steps.std(axis=0)
        ax.plot(gens, mean, label=label, color=color)
        ax.fill_between(gens, mean - std, mean + std, alpha=0.2, color=color)

    ax.set_xlabel("Generation")
    ax.set_ylabel("Best CA Step")
    ax.set_title(title or "CA Steps to Best Grid Match")
    ax.legend()
    ax.grid(True, alpha=0.3)
    fig.tight_layout()
    fig.savefig(out_path, dpi=150, bbox_inches="tight")
    print(f"Saved: {out_path}")
    plt.close(fig)


# ---------------------------------------------------------------------------
# Plotting: grouped bar comparison
# ---------------------------------------------------------------------------


def plot_bar_comparison(condition_names, method_means, method_stds, out_path,
                        ylabel="Final Best Error", title=None):
    """Grouped bar chart comparing a metric across conditions and methods.

    condition_names: list of str
    method_means:  dict of {method_key: array of means (one per condition)}
    method_stds:   dict of {method_key: array of stds (one per condition)}
    """
    n_conditions = len(condition_names)
    n_methods = len(method_means)
    x = np.arange(n_conditions)
    width = 0.8 / n_methods

    fig, ax = plt.subplots(figsize=(max(8, n_conditions * 1.5), 5))

    for i, mk in enumerate(method_means):
        color = METHOD_COLORS.get(mk, None)
        label = METHOD_LABELS.get(mk, mk)
        offset = (i - (n_methods - 1) / 2) * width
        ax.bar(x + offset, method_means[mk], width, yerr=method_stds[mk],
               label=label, color=color, alpha=0.85, capsize=3)

    ax.set_xticks(x)
    ax.set_xticklabels(condition_names, rotation=30, ha="right")
    ax.set_ylabel(ylabel)
    if title:
        ax.set_title(title)
    ax.legend()
    ax.grid(True, alpha=0.3, axis="y")
    fig.tight_layout()
    fig.savefig(out_path, dpi=150, bbox_inches="tight")
    print(f"Saved: {out_path}")
    plt.close(fig)


# ---------------------------------------------------------------------------
# Plotting: line comparison (for scaling experiments)
# ---------------------------------------------------------------------------


def plot_line_comparison(x_values, x_label, method_means, method_stds, out_path,
                         ylabel="Final Best Error", title=None):
    """Line plot comparing a metric across a numeric axis for multiple methods."""
    fig, ax = plt.subplots(figsize=(8, 5))

    for mk in method_means:
        color = METHOD_COLORS.get(mk, None)
        label = METHOD_LABELS.get(mk, mk)
        means = np.array(method_means[mk])
        stds = np.array(method_stds[mk])
        ax.plot(x_values, means, "o-", label=label, color=color)
        ax.fill_between(x_values, means - stds, means + stds, alpha=0.2, color=color)

    ax.set_xlabel(x_label)
    ax.set_ylabel(ylabel)
    if title:
        ax.set_title(title)
    ax.legend()
    ax.grid(True, alpha=0.3)
    fig.tight_layout()
    fig.savefig(out_path, dpi=150, bbox_inches="tight")
    print(f"Saved: {out_path}")
    plt.close(fig)


# ---------------------------------------------------------------------------
# Summary printing
# ---------------------------------------------------------------------------


def print_condition_summary(condition, n_runs=None):
    """Print summary stats for one condition."""
    name = condition["name"]
    print(f"\n  --- {name} ---")

    for mk in get_method_keys(condition):
        section = condition[mk]
        _, _, best, steps = runs_to_arrays(section["runs"])
        if n_runs is None:
            n_runs = best.shape[0]
        overall = section["best-overall"]

        label = METHOD_LABELS.get(mk, mk)
        final_best = best[:, -1]
        final_step = steps[:, -1]

        print(f"    [{label}]")
        print(f"      Final best-error:  {final_best.mean():.4f} +/- {final_best.std():.4f}")
        print(f"      Final best-step:   {final_step.mean():.1f} +/- {final_step.std():.1f}")
        print(f"      Overall best:      {overall['error']} "
              f"(gen {overall['generation']}, step {overall['step']})")

        for thresh in [0.4, 0.3, 0.2, 0.1, 0.05]:
            reached = []
            for run in best:
                idxs = np.where(run < thresh)[0]
                if len(idxs) > 0:
                    reached.append(idxs[0])
            if reached:
                print(f"      Gen to <{thresh}:      "
                      f"{np.mean(reached):.1f} +/- {np.std(reached):.1f}  "
                      f"({len(reached)}/{n_runs} runs)")
            else:
                print(f"      Gen to <{thresh}:      never  (0/{n_runs} runs)")


def print_experiment_summary(data):
    """Print full experiment summary."""
    cfg = data.get("base-config", data.get("config", {}))
    conditions = data["conditions"]

    print("\n" + "=" * 60)
    print(f"  EXPERIMENT: {data['experiment']}")
    print("=" * 60)
    for key in ["grid-limits", "population-size", "generation-limit", "ca-steps"]:
        if key in cfg:
            print(f"  {key}: {cfg[key]}")

    n_runs_sample = None
    for cond in conditions:
        for mk in get_method_keys(cond):
            n_runs_sample = len(cond[mk]["runs"])
            break
        if n_runs_sample:
            break
    if n_runs_sample:
        print(f"  runs: {n_runs_sample}")

    for cond in conditions:
        print_condition_summary(cond, n_runs_sample)

    print("\n" + "=" * 60)
