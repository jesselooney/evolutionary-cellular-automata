import sys
from pathlib import Path

import numpy as np

METHODS = ["pca", "nca", "hybrid"]
INPUT_NAMES = ["N0", "N1", "N2", "N3", "N4"]

PROJECT_ROOT = Path(__file__).resolve().parent.parent.parent


def load_truth_table(csv_path):
    data = np.loadtxt(csv_path, delimiter=",", skiprows=1, dtype=int)
    return data[:, :5], data[:, 5]


def discover_truth_tables(base_dir):
    base = Path(base_dir)
    results = {}

    for pattern_dir in sorted(d for d in base.iterdir() if d.is_dir()):
        pattern = pattern_dir.name
        results[pattern] = {}

        for method in METHODS:
            method_dir = pattern_dir / method
            if not method_dir.exists():
                continue
            csvs = sorted(
                method_dir.glob("run_*.csv"),
                key=lambda p: int(p.stem.split("_")[1]),
            )
            if csvs:
                results[pattern][method] = [load_truth_table(csv) for csv in csvs]

    return results


def load_from_experiments(*experiment_names):
    all_data = {}
    for name in experiment_names:
        tt_dir = PROJECT_ROOT / "experiments" / name / "results" / "truth_tables"
        if tt_dir.exists():
            all_data[name] = discover_truth_tables(tt_dir)
        else:
            print(f"Warning: {tt_dir} not found, skipping {name}")
    return all_data


def flatten_outputs(data):
    outputs = {}
    for pattern, methods in data.items():
        outputs[pattern] = {}
        for method, runs in methods.items():
            outputs[pattern][method] = [out for (_, out) in runs]
    return outputs
