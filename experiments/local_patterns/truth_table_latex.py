"""Generate LaTeX truth table comparing approaches for a given pattern/run."""

import sys
from pathlib import Path

import numpy as np

TRUTH_TABLES_DIR = Path(__file__).parent / "results" / "truth_tables"
METHODS = ["pca", "nca", "hybrid"]
METHOD_HEADERS = {"pca": "PCA", "nca": "NCA", "hybrid": "HyCA"}


def load_truth_table(csv_path):
    data = np.loadtxt(csv_path, delimiter=",", skiprows=1, dtype=int)
    return data[:, :5], data[:, 5]


def neighborhood_tikz(n0, n1, n2, n3, n4):
    """Render a Von Neumann neighborhood as a tiny TikZ cross.

    Layout:     N1
              N2 N0 N3
                N4
    Filled squares = 1, empty squares = 0.
    """
    def cell(x, y, val):
        fill = "black" if val else "white"
        return (f"\\fill[{fill}, draw=black, line width=0.2pt] "
                f"({x},{y}) rectangle ++(0.22,0.22);")

    lines = [
        "\\begin{tikzpicture}[baseline=-0.5ex, scale=0.55]",
        cell(0.24, 0.48, n1),   # top
        cell(0, 0.24, n2),      # left
        cell(0.24, 0.24, n0),   # center
        cell(0.48, 0.24, n3),   # right
        cell(0.24, 0, n4),      # bottom
        "\\end{tikzpicture}",
    ]
    return " ".join(lines)


def generate_latex(pattern, run, base_dir):
    tables = {}
    for mk in METHODS:
        path = base_dir / pattern / mk / f"run_{run}.csv"
        _, out = load_truth_table(path)
        tables[mk] = out

    # reference inputs from whichever table (all identical input columns)
    inputs, _ = load_truth_table(base_dir / pattern / METHODS[0] / f"run_{run}.csv")

    n_disagree = sum(
        1 for i in range(32)
        if not (tables["pca"][i] == tables["nca"][i] == tables["hybrid"][i])
    )

    lines = []
    lines.append("% Requires: \\usepackage{tikz, booktabs}")
    lines.append("")
    lines.append("\\begin{table}[htbp]")
    lines.append("\\centering")
    lines.append("\\caption{Truth tables for \\texttt{" + pattern +
                  "} (run " + str(run) + "). "
                  f"Approaches disagree on {n_disagree}/32 inputs.}}")
    lines.append("\\label{tab:truth-table-" + pattern + "}")
    lines.append("\\vspace{6pt}")
    lines.append("\\small")
    lines.append("\\begin{tabular}{c @{\\hskip 12pt} c @{\\hskip 12pt} c @{\\hskip 12pt} c}")
    lines.append("\\toprule")
    lines.append("Neighborhood & PCA & NCA & HyCA \\\\")
    lines.append("\\midrule")

    for i in range(32):
        n0, n1, n2, n3, n4 = inputs[i]
        tikz = neighborhood_tikz(n0, n1, n2, n3, n4)

        pca_v = int(tables["pca"][i])
        nca_v = int(tables["nca"][i])
        hyb_v = int(tables["hybrid"][i])

        lines.append(f"{tikz} & {pca_v} & {nca_v} & {hyb_v} \\\\")

        # visual separator every 8 rows
        if i % 8 == 7 and i < 31:
            lines.append("\\addlinespace[2pt]")

    lines.append("\\bottomrule")
    lines.append("\\end{tabular}")
    lines.append("\\end{table}")

    return "\n".join(lines)


def main():
    pattern = sys.argv[1] if len(sys.argv) > 1 else "sparse-dots"
    run = int(sys.argv[2]) if len(sys.argv) > 2 else 0
    base_dir = Path(sys.argv[3]) if len(sys.argv) > 3 else TRUTH_TABLES_DIR

    latex = generate_latex(pattern, run, base_dir)

    out_path = base_dir / f"truth_table_{pattern}_run{run}.tex"
    with open(out_path, "w") as f:
        f.write(latex)
    print(f"Saved: {out_path}")
    print()
    print(latex)


if __name__ == "__main__":
    main()
