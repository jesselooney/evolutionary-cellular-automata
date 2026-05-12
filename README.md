# Evolutionary Cellular Automata

This project explores evolving cellular automata (CA) rules using three genome representations: Push-based (PCA), neural network-based (NCA), and a novel hybrid (HyCA). Rather than manually designing CA update rules, evolutionary algorithms discover rules that produce target patterns, from simple checkerboards and stripes to complex structures like the Sierpinski triangle.

The core question is how different genome representations, discrete Boolean logic (Push), continuous neural networks (CPPN/NEAT), and a hybrid combining both, compare in their ability to evolve local CA rules that produce desired global patterns. This mirrors a fundamental feature of biological development: multicellular organisms frequently combine binary-state mechanisms (gene on/off switches) with continuous regulatory networks (chemical gradients) to control cell processes. Restricting local computation to only one of these mechanisms, as PCA and NCA do, limits adaptability, much as a cell with only a light switch or only a dimmer would struggle to regulate complex behavior. The hybrid representation (HyCA) combines both, using its CPPN as a continuous signal that the Push program can threshold against to gate further updates, separating construction logic from stability control. This parallels how external chemical signals regulate gene networks in biological cells, and makes HyCA the only representation studied that learns, from purely local information, both to build and to preserve a complex pattern like the Sierpinski triangle.

The generalization experiments further connect to open questions in developmental biology: evolved rules tend to learn specific growth trajectories rather than true local update rules, failing when run from different starting conditions. This raises the same questions as morphogenesis and self-repair, how a heterogeneous body grows from a single cell, and how organisms know to regrow damaged structures correctly. The work is described in detail in `docs/writeup/Local_Computation.pdf`.

## Project Structure

### `src/`
Clojure source code for the CA engine, genome representations, and evolution.
- `cellular_automata.clj` — Grid creation, next-state computation, neighborhood definitions (Moore, von Neumann)
- `cppn.clj` — Compositional Pattern Producing Networks with heterogeneous activation functions
- `neat.clj` — NEAT genome construction and mutation operators
- `push.clj` — Push language implementation (stack-based genetic programming)
- `evolve_neat.clj` — Evolution operators for NEAT genomes (tournament, lexicase, elitist selection)
- `evolve_pca.clj` — Evolution operators for Push-based cellular automata
- `evolve_hybrid.clj` — Hybrid evolution combining NEAT/CPPN with UMAD on Push programs
- `experiment_runner.clj` — Experiment infrastructure, grid setup, error computation
- `grid_draw.clj` — Visualization via Quil (binary and 8-color grid rendering)
- `helpers.clj` — Utility functions

### `experiments/`
Experiment configurations (Clojure) and analysis scripts (Python).
- `simple/` — Basic pattern comparisons (checkerboard, stripes) across all three methods
- `local_patterns/` — Local pattern generation with truth table extraction and analysis
- `sierpinski/` — Sierpinski triangle pattern evolution
- `thue_morse/` — Thue-Morse sequence pattern evolution
- `generalization/` — Testing evolved rules' generalization across grid sizes
- `rule_analysis/` — Rule symmetry and totalistic property analysis
- `shared/` — Common Python analysis utilities

### `examples/`
Standalone runnable examples demonstrating specific patterns.
- `checkerboard_neat.clj`, `sierpinski.clj`, `life.clj`, `hybrid_test.clj`

### `docs/`
- `writeup/` — Project writeup: *Local Computation: Genome Choice and Evolved Cellular Automata Rules*
- `papers/` — Referenced literature (CA-NEAT)

### `scratch/`
Development and prototyping code for Push-based CA experiments.