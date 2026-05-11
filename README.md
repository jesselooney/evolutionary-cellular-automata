# Evolutionary Cellular Automata

This project explores evolving cellular automata (CA) rules using three genome representations: Push-based (PCA), neural network-based (NCA), and a novel hybrid (HyCA). Rather than manually designing CA update rules, evolutionary algorithms discover rules that produce target patterns — from simple checkerboards and stripes to complex structures like the Sierpinski triangle.

The core question is how different genome representations — discrete Boolean logic (Push), continuous neural networks (CPPN/NEAT), and a hybrid combining both — compare in their ability to evolve local CA rules that produce desired global patterns. The work is described in detail in `docs/writeup/Local_Computation.pdf`.

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

### `evolved-img-vids/`
Generated images and videos of evolved CA behavior.

## Dependencies

- [Clojure](https://clojure.org/) with `deps.edn`
- [Quil](http://quil.info/) 4.3.1563 for visualization
- Python 3 with matplotlib for experiment analysis

## Authors

Henry LeCates, Jesse Looney, Paolo Canigiula, Michael Allers — Amherst College
