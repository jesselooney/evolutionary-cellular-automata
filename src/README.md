# Source

| File | Description |
|------|-------------|
| `cellular_automata.clj` | Core CA utilities — grid creation, next-state computation, neighborhood rules |
| `cppn.clj` | Compositional Pattern Producing Networks (directed acyclic graphs with heterogeneous activation functions) |
| `neat.clj` | NEAT genome construction and mutation operations |
| `evolve_neat.clj` | Evolution operators for NEAT genomes (tournament, lexicase, elitist selection) |
| `evolve_pca.clj` | Evolution operators for Push-based cellular automata |
| `evolve_hybrid.clj` | Hybrid PCA+NCA evolution combining NEAT on CPPN with UMAD on Push programs |
| `push.clj` | Push language implementation (stack-based GP language) |
| `helpers.clj` | Utility functions (cartesian product, taxicab norm, etc.) |
| `grid_draw.clj` | Visualization with Quil — binary and 8-color grid rendering |
| `experiment_runner.clj` | Experiment infrastructure — grid setup, error computation, orchestration |
