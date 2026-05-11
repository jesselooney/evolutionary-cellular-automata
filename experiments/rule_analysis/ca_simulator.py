import numpy as np


STANDARD_INPUTS = np.array(
    [[int(b) for b in format(i, "05b")] for i in range(32)], dtype=int
)


def build_lookup(output, inputs=None):
    if inputs is None:
        inputs = STANDARD_INPUTS
    return {tuple(int(v) for v in inputs[i]): int(output[i]) for i in range(32)}


def get_neighborhood(grid, x, y):
    h, w = grid.shape
    n0 = grid[y, x]
    n1 = grid[(y - 1) % h, x]
    n2 = grid[y, (x - 1) % w]
    n3 = grid[y, (x + 1) % w]
    n4 = grid[(y + 1) % h, x]
    return (int(n0), int(n1), int(n2), int(n3), int(n4))


def step_ca(grid, lookup):
    h, w = grid.shape
    new_grid = np.zeros((h, w), dtype=int)

    for y in range(h):
        for x in range(w):
            neighborhood = get_neighborhood(grid, x, y)
            new_grid[y, x] = lookup[neighborhood]

    return new_grid


def grid_error(grid, target):
    return float(np.sum(grid != target)) / grid.size


def run_ca(output, init_grid, target_grid, n_steps, inputs=None):
    lookup = build_lookup(output, inputs)
    grid = init_grid.copy()
    errors = []

    for _ in range(n_steps):
        grid = step_ca(grid, lookup)
        errors.append(grid_error(grid, target_grid))

    return errors


def make_single_seed(h, w, position=(0, 0)):
    grid = np.zeros((h, w), dtype=int)
    x, y = position
    grid[y % h, x % w] = 1
    return grid


def make_random_grid(h, w, density=0.5, seed=None):
    rng = np.random.default_rng(seed)
    return (rng.random((h, w)) < density).astype(int)


def make_sparse_seeds(h, w, n_seeds=3, seed=None):
    rng = np.random.default_rng(seed)
    grid = np.zeros((h, w), dtype=int)
    positions = rng.choice(h * w, size=min(n_seeds, h * w), replace=False)
    for pos in positions:
        grid[pos // w, pos % w] = 1
    return grid


def make_target_pattern(h, w, pattern_name):
    grid = np.zeros((h, w), dtype=int)
    for y in range(h):
        for x in range(w):
            if pattern_name == "checkerboard":
                grid[y, x] = 1 if (x + y) % 2 == 0 else 0
            elif pattern_name == "vertical-stripes":
                grid[y, x] = 1 if x % 2 == 0 else 0
            elif pattern_name == "horizontal-stripes":
                grid[y, x] = 1 if y % 2 == 0 else 0
            elif pattern_name == "sparse-dots":
                grid[y, x] = 1 if (x % 2 == 0 and y % 2 == 0) else 0
            elif pattern_name == "dense-dots":
                grid[y, x] = 1 if (x % 2 == 0 or y % 2 == 0) else 0
            elif pattern_name == "wide-stripes":
                grid[y, x] = 1 if (x % 4) < 2 else 0
            else:
                raise ValueError(f"Unknown pattern: {pattern_name}")
    return grid
