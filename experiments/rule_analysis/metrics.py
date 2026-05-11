import numpy as np
from itertools import combinations, permutations


def langton_lambda(output):
    return output.sum() / len(output)


def input_influence(inputs, output):
    n_inputs = inputs.shape[1]
    influences = np.zeros(n_inputs)

    for i in range(n_inputs):
        mask_cols = [c for c in range(n_inputs) if c != i]
        other_inputs = inputs[:, mask_cols]

        flips = 0
        pairs = 0
        for r in range(len(inputs)):
            for s in range(r + 1, len(inputs)):
                if np.array_equal(other_inputs[r], other_inputs[s]):
                    pairs += 1
                    if output[r] != output[s]:
                        flips += 1
        influences[i] = flips / pairs if pairs > 0 else 0.0

    return influences


def is_fully_neighbor_symmetric(inputs, output):
    lookup = {tuple(row): out for row, out in zip(inputs, output)}

    for perm in permutations(range(1, 5)):
        col_order = [0] + list(perm)
        for row_in, row_out in zip(inputs, output):
            permuted = tuple(row_in[col_order])
            if lookup.get(permuted, row_out) != row_out:
                return False
    return True


def inter_run_agreement(output_list):
    if len(output_list) <= 1:
        return 1.0
    stacked = np.stack(output_list)
    all_same = np.all(stacked == stacked[0:1, :], axis=0)
    return all_same.sum() / stacked.shape[1]


def pairwise_hamming(output_list):
    if len(output_list) <= 1:
        return 0.0
    distances = []
    for i, j in combinations(range(len(output_list)), 2):
        distances.append(np.sum(output_list[i] != output_list[j]))
    return np.mean(distances)


def n_unique_tables(output_list):
    return len(set(tuple(out) for out in output_list))


def cross_approach_hamming(outputs_for_pattern, methods=None):
    if methods is None:
        methods = list(outputs_for_pattern.keys())
    result = {}
    available = [m for m in methods if m in outputs_for_pattern]
    for ma, mb in combinations(available, 2):
        distances = []
        for out_a in outputs_for_pattern[ma]:
            for out_b in outputs_for_pattern[mb]:
                distances.append(np.sum(out_a != out_b))
        result[(ma, mb)] = np.mean(distances)
    return result


def reflection_symmetry_ew(inputs, output):
    lookup = {tuple(row): out for row, out in zip(inputs, output)}
    n_same = 0
    for row, out in zip(inputs, output):
        reflected = (row[0], row[3], row[2], row[1], row[4])
        if lookup[reflected] == out:
            n_same += 1
    return n_same / len(output)


def reflection_symmetry_ns(inputs, output):
    lookup = {tuple(row): out for row, out in zip(inputs, output)}
    n_same = 0
    for row, out in zip(inputs, output):
        reflected = (row[0], row[1], row[4], row[3], row[2])
        if lookup[reflected] == out:
            n_same += 1
    return n_same / len(output)


def is_ew_symmetric(inputs, output):
    return reflection_symmetry_ew(inputs, output) == 1.0


def is_ns_symmetric(inputs, output):
    return reflection_symmetry_ns(inputs, output) == 1.0


def totalistic_deviation(inputs, output):
    sums = inputs.sum(axis=1)
    violations = 0
    per_class_probs = {}

    for s in range(6):
        mask = sums == s
        class_out = output[mask]
        if len(class_out) == 0:
            continue
        per_class_probs[s] = float(class_out.mean())
        majority = 1 if class_out.sum() > len(class_out) / 2 else 0
        violations += int(np.sum(class_out != majority))

    return violations / len(output), per_class_probs


def classify_rule_type(inputs, output):
    dev, _ = totalistic_deviation(inputs, output)

    if dev == 0.0:
        return "totalistic", dev

    if dev <= 3 / 32:
        return "approx_totalistic", dev

    parity_subsets = [
        [0, 1, 2, 3, 4],
        [1, 2, 3, 4],
        [0, 1, 3],
        [0, 2, 4],
        [1, 3],
        [2, 4],
    ]
    for cols in parity_subsets:
        parity = inputs[:, cols].sum(axis=1) % 2
        if np.array_equal(output, parity) or np.array_equal(output, 1 - parity):
            return "parity", dev

    return "arbitrary", dev


def generalization_errors(truth_table_output, pattern_name, init_grids,
                          n_steps=30, grid_size=(10, 10), inputs=None):
    from . import ca_simulator as cas

    h, w = grid_size
    target = cas.make_target_pattern(h, w, pattern_name)
    results = {}

    for ic_name, ic_grid in init_grids.items():
        errors = cas.run_ca(truth_table_output, ic_grid, target, n_steps, inputs)
        results[ic_name] = min(errors)

    return results
