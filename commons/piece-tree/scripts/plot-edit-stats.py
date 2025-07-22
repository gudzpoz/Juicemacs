import argparse
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt


parser = argparse.ArgumentParser()
parser.add_argument('--piece-tree-csv', type=str, required=True)
parser.add_argument('--gap-buffer-csv', type=str, required=True)
parser.add_argument('--ropes-csv', type=str, required=True)
parser.add_argument('--string-csv', type=str, required=True)
parser.add_argument('--truffle-string-csv', type=str, required=True)
args = parser.parse_args()

csv = pd.read_csv(args.piece_tree_csv)
gap_csv = pd.read_csv(args.gap_buffer_csv)
ropes_csv = pd.read_csv(args.ropes_csv)
string_csv = pd.read_csv(args.string_csv)
truffle_csv = pd.read_csv(args.truffle_string_csv)


def plot_latency(csvs: list[pd.DataFrame], names: list[str]):
    fig, axes = plt.subplots(len(csvs), 1, figsize=(10, 10))
    for name, csv, ax in zip(names, csvs, axes):
        stats = {
            f: np.array(csv[csv['trace'] == f]['latency'], dtype=float) / 1000
            for f in sorted(set(csv['trace']))
        }
        ax.boxplot([
            np.log10(stats[f])
            for f in sorted(set(csv['trace']))
        ], vert=False)
        ax.set_yticklabels(sorted(f[:-len('.json.gz')] for f in set(csv['trace'])))
        ax.set_ylabel('Trace')
        ax.set_xlabel('Latency (μs)')
        ax.set_xticks(np.arange(-1, 6))
        ax.set_xticklabels(10.0**np.arange(-1, 6))
        ax.set_title(name)
        for f, latencies in stats.items():
            print(
                f'{name}:\t'
                f'{f[:-len(".json.gz")]}:\t{np.mean(latencies):.3f} ± {np.std(latencies):.3f} (μs)'
                f'(total: {np.sum(latencies) / 1000:.3f} ms, {len(latencies)} samples) '
                f'(95th: {np.percentile(latencies, 95):.3f} μs, 99th: {np.percentile(latencies, 99):.3f} μs)'
            )
    fig.show()


plot_latency(
    [csv, gap_csv, ropes_csv, string_csv, truffle_csv],
    ['Piece Tree', 'Gap Buffer', 'Ropes', 'Strings', 'TString'],
)
plt.show()
