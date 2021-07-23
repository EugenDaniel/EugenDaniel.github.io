import pandas as pd
from datetime import *

from collections import defaultdict

# Starting time
begin_time = datetime.now()
print('This script startet at {}'.format(datetime.now()))

# Reading the data
data = pd.read_csv('twitter_10M.csv')

# Creation of a function to iterate over the values
def DFS(G,v,seen=None,path=None):
    if seen is None: seen = []
    if path is None: path = [v]

    seen.append(v)
    paths = []
    for t in G[v]:
        if t not in seen:
            t_path = path + [t]
            paths.append(tuple(t_path))
            paths.extend(DFS(G, t, seen[:], t_path))
    return paths


# Creating a column with the path
data['path'] = data.values.tolist()

# Build graph dictionary
G = defaultdict(list)
for s in data['path']:
    G[s[0]].append(s[1])


# Runing DFS and computing metrics
all_paths = DFS(G, 20)
print(all_paths)
max_len   = max(len(p) for p in all_paths)
max_paths = [p for p in all_paths if len(p) == max_len]

# Output
print("All Paths:")
print(all_paths)
print("Longest Paths:")
for p in max_paths: print("  ", p)
print("Longest Path Length:")
print(max_len)

print('This script took {} to execute'.format(datetime.now()-begin_time))