import networkx as nx

with open("inputs/25.txt") as f:
    text = f.read()

nodes = set()
edges = set()

for line in text.splitlines():
    name, _, conns = line.partition(": ")
    nodes.add(name)
    for conn in conns.split(" "):
        nodes.add(conn)
        edges.add(tuple(sorted([name, conn])))

G = nx.Graph(edges)
cut_edges = nx.minimum_edge_cut(G)
G.remove_edges_from(cut_edges)
[a, b] = nx.connected_components(G)
print(len(a) * len(b))