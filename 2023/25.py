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
[a, b] = nx.k_edge_components(G, 4) 
print(len(a) * len(b))