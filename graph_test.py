
"""Quick Python script to test the connectivity of the London
Underground.

Conclusion: The London Underground is very much not a diameter 2
graph.

"""

from __future__ import annotations

import re
import networkx as nx
from collections import defaultdict
import itertools
import matplotlib.pyplot as plt
from dataclasses import dataclass
from queue import PriorityQueue
import json


@dataclass(frozen=True, kw_only=True)
class PathNode:
    line: str
    node: str


# We can't use these as middle hops in our shortest path algorithm,
# because they had side effects.
SPECIAL_NODES = frozenset((
    'Upminster',
    'Chalfont & Latimer',
    'Cannon Street',
    'Preston Road',
    'Bounds Green',
    'Manor House',
    'Holland Park',
    'Turnham Green',
    'Stepney Green',
    'Russell Square',
    'Notting Hill Gate',
    'Parsons Green',
    'Seven Sisters',
    'Charing Cross',
    'Paddington',
    'Gunnersbury',
    'Mile End',
    'Upney',
    'Hounslow Central',
    'Turnpike Lane',
    'Bank',
    'Hammersmith',
    'Temple',
    'Angel',
    'Marble Arch',
    'Mornington Crescent',
))


def load_file() -> dict[str, list[str]]:
    with open('lines.txt') as f:
        next(f)  # Skip header
        results = {}
        for line in f:
            m = re.match(r'^[^\[]*', line)
            assert m
            key = m.group(0).strip()
            values = [m.group(0)[1:-1] for m in re.finditer(r'\[[^\]]*\]', line)]
            results[key] = values
        return results


def build_graph(lines_map: dict[str, list[str]]) -> nx.Graph[str]:
    '''Connect every node to every other node that shares a line with
    it.

    '''
    graph: nx.Graph[str] = nx.Graph()
    graph.add_nodes_from(lines_map.keys())

    indexed_by_line = defaultdict(list)
    for node, lines in lines_map.items():
        for line in lines:
            indexed_by_line[line].append(node)

    for line, nodes in indexed_by_line.items():
        for a, b in itertools.permutations(nodes, 2):
            graph.add_edge(a, b, line=line)

    return graph


def graph_has_diameter_2(lines_map: dict[str, list[str]]) -> bool:
    okay = True
    for src, src_lines in lines_map.items():
        for dest, dest_lines in lines_map.items():
            if set(src_lines) & set(dest_lines):
                # There's a direct line in common, so we're good
                continue
            # Look for a line that is a 2-hop path
            for middle_node, middle_lines in lines_map.items():
                if set(src_lines) & set(middle_lines) and set(dest_lines) & set(middle_lines):
                    break
            else:
                print(f"{src} and {dest} are not 2-hop connected")
                okay = False
    return okay


def dijkstra(graph: nx.Graph[str], src: str) -> dict[str, list[PathNode]]:
    results: dict[str, list[PathNode]] = {}
    frontier: PriorityQueue[tuple[int, str]] = PriorityQueue()

    results[src] = []
    frontier.put((0, src))

    while not frontier.empty():
        _, curr = frontier.get()
        if curr in SPECIAL_NODES:
            # We can't use the special nodes as midpoints in our
            # paths, because they have side effects. They can (and
            # will) be the endpoint but they can't be the midpoint.
            continue
        line: str
        for _, dest, line in graph.edges(curr, data='line'):
            if dest not in results or len(results[dest]) > len(results[curr]) + 1:
                if dest not in results:
                    frontier.put((len(results[curr]) + 1, dest))
                results[dest] = results[curr] + [PathNode(line=line, node=dest)]

    return results


def all_dijkstra(graph: nx.Graph[str]) -> dict[str, dict[str, list[PathNode]]]:
    results = {}
    for src in graph.nodes():
        results[src] = dijkstra(graph, src)
    return results


def custom_json(obj: object):
    if isinstance(obj, PathNode):
        return {'line': obj.line, 'node': obj.node}
    raise TypeError(f"Cannot serialize object of type {type(obj)}")


mapping = load_file()
graph = build_graph(mapping)
print(graph)
print(nx.diameter(graph))

paths = all_dijkstra(graph)
print(paths['Barkingside']['Woodside Park'])

with open('allpaths.json', 'w') as f:
    json.dump(paths, f, default=custom_json)

nx.draw(graph)
plt.show()
