from math import inf
from collections import defaultdict
from copy import deepcopy
from csv import reader, writer
letters = []
for i in range(65, 91):
	letters.append(chr(i))

dist = defaultdict(lambda: inf)
edges = []
backup = []
def bellman_ford(city):
	global backup, dist
	dist = defaultdict(lambda: inf)
	dist[city] = 0
	for i in range(1, 6):
		backup = deepcopy(dist)
		for j in edges:
			dist[j[1]] = min(dist[j[1]], backup[j[0]] + j[2])
	return dist

with open('附件3.csv') as f:
	attachment3 = reader(f)
	for i in attachment3:
		try:
			fixed = float(i[2])
			edges.append((i[0], i[1], fixed))
		except:
			pass
shortest_paths = {}
for i in bellman_ford('A'):
	shortest_paths[i] = {}
	temp = bellman_ford(i)
	for j in letters:
		shortest_paths[i][j] = temp[j]
with open('第四问.csv', 'w') as f:
	write = writer(f)
	write.writerow(shortest_paths.keys())
	for i in shortest_paths:
		write.writerow(shortest_paths[i].values())

sum23 = 0
sum24 = 0
sum25 = 0
sum26 = 0
sum27 = 0
with open('附件2.csv') as f:
	attachment2 = reader(f)
	for i in attachment2:
		if i[0] == '2023/4/23' and shortest_paths[i[1]][i[2]] != inf:
			sum23 += shortest_paths[i[1]][i[2]] * (1 + (int(i[3]) / 200) ** 3)
		elif i[0] == '2023/4/24' and shortest_paths[i[1]][i[2]] != inf:
			sum24 += shortest_paths[i[1]][i[2]] * (1 + (int(i[3]) / 200) ** 3)
		elif i[0] == '2023/4/25' and shortest_paths[i[1]][i[2]] != inf:
			sum25 += shortest_paths[i[1]][i[2]] * (1 + (int(i[3]) / 200) ** 3)
		elif i[0] == '2023/4/26' and shortest_paths[i[1]][i[2]] != inf:
			sum26 += shortest_paths[i[1]][i[2]] * (1 + (int(i[3]) / 200) ** 3)
		elif i[0] == '2023/4/27' and shortest_paths[i[1]][i[2]] != inf:
			sum27 += shortest_paths[i[1]][i[2]] * (1 + (int(i[3]) / 200) ** 3)
print(sum23, sum24, sum25, sum26, sum27)
# 1007.4076212750001 1278.0700906000002 1133.2728157000001 985.9556935499996 1014.4105408999999