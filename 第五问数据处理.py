from collections import defaultdict
from csv import reader, writer
from datetime import date, datetime, timedelta
from math import inf
from statistics import mean
deliver_time = defaultdict(lambda: [])
deliver_route = {}
can_deliver = {}
no_zero = {}
routes = set()
oneday = timedelta(1)
day = date(2020, 4, 28)
with open('附件2.csv') as f:
	attachment2 = reader(f)
	for i in attachment2:
		try:
			PCS = int(i[-1])
			routes.add(i[1] + i[2])
			time = datetime.strptime(i[0], '%Y/%m/%d')
			season = i[0][0:4] + str((time.month - 1) // 3 + 1)
			if season not in deliver_route:
				deliver_route[season] = defaultdict(lambda: [])
				if PCS:
					no_zero[season] = defaultdict(lambda: [])
			deliver_route[season][i[1] + i[2]].append(PCS)
			if PCS:
				no_zero[season][i[1] + i[2]].append(PCS)
				deliver_time[i[1] + i[2]].append(PCS)
			if i[1] + i[2] not in deliver_route:
				deliver_route[i[1] + i[2]] = []
			deliver_route[i[1] + i[2]].append(PCS)
		except:
			pass
result = defaultdict(lambda: [])
for i in no_zero:
	for j in sorted(no_zero[i]):
		result[i].append([j, *no_zero[i][j]])
for i in result:
	with open('附件2-分季度分布/' + i + '季度.csv', 'w') as f:
		writer(f).writerows(result[i]) # 手动转置
all_result = []
for i in sorted(deliver_time):
	all_result.append([i, *deliver_time[i]])
with open('附件2-全分布.csv', 'w') as f:
	writer(f).writerows(all_result) # 手动转置
for i in deliver_route:
	if i[0] != '2':
		continue
	for j in deliver_route[i]:
		for k, v in enumerate(deliver_route[i][j]):
			if v == 0:
				deliver_route[i][j][k] = inf
with open('附件2-季度最小值.csv', 'w') as f:
	write = writer(f)
	write.writerow(sorted(routes))
	row = 1
	for i in deliver_route:
		row = list()
		if i[0] == '2':
			for j in routes:
				if len(deliver_route[i][j]):
					if (min(deliver_route[i][j]) == 0):
						print(i, j)
					row.append(min(deliver_route[i][j]))
				else:
					row.append(mean(deliver_route[j]))
			write.writerow(row)