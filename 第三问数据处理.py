from csv import reader, writer
from datetime import date, timedelta
from math import nan
can_deliver = {}
routes = set()
oneday = timedelta(1)
day = date(2020, 4, 28)
with open('附件2.csv') as f:
	attachment2 = reader(f)
	for i in attachment2:
		try:
			PCS = int(i[-1])
			routes.add(i[1]+i[2])
		except:
			pass
# while day.year != 2023 or day.month != 4 or day.day != 27:
# 	temp = str(day.year) + '/' + str(day.month) + '/' + str(day.day)
# 	can_deliver[temp] = {}
# 	for k in sorted(routes):
# 		can_deliver[temp][k] = nan
# 	day += oneday
with open('附件2.csv') as f:
	attachment2 = reader(f)
	for i in attachment2:
		try:
			PCS = int(i[-1])
			if i[0] not in can_deliver:
				can_deliver[i[0]] = {}
			can_deliver[i[0]][i[1]+i[2]] = 1
		except:
			pass
for i in can_deliver:
	for j in routes:
		if j not in can_deliver[i]:
			can_deliver[i][j] = 0
result = {}
for i in can_deliver:
	for j in sorted(can_deliver[i]):
		if i not in result:
			result[i] = []
		result[i].append(can_deliver[i][j])
with open('附件2-时间序列预处理.csv', 'w') as f:
	write = writer(f)
	write.writerow(sorted(routes))
	row = 1
	for i in result:
		# if row == 截断数值:
		# 	break
		write.writerow(result[i])