from csv import reader, writer
letters = []
for i in range(65, 91):
	letters.append(chr(i))
numbers = {}
index = 0
for i in letters:
	numbers[i] = index
	index += 1
deliver, receive, every = {}, {}, {}
with open('附件1.csv') as f:
	attachment1 = reader(f)
	for i in attachment1:
		try:
			PCS = int(i[-1])
			if i[0] not in deliver:
				deliver[i[0]] = [0] * 26
			deliver[i[0]][numbers[i[1]]] += PCS
		except:
			pass
	for i in attachment1:
		try:
			PCS = int(i[-1])
			if i[0] not in receive:
				receive[i[0]] = [0] * 26
			receive[i[0]][numbers[i[2]]] += PCS
		except:
			pass
for i in deliver:
	every[i] = deliver[i]
for i in receive:
	if i not in every:
		every[i] = 0
	every[i] += receive[i]
with open('附件1-趋势预处理.csv', 'w') as f:
	write = writer(f)
	write.writerow(letters)
	for i in every:
		write.writerow(every[i])
pass