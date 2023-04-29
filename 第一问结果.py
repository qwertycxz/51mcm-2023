from csv import reader, writer
with open("附件1-权重.csv") as f:
    read = reader(f)
    for i in read:
        try:
            weight = tuple(map(float, i[1:]))
        except:
            pass
result = {}
with open("附件1-标准化.csv") as f:
    read = reader(f)
    for i in read:
        if (len(i[0])):
            result[i[0]] = 0
            for j in range(4):
                result[i[0]] += float(i[j + 1]) * weight[j]
print(sorted(result.items(), key = lambda d: d[1], reverse = True))
with open("第一问.csv", 'w') as f:
    write = writer(f)
    write.writerows(sorted(result.items(), key = lambda d: d[1], reverse = True))