y = 2
xs = [1,2,3]
count = 0
for i in range(y-1,y+2):
    for j in range(xs[0]-1, xs[-1]+2):
        count+=1
        print(count)
        print(i, j)