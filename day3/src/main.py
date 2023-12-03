file = open('input.txt')
import re

grid = [[]]
i,j = 0,0
for line in file.readlines():
    
    grid.append([])
    for char in line:
        grid[i].append(char)
        j+=1

    i+=1

print(len(grid), len(grid[0]))


def find_numbers_in_line(line):
    numbers = []
    positions = []
    for i in range(len(line)):
        if line[i].isdigit():
            num = int(line[i])
            if i == 0:
                numbers.append(num)
                positions.append([i])

            #adjacent case
            if i > 0 and line[i-1].isdigit():
                numbers[-1] = numbers[-1] * 10 + num
                positions[-1].append(i)
                
            if i > 0 and not line[i-1].isdigit():
                numbers.append(num)
                positions.append([i])
                pow = 0
            
    return numbers, positions

def is_symbol(s):
    return not s.isdigit() and not s=='.'

def has_adjacent(y, xs):
    lookup = []
    for i in range(y-1,y+2):
        for j in range(xs[0]-1, xs[-1]+2):
            if i>=0 and j>=0 and i<len(grid) and j<len(grid[0]):
                lookup.append((i,j))
    

    for (x,y) in lookup:
        print(x,y)
        if x<len(grid) and y<len(grid[x]):
            if is_symbol(grid[x][y]):
                return True
    
    return False
                
   
sum = 0 
for i in range(len(grid)):
    numbers, positions = find_numbers_in_line(grid[i])
    #find which numbers have an adjacent symbol
    print(grid[i])
    for j in range(len(numbers)):
        if has_adjacent(i, positions[j]):
            sum+= numbers[j]
        else:
            print(numbers[j], "is not adjacent")

print(sum)

