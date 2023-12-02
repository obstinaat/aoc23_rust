from enum import Enum
import re

file = open('input.txt')



sum_ids = 0

MAX_RED = 12
MAX_GREEN = 13
MAX_BLUE = 14

def getGameId(line):
    tag = re.search("Game [0-9]+:", line) #<re.Match object; span=(0, 7), match='Game 1:'>
    id = re.search("[0-9]+", tag.group(0)).group(0)
    return int(id)

def getSets(line):
    splits = line.split(";")
    return splits
    
class COLOR(Enum):
    RED = 1
    GREEN = 2
    BLUE = 3

def getColor(draw):
    if re.search("blue", draw) != None:
        return COLOR.BLUE
    if re.search("red", draw) != None:
        return COLOR.RED
    if re.search("green", draw) != None:
        return COLOR.GREEN

def getCount(draw):
    result = re.search("[0-9]+", draw)
    if result != None:
        return int(result.group(0))
    return 0

def getDraws(draws):
    return re.findall("[0-9]+ [a-z]+", draws)
    
for line in file.readlines():
    possible = True
    game_id = getGameId(line)
    sets = getSets(line)
    for set in sets:
        count_red = 0
        count_blue = 0
        count_green = 0

        for draw in getDraws(set):
            print("draw", draw)
            color = getColor(draw)
            count = getCount(draw)
            if color == COLOR.GREEN:
                count_green += count
            if color == COLOR.RED:
                count_red += count
            if color == COLOR.BLUE:
                count_blue += count
        
        if count_red > MAX_RED:
            possible= False
            break
        if count_blue > MAX_BLUE:
            possible= False
            break
        if count_green > MAX_GREEN:
            possible= False
            break

        print("red: ", count_red, "blue: ", count_blue, "green: ", count_green)
    if possible: 
        sum_ids += game_id

print(sum_ids)


