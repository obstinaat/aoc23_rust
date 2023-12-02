from enum import Enum
import re

file = open('input.txt')



sum_ids = 0
sum_pow = 0

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
    max_red = 0
    max_blue = 0
    max_green = 0

    for set in sets:
        count_red = 0
        count_blue = 0
        count_green = 0

        for draw in getDraws(set):
            color = getColor(draw)
            count = getCount(draw)
            if color == COLOR.GREEN:
                count_green += count
            if color == COLOR.RED:
                count_red += count
            if color == COLOR.BLUE:
                count_blue += count
        if count_red > max_red:
            max_red = count_red
        if count_blue > max_blue:
            max_blue = count_blue
        if count_green > max_green:
            max_green = count_green

        if count_red > MAX_RED:
            possible= False
        if count_blue > MAX_BLUE:
            possible= False
        if count_green > MAX_GREEN:
            possible= False

    if possible: 
        sum_ids += game_id
    power = max_red * max_green * max_blue
    sum_pow += power

print(sum_ids, sum_pow)


