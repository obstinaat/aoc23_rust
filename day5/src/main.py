import re

file = open('input.txt')

def parse_nums(line):
    return [int(x) for x in (re.findall('[0-9]+', line))]

def read_seeds(lines):
    line = lines.pop(0)
    seeds = parse_nums(line)
    lines.pop(0) #remove empty line
    return (seeds, lines)



def read_seed_to_soil(lines):
    lines.pop(0) #seed-to-soil map:
    seed_to_soil = []
    line = lines.pop(0)
    while line != '\n':
        seed_to_soil.append(parse_nums(line))
        line = lines.pop(0)
        
    return(seed_to_soil, lines)

def read_soil_to_fertilizer(lines):
    lines.pop(0) #seed-to-soil map:
    soil_to_fertilizer = []
    line = lines.pop(0)
    while line != '\n':
        soil_to_fertilizer.append(parse_nums(line))
        line = lines.pop(0)
    return(soil_to_fertilizer, lines)

def read_fertilizer_to_water(lines):
    lines.pop(0) #seed-to-soil map:
    fertilizer_to_water = []
    line = lines.pop(0)
    while line != '\n':
        fertilizer_to_water.append(parse_nums(line))
        line = lines.pop(0)
    return(fertilizer_to_water, lines)

def read_water_to_light(lines):
    lines.pop(0) #seed-to-soil map:
    water_to_light = []
    line = lines.pop(0)
    while line != '\n':
        water_to_light.append(parse_nums(line))
        line = lines.pop(0)
    return(water_to_light, lines)

def read_light_to_temperature(lines):
    lines.pop(0) #seed-to-soil map:
    light_to_temperature = []
    line = lines.pop(0)
    while line != '\n':
        light_to_temperature.append(parse_nums(line))
        line = lines.pop(0)
    return(light_to_temperature, lines)

def read_temperature_to_humidity(lines):
    lines.pop(0) #seed-to-soil map:
    temperature_to_humidity = []
    line = lines.pop(0)
    while line != '\n':
        temperature_to_humidity.append(parse_nums(line))
        line = lines.pop(0)
    return(temperature_to_humidity, lines)

def read_humidity_to_location(lines):
    lines.pop(0) #seed-to-soil map:
    humidity_to_location = []
    line = lines.pop(0)
    while line != '\n':
        humidity_to_location.append(parse_nums(line))
        if len(lines)==0:
            return(humidity_to_location, lines)
        line = lines.pop(0)
    return(humidity_to_location, lines)


lines = file.readlines()

def route(dest, map):
    for plausibleroute in map:
        rangestart=plausibleroute[1]
        rangeend=plausibleroute[1]+plausibleroute[2]
        if dest >= rangestart and dest < rangeend:
            position_in_range = dest-rangestart
            return plausibleroute[0]+position_in_range
    return dest

def reverse_route(dest, map):
    for plausibleroute in map:
        rangestart=plausibleroute[0]
        rangeend=plausibleroute[0]+plausibleroute[2]
        if dest>= rangestart and dest < rangeend:
            position_in_range = dest-rangestart
            return plausibleroute[1]+position_in_range
    return dest

(seeds, rest) = read_seeds(lines)
(seed_to_soil, rest) = read_seed_to_soil(rest)
(soil_to_fertilizer, rest) = read_soil_to_fertilizer(rest)
(fertilizer_to_water, rest) = read_fertilizer_to_water(rest)
(water_to_light, rest) = read_water_to_light(rest)
(light_to_temperature, rest) = read_light_to_temperature(rest)
(temperature_to_humidity, rest) = read_temperature_to_humidity(rest)
(humidity_to_location, rest) = read_humidity_to_location(rest)

def print_all_maps():
    print(seeds)
    print(seed_to_soil)
    print(soil_to_fertilizer)
    print(fertilizer_to_water)
    print(water_to_light)
    print(light_to_temperature)
    print(temperature_to_humidity)
    print(humidity_to_location)


locations = []
# print_all_maps()

###### PART 1
def traverse(seed):
    soil = route(seed,seed_to_soil)
    print(soil)
    fertilizer = route(soil, soil_to_fertilizer)
    print(fertilizer)
    water = route(fertilizer, fertilizer_to_water)
    print(water)
    light = route(water, water_to_light)
    print(light)
    temperature = route(light, light_to_temperature)
    print(temperature)
    humidity = route(temperature, temperature_to_humidity)
    print(humidity)
    location = route(humidity, humidity_to_location)
    print(location)
    return location

def reverse_traverse(location):
    humidity = reverse_route(location, humidity_to_location)
    # print("humidity:", humidity)
    if humidity==-1:
        return False
    temperature = reverse_route(humidity, temperature_to_humidity)
    # print(temperature)
    if temperature==-1:
        return False
    light = reverse_route(temperature, light_to_temperature)
    # print(light)
    if light==-1:
        return False
    water = reverse_route(light, water_to_light)
    # print(water)
    if water==-1:
        return False
    fertilizer = reverse_route(water, fertilizer_to_water)
    # print(fertilizer)
    if fertilizer==-1:
        return False
    soil = reverse_route(fertilizer, soil_to_fertilizer)
    # print(soil)
    if soil==-1:
        return False
    seed = reverse_route(soil, seed_to_soil)
    if seed==-1 or seed==0:
        return False    
    if seed_exists(seed):
        return seed
    else:
        return False

global pairs
pairs = []
for i in range(0,len(seeds), 2):
    pair = (seeds[i], seeds[i+1])
    pairs.append(pair)
pairs = sorted(pairs)

def seed_exists(seed):
    for pair in pairs:
        (min,num) = pair
        if seed>= min and seed <= min+num:
            return True
    return False

seed = reverse_traverse(1)
print(traverse(504595750))
# while(seed==False):
#     seed = reverse_traverse(i)
#     if i%1000000==0:
#         print("checking:::", i)
#     i=i+1
# print("seed", seed)

print("end")
# print(min(locations))
# minlocation = 99999999999
# newseeds = []
# pairs = []


# print(pairs)

# #We remove overlaps on pairs based 
# pairs = sorted(pairs)
# for i in range(len(pairs)-1):
#     (min, max) = pairs[i]
#     (min2,max2) = pairs[i+1]
#     if max > min2 and max < max2:
#         diff = max-min2
#         pairs[i+1] = (max,max2)
#         print("threw away", diff, "seeds")



# for j in range(3000312676, 3900312676):
#     soil = route(j,seed_to_soil)
#     fertilizer = route(soil, soil_to_fertilizer)
#     water = route(fertilizer, fertilizer_to_water)
#     light = route(water, water_to_light)
#     temperature = route(light, light_to_temperature)
#     humidity = route(temperature, temperature_to_humidity)
#     location = route(humidity, humidity_to_location)
#     if location < minlocation:
#         minlocation = location
#         print(minlocation,j)


