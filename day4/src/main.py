import time

def take_card(line):
    split = line.split(":")
    
    return split[0], split[1]

def read_card(input):
    split = input.split("|")
    winners = [int(i) for i in split[0].split(" ") if i != '']
    drawn = [int(i) for i in split[1].split(" ") if i != '']
    return winners, drawn

def calc_points(winners, drawn):
    
    matches = 0
    for draw in drawn:
        if draw in winners:
            matches+=1
    if matches > 0:
        return 2 ** (matches-1) 
    return 0

def calc_matches(winners, drawn):
    
    matches = 0
    for draw in drawn:
        if draw in winners:
            matches+=1 
    return matches

sum_of_points = 0

def main():
    cards = []
    lines = []
    start_time = time.time_ns()
    for line in file.readlines():
        lines.append(line)
        card, rest = take_card(line)
        winners, drawn = read_card(rest)
        cards.append((1,winners, drawn))
    print("--- %s reading the file: ---" % (time.time_ns() - start_time))
   
    
    for (i,card) in enumerate(cards):
        (occurences, winners, drawn) = card
        matches = calc_matches(winners, drawn)
        for j in range(i+1, i+matches+1):
            if j < len(cards):
                (_occurences, _winners, _drawn) = cards[j]
                cards[j] = (_occurences+occurences, _winners, _drawn)

    number_of_cards = 0
    for (occurences, _, _ ) in cards:
        number_of_cards+= occurences

    print(number_of_cards)


start_time = time.time_ns()
file = open('input.txt')
main()
print("--- %s seconds ---" % ((time.time_ns() - start_time)/(10**6)))

#part 1
# for line in file.readlines():
#     card, rest = take_card(line)
#     winners, drawn = read_cards(rest)
#     points = calc_points(winners,drawn)
#     sum_of_points+=points;
#     print(points)

# print(sum_of_points)
