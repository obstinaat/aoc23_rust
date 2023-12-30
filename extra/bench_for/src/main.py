import time
COUNT = 1000000000

# Record the start time
start_time = time.time()

# Code to be measured goes here
for _ in range(COUNT):
    # Some time-consuming operation
    pass

# Record the end time
end_time = time.time()

# Calculate and print the elapsed time
elapsed_time = end_time - start_time
print(f"Elapsed time: {elapsed_time} seconds")

