import time

i = 0

def pr(string):
    print(string, i)


for _ in range(5):
    pr("hello world")
    i += 1
    time.sleep(1)
