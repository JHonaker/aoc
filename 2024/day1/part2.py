
with open("input", "r") as f:
    lines = f.readlines()

ints = (l.split("   ") for l in lines)
ints = ((int(xs[0]), int(xs[1])) for xs in ints)
left, right = zip(*ints)
counts = {}

for r in right:
    counts[r] = counts.get(r, 0) + 1

similarity = sum(l * counts.get(l, 0) for l in left)


print("=====")

for l in sorted(left):
    print(f"{l}\t{counts.get(l, 0)}")
    
print(similarity)


