l ← •FLines "aoc21/2.in"
dir ← ⊑¨l
amt ← '0'-˜¯1⊑¨l

h ← +´amt×-˝"fb"=⌜dir
v ← +´amt×-˝"du"=⌜dir
h×v

aim ← +`amt×-˝"du"=⌜dir
v1 ← +´amt×aim×'f'=dir
h1 ← +´amt    ×'f'=dir
h1×v1