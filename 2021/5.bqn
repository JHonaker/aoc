Bits ← '0'-˜⊢
Base10 ⇐ 10⊸×⊸+˜´∘⌽
Number ⇐ Base10 -⟜'0'
Ints ⇐ {Number¨((¯1+⊢×·+`»⊸<)𝕩∊'0'+↕10)⊔𝕩}


Parse ← { (Base10∘Bits¨)¨ ','⊸Split¨ 1‿0‿1/' ' Split 𝕩 }

l ← {<˘2‿2⥊𝕩}¨ Ints¨ •FLines "sample5.in"
vs ← {s‿e: s<⊸+(<×e-s)×↕1+⌈´|e-s}¨ l
# For each take the start and end
# ⌈´|e-s              - Maximum of the absolute value of coordinates of end - start -- biggest change in x or y
# ↕1+⌈´|e-s           - Range from 0 to above
# (<×e-s)             - Enclose the sign of e-s -- The slope of the line wrt time
# (<×e-s)×↕1+⌈´|e-s   - coordinates in the line as a vector from origin
# s<⊸+(above)        - enclose start and add to each point to get line as vector from start

{•Show +´1=⊒∾𝕩}¨ vs⋈˜vs/˜(∨´=´)¨l
# vs⋈˜vs/˜(∨´=´)¨l
# (∨´=´)¨l - mask that determines if x1=x2 or y1=y2 (i.e. a vertical/horizontal line)
# vs⋈˜vs/˜mask = (mask/vs) ⋈ vs (i.e problem 1 data or problem 2 data)
# +´1=⊒∾𝕩
#  ∾𝕩    - Join all vertices together
#  ⊒∾𝕩   - Return the number of times the major cell (vertex) has appears before this index
#  +´1=⊒∾x - If something has appeared once already than it's appeared ≥2 times, so add these up (don't count additional times)


# OnLine ← {⟨x1, y1⟩‿⟨x2, y2⟩𝕊⟨x, y⟩:
#         {
#             y1=y2 ? (y1=y)∧0≥×´⟨x1, x2⟩-x;
#             x1=x2 ? (x1=x)∧0≥×´⟨y1, y2⟩-y;
#             0
#         }
# }

# biggest ← ⌈´⥊>l
# •Show +´⥊2≤+˝(<˘l) OnLine⌜ ⥊⋈⌜˜↕1+biggest