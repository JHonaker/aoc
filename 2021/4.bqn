sample ← 0
number ← '0' + 4
lines ← •FLines  (sample⊑⟨"", "sample"⟩) ∾ number ∾ ".in"

balls ← •BQN'⟨'⊸∾∾⟜'⟩'⊑lines

solutions ← {
    cols ← (↕5)=⌜5‿5⥊↕5
    rows ← ⍉˘cols
    rows∾cols
}

Bits ← '0'-˜⊢
Base ← {𝕨⊸×⊸+´𝕩}
ToNum ← {𝕊𝕩:
    charNums ← ((⟨⟩≢⚇1⊢)/⊢)' '((⊢-˜¬×+`)∘=⊔⊢) 𝕩
    10⊸Base∘Bits¨ charNums
}

cards ← >{>ToNum¨ 1↓𝕩}¨1↓((+`⟨⟩≡⚇1⊢)⊔⊢)1↓lines
nBalls ← ≠balls
nCards ← ≠cards
plays ← ∨`balls=⌜cards
h ← nBalls‿nCards⥊∨˝>(<˘solutions){𝕨≡𝕨∧𝕨∧𝕩}⌜<˘∘‿5‿5⥊plays
n‿w ← ⋈⟜⍋(≠-+˝)h
w ↩ ⊑w
n ↩ w⊑n
•Show (n⊏balls)×+´⥊(¬w⊏n⊏plays)×w⊏cards

n‿w ← ⋈⟜⍒(≠-+˝)h
w ↩ ⊑w
n ↩ w⊑n
•Show (n⊏balls)×+´⥊(¬w⊏n⊏plays)×w⊏cards
