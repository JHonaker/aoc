Stencil ← { 𝕨𝕊𝕩:
    shape ← (≠𝕨)↑≢𝕩
    frame ← 1+shape-𝕨
    mult ← ⌽×`⌽1↓shape∾1
    i ← (+´(∾˜mult)×⊢)⚇1↕frame∾𝕨
    i ⊏ 𝕩⥊˜(≠𝕨)(×´∘↑∾↓)(≢𝕩)
}

arr ← 6‿10‿10⥊↕600

≢st ← 3‿5 Stencil arr
≢win ← 3‿5 ↕ arr       # The buggy line

3↕arr