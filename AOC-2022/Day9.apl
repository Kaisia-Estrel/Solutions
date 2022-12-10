⎕IO←0 ⋄ s ← ↓⍉9 11∘.○+\0j1*'RULD'⍳⊃,/({⍎2↓⍵}⊢⍤/⊃)¨⊃⎕NGET'input.txt' 1
sim ← {⍺{(a b)←⍺-⍵ ⋄ a∧⍥(1≥|)b}⍵: ⍺ ⋄ ⍺+×⍵-⍺}
solve ← { z ← 0 0 ⋄ {z⊢← z sim ⍵ ⋄ z}¨⍵}
≢∪solve s
≢∪(solve⍣9) s
