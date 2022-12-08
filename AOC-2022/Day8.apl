input ← ⍎¨¨⊃⎕NGET 'input.txt' 1
visibility ← {s←⍬⋄{x←∧/⍵>s⋄s⊢←(s,⍵)⋄x}¨⍵}
distance ← {s←⍬
  { ∧/⍵>s: {x ← ≢s ⋄ s⊢←(⍵,s) ⋄ x}⍵
           {x ← 1++/∧\⍵>s ⋄ s⊢←(⍵,s) ⋄ x}⍵
  }¨⍵}
+/,⊃∨/↑¨{(a b c d)←⍵ ⋄a (⌽¨b) (↓⍉↑c) (↓⍉↑⌽¨d)} visibility¨¨{⍵ (⌽¨⍵) (↓⍉↑⍵) (⌽¨↓⍉↑⍵)} input
⌈/,⊃×/↑¨{(a b c d)←⍵ ⋄a (⌽¨b) (↓⍉↑c) (↓⍉↑⌽¨d)} distance¨¨{⍵ (⌽¨⍵) (↓⍉↑⍵) (⌽¨↓⍉↑⍵)} input
