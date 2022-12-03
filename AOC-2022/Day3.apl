⍝ First problem
+/{⍵≥97:⍵-96⋄⍵-38 }¨ ⎕UCS ⊃¨∩/↑{⍵⊆⍨{1+(2÷⍨≢⍵)<⍳≢⍵}⍵}¨¯1∘↓⊃⎕NGET 'input.txt' 1
⍝ Second Problem
+/{⍵≥97:⍵-96⋄⍵-38 }¨ ⎕UCS⊃∘⊃¨∩/¨{⍵⊂[1]⍨(≢⍵)⍴3↑1}¯1∘↓⊃⎕NGET 'input.txt' 1
