#!/usr/bin/env  cbqn

To ← {𝕨>𝕩? ⌽𝕩↓𝕨∾˜↕𝕨 ; 𝕨↓𝕩∾˜↕𝕩}
F ← ⥊⎉2 ¯1↓˘{{ ⟨a,b⟩‿⟨c,d⟩ ← 𝕩 ⋄ {a=c? {a‿𝕩}¨ b To d; b=d? {𝕩‿b}¨ a To c}‿⟨⟩ }˘2↕𝕩}

input← ⌽¨∾∾´F∘•BQN∘∾∘{"⟨⟨"∾𝕩∾"⟩⟩"}¨{(⊑"->"⊐𝕩)⊑"⟩,"‿'⟨'‿𝕩}¨¨•Flines "input.txt"
⟨maxCol,minCol⟩‿⟨maxRow,minRow⟩ ← <∘(⌈´∾⌊´)˘⍉>input
Disp ← (2×maxRow-minRow)⊸+∘(maxRow⊸-)⌾(1⊸⊑) #Displacement
source ← Disp ⟨0,500⟩
input ↩ Disp¨input

grid←(3+maxCol)‿(5×maxRow-minRow)⥊'.'
{ grid↩ '#'⌾(𝕩⊸⊑)grid ⋄ @}¨input

View←⟨0‿1,¯1‿1,¯1‿0,¯1‿¯1,0‿¯1⟩ -˜⌜<

SimStep ← { 
  l‿dl‿d‿dr‿r ← {𝕩⊑⎊'!'grid}¨li‿dli‿di‿dri‿ri ← View org ← 𝕩
  {d='.'? di;
   d='#'? {dr='.'?dri; dl='.'?dli; org};
   d='!'? @
  }
} 
Sim ← {
  step ← SimStep org ← 𝕩
  { step≡@? @;
    step≡org? org;
    Sim step
}}

AddSand ← {
  step ← Sim 𝕩
  {step≡@? @;
   grid↩ '#'⌾(step⊸⊑)grid
  }
}

sands ← ¯1
{𝕩 ⋄ sands +↩ 1 ⋄ AddSand source}•_while_{@≢𝕩} grid
•Show sands
grid↩(3+maxCol)‿∘⥊∾´'#'¨⌾(¯1⊸⊑)<˘grid
{𝕩 ⋄ sands +↩ 1 ⋄ AddSand source}•_while_{('#'≠source⊑𝕩)∧@≢𝕩} grid
•Show sands
•Show grid
