#!/usr/bin/env dyalog -script DYALOG_LINEEDITOR_MODE=1
'display' 'disp'⎕CY'dfns'

⍝solution1
rps1←{
    0=≢⍵: 0
    ∧/⍵='C X': 7
    ∧/⍵='A Y': 8
    ∧/⍵='B Z': 9
    ∧/⍵='B X': 1
    ∧/⍵='C Y': 2
    ∧/⍵='A Z': 3
    ∧/⍵='A X': 4
    ∧/⍵='B Y': 5
    ∧/⍵='C Z': 6
  }
display +/rps1¨⊃⎕NGET'input.txt'1

⍝solution2
rps2←{
    0=≢⍵: 0
    ∧/⍵='C X': 2
    ∧/⍵='A Y': 4
    ∧/⍵='B Z': 9
    ∧/⍵='B X': 1
    ∧/⍵='C Y': 6
    ∧/⍵='A Z': 8
    ∧/⍵='A X': 3
    ∧/⍵='B Y': 5
    ∧/⍵='C Z': 7
  }
display +/rps2¨⊃⎕NGET'input.txt'1
