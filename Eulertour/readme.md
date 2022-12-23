# Solutions to [Project Euler](https://projecteuler.net/archives)

## Problem-1

```apl
+´((0=3⊸|)∨(0=5⊸|))⊸/↕1000
```

## Problem-2

```apl
´(0=2⊸|)⊸/ ∾⟜(+´¯2⊸↑)⍟33 1‿1
```

## Problem-3
```apl
⊢´{ out ← ⟨⟩
  n←{out∾↩2⋄𝕩÷2}•_while_(0=2⊸|)𝕩
  {{out∾↩𝕩 ⋄ n÷↩𝕩⋄𝕩}•_while_{0=𝕩|n}𝕩 ⋄ 𝕩+2}•_while_{𝕩≤√n} 3
  {n>2? out∾↩n; @}
} 600851475143
```

## Problem-4
```
∾´<˘×⌜˜ 100+↕900
```

## Problem-5
```
1⊸+•_while_ {¬(∧´(2+↕20) (0=|)¨<)𝕩} 230000000
```

## Problem-17
```
≠∾{𝕩=0? "";
   𝕩=1? "one";
   𝕩=2? "two";
   𝕩=3? "three";
   𝕩=4? "four";
   𝕩=5? "five";
   𝕩=6? "six";
   𝕩=7? "seven";
   𝕩=8? "eight";
   𝕩=9? "nine";
   𝕩=10? "ten";
   𝕩=11? "eleven";
   𝕩=12? "twelve";
   𝕩=13? "thirteen";
   𝕩=15? "fifteen";
   𝕩=18? "eighteen";
   (𝕩≤19)∧𝕩≥14? (𝕊 𝕩-10)∾"teen";
   (𝕩≥20)∧𝕩<30? "twenty"∾(𝕊 10|𝕩);
   (𝕩≥30)∧𝕩<40? "thirty"∾(𝕊 10|𝕩);
   (𝕩≥40)∧𝕩<50? "forty"∾(𝕊 10|𝕩);
   (𝕩≥50)∧𝕩<60? "fifty"∾(𝕊 10|𝕩);
   (𝕩≥80)∧𝕩<90? "eighty"∾(𝕊 10|𝕩);
   (𝕩≥60)∧𝕩<100? (𝕊 ⌊𝕩÷10)∾"ty"∾(𝕊 10|𝕩);
   𝕩=1000? "onethousand";
   (100×⌊𝕩÷100)=𝕩? (𝕊 ⌊𝕩÷100)∾"hundred";
   (𝕩≥100)∧𝕩<1000? (𝕊 ⌊𝕩÷100)∾"hundredand"∾(𝕊 100|𝕩)
}¨1+↕1000
```

## Problem-18
```apl
input ← >≠⊸(↑¨)•BQN¨{' '=𝕩?'‿';𝕩}¨¨•FLines "input.txt"
decisions ← {∾{{⟨𝕨⟩⊸∾¨ 𝕩}´ {𝕩⊸⋈¨ {𝕩⊸+¨ ⟨1‿0,1‿1⟩}𝕩}⌾(¯1⊸⊑) 𝕩}¨𝕩}⍟(1-˜≠input) ⟨⟨0‿0⟩⟩
⌈´+´¨decisions ⊑ input
```
