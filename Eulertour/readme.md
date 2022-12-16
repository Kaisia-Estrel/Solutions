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
