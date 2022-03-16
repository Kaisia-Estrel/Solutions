# Solutions to [Project Euler](https://projecteuler.net/archives)

## Problem-1

```apl
{+/⍵/⍨(0=5|⍵)∨0=3|⍵}⍳999
```

## Problem-2

```apl
+/{⍵/⍨0=2|⍵}{⍵/⍨4000000>⍵}({⍵,+/¯2↑⍵} ⍣ 50) 1 1
```

## Problem-3
(using dfns)
```apl
⊢/factors 600851475143
```

## Problem-4

## Problem-5
