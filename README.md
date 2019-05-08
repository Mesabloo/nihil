# The Blob FP language

Blob is a small FP language focused on implementing a basic linear type system.

## The type system

Even though Blob's type system may seem pointless to many people, it's actually of a great strength. Thanks to it, it is possible to write GC-free VMs easily, because of the automatic memory handling.
For example, a function whose signature is `f: a ⊸ b` would see its first argument forced to be consumed exactly once (`⊸ = →[1]`) in the function body.
As another example, a function `g: a →[3] b ⊸ a` would force the developer to use the first argument 3 times and the second argument once.

## Unicode characters mapping

Some unicode characters are part of the language itself, and may be used interchangeably with their ASCII correspondants.

| ASCII | Unicode |
|------:|:--------|
   |`->`|`→`|
   |`-o`|`⊸`|
   |`=>`|`⇒`|
   | `\`|`λ`|

## Code examples

#### The Fibonacci sequence

```haskell
fib: Num a ⇒ a ⊸ a
fib n = match !n with --  we clone “n”. I will choose later whether pattern matching auto-clones or not
    0  → 1
    1  → 1
    n' → fib (n' - 2) + fib (n' - 1)
```

#### Factorial calculation

```haskell
fact: Num a ⇒ a ⊸ a
fact n = match !n with --  we clone “n”. I will choose later whether pattern matching auto-clones or not
    0  → 1
    n' → n * fact (n' - 1)
```

#### The Ackermann function

```haskell
ack: Num a ⇒ a ⊸ a ⊸ a
ack n m = match !m with --  we clone “m”. I will choose later whether pattern matching auto-clones or not
    0  → n + 1
    m' → match !n with --  we clone “n”. I will choose later whether pattern matching auto-clones or not
        0  → ack (m' - 1) 1
        n' → ack (m' - 1) $ ack m (n' - 1)
```

## Roadmap

- [ ] Implementing a fully working λ language inside the REPL.
    The goal is basically to replicate a small Haskell.
- [ ] Making a VM using the Linear Abstract Machine scheme.
- [ ] Making a full compiler targetting NASM.