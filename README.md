# The Blob FP language

Blob is a small FP language focused on implementing a basic linear type system.

<details>

<summary>The type system</summary>
<br>
Even though Blob's type system may seem pointless to many people, it's actually of a great strength. Thanks to it, it is possible to write GC-free VMs easily, because of the automatic memory handling.
For example, a function whose signature is <code>f: a ⊸ b</code> would see its first argument forced to be consumed exactly once (<code>⊸ = →[1]</code>) in the function body.
As another example, a function <code>g: a →[3] b ⊸ a</code> would force the developer to use the first argument 3 times and the second argument once.

</details>
<details>

<summary>Unicode characters mapping</summary>
<br>
Some unicode characters are part of the language itself, and may be used interchangeably with their ASCII correspondants.

| ASCII | Unicode |
|------:|:--------|
   |`->`|`→`|
   |`-o`|`⊸`|
   |`=>`|`⇒`|
   | `\`|`λ`|
   |`::`|`∷`|

</details>
<details>

<summary>Code examples</summary>

#### The Fibonacci sequence

```haskell
fib: Num a ⇒ a ⊸ a
fib n = match n with --  we clone “n”.
    0  → 1
    1  → 1
    n' → fib (n' - 2) + fib (n' - 1)
```

#### Factorial calculation

```haskell
fact: Num a ⇒ a ⊸ a
fact n = match n with --  we clone “n”.
    0  → 1
    n' → n * fact (n' - 1)
```

#### The Ackermann function

```haskell
ack: Num a ⇒ a ⊸ a ⊸ a
ack n m = match m with --  we clone “m”.
    0  → n + 1
    m' → match n with --  we clone “n”.
        0  → ack (m' - 1) 1
        n' → ack (m' - 1) $ ack m (n' - 1)
```

</details>
<details>

<summary>Roadmap</summary>
<br>

- [ ] Implementing a fully working λ language inside the REPL.
    The goal is basically to replicate a small Haskell.
- [ ] Making a VM using the Linear Abstract Machine scheme.
- [ ] Making a full compiler targetting NASM.

</details>