nyancode
========

A Brainfuck dialect for NekoVM that uses variations of "nyan" as keywords.

Keywords:

| Keyword | Brainfuck equivalent |
| ------- | -------------------- |
| nyan    | >                    |
| Nyan    | <                    |
| nyaan   | +                    |
| Nyaan   | -                    |
| nyaaan  | .                    |
| Nyaaan  | ,                    |
| nyaaaan | [                    |
| Nyaaaan | ]                    |

Hello world program, mechanically translated from Brainfuck:

```
nyaan nyaan nyaan nyaan nyaan nyaan nyaan nyaan nyaaaan
nyan nyaan nyaan nyaan nyaan nyaaaan nyan nyaan nyaan
nyan nyaan nyaan nyaan nyan nyaan nyaan nyaan nyan nyaan
Nyan Nyan Nyan Nyan Nyaan Nyaaaan nyan nyaan nyan nyaan
nyan Nyaan nyan nyan nyaan nyaaaan Nyan Nyaaaan Nyan
Nyaan Nyaaaan nyan nyan nyaaan nyan Nyaan Nyaan Nyaan
nyaaan nyaan nyaan nyaan nyaan nyaan nyaan nyaan nyaaan
nyaaan nyaan nyaan nyaan nyaaan nyan nyan nyaaan Nyan
Nyaan nyaaan Nyan nyaaan nyaan nyaan nyaan nyaaan Nyaan
Nyaan Nyaan Nyaan Nyaan Nyaan nyaaan Nyaan Nyaan Nyaan
Nyaan Nyaan Nyaan Nyaan Nyaan nyaaan nyan nyan nyaan
nyaaan nyan nyaan nyaan nyaaan 
```

# Building

You need NekoVM (http://nekovm.org) and Ocaml compiler.

1. Build nyanc: ocamlc -o nyanc str.cma ./nyanc.ml
2. Build the chr moduke: nekoc ./chr.neko
3. Build your program: ./nyanc ./file.nyan > file.neko
4. Build generated NekoVM source: nekoc ./file.neko
5. Run it: neko ./file.n

# Limitations

NekoVM lacks chr() function, or it's buried to deep somewhere,
so Nyancode uses an incomplete implementation, some non-printable
characters are displayed as space.

NekoVM docs mention it has a function for reading from stdin,
but I couldn't get it to work, so "Nyaaan"/"," does nothing.
