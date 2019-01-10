# shiki
Shiki is the rust-like programming language.


## Setup
```
$ git clone https://github.com/mizdra/shiki
$ cd shiki
$ cargo build
```


## Usage
```
## REPL
$ cargo run --bin shiki --features="repl"
>> -1 + 2 * 3 - (4 + 5) * 6;
-49
>> let val = "Hello World";
()
>> val
"Hello World"
>> let pow = |a, b| { if b <= 1 { return a; } a * pow(a, b - 1) };
()
>> pow(3, 3)
27
>> let sum = 0;
()
>> while sum < 10 { sum = sum + 1; }
()
>> sum
10
>> undefined_val
runtime error: cannot find identifier `undefined_val` in this scope
>> "str" - "s"
runtime error: no implementation for `String - String`
>> return 1;
runtime error: cannnot return from outside of lambda expression
>> let invalid := 1;
parse error: expected `=`, found `:`
>> ^C
bye

## Test
$ cargo test
```

## License
- MIT License
- This software is made based on [tsuyoshiwada/rs-monkey-lang](https://github.com/tsuyoshiwada/rs-monkey-lang) ([License](https://raw.githubusercontent.com/tsuyoshiwada/rs-monkey-lang/2189321538e58416708bea361b1c080a3f9c7c49/LICENSE)).
