## SMT-LIBv2 format まとめ
created: 2023/04/24
last updated: 2023/04/25
by ez2

reference: [smt-lib-reference-v2.0](https://smtlib.cs.uiowa.edu/papers/smt-lib-reference-v2.0-r10.12.21.pdf), Clark Barrett, Aaron Stump,Cesare Tinelli,2010 
上記の公式リファレンスをまとめました.具体例など所々引用がありますが,明示は省略しています.ご了承下さい.  
Excuse me for some implicit quotes from the referene.

### 1. Basic Syntax
構文の説明前半です.以下のような順番で説明します.
1. 許可される文字集合について
2. 空白部分の説明
3. トークンおよび各トークンの定義
4. 予約語

#### 1.1. Char Set
* Only allowed character set is ascii.
#### 1.2. White Spaces
* comment out starts by semicolon, ';'.
* *whitespace* := comments | ' ' | '\t' | '\n'

#### 1.3. Tokens
I mostly abbreviated quotes and double quotes to avoid confusion. For simplicity, regex-like notation is used.
* *token* 
    := ( | ) | *\<numeral>* | *\<decimal>* | *\<hexadecimal>* | *\<binary>* | *\<string>* | *\<symbol>* | *\<keyword>* | *\<reserved word>*

###### Numerals
* *\<numeral>* := 0 | [1-9][0-9]*

###### Decimals(小数)
* *\<decimal>* := *\<numeral>*.0\**\<numeral>*

###### Hexdecimals
* *\<hexadecimal>* := #x[0-9a-fA-F]+
>(e.g. #xA04, #x01ab)

###### Binaries
* *\<binary>* := #b[01]+
>(e.g. #b0110)

###### String literals
* *\<string>* := Any sequence of printable ascii characters delimited by double quotes. Escape sequences are only \\" and \\\\. They are treated as " and \\, respectively.
>(e.g. "this is a string literal", "one\n two”, "She said : \\"Hello ! \\" " "Here is a backslash : \\\\")

###### Symbols
* *\<symbol>* := *\<simple_symbol>* | *\<quoted_symbol>*
*    *\<simple_symbol>* := [A-Za-z\~!@\$%\^&\*\_-+=\<\>.?/][0-9A-Za-z\~!@\$%\^&\*\_-+=\<\>.?/]*
>(e.g. +, <=, x, plus, ∗∗, \$, \<sas, \<adf\>, abc77, ∗\$s&6, ., kkk, +34−32)

* *\<quoted_symbol>* := |[\^\\]*|

> | this is a single quoted symbol |
> | |
> | af klj ˆ∗( 0 asfsfe2(&∗)&(#ˆ\$> > >?” ’]]984|)

* Supplemental info about *\<symbol>* ... case insensitive, *\<simple_symbol>* enclosed in vertical bars and one without bars are the same. (e.g. |abc| and abc are identical as a symbol), mainly used as an opecode or an identifier.

###### Keywords
* *\<keyword>* := :[0-9A-Za-z\~!@\$%\^&\*\_-+=\<\>.?/]+
    - always starts with comma ':'.
    - Use *\<keyword>* for either *\<attribute>* or *\<option>*. They will be explained later, but I guess *\<attribute>* is used to provide some meta information, and *\<option>* is just a solver-specific option.
>(e.g. :date, :a2, :foo−bar, :<=, :56, :−>)

#### 1.4. Reserved Words
* *\<reserved word>*
:= one of { par, NUMERAL, DECIMAL, STRING, '\_', ' ! ', as, let, forall, exists } | *\<command>* | another additional reserved word
    - SMT-LIBv2 format has inline scripting commands, and they are also counted as reserved. There might be a further explanation later in this article.
    - *\<command>*
        := ( set-logic *\<symbol>* )
         | ( set-option *\<option>* )
         | ( set-info *\<attribute>* )
         | ( declare-sort *\<symbol>* *\<numeral>* )
         | ( define-sort *\<symbol>* *\<symbol>*\* *\<sort>* )
         | ( declare-fun *\<symbol>* *\<sort>*\* *\<sort>* )
         | ( define-fun *\<symbol>* *\<sorted_var>*\* *\<sort>* *\<term>* )
         | ( push *\<numeral>* )
         | ( pop *\<numeral>* )
         | ( assert *\<term>* )
         | ( check-sat )
         | ( get-assertions )
         | ( get-proof )
         | ( get-unsat-core )
         | ( get-value *\<term>*+ )
         | ( get-assignment )
         | ( get-option *\<keyword>* )
         | ( get-info *\<info_flag>* )
         | ( exit )

### 2. Higher Syntax
構文説明の後半.上で定義されたトークンを組み合わせて,以下のように高次の要素が定義されていきます.

1. トークン列(S-expressions)
2. 識別子(Identifiers)
3. 属性(Attributes)
4. 型(Sorts)
5. 語句または式(Terms)
6. 拡張定義(Theory Declarations)

#### 2.1 S-expressions
S-expression is either a non-parenthesis single token, or a bracketed sequence of zero or more tokens. Some tokens that expected to have static values are called *\<spec-constant>*, but don't be fooled by each token category's name. Note that the precise semantics of *\<spec-constant>* can be defined by the user of SMT-LIBv2, with its including language: "theory" which you may see later.
* *\<spec-constant>* := *\<numeral>* | *\<decimal>* | *\<hexadecimal>* | *\<binary>* | *\<string>*
* *\<s_expr>* := *\<spec-constant>* | *\<symbol>* | *\<keyword>* | (*\<s_expr>*\*)
#### 2.2 Identifiers
Identifier is a wrapper to index a *\<symbol>*. It is to share names without the risk of conflict.
  - *\<identifier>* := *\<symbol>* | ( _ *\<symbol> \<numeral>*+) 
>e.g. Bar, ( _ Foo 10 )
#### 2.3 Attributes
Attributes are generally pairs of a name and a value, although attributes with no value are ok. *\<attribute_value>* is equivalent to *\<s_expr>* without *\<keyword>*.
* *\<attribute_value>* := *\<spec_constant>* | *\<symbol>* | (*\<s_expr>*\*)
* *\<attribute>* := *\<keyword>* | *\<keyword> \<attribute_value>*
>e.g.
:left-assoc
:status unsat
:my_attribute ( humpty dumpty )
:authors ”Jack and Jill”

#### 2.4 Sorts
*\<sort>* is used as a type, especially to type *\<term>*.
* *\<sort>* := *\<identifier>* | ( *\<identifier> \<sort>*+)
>e.g.
Bool
( _ BitVec 3 )
( List ( Array Int Real ) )

#### 2.5 Terms
If it is used in a most simply way, *\<term>* is a special constant symbol, a variable, a function symbol, or the adoption of a funcion symbol to one or more terms (a function symbol without arguments is used as a constant symbol).
* *\<qual_identifier>* := *\<identifier>* | (as *\<identifer> \<sort>*)
* *\<var_binding>* := (*\<symbol> \<term>*)
* *\<sorted_var>* := ( *\<symbol> \<sort>*)
* *\<term>* := *\<spec_constant>*
| *\<qual_identifier>*
| (*\<qual_identifier> \<term>*+)
| (let (*\<var_binding>*+) *\<term>*)
| (forall (*\<sorted_var>*+) *\<term>*)
| (exists (*\<sorted_var>*+) *\<term>*)
| (! *\<term> \<attribute>+)
```
e.g.
(forall ((x (List Int)) (y (List Int)))
    (= (append x y)
        ite (= x (as nil (List Int)))
            y
            (let ((h (head x)) (t (tail x)) ) (insert h (append t y)))
    )
)
```

#### 2.6 Theory Declarations