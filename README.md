# [University of Southampton](ecs.soton.ac.uk)
## COMP2212 - Programming Language Concepts
## DSL Coursework 
### By:
	- Konrad K Sobczak
	- Filip Kubacki
	- Michael Ryan

### Usage
Basic usage:
```bash
./csvql
```
This will trigger the Interactive Mode.
```sql
Interactive Mode - enter an expression: 
> SELECT * FROM FILE(A.csv) WHERE 1 == 1
```

You can also use the inline mode...

```bash
./csvql -l "SELECT * FROM FILE(A.csv) WHERE 1 == 1"
./csvql --inline "SELECT * FROM FILE(A.csv) WHERE 1 == 1"
```

... or the (prefered) file mode
```sql
--pr1.dsl file
SELECT $0, $2 FROM FILE(A.csv) WHERE $0 == $1
```
```bash
./csvql pr1.dsl
```

### Building
The recommended way is to build the software with Cabal.
```bash
cabal update && cabal build
```
Or you can compile it yourself. The required packages are: 
- alex
- happy
- split

### Syntax
The syntax is as follows: 
```sql
SELECT <SList> FROM <FList> [WHERE <Cond>]

Atr := $[0-9]+ | Int | Float | Text
SList := Atr [, SList]
FList := FILE(<filename>) | FILE(Text) | FList JOIN FList [ON Cond] | FList MERGE FList [ON Cond] | FList CROSS FList | FList UNION FList
Cond := Atr == Atr | Atr != Atr | Atr >= Atr | Atr <= Atr | Atr > Atr | Atr > Atr | Atr LIKE Pattern |  Atr NOT LIKE Pattern 
	|  Atr IN '[' SList ']' | Atr NOT IN '[' SList ']' | Cond AND Cond | Cond OR Cond | NOT Cond
```
