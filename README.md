# Black Programming Language
A simple language that I am working on to practice programming language implementation.

I am planning to implement some of the features in the example compiler used at the 
University of Kent for the programming language implementation module.  The Syntax 
of this language is demonstrated below however the final syntax of black may be 
completely different. 
```
{
  ident := 1;
  while (ident < 100) {
    ident := ident + 1;
  }
}
```

Which is equivilent to the c code
```
int main(){
  int ident = 1;
  while (ident < 100){
    ident++;
  }
  
  return 0;
}
```

## Syntax Overview
Black programs consist of a series of statements. There's one root statement that must be a code block. The language currently support while loops, if statements and assignments. 

### Code blocks
Code blocks are sections of code that will contain scope. They can modify the parents code blocks scope however any variables declared inside a code block are local to that block.

```
{
  <statements>
}
```
### Assignments
There's currently only 1 data type and that is integers. Unlike C, variables have to be declared and initialised at the same time. 
```
<lidentifier> := <expression>;
```
```
a := 1;
```
### While 
```
while (<expression>) <code block>
```
```
while (i < 100) {
  i := i + 1;
}
```

### If
If statements consist of a condition, code block and optionaly an else statement. Else statements consist of either a code block or another if statement. 

```
if (<expression>) <codeblock>
[else <codeblock | if>]
```
```
if (a < b) {
  c := 2;
} else if (a > b) {
  c := 2;
}
```

