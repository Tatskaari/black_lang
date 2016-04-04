# Black Programming Language
A simple language that I am working on to practice programming language implementation.

I am planning to implemnt some of the features in the example compiler used at the 
University of Kent for the programming language implementation module.  The Syntax 
of this language is demonstrated below however the final syntax of black may be 
completely different. 
```
{
  ident := 1;
  while ident < 100; {
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
